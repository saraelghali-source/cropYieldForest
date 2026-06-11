#' Résumé statistique des parcelles
#'
#' Calcule les statistiques descriptives du rendement et des indices
#' de végétation par parcelle et par culture. Produit un tableau
#' synthétique prêt pour l'export.
#'
#' @param fields Objet sf ou data.frame des parcelles.
#' @param features data.frame des features (issu de \code{extract_features}).
#'   NULL par défaut (utilise uniquement les colonnes de fields).
#' @param target Nom de la colonne rendement (défaut: "yield").
#' @param group_by_crop Grouper par type de culture ? (défaut: TRUE).
#'
#' @return data.frame avec les statistiques :
#'   n, yield_mean, yield_sd, yield_min, yield_max,
#'   ndvi_mean (si disponible).
#'
#' @examples
#' \dontrun{
#' summary_df <- summarize_fields(
#'   fields   = fields$sf_object,
#'   features = features_df
#' )
#' print(summary_df)
#' }
#'
#' @export
summarize_fields <- function(fields,
                             features      = NULL,
                             target        = "yield",
                             group_by_crop = TRUE) {

  # Extraction du data.frame
  if (inherits(fields, "sf")) {
    df <- sf::st_drop_geometry(fields)
  } else {
    df <- fields
  }

  # Jointure des features si fournis
  if (!is.null(features)) {
    common_id <- intersect(names(df), names(features))
    common_id <- setdiff(common_id, c(target, "geometry"))
    if (length(common_id) > 0) {
      df <- merge(df, features, by = common_id[1], all.x = TRUE, suffixes = c("", ".feat"))
    } else {
      df <- cbind(df, features[, !names(features) %in% names(df), drop = FALSE])
    }
  }

  if (!target %in% names(df)) {
    stop("Colonne cible '", target, "' introuvable.")
  }

  # ── Résumé global ─────────────────────────────────────────────────────────────
  global_stats <- data.frame(
    group       = "Toutes cultures",
    n           = sum(!is.na(df[[target]])),
    yield_mean  = round(mean(df[[target]], na.rm = TRUE), 3),
    yield_sd    = round(sd(df[[target]], na.rm = TRUE), 3),
    yield_min   = round(min(df[[target]], na.rm = TRUE), 3),
    yield_max   = round(max(df[[target]], na.rm = TRUE), 3),
    stringsAsFactors = FALSE
  )

  if ("ndvi_mean" %in% names(df)) {
    global_stats$ndvi_mean <- round(mean(df$ndvi_mean, na.rm = TRUE), 4)
  }

  # ── Résumé par culture ────────────────────────────────────────────────────────
  result_list <- list(global_stats)

  if (group_by_crop && "crop" %in% names(df)) {
    crops <- unique(df$crop[!is.na(df$crop)])
    for (cr in crops) {
      sub   <- df[df$crop == cr & !is.na(df$crop), ]
      stats <- data.frame(
        group       = as.character(cr),
        n           = sum(!is.na(sub[[target]])),
        yield_mean  = round(mean(sub[[target]], na.rm = TRUE), 3),
        yield_sd    = round(sd(sub[[target]], na.rm = TRUE), 3),
        yield_min   = round(min(sub[[target]], na.rm = TRUE), 3),
        yield_max   = round(max(sub[[target]], na.rm = TRUE), 3),
        stringsAsFactors = FALSE
      )
      if ("ndvi_mean" %in% names(sub)) {
        stats$ndvi_mean <- round(mean(sub$ndvi_mean, na.rm = TRUE), 4)
      }
      result_list[[length(result_list) + 1]] <- stats
    }
  }

  result <- do.call(rbind, result_list)
  rownames(result) <- NULL

  message("Résumé : ", nrow(result), " groupes / ",
          global_stats$n, " parcelles au total")
  result
}


#' Génération d'un rapport automatique HTML ou PDF
#'
#' Génère un rapport complet au format HTML ou PDF intégrant :
#' la description des données, les performances du modèle,
#' les cartes de rendement et l'importance des variables.
#' Utilise R Markdown.
#'
#' @param fields Objet sf des parcelles (sortie de \code{import_field_data}).
#' @param model_result Liste issue de \code{train_rf_model}.
#' @param eval_result Liste issue de \code{evaluate_model} (optionnel).
#' @param yield_rast SpatRaster du rendement prédit (optionnel).
#' @param summary_df data.frame issu de \code{summarize_fields} (optionnel).
#' @param output_format Format de sortie : "html" (défaut) ou "pdf".
#' @param output_dir Répertoire de sortie (défaut: "outputs/").
#' @param title Titre du rapport (défaut: "Rapport cropYieldForest").
#'
#' @return Chemin vers le fichier rapport généré.
#'
#' @examples
#' \dontrun{
#' report_path <- generate_report(
#'   fields        = fields$sf_object,
#'   model_result  = model,
#'   eval_result   = eval,
#'   output_format = "html",
#'   output_dir    = "outputs/"
#' )
#' browseURL(report_path)
#' }
#'
#' @export
generate_report <- function(fields        = NULL,
                            model_result  = NULL,
                            eval_result   = NULL,
                            yield_rast    = NULL,
                            summary_df    = NULL,
                            output_format = "html",
                            output_dir    = "outputs/",
                            title         = "Rapport cropYieldForest") {

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  output_format <- match.arg(output_format, c("html", "pdf"))
  ext           <- if (output_format == "html") "html" else "pdf"
  out_file      <- file.path(normalizePath(output_dir, mustWork = FALSE),
                             paste0("rapport_cropYieldForest.", ext))

  # ── Création du fichier Rmd temporaire ───────────────────────────────────────
  rmd_content <- .build_report_rmd(
    title         = title,
    fields        = fields,
    model_result  = model_result,
    eval_result   = eval_result,
    yield_rast    = yield_rast,
    summary_df    = summary_df,
    output_format = output_format,
    output_dir    = output_dir
  )

  rmd_path <- tempfile(fileext = ".Rmd")
  writeLines(rmd_content, rmd_path)

  message("Génération du rapport ", toupper(output_format), " ...")

  tryCatch({
    rmarkdown::render(
      input         = rmd_path,
      output_format = if (output_format == "html") "html_document" else "pdf_document",
      output_file   = out_file,
      quiet         = TRUE,
      envir         = new.env(parent = environment())
    )
    message("Rapport généré : ", out_file)
    out_file
  }, error = function(e) {
    warning("Erreur lors de la génération du rapport : ", conditionMessage(e))
    message("Astuce : Pour PDF, installez TinyTeX avec : tinytex::install_tinytex()")
    NA_character_
  })
}


# ── Fonctions internes ────────────────────────────────────────────────────────

#' @keywords internal
.build_report_rmd <- function(title, fields, model_result, eval_result,
                              yield_rast, summary_df, output_format, output_dir) {

  # Sérialisation des objets dans l'environnement global du Rmd
  env_setup <- '
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE,
                      fig.width = 7, fig.height = 5)
library(cropYieldForest)
library(ggplot2)
library(terra)
library(knitr)
```
'

  sections <- c(
    paste0("---\ntitle: \"", title, "\"\n",
           "date: \"`r format(Sys.Date(), '%d %B %Y')`\"\n",
           "output:\n",
           if (output_format == "html")
             "  html_document:\n    toc: true\n    toc_float: true\n    theme: flatly\n"
           else
             "  pdf_document:\n    toc: true\n",
           "---\n"),
    env_setup,

    "## 1. Description des données\n",
    if (!is.null(fields)) {
      df <- if (inherits(fields, "sf")) sf::st_drop_geometry(fields) else fields
      paste0(
        '```{r}\n',
        'cat("Nombre de parcelles :", ', nrow(df), ', "\\n")\n',
        if ("crop" %in% names(df))
          paste0('cat("Cultures :", paste(unique(', deparse(df$crop), '), collapse = ", "), "\\n")\n')
        else "",
        '```\n'
      )
    } else "*(Données parcellaires non fournies)*\n",

    if (!is.null(summary_df)) {
      paste0(
        "### Résumé des parcelles\n\n",
        '```{r}\nknitr::kable(', deparse(summary_df), ',\n',
        '  caption = "Statistiques par groupe de culture")\n```\n'
      )
    } else "",

    "## 2. Performances du modèle\n",
    if (!is.null(model_result)) {
      m <- model_result$test_metrics
      paste0(
        "| Métrique | Valeur |\n|----------|--------|\n",
        "| RMSE     | ", round(m$RMSE, 4), " |\n",
        "| R²       | ", round(m$R2,   4), " |\n",
        "| MAE      | ", round(m$MAE,  4), " |\n\n",
        "> Meilleur ntree : **", model_result$best_ntree, "**\n"
      )
    } else "*(Modèle non fourni)*\n",

    "## 3. Cartes de rendement\n",
    if (!is.null(yield_rast)) {
      '```{r fig.cap="Rendement prédit (t/ha)"}\n',
      'terra::plot(yield_rast, main = "Rendement prédit (t/ha)",\n',
      '           col = rev(terrain.colors(20)))\n',
      '```\n'
    } else {
      img_path <- file.path(output_dir, "yield_map.png")
      if (file.exists(img_path)) {
        paste0('```{r}\nknitr::include_graphics("', img_path, '")\n```\n')
      } else "*(Carte de rendement non disponible)*\n"
    },

    "## 4. Importance des variables\n",
    if (!is.null(model_result) && !is.null(model_result$importance)) {
      imp_path <- file.path(output_dir, "feature_importance.png")
      if (file.exists(imp_path)) {
        paste0('```{r}\nknitr::include_graphics("', imp_path, '")\n```\n')
      } else {
        imp <- head(model_result$importance, 10)
        paste0(
          '```{r fig.cap="Importance des variables"}\n',
          'imp_df <- ', deparse(imp), '\n',
          'imp_df$variable <- factor(imp_df$variable, levels = rev(imp_df$variable))\n',
          'ggplot(imp_df, aes(x = variable, y = IncMSE)) +\n',
          '  geom_bar(stat = "identity", fill = "#2E86AB") +\n',
          '  coord_flip() + theme_minimal() +\n',
          '  labs(title = "Importance des variables", x = NULL, y = "%IncMSE")\n',
          '```\n'
        )
      }
    } else "*(Importance non disponible)*\n",

    "## 5. Interprétation et conclusions\n",
    paste0(
      "Ce rapport a été généré automatiquement par le package **cropYieldForest**.\n\n",
      "- Les indices de végétation NDVI/SAVI issus de données MODIS/Sentinel-2 ",
      "constituent les variables explicatives principales du modèle.\n",
      "- Le modèle Random Forest capture les relations non-linéaires ",
      "entre végétation, météo et rendement.\n",
      "- Pour améliorer les performances, enrichir le dataset avec ",
      "des données sol (texture, matière organique) et augmenter ",
      "le nombre de parcelles d'entraînement.\n\n",
      "---\n*Rapport généré le `r format(Sys.time(), '%d/%m/%Y %H:%M')`*\n"
    )
  )

  paste(sections, collapse = "\n")
}
