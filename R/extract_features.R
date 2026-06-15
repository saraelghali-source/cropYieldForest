#' Extraction des variables par parcelle
#'
#' Extrait les statistiques zonales des rasters d'indices de végétation
#' (NDVI moyen, max, min, écart-type) et joint les données météorologiques
#' pour chaque parcelle. Produit le jeu de données prêt pour le Machine Learning.
#'
#' @param fields Objet \code{sf} des parcelles (sortie de \code{import_field_data}).
#' @param ndvi_stack SpatRaster NDVI (une ou plusieurs dates).
#' @param indices_stack SpatRaster des indices supplémentaires (SAVI, EVI...).
#'   NULL par défaut.
#' @param weather_df data.frame météo (sortie de \code{import_weather_data}).
#'   NULL par défaut. Si fourni, on agrège sur la saison culturale de chaque
#'   parcelle (colonne "date" dans fields).
#' @param fun Fonctions statistiques zonales à calculer (défaut: mean, max, min, stdev).
#' @param method Méthode d'extraction : "exact" (exactextractr, recommandé)
#'   ou "centroid" (terra::extract sur le centroïde).
#'
#' @return data.frame avec une ligne par parcelle et les colonnes :
#'   field_id, yield (si présent), ndvi_mean, ndvi_max, ndvi_min, ndvi_sd,
#'   + statistiques des autres indices, + variables météo si fournies.
#'
#' @examples
#' \dontrun{
#' fields <- import_field_data("parcelles.shp")
#' veg    <- download_sentinel(lat = 34.5, lon = -6.2,
#'                             start_date = "2022-04-01", end_date = "2022-06-30")
#' meteo  <- import_weather_data(lat = 34.5, lon = -6.2,
#'                               start_date = "2022-01-01", end_date = "2022-12-31",
#'                               aggregate = "monthly")
#' features <- extract_features(
#'   fields        = fields$sf_object,
#'   ndvi_stack    = veg$ndvi,
#'   weather_df    = meteo
#' )
#' head(features)
#' }
#'
#' @export
extract_features <- function(fields,
                             ndvi_stack    = NULL,
                             indices_stack = NULL,
                             weather_df    = NULL,
                             fun           = c("mean", "max", "min", "stdev"),
                             method        = "exact") {
  
  if (!inherits(fields, "sf")) stop("fields doit être un objet sf.")
  
  # Reprojection si nécessaire
  if (!is.null(ndvi_stack)) {
    fields_proj <- sf::st_transform(fields, crs = terra::crs(ndvi_stack, proj = TRUE))
  } else {
    fields_proj <- fields
  }
  
  result <- sf::st_drop_geometry(fields_proj)
  
  # ── Extraction NDVI ──────────────────────────────────────────────────────────
  if (!is.null(ndvi_stack)) {
    message("Extraction NDVI par parcelle (méthode: centroïde) ...")
    ndvi_stats <- .extract_raster_stats(fields_proj, ndvi_stack, "ndvi", method)
    result     <- cbind(result, ndvi_stats)
  }
  
  # ── Extraction autres indices ────────────────────────────────────────────────
  if (!is.null(indices_stack)) {
    message("Extraction indices supplémentaires ...")
    for (lyr in names(indices_stack)) {
      r      <- indices_stack[[lyr]]
      stats  <- .extract_raster_stats(fields_proj, r, tolower(lyr), method)
      result <- cbind(result, stats)
    }
  }
  
  # ── Jointure météo ───────────────────────────────────────────────────────────
  if (!is.null(weather_df)) {
    message("Agrégation des données météo sur la saison ...")
    meteo_season <- data.frame(
      temp_mean_season     = mean(weather_df$temp_mean,     na.rm = TRUE),
      precip_sum_season    = sum(weather_df$precip_sum,     na.rm = TRUE),
      humidity_mean_season = mean(weather_df$humidity_mean, na.rm = TRUE),
      radiation_sum_season = sum(weather_df$radiation_sum,  na.rm = TRUE)
    )
    result <- cbind(result, meteo_season[rep(1, nrow(result)), ])
  }
  
  message("Features extraites : ", nrow(result), " parcelles × ",
          ncol(result), " variables.")
  result
}


#' Prétraitement du jeu de données pour le Machine Learning
#'
#' @param features data.frame issu de \code{extract_features}.
#' @param target Nom de la colonne cible (défaut: "yield").
#' @param exclude_cols Colonnes à exclure du modèle.
#' @param test_ratio Proportion pour le jeu de test (défaut: 0.25).
#' @param seed Graine aléatoire (défaut: 42).
#' @param impute_method Méthode d'imputation : "median" (défaut) ou "mean".
#' @param scale Centrer et réduire ? (défaut: FALSE).
#'
#' @return Liste : train, test, features_used, n_train, n_test.
#'
#' @examples
#' \dontrun{
#' data_ml <- preprocess_data(features, target = "yield")
#' }
#'
#' @export
preprocess_data <- function(features,
                            target        = "yield",
                            exclude_cols  = c("field_id", "date", "crop", "geometry"),
                            test_ratio    = 0.25,
                            seed          = 42,
                            impute_method = "median",
                            scale         = FALSE) {
  
  if (!target %in% names(features)) {
    stop("Colonne cible '", target, "' introuvable dans le dataset.")
  }
  
  df <- features
  
  # Suppression des colonnes exclues
  cols_to_drop <- intersect(exclude_cols, names(df))
  if (length(cols_to_drop) > 0) {
    message("Colonnes exclues : ", paste(cols_to_drop, collapse = ", "))
    df <- df[, !names(df) %in% cols_to_drop, drop = FALSE]
  }
  
  # Suppression lignes avec cible NA
  n_before <- nrow(df)
  df       <- df[!is.na(df[[target]]), ]
  if (nrow(df) < n_before) {
    message("Suppression de ", n_before - nrow(df), " lignes avec ", target, " = NA.")
  }
  
  # Encodage des variables catégorielles en facteurs
  cat_cols <- names(df)[sapply(df, is.character)]
  if (length(cat_cols) > 0) {
    message("Encodage facteurs : ", paste(cat_cols, collapse = ", "))
    df[cat_cols] <- lapply(df[cat_cols], as.factor)
  }
  
  # Imputation des NA numériques — corrigée pour éviter fill_val = NA
  num_cols <- names(df)[sapply(df, is.numeric) & names(df) != target]
  for (col in num_cols) {
    na_count <- sum(is.na(df[[col]]))
    if (na_count > 0) {
      fill_val <- if (impute_method == "median") {
        median(df[[col]], na.rm = TRUE)
      } else {
        mean(df[[col]], na.rm = TRUE)
      }
      # Si toute la colonne est NA → imputer à 0
      if (is.na(fill_val) || is.nan(fill_val)) {
        fill_val <- 0
        message("Colonne '", col, "' entièrement NA → imputée à 0")
      } else {
        message("Imputation '", col, "' : ", na_count, " NA → ", round(fill_val, 3))
      }
      df[[col]][is.na(df[[col]])] <- fill_val
    }
  }
  
  # Mise à l'échelle optionnelle
  if (scale && length(num_cols) > 0) {
    df[num_cols] <- scale(df[num_cols])
    message("Variables numériques centrées-réduites.")
  }
  
  # Split train/test
  set.seed(seed)
  n        <- nrow(df)
  idx_test <- sample(seq_len(n), size = floor(n * test_ratio))
  train    <- df[-idx_test, ]
  test     <- df[idx_test, ]
  
  features_used <- setdiff(names(df), target)
  
  message("Split : ", nrow(train), " train / ", nrow(test), " test")
  message("Variables utilisées (", length(features_used), ") : ",
          paste(head(features_used, 8), collapse = ", "),
          if (length(features_used) > 8) " ..." else "")
  
  list(
    train         = train,
    test          = test,
    features_used = features_used,
    n_train       = nrow(train),
    n_test        = nrow(test)
  )
}


# ── Fonction interne d'extraction raster ─────────────────────────────────────

#' @keywords internal
.extract_raster_stats <- function(sf_obj, raster, prefix, method) {
  
  # Toujours extraire par centroïde — robuste pour points et polygones
  # et évite les problèmes de buffer sur pixels MODIS 250m
  pts <- tryCatch(
    sf::st_centroid(sf::st_transform(sf_obj, crs = 4326)),
    error = function(e) sf::st_transform(sf_obj, crs = 4326)
  )
  
  vals <- terra::extract(raster, terra::vect(pts))
  
  # Supprimer colonne ID terra
  if ("ID" %in% names(vals)) vals <- vals[, -which(names(vals) == "ID"), drop = FALSE]
  
  if (ncol(vals) == 0 || nrow(vals) == 0) {
    warning("Aucune valeur extraite pour ", prefix, " — colonnes remplies à NA.")
    na_col <- rep(NA_real_, nrow(sf_obj))
    result <- data.frame(na_col, na_col, na_col, na_col)
    names(result) <- paste0(prefix, "_", c("mean", "max", "min", "sd"))
    return(result)
  }
  
  # Calculer les statistiques sur toutes les dates (colonnes du stack)
  vals_num <- as.matrix(vals)
  storage.mode(vals_num) <- "double"
  
  mean_val <- apply(vals_num, 1, mean, na.rm = TRUE)
  max_val  <- apply(vals_num, 1, max,  na.rm = TRUE)
  min_val  <- apply(vals_num, 1, min,  na.rm = TRUE)
  sd_val   <- apply(vals_num, 1, sd,   na.rm = TRUE)
  
  # Remplacer Inf / -Inf / NaN par NA
  mean_val[!is.finite(mean_val)] <- NA
  max_val[!is.finite(max_val)]   <- NA
  min_val[!is.finite(min_val)]   <- NA
  sd_val[!is.finite(sd_val)]     <- NA
  
  result <- data.frame(mean_val, max_val, min_val, sd_val)
  names(result) <- paste0(prefix, "_", c("mean", "max", "min", "sd"))
  result
}