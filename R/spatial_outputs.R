#' Prédiction spatiale du rendement sur raster
#'
#' @param model_result Liste issue de \code{train_rf_model}.
#' @param ndvi_stack SpatRaster NDVI utilisé pour la prédiction spatiale.
#' @param indices_stack SpatRaster des autres indices (SAVI, EVI...). NULL par défaut.
#' @param weather_df data.frame météo.
#' @param features_used Vecteur des noms de variables du modèle.
#' @param train_data data.frame d'entraînement (data_ml$train) pour aligner les types.
#' @param out_dir Répertoire de sortie (défaut: "outputs/").
#'
#' @return SpatRaster du rendement prédit (t/ha).
#'
#' @examples
#' \dontrun{
#' yield_map <- predict_yield_map(
#'   model_result  = model,
#'   ndvi_stack    = veg$ndvi,
#'   weather_df    = meteo,
#'   features_used = data_ml$features_used,
#'   train_data    = data_ml$train
#' )
#' }
#'
#' @export
predict_yield_map <- function(model_result,
                              ndvi_stack    = NULL,
                              indices_stack = NULL,
                              weather_df    = NULL,
                              features_used = NULL,
                              train_data    = NULL,
                              out_dir       = "outputs/") {
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  if (!inherits(ndvi_stack, "SpatRaster")) {
    stop("ndvi_stack doit être un SpatRaster.")
  }
  
  message("Construction du raster de prédiction ...")
  
  # Agrégation temporelle NDVI → statistiques pixel
  ndvi_vals   <- terra::values(ndvi_stack)
  ndvi_mean_v <- apply(ndvi_vals, 1, mean, na.rm = TRUE)
  ndvi_max_v  <- apply(ndvi_vals, 1, max,  na.rm = TRUE)
  ndvi_min_v  <- apply(ndvi_vals, 1, min,  na.rm = TRUE)
  ndvi_sd_v   <- apply(ndvi_vals, 1, sd,   na.rm = TRUE)
  
  # Coordonnées de chaque pixel
  coords <- terra::xyFromCell(ndvi_stack[[1]], 1:terra::ncell(ndvi_stack[[1]]))
  
  pixel_df <- data.frame(
    lon       = coords[, 1],
    lat       = coords[, 2],
    ndvi_mean = ndvi_mean_v,
    ndvi_max  = ndvi_max_v,
    ndvi_min  = ndvi_min_v,
    ndvi_sd   = ndvi_sd_v
  )
  
  # Ajout indices supplémentaires
  if (!is.null(indices_stack)) {
    for (lyr in names(indices_stack)) {
      v <- terra::values(indices_stack[[lyr]])
      pixel_df[[paste0(tolower(lyr), "_mean")]] <- apply(v, 1, mean, na.rm = TRUE)
    }
  }
  
  # Ajout météo (constante pour tous les pixels)
  if (!is.null(weather_df)) {
    pixel_df$temp_mean_season     <- mean(weather_df$temp_mean,     na.rm = TRUE)
    pixel_df$precip_sum_season    <- sum(weather_df$precip_sum,     na.rm = TRUE)
    pixel_df$humidity_mean_season <- mean(weather_df$humidity_mean, na.rm = TRUE)
    pixel_df$radiation_sum_season <- sum(weather_df$radiation_sum,  na.rm = TRUE)
  }
  
  # Sélection des features du modèle
  if (!is.null(features_used)) {
    missing <- setdiff(features_used, names(pixel_df))
    if (length(missing) > 0) {
      message("Variables manquantes comblées : ", paste(missing, collapse = ", "))
      for (m in missing) pixel_df[[m]] <- NA
    }
    pixel_df <- pixel_df[, features_used, drop = FALSE]
  }
  
  # ── Alignement des types avec le train ───────────────────────────────────────
  if (!is.null(train_data)) {
    for (col in names(pixel_df)) {
      if (col %in% names(train_data)) {
        train_type <- class(train_data[[col]])[1]
        if (train_type == "factor") {
          # Reprendre les mêmes niveaux que le train
          lvls <- levels(train_data[[col]])
          # Pour un raster on n'a pas de catégories → prendre le niveau médian
          median_lvl <- lvls[ceiling(length(lvls) / 2)]
          pixel_df[[col]] <- factor(median_lvl, levels = lvls)
        } else if (train_type == "numeric" || train_type == "integer") {
          pixel_df[[col]] <- as.numeric(pixel_df[[col]])
        }
      }
    }
  } else {
    # Sans train_data : convertir tous les character/factor en numérique avec médiane 0
    for (col in names(pixel_df)) {
      if (is.factor(pixel_df[[col]]) || is.character(pixel_df[[col]])) {
        pixel_df[[col]] <- 0
      }
    }
  }
  
  # Imputer les NA restants par la médiane de chaque colonne
  for (col in names(pixel_df)) {
    if (is.numeric(pixel_df[[col]])) {
      na_idx <- is.na(pixel_df[[col]])
      if (any(na_idx)) {
        med <- median(pixel_df[[col]], na.rm = TRUE)
        if (is.na(med)) med <- 0
        pixel_df[[col]][na_idx] <- med
      }
    }
  }
  
  # Gestion des NA résiduels
  na_mask <- apply(pixel_df, 1, function(x) any(is.na(x)))
  message("Prédiction sur ", sum(!na_mask), " pixels valides (",
          sum(na_mask), " masqués) ...")
  
  pred_vals <- rep(NA_real_, nrow(pixel_df))
  if (sum(!na_mask) > 0) {
    pred_vals[!na_mask] <- stats::predict(
      model_result$model,
      pixel_df[!na_mask, , drop = FALSE]
    )
  }
  
  # Reconstruction du raster
  ref_rast   <- ndvi_stack[[1]]
  yield_rast <- terra::rast(ref_rast)
  terra::values(yield_rast) <- pred_vals
  names(yield_rast) <- "yield_predicted_t_ha"
  
  out_path <- file.path(out_dir, "yield_map.tif")
  terra::writeRaster(yield_rast, out_path, overwrite = TRUE)
  message("Carte de rendement sauvegardée : ", out_path)
  
  yield_rast
}


#' Cartographie du rendement prédit
#'
#' @param yield_rast SpatRaster issu de \code{predict_yield_map}.
#' @param fields Objet sf des parcelles (optionnel).
#' @param low_threshold Seuil rendement faible (défaut: 2).
#' @param out_dir Répertoire de sortie (défaut: "outputs/").
#' @param format Format d'export : "png", "pdf" ou c("png","pdf").
#'
#' @return Liste des objets ggplot (invisible).
#'
#' @examples
#' \dontrun{
#' plot_yield_map(yield_rast = yield_rast, fields = fields$sf_object)
#' }
#'
#' @export
plot_yield_map <- function(yield_rast,
                           fields        = NULL,
                           low_threshold = 2,
                           out_dir       = "outputs/",
                           format        = "png") {
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  if (!inherits(yield_rast, "SpatRaster")) stop("yield_rast doit être un SpatRaster.")
  
  df <- as.data.frame(yield_rast, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "yield"
  
  stats_str <- paste0("Moy=", round(mean(df$yield), 2),
                      " | Min=", round(min(df$yield), 2),
                      " | Max=", round(max(df$yield), 2))
  
  # Carte 1 : Rendement absolu
  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = yield)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradientn(
      colours = c("#d73027", "#fdae61", "#ffffbf", "#a6d96a", "#1a9641"),
      name    = "t/ha"
    ) +
    ggplot2::labs(
      title    = "Rendement prédit par pixel",
      subtitle = stats_str,
      x = "Longitude", y = "Latitude"
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal(base_size = 12)
  
  if (!is.null(fields)) {
    fields_wgs <- sf::st_transform(fields, crs = 4326)
    p1 <- p1 + ggplot2::geom_sf(
      data        = fields_wgs,
      inherit.aes = FALSE,
      fill        = NA, color = "black", linewidth = 0.4
    )
  }
  
  # Carte 2 : Zones à faible rendement
  df$low_yield <- ifelse(df$yield < low_threshold, "Faible", "Normal")
  p2 <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = low_yield)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_manual(
      values = c("Faible" = "#d73027", "Normal" = "#1a9641"),
      name   = paste0("Rendement\n(seuil: ", low_threshold, " t/ha)")
    ) +
    ggplot2::labs(title = "Zones à faible rendement", x = "Longitude", y = "Latitude") +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal(base_size = 12)
  
  plots <- list(yield_map = p1, low_yield_map = p2)
  
  for (fmt in format) {
    ggplot2::ggsave(file.path(out_dir, paste0("yield_map.", fmt)),
                    p1, width = 8, height = 7, dpi = 150)
    ggplot2::ggsave(file.path(out_dir, paste0("low_yield_map.", fmt)),
                    p2, width = 8, height = 7, dpi = 150)
    message("Cartes exportées (", fmt, ") dans : ", out_dir)
  }
  
  invisible(plots)
}


#' Importance des variables du modèle Random Forest
#'
#' @param model_result Liste issue de \code{train_rf_model}.
#' @param top_n Nombre de variables à afficher (défaut: 15).
#' @param out_dir Répertoire de sortie (défaut: "outputs/").
#'
#' @return Liste des objets ggplot (invisible).
#'
#' @examples
#' \dontrun{
#' plot_feature_importance(model_result = model, top_n = 10)
#' }
#'
#' @export
plot_feature_importance <- function(model_result,
                                    top_n   = 15,
                                    out_dir = "outputs/") {
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  if (is.null(model_result$importance)) {
    stop("Importance non disponible. Entraînez avec importance = TRUE.")
  }
  
  imp <- head(model_result$importance, top_n)
  imp$variable <- factor(imp$variable, levels = rev(imp$variable))
  
  p <- ggplot2::ggplot(imp, ggplot2::aes(x = variable, y = IncMSE)) +
    ggplot2::geom_bar(stat = "identity", fill = "#2E86AB", alpha = 0.85) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title    = "Importance des variables (Random Forest)",
      subtitle = paste0("Top ", nrow(imp), " variables — %IncMSE"),
      x        = NULL,
      y        = "% Augmentation MSE si variable permutée"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10))
  
  print(p)
  ggplot2::ggsave(file.path(out_dir, "feature_importance.png"),
                  p, width = 8, height = 6, dpi = 150)
  message("Importance des variables sauvegardée : ",
          file.path(out_dir, "feature_importance.png"))
  
  invisible(list(importance_plot = p))
}