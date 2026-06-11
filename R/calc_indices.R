#' Calcul du NDVI (Normalized Difference Vegetation Index)
#'
#' Calcule le NDVI à partir des bandes Rouge et NIR.
#' Formule : NDVI = (NIR - Red) / (NIR + Red).
#' Fonctionne sur des scalaires, des vecteurs numériques ou des SpatRasters.
#'
#' @param nir Bande NIR : valeur numérique, vecteur ou SpatRaster.
#' @param red Bande Rouge : valeur numérique, vecteur ou SpatRaster.
#'
#' @return Même type que l'entrée (scalaire, vecteur ou SpatRaster).
#'   Les valeurs sont comprises entre -1 et 1.
#'
#' @examples
#' # Scalaire
#' calc_ndvi(nir = 0.8, red = 0.1)
#' # [1] 0.7777778
#'
#' # Vecteur
#' calc_ndvi(nir = c(0.7, 0.6, 0.5), red = c(0.1, 0.2, 0.3))
#'
#' \dontrun{
#' # SpatRaster
#' nir_r  <- terra::rast("nir_band.tif")
#' red_r  <- terra::rast("red_band.tif")
#' ndvi_r <- calc_ndvi(nir = nir_r, red = red_r)
#' terra::plot(ndvi_r, main = "NDVI")
#' }
#'
#' @export
calc_ndvi <- function(nir, red) {
  if (any(c(inherits(nir, "SpatRaster"), inherits(red, "SpatRaster")))) {
    ndvi <- (nir - red) / (nir + red)
    names(ndvi) <- paste0("NDVI_", seq_len(terra::nlyr(ndvi)))
    return(ndvi)
  }
  (nir - red) / (nir + red)
}


#' Calcul des indices de végétation simples
#'
#' Calcule plusieurs indices spectraux à partir des bandes Sentinel-2 ou MODIS :
#' NDVI, SAVI, EVI (optionnel) et GNDVI.
#' Tous les indices sont empilés dans un seul SpatRaster multi-couches.
#'
#' @param nir SpatRaster bande NIR (Sentinel-2 B08 ou MODIS bande 2).
#' @param red SpatRaster bande Rouge (Sentinel-2 B04 ou MODIS bande 1).
#' @param green SpatRaster bande Verte (Sentinel-2 B03 ou MODIS bande 4).
#'   Requis pour GNDVI. NULL par défaut.
#' @param blue SpatRaster bande Bleue (Sentinel-2 B02). Requis pour EVI.
#'   NULL par défaut.
#' @param savi_L Facteur de correction du sol pour SAVI (défaut: 0.5).
#' @param compute_evi Calculer l'EVI ? Nécessite la bande bleue (défaut: FALSE).
#'
#' @return SpatRaster avec les couches :
#'   \describe{
#'     \item{NDVI}{Normalized Difference Vegetation Index.}
#'     \item{SAVI}{Soil-Adjusted Vegetation Index.}
#'     \item{EVI}{Enhanced Vegetation Index (si compute_evi = TRUE).}
#'     \item{GNDVI}{Green NDVI (si green fourni).}
#'   }
#'
#' @references
#' Huete, A.R. (1988). A soil-adjusted vegetation index (SAVI).
#' Remote Sensing of Environment, 25(3), 295-309.
#'
#' Huete, A. et al. (2002). Overview of the radiometric and biophysical
#' performance of the MODIS vegetation indices.
#' Remote Sensing of Environment, 83(1-2), 195-213.
#'
#' @examples
#' \dontrun{
#' nir   <- terra::rast("nir.tif")
#' red   <- terra::rast("red.tif")
#' green <- terra::rast("green.tif")
#'
#' indices <- calc_simple_indices(nir = nir, red = red, green = green)
#' terra::plot(indices)
#'
#' # Avec EVI
#' blue    <- terra::rast("blue.tif")
#' indices <- calc_simple_indices(nir, red, green, blue, compute_evi = TRUE)
#' }
#'
#' @export
calc_simple_indices <- function(nir,
                                red,
                                green       = NULL,
                                blue        = NULL,
                                savi_L      = 0.5,
                                compute_evi = FALSE) {

  if (!inherits(nir, "SpatRaster") || !inherits(red, "SpatRaster")) {
    stop("nir et red doivent être des objets SpatRaster (terra::rast).")
  }

  indices_list <- list()

  # NDVI
  ndvi <- (nir - red) / (nir + red)
  names(ndvi) <- "NDVI"
  indices_list[["NDVI"]] <- ndvi

  # SAVI : (NIR - Red) / (NIR + Red + L) * (1 + L)
  savi <- ((nir - red) / (nir + red + savi_L)) * (1 + savi_L)
  names(savi) <- "SAVI"
  indices_list[["SAVI"]] <- savi

  # EVI (nécessite la bande bleue)
  if (compute_evi) {
    if (is.null(blue)) {
      warning("EVI nécessite la bande bleue. Ignoré.")
    } else {
      evi <- 2.5 * ((nir - red) / (nir + 6 * red - 7.5 * blue + 1))
      names(evi) <- "EVI"
      indices_list[["EVI"]] <- evi
    }
  }

  # GNDVI : (NIR - Green) / (NIR + Green)
  if (!is.null(green)) {
    gndvi <- (nir - green) / (nir + green)
    names(gndvi) <- "GNDVI"
    indices_list[["GNDVI"]] <- gndvi
  }

  # Empilement
  stack <- terra::rast(indices_list)
  message("Indices calculés : ", paste(names(stack), collapse = ", "))
  stack
}
