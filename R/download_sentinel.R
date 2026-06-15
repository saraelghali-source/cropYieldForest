#' Téléchargement des indices de végétation via MODIS (MODISTools)
#'
#' Télécharge les données MODIS MOD13Q1 (NDVI/EVI, résolution 250 m, 16 jours)
#' et les bandes spectrales Sentinel-2 si disponibles en local.
#'
#' **Pourquoi MODIS ?**  
#' L'accès automatique à Sentinel-2 nécessite un compte Copernicus et une
#' authentification OAuth2. Pour un workflow reproductible sans identifiants,
#' ce package utilise MODIS via MODISTools (gratuit, sans authentification).
#' Les bandes Sentinel-2 peuvent être importées localement si vous les avez
#' téléchargées manuellement depuis \url{https://dataspace.copernicus.eu/}.
#'
#' @param lat Latitude du centroïde de la zone (degrés décimaux).
#' @param lon Longitude du centroïde de la zone (degrés décimaux).
#' @param start_date Date de début au format "YYYY-MM-DD".
#' @param end_date Date de fin au format "YYYY-MM-DD".
#' @param km_lr Extension spatiale Est-Ouest en km (défaut: 20).
#' @param km_ab Extension spatiale Nord-Sud en km (défaut: 20).
#' @param product Produit MODIS (défaut: "MOD13Q1" = NDVI/EVI 250m/16j).
#' @param sentinel_dir Dossier contenant des fichiers .tif Sentinel-2 locaux.
#'   Le dossier doit contenir des fichiers nommés *_B04.tif (rouge),
#'   *_B08.tif (NIR), *_B03.tif (vert).
#' @param out_dir Répertoire de sortie pour les rasters (défaut: "outputs/").
#'
#' @return Liste avec :
#'   \describe{
#'     \item{ndvi}{SpatRaster NDVI (MOD13Q1 ou calculé depuis Sentinel-2).}
#'     \item{evi}{SpatRaster EVI si disponible (MODIS uniquement).}
#'     \item{red}{SpatRaster bande rouge (Sentinel-2 local uniquement).}
#'     \item{nir}{SpatRaster bande NIR (Sentinel-2 local uniquement).}
#'     \item{green}{SpatRaster bande verte (Sentinel-2 local uniquement).}
#'     \item{source}{Chaîne : "MODIS" ou "Sentinel-2_local".}
#'     \item{metadata}{data.frame des métadonnées du téléchargement.}
#'   }
#'
#' @source MODIS MOD13Q1 : \url{https://lpdaac.usgs.gov/products/mod13q1v006/}
#' @source Sentinel-2 : \url{https://dataspace.copernicus.eu/}
#' @source MODISTools : \url{https://cran.r-project.org/package=MODISTools}
#'
#' @examples
#' \dontrun{
#' # Via MODIS (recommandé, sans authentification)
#' veg <- download_sentinel(
#'   lat        = 34.5,
#'   lon        = -6.2,
#'   start_date = "2022-01-01",
#'   end_date   = "2022-12-31",
#'   km_lr      = 20,
#'   km_ab      = 20
#' )
#' terra::plot(veg$ndvi, main = "NDVI MODIS")
#'
#' # Depuis fichiers Sentinel-2 téléchargés manuellement
#' veg <- download_sentinel(
#'   sentinel_dir = "inst/sentinel_data/T29SPP_20220601/"
#' )
#' }
#'
#' @export
download_sentinel <- function(lat          = NULL,
                              lon          = NULL,
                              start_date   = NULL,
                              end_date     = NULL,
                              km_lr        = 20,
                              km_ab        = 20,
                              product      = "MOD13Q1",
                              sentinel_dir = NULL,
                              out_dir      = "outputs/") {
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  # ── Mode Sentinel-2 local ────────────────────────────────────────────────────
  if (!is.null(sentinel_dir)) {
    message("Mode Sentinel-2 local : ", sentinel_dir)
    return(.load_sentinel_local(sentinel_dir, out_dir))
  }
  
  # ── Mode MODIS via MODISTools ────────────────────────────────────────────────
  if (is.null(lat) || is.null(lon) || is.null(start_date) || is.null(end_date)) {
    stop("Spécifiez lat, lon, start_date et end_date (ou fournissez sentinel_dir).")
  }
  
  if (!requireNamespace("MODISTools", quietly = TRUE)) {
    stop("Installez le package MODISTools : install.packages('MODISTools')")
  }
  
  message("Téléchargement MODIS ", product, " ...")
  message("Zone : lat=", lat, ", lon=", lon,
          ", km_lr=", km_lr, ", km_ab=", km_ab)
  message("Période : ", start_date, " → ", end_date)
  
  bands_ndvi <- "250m_16_days_NDVI"
  bands_evi  <- "250m_16_days_EVI"
  
  # Téléchargement NDVI
  df_ndvi <- tryCatch(
    MODISTools::mt_subset(
      product    = product,
      lat        = lat,
      lon        = lon,
      band       = bands_ndvi,
      start      = start_date,
      end        = end_date,
      km_lr      = km_lr,
      km_ab      = km_ab,
      progress   = TRUE
    ),
    error = function(e) {
      stop("Erreur téléchargement NDVI MODIS : ", conditionMessage(e),
           "\nVérifiez votre connexion et les paramètres de zone.")
    }
  )
  
  # Téléchargement EVI
  df_evi <- tryCatch(
    MODISTools::mt_subset(
      product    = product,
      lat        = lat,
      lon        = lon,
      band       = bands_evi,
      start      = start_date,
      end        = end_date,
      km_lr      = km_lr,
      km_ab      = km_ab,
      progress   = TRUE
    ),
    error = function(e) {
      warning("Téléchargement EVI échoué : ", conditionMessage(e))
      NULL
    }
  )
  
  # Conversion en SpatRaster
  message("Conversion en rasters ...")
  ndvi_rast <- .modis_to_raster(df_ndvi, scale_factor = 0.0001)
  evi_rast  <- if (!is.null(df_evi)) .modis_to_raster(df_evi, scale_factor = 0.0001) else NULL
  
  # Sauvegarde
  ndvi_path <- file.path(out_dir, "ndvi_modis.tif")
  terra::writeRaster(ndvi_rast, ndvi_path, overwrite = TRUE)
  message("NDVI sauvegardé : ", ndvi_path)
  
  if (!is.null(evi_rast)) {
    evi_path <- file.path(out_dir, "evi_modis.tif")
    terra::writeRaster(evi_rast, evi_path, overwrite = TRUE)
    message("EVI sauvegardé : ", evi_path)
  }
  
  meta <- data.frame(
    source      = product,
    lat         = lat,
    lon         = lon,
    start_date  = start_date,
    end_date    = end_date,
    n_dates     = terra::nlyr(ndvi_rast),
    resolution  = "250m",
    downloaded  = Sys.time()
  )
  
  list(
    ndvi     = ndvi_rast,
    evi      = evi_rast,
    red      = NULL,
    nir      = NULL,
    green    = NULL,
    source   = "MODIS",
    metadata = meta
  )
}


# ── Fonctions internes ────────────────────────────────────────────────────────

#' Convertit un data.frame MODISTools en SpatRaster multi-dates
#'
#' MODISTools (\code{mt_subset}) renvoie une grille de pixels par date, mais
#' les colonnes \code{longitude}/\code{latitude} sont constantes (égales au
#' point central demandé) et ne reflètent PAS la position de chaque pixel.
#' La position réelle de la grille doit être reconstruite à partir des
#' colonnes \code{xllcorner}, \code{yllcorner}, \code{nrows}, \code{ncols}
#' et \code{cellsize}, qui définissent une grille régulière en projection
#' sinusoïdale MODIS (mètres). Les pixels sont numérotés de façon séquentielle
#' (ligne par ligne, en commençant en haut à gauche) dans la colonne
#' \code{pixel}.
#'
#' @keywords internal
.modis_to_raster <- function(df, scale_factor = 0.0001) {
  dates <- unique(df$calendar_date)
  
  # ── Grille MODIS en projection sinusoïdale (mètres) ──────────────────────
  xll      <- as.numeric(unique(df$xllcorner)[1])
  yll      <- as.numeric(unique(df$yllcorner)[1])
  ncols    <- as.integer(unique(df$ncols)[1])
  nrows    <- as.integer(unique(df$nrows)[1])
  cellsize <- as.numeric(unique(df$cellsize)[1])
  
  crs_modis <- "+proj=sinu +R=6371007.181 +nadgrees=0 +lon_0=0 +x_0=0 +y_0=0 +units=m +no_defs"
  
  xmin <- xll
  xmax <- xll + ncols * cellsize
  ymin <- yll
  ymax <- yll + nrows * cellsize
  
  ext_global <- terra::ext(xmin, xmax, ymin, ymax)
  
  rast_list <- vector("list", length(dates))
  
  for (i in seq_along(dates)) {
    sub <- df[df$calendar_date == dates[i], ]
    sub <- sub[order(sub$pixel), ]
    
    vals <- as.numeric(sub$value)
    
    # Valeurs nodata MODIS (ex : -3000 pour NDVI/EVI) -> NA avant mise à l'échelle
    vals[vals <= -2000] <- NA
    vals <- vals * scale_factor
    
    r <- terra::rast(ext_global,
                     nrows = nrows, ncols = ncols,
                     crs = crs_modis)
    
    # Le champ "pixel" est numéroté ligne par ligne en commençant en haut à
    # gauche, ce qui correspond exactement à l'ordre attendu par terra::values()
    terra::values(r) <- vals
    names(r) <- dates[i]
    
    # Reprojection en WGS84 (lon/lat) pour usage avec ggplot / alignement
    # avec les autres rasters du projet (ex: yield_map)
    r <- terra::project(r, "EPSG:4326")
    
    rast_list[[i]] <- r
  }
  
  # Aligner tous les rasters de dates sur la même grille (référence = premier)
  ref <- rast_list[[1]]
  if (length(rast_list) > 1) {
    rast_list[-1] <- lapply(rast_list[-1], function(r) {
      terra::resample(r, ref, method = "near")
    })
  }
  
  terra::rast(rast_list)
}

#' Charge des bandes Sentinel-2 depuis un répertoire local
#' @keywords internal
.load_sentinel_local <- function(sentinel_dir, out_dir) {
  if (!dir.exists(sentinel_dir)) stop("Répertoire Sentinel-2 introuvable : ", sentinel_dir)
  
  find_band <- function(pattern) {
    files <- list.files(sentinel_dir, pattern = pattern, full.names = TRUE,
                        recursive = TRUE, ignore.case = TRUE)
    if (length(files) == 0) return(NULL)
    terra::rast(files[1])
  }
  
  red   <- find_band("_B04\\.tif$|_B4\\.tif$|_red\\.tif$")
  nir   <- find_band("_B08\\.tif$|_B8\\.tif$|_nir\\.tif$")
  green <- find_band("_B03\\.tif$|_B3\\.tif$|_green\\.tif$")
  
  if (is.null(red) || is.null(nir)) {
    stop("Bandes B04 (rouge) et B08 (NIR) introuvables dans : ", sentinel_dir,
         "\nFichiers trouvés : ", paste(list.files(sentinel_dir), collapse = ", "))
  }
  
  ndvi <- (nir - red) / (nir + red)
  names(ndvi) <- "NDVI"
  terra::writeRaster(ndvi, file.path(out_dir, "ndvi_sentinel.tif"), overwrite = TRUE)
  
  message("Bandes Sentinel-2 chargées depuis : ", sentinel_dir)
  message("NDVI calculé et sauvegardé.")
  
  list(
    ndvi     = ndvi,
    evi      = NULL,
    red      = red,
    nir      = nir,
    green    = green,
    source   = "Sentinel-2_local",
    metadata = data.frame(source = "Sentinel-2_local", dir = sentinel_dir)
  )
}
