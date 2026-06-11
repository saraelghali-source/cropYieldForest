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

  # Bandes disponibles dans MOD13Q1
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
#' @keywords internal
.modis_to_raster <- function(df, scale_factor = 0.0001) {
  dates     <- unique(df$calendar_date)
  rast_list <- vector("list", length(dates))

  for (i in seq_along(dates)) {
    sub    <- df[df$calendar_date == dates[i], ]
    # Dimensions
    n_col <- length(unique(sub$pixel))  # approximation
    # Construction via SpatRaster à partir des coordonnées
    pts   <- terra::vect(sub, geom = c("longitude", "latitude"),
                         crs = "EPSG:4326")
    # Rasterisation sur une grille régulière 250m ~ 0.00225 deg
    ext   <- terra::ext(pts)
    r     <- terra::rast(ext, resolution = 0.00225, crs = "EPSG:4326")
    r_val <- terra::rasterize(pts, r, field = "value", fun = "mean")
    r_val <- r_val * scale_factor
    names(r_val) <- dates[i]
    rast_list[[i]] <- r_val
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
