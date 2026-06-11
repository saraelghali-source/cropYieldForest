#' Import des données parcellaires
#'
#' Importe et nettoie les données de terrain (parcelles agricoles).
#' Supporte les formats CSV, Shapefile et GeoJSON.
#' Les variables attendues sont le rendement, la culture, la date
#' et les pratiques agricoles.
#'
#' @param path Chemin vers le fichier de données (CSV, .shp ou .geojson).
#' @param yield_col Nom de la colonne rendement (défaut: "yield").
#' @param crop_col Nom de la colonne culture (défaut: "crop").
#' @param date_col Nom de la colonne date (défaut: "date").
#' @param crs Système de coordonnées souhaité, code EPSG (défaut: 4326 = WGS84).
#' @param remove_na Supprimer les lignes avec rendement NA ? (défaut: TRUE).
#'
#' @return Liste avec deux éléments :
#'   \describe{
#'     \item{sf_object}{Objet \code{sf} avec géométrie des parcelles.}
#'     \item{dataframe}{data.frame nettoyé sans géométrie.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Depuis un CSV avec coordonnées lon/lat
#' fields <- import_field_data(
#'   path      = "inst/field_data/parcelles.csv",
#'   yield_col = "rendement_t_ha",
#'   crop_col  = "culture",
#'   date_col  = "date_recolte"
#' )
#' head(fields$dataframe)
#' plot(fields$sf_object["yield"])
#' }
#'
#' @export
import_field_data <- function(path,
                              yield_col  = "yield",
                              crop_col   = "crop",
                              date_col   = "date",
                              crs        = 4326,
                              remove_na  = TRUE) {

  if (!file.exists(path)) {
    stop("Fichier introuvable : ", path)
  }

  ext <- tolower(tools::file_ext(path))

  # ── Lecture selon le format ──────────────────────────────────────────────────
  if (ext == "csv") {
    message("Lecture CSV : ", path)
    df_raw <- utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")

    # Détection automatique des colonnes lon/lat
    lon_col <- grep("^(lon|longitude|x)$", names(df_raw), ignore.case = TRUE, value = TRUE)[1]
    lat_col <- grep("^(lat|latitude|y)$", names(df_raw), ignore.case = TRUE, value = TRUE)[1]

    if (is.na(lon_col) || is.na(lat_col)) {
      stop("Colonnes longitude/latitude non trouvées. Nommez-les 'lon'/'lat' ou 'longitude'/'latitude'.")
    }

    sf_obj <- sf::st_as_sf(df_raw,
                           coords = c(lon_col, lat_col),
                           crs    = crs,
                           remove = FALSE)

  } else if (ext %in% c("shp", "geojson", "gpkg")) {
    message("Lecture vecteur : ", path)
    sf_obj <- sf::st_read(path, quiet = TRUE)

    if (sf::st_crs(sf_obj)$epsg != crs) {
      message("Reprojection vers EPSG:", crs)
      sf_obj <- sf::st_transform(sf_obj, crs = crs)
    }

  } else {
    stop("Format non supporté : ", ext, ". Utilisez CSV, SHP, GeoJSON ou GPKG.")
  }

  # ── Renommage des colonnes clés ──────────────────────────────────────────────
  col_map <- c(yield_col, crop_col, date_col)
  new_names <- c("yield", "crop", "date")

  for (i in seq_along(col_map)) {
    if (col_map[i] %in% names(sf_obj) && col_map[i] != new_names[i]) {
      names(sf_obj)[names(sf_obj) == col_map[i]] <- new_names[i]
    }
  }

  # ── Vérification colonnes obligatoires ──────────────────────────────────────
  missing_cols <- setdiff(c("yield"), names(sf_obj))
  if (length(missing_cols) > 0) {
    warning("Colonne(s) manquante(s) : ", paste(missing_cols, collapse = ", "))
  }

  # ── Nettoyage ────────────────────────────────────────────────────────────────
  # Rendement : numérique et positif
  if ("yield" %in% names(sf_obj)) {
    sf_obj$yield <- suppressWarnings(as.numeric(sf_obj$yield))
    n_neg <- sum(sf_obj$yield < 0, na.rm = TRUE)
    if (n_neg > 0) {
      message(n_neg, " valeurs de rendement négatives mises à NA.")
      sf_obj$yield[sf_obj$yield < 0] <- NA
    }
    if (remove_na) {
      n_before <- nrow(sf_obj)
      sf_obj <- sf_obj[!is.na(sf_obj$yield), ]
      message("Suppression de ", n_before - nrow(sf_obj), " parcelles avec rendement NA.")
    }
  }

  # Date : conversion
  if ("date" %in% names(sf_obj)) {
    sf_obj$date <- tryCatch(
      lubridate::ymd(sf_obj$date),
      error = function(e) {
        warning("Conversion de date échouée. Colonne conservée en texte.")
        sf_obj$date
      }
    )
  }

  # Ajout colonne identifiant parcelle si absente
  if (!"field_id" %in% names(sf_obj)) {
    sf_obj$field_id <- paste0("F", sprintf("%03d", seq_len(nrow(sf_obj))))
  }

  message("Import terminé : ", nrow(sf_obj), " parcelles valides.")

  list(
    sf_object = sf_obj,
    dataframe = sf::st_drop_geometry(sf_obj)
  )
}
