#' Import des données météorologiques depuis Open-Meteo (ERA5)
#'
#' Télécharge les données météorologiques historiques depuis l'API Open-Meteo
#' (réanalyse ERA5, gratuite et sans clé API). Les variables récupérées sont
#' la température, les précipitations, l'humidité et le rayonnement solaire.
#' Les données peuvent ensuite être agrégées mensuellement et jointes aux
#' données parcellaires.
#'
#' @param lat Latitude du centroïde de la zone d'étude (degrés décimaux).
#' @param lon Longitude du centroïde de la zone d'étude (degrés décimaux).
#' @param start_date Date de début au format "YYYY-MM-DD".
#' @param end_date Date de fin au format "YYYY-MM-DD".
#' @param aggregate Agrégation temporelle : "daily" (défaut) ou "monthly".
#' @param local_file Chemin vers un fichier CSV météo local (bypass API).
#'   Colonnes attendues : date, temperature_2m_mean, precipitation_sum,
#'   relative_humidity_2m_mean, shortwave_radiation_sum.
#'
#' @return data.frame avec colonnes :
#'   \describe{
#'     \item{date}{Date (classe Date).}
#'     \item{temp_mean}{Température moyenne (°C).}
#'     \item{precip_sum}{Précipitations cumulées (mm).}
#'     \item{humidity_mean}{Humidité relative moyenne (\%).}
#'     \item{radiation_sum}{Rayonnement solaire cumulé (MJ/m²).}
#'   }
#'   Si \code{aggregate = "monthly"}, les colonnes sont agrégées par mois.
#'
#' @source \url{https://open-meteo.com/en/docs/historical-weather-api}
#' @source ERA5 : \url{https://cds.climate.copernicus.eu/}
#'
#' @examples
#' \dontrun{
#' # Données météo pour la plaine du Gharb (Maroc), 2022-2023
#' meteo <- import_weather_data(
#'   lat        = 34.5,
#'   lon        = -6.2,
#'   start_date = "2022-01-01",
#'   end_date   = "2023-12-31",
#'   aggregate  = "monthly"
#' )
#' head(meteo)
#'
#' # Depuis un fichier local (données déjà téléchargées)
#' meteo <- import_weather_data(
#'   local_file = "inst/weather_data/meteo_gharb.csv"
#' )
#' }
#'
#' @export
import_weather_data <- function(lat         = NULL,
                                lon         = NULL,
                                start_date  = NULL,
                                end_date    = NULL,
                                aggregate   = "daily",
                                local_file  = NULL) {

  # ── Mode fichier local ───────────────────────────────────────────────────────
  if (!is.null(local_file)) {
    if (!file.exists(local_file)) stop("Fichier local introuvable : ", local_file)
    message("Chargement météo depuis fichier local : ", local_file)
    df <- utils::read.csv(local_file, stringsAsFactors = FALSE)
    df <- .harmonize_weather_cols(df)
    if (aggregate == "monthly") df <- .aggregate_monthly(df)
    return(df)
  }

  # ── Validation des paramètres ────────────────────────────────────────────────
  if (is.null(lat) || is.null(lon))   stop("Spécifiez lat et lon.")
  if (is.null(start_date) || is.null(end_date)) stop("Spécifiez start_date et end_date.")

  if (lat < -90 || lat > 90)  stop("Latitude invalide (doit être entre -90 et 90).")
  if (lon < -180 || lon > 180) stop("Longitude invalide (doit être entre -180 et 180).")

  # ── Construction de l'URL Open-Meteo ERA5 ───────────────────────────────────
  base_url <- "https://archive-api.open-meteo.com/v1/archive"

  variables <- paste(c(
    "temperature_2m_mean",
    "precipitation_sum",
    "relative_humidity_2m_mean",
    "shortwave_radiation_sum"
  ), collapse = ",")

  url <- paste0(
    base_url,
    "?latitude=",   lat,
    "&longitude=",  lon,
    "&start_date=", start_date,
    "&end_date=",   end_date,
    "&daily=",      variables,
    "&timezone=auto"
  )

  message("Téléchargement météo ERA5 (Open-Meteo) ...")
  message("URL : ", url)

  resp <- tryCatch(
    httr::GET(url, httr::timeout(60)),
    error = function(e) stop("Connexion API impossible : ", conditionMessage(e))
  )

  if (httr::status_code(resp) != 200) {
    stop("Erreur API Open-Meteo (code ", httr::status_code(resp), ") : ",
         httr::content(resp, "text", encoding = "UTF-8"))
  }

  data_json <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))

  if (is.null(data_json$daily)) {
    stop("Aucune donnée renvoyée par l'API. Vérifiez les coordonnées et les dates.")
  }

  df <- data.frame(
    date          = as.Date(data_json$daily$time),
    temp_mean     = data_json$daily$temperature_2m_mean,
    precip_sum    = data_json$daily$precipitation_sum,
    humidity_mean = data_json$daily$relative_humidity_2m_mean,
    radiation_sum = data_json$daily$shortwave_radiation_sum,
    stringsAsFactors = FALSE
  )

  message("Météo téléchargée : ", nrow(df), " jours (",
          format(min(df$date)), " → ", format(max(df$date)), ")")

  if (aggregate == "monthly") {
    df <- .aggregate_monthly(df)
    message("Agrégation mensuelle : ", nrow(df), " mois.")
  }

  df
}


# ── Fonctions internes ────────────────────────────────────────────────────────

#' @keywords internal
.harmonize_weather_cols <- function(df) {
  rename_map <- list(
    temp_mean     = c("temperature_2m_mean", "temp", "temperature", "temp_mean"),
    precip_sum    = c("precipitation_sum", "precip", "precipitation", "rain"),
    humidity_mean = c("relative_humidity_2m_mean", "humidity", "rh"),
    radiation_sum = c("shortwave_radiation_sum", "radiation", "solar_radiation")
  )
  for (new_name in names(rename_map)) {
    candidates <- rename_map[[new_name]]
    match_col  <- intersect(candidates, names(df))[1]
    if (!is.na(match_col) && match_col != new_name) {
      names(df)[names(df) == match_col] <- new_name
    }
  }
  if (!"date" %in% names(df)) {
    date_col <- grep("date|time", names(df), ignore.case = TRUE, value = TRUE)[1]
    if (!is.na(date_col)) names(df)[names(df) == date_col] <- "date"
  }
  df$date <- as.Date(df$date)
  df
}

#' @keywords internal
.aggregate_monthly <- function(df) {
  df$year  <- lubridate::year(df$date)
  df$month <- lubridate::month(df$date)

  numeric_cols <- intersect(c("temp_mean", "precip_sum", "humidity_mean", "radiation_sum"),
                            names(df))

  result <- aggregate(df[, numeric_cols, drop = FALSE],
                      by   = list(year = df$year, month = df$month),
                      FUN  = function(x) mean(x, na.rm = TRUE))

  result$date <- as.Date(paste(result$year, result$month, "01", sep = "-"))
  result      <- result[order(result$date), ]
  result      <- result[, c("date", numeric_cols)]
  result
}
