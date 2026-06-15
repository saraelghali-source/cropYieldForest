# data-raw/run_workflow.R
# Script complet pour générer TOUS les outputs du package
# Exécuter depuis la racine du projet : source("data-raw/run_workflow.R")

devtools::load_all()
library(dplyr)

# ── Dossiers de sortie ────────────────────────────────────────────────
dir.create("outputs",      showWarnings = FALSE)
dir.create("outputs/maps", showWarnings = FALSE)

# ── 1. Import des données parcellaires ───────────────────────────────
fields <- import_field_data(
  path      = system.file("field_data/parcelles_gharb.csv",
                          package = "cropYieldForest"),
  yield_col = "yield",
  crop_col  = "crop",
  date_col  = "date"
)

# ── 2. Import météo (fichier local) ──────────────────────────────────
meteo <- import_weather_data(
  local_file = system.file("weather_data/meteo_gharb_2022.csv",
                           package = "cropYieldForest")
)

# ── 3. Téléchargement MODIS NDVI/EVI ────────────────────────────────
veg <- download_sentinel(
  lat        = 34.5,
  lon        = -6.2,
  start_date = "2022-01-01",
  end_date   = "2022-12-31",
  km_lr      = 30,
  km_ab      = 30,
  out_dir    = "outputs/"
)

# ── 4. Extraction des features ───────────────────────────────────────
features <- extract_features(
  fields     = fields$sf_object,
  ndvi_stack = veg$ndvi,
  weather_df = meteo,
  method     = "exact"
)

# ── 5. Prétraitement ─────────────────────────────────────────────────
data_ml <- preprocess_data(
  features   = features,
  target     = "yield",
  test_ratio = 0.25,
  seed       = 42
)

# ── 6. Entraînement Random Forest ────────────────────────────────────
model <- train_rf_model(
  data_ml      = data_ml,
  ntree_values = c(100, 300, 500),
  importance   = TRUE
)

# ── 7. Évaluation → génère obs_vs_pred.png et residuals.png ─────────
eval_result <- evaluate_model(
  model_result = model,
  test_data    = data_ml$test,
  out_dir      = "outputs/"
)

# ── 8. Carte rendement → génère yield_map.tif ───────────────────────
yield_map <- predict_yield_map(
  model_result  = model,
  ndvi_stack    = veg$ndvi,
  weather_df    = meteo,
  features_used = data_ml$features_used,
  out_dir       = "outputs/"
)

# ── 9. Cartes PNG/PDF → génère maps/yield_map.png et .pdf ───────────
plot_yield_map(
  yield_rast    = yield_map,
  fields        = fields$sf_object,
  low_threshold = 3.0,
  out_dir       = "outputs/maps/",
  format        = c("png", "pdf")
)

# ── 10. Importance variables → génère feature_importance.png ────────
plot_feature_importance(
  model_result = model,
  out_dir      = "outputs/"
)

# ── 11. Rapport HTML final ───────────────────────────────────────────
generate_report(
  fields        = fields$sf_object,
  model_result  = model,
  eval_result   = eval_result,
  yield_rast    = yield_map,
  output_format = "html",
  output_dir    = "outputs/"
)

message("✅ Tous les outputs sont générés dans outputs/")