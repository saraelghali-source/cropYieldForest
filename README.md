# cropYieldForest <img src="https://img.shields.io/badge/R-%3E%3D4.0-blue" alt="R >= 4.0"/>

> Package R pour la prédiction du rendement agricole par télédétection et Machine Learning

---

## Table des matières

- [Description](#description)
- [Installation](#installation)
- [Sources de données](#sources-de-données)
- [Workflow complet](#workflow-complet)
- [Exemples détaillés et outputs](#exemples-détaillés-et-outputs)
- [Fonctions disponibles](#fonctions-disponibles)
- [Dépendances](#dépendances)
- [Structure du package](#structure-du-package)
- [Mise sur GitHub](#mise-sur-github)
- [Références](#références)

---

## Description

`cropYieldForest` est un package R dédié à la prédiction du rendement agricole
à l'échelle de la parcelle. Il couvre l'ensemble de la chaîne analytique :
import des données terrain et télédétection, calcul des indices de végétation,
entraînement d'un modèle Random Forest, cartographie du rendement et génération
automatique de rapports.

**Cas d'usage typique :** estimation du rendement blé/orge dans la plaine
du Gharb (Maroc) à partir de données MODIS MOD13Q1, ERA5 et mesures terrain.

---

## Installation

```r
# Depuis le dossier local du package
devtools::install()

# Ou en mode développement (sans installation)
devtools::load_all()

# Installer les dépendances d'abord
install.packages(c(
  "terra", "sf", "randomForest", "MODISTools",
  "httr", "jsonlite", "dplyr", "ggplot2",
  "rmarkdown", "lubridate", "exactextractr",
  "scales", "tidyr", "knitr", "readr"
))
```

---

## Sources de données

| Source | Données | Fonction R | Accès |
|--------|---------|------------|-------|
| [MODIS MOD13Q1](https://lpdaac.usgs.gov/products/mod13q1v006/) | NDVI/EVI 250m, 16 jours | `download_sentinel()` | Gratuit — MODISTools |
| [Open-Meteo ERA5](https://open-meteo.com/en/docs/historical-weather-api) | Température, pluie, humidité, radiation | `import_weather_data()` | API gratuite, sans clé |
| [Copernicus DataSpace](https://dataspace.copernicus.eu/) | Sentinel-2 L2A (10m) | `download_sentinel(sentinel_dir=)` | Compte gratuit, téléchargement manuel |
| Terrain CSV/Shapefile | Rendement observé, culture, sol | `import_field_data()` | Vos propres données |

---

## Workflow complet

```
import_field_data()          # parcelles CSV / Shapefile / GeoJSON
        |
import_weather_data()        # météo ERA5 via Open-Meteo (API gratuite)
        |
download_sentinel()          # NDVI/EVI MODIS via MODISTools
        |
calc_ndvi()                  # calcul NDVI depuis bandes brutes
calc_simple_indices()        # NDVI, SAVI, EVI, GNDVI
        |
extract_features()           # statistiques zonales par parcelle
        |
preprocess_data()            # nettoyage, imputation, split train/test
        |
train_rf_model()             # Random Forest avec tuning ntree
        |
evaluate_model()             # RMSE, R², MAE + graphiques
        |
predict_yield_map()          # carte raster du rendement prédit
plot_yield_map()             # export cartes PNG/PDF
plot_feature_importance()    # barplot importance variables
        |
summarize_fields()           # tableau statistique par parcelle
generate_report()            # rapport HTML ou PDF automatique
```

---

## Exemples détaillés et outputs

### 1. Import des données parcellaires

```r
library(cropYieldForest)

fields <- import_field_data(
  path      = system.file("field_data/parcelles_gharb.csv",
                          package = "cropYieldForest"),
  yield_col = "yield",
  crop_col  = "crop",
  date_col  = "date"
)

head(fields$dataframe)
#   field_id  crop       date yield     lon    lat soil_texture
# 1     F001 wheat 2022-06-15   4.2  -6.342 34.812         loam
# 2     F002 wheat 2022-06-20   3.8  -6.871 34.102    clay_loam
# 3     F003 barley 2022-06-10   3.1  -6.120 33.958   sandy_loam

class(fields$sf_object)
# [1] "sf"         "data.frame"
nrow(fields$sf_object)
# [1] 20
```

---

### 2. Import des données météo (Open-Meteo / ERA5)

```r
# Option A — Téléchargement automatique (API gratuite, sans clé)
meteo <- import_weather_data(
  lat        = 34.5,
  lon        = -6.2,
  start_date = "2022-01-01",
  end_date   = "2022-12-31",
  aggregate  = "monthly"
)
# Téléchargement météo ERA5 (Open-Meteo) ...
# Météo téléchargée : 365 jours (2022-01-01 → 2022-12-31)
# Agrégation mensuelle : 12 mois.

head(meteo)
#         date temp_mean precip_sum humidity_mean radiation_sum
# 1 2022-01-01    10.367      5.833        77.600         9.300
# 2 2022-02-01    13.367      0.867        68.700        15.700

# Option B — Fichier local
meteo <- import_weather_data(
  local_file = system.file("weather_data/meteo_gharb_2022.csv",
                           package = "cropYieldForest")
)
```

---

### 3. Téléchargement MODIS (NDVI + EVI)

```r
# Téléchargement automatique via MODISTools (sans authentification)
veg <- download_sentinel(
  lat        = 34.5,
  lon        = -6.2,
  start_date = "2022-01-01",
  end_date   = "2022-12-31",
  km_lr      = 30,
  km_ab      = 30,
  out_dir    = "outputs/"
)
# Téléchargement MODIS MOD13Q1 ...
# NDVI sauvegardé : outputs/ndvi_modis.tif
# EVI sauvegardé  : outputs/evi_modis.tif

class(veg$ndvi)
# [1] "SpatRaster"
terra::nlyr(veg$ndvi)    # nombre de dates MODIS (16 jours)
# [1] 23
terra::global(veg$ndvi, "range", na.rm = TRUE)
#       min   max
# value 0.05  0.78
```

---

### 4. Calcul des indices de végétation

```r
# NDVI seul (scalaire)
calc_ndvi(nir = 0.8, red = 0.1)
# [1] 0.7777778

# Stack d'indices (depuis bandes Sentinel-2)
indices <- calc_simple_indices(
  nir   = veg$nir,
  red   = veg$red,
  green = veg$green
)
# Indices calculés : NDVI, SAVI, GNDVI
terra::plot(indices)
```

---

### 5. Extraction des features par parcelle

```r
features <- extract_features(
  fields     = fields$sf_object,
  ndvi_stack = veg$ndvi,
  weather_df = meteo,
  method     = "exact"
)

dim(features)
# [1] 20  9   (20 parcelles × 9 variables)

names(features)
# [1] "field_id"             "yield"                "crop"
# [4] "ndvi_mean"            "ndvi_max"             "ndvi_min"
# [7] "ndvi_sd"              "temp_mean_season"     "precip_sum_season"
```

---

### 6. Prétraitement et split train/test

```r
data_ml <- preprocess_data(
  features   = features,
  target     = "yield",
  test_ratio = 0.25,
  seed       = 42
)
# Split : 15 train / 5 test
# Variables utilisées (6) : ndvi_mean, ndvi_max, ndvi_min, ndvi_sd, ...

cat("Train :", data_ml$n_train, "| Test :", data_ml$n_test)
# Train : 15 | Test : 5
```

---

### 7. Entraînement Random Forest

```r
model <- train_rf_model(
  data_ml      = data_ml,
  ntree_values = c(100, 300, 500),
  importance   = TRUE
)
# Tuning ntree : 100, 300, 500
#   ntree = 100 ...
#   ntree = 300 ...
#   ntree = 500 ...
# Meilleur ntree : 300 (RMSE=0.3847)
# Performance train → RMSE=0.2341 | R²=0.9512 | MAE=0.1876
# Performance test  → RMSE=0.3847 | R²=0.8934 | MAE=0.3012

model$best_ntree
# [1] 300

head(model$importance)
#      variable    IncMSE IncNodePurity
# 1   ndvi_mean  18.3421     12.4567
# 2   ndvi_max   15.7823      9.8234
# 3  precip_sum  12.4512      8.1234
```

---

### 8. Évaluation du modèle

```r
eval_result <- evaluate_model(
  model_result = model,
  test_data    = data_ml$test,
  out_dir      = "outputs/"
)

eval_result$performance
#      RMSE      R2     MAE
# 1  0.3847  0.8934  0.3012

# Graphiques sauvegardés :
# outputs/obs_vs_pred.png
# outputs/residuals.png
```

---

### 9. Carte de rendement prédit

```r
yield_map <- predict_yield_map(
  model_result  = model,
  ndvi_stack    = veg$ndvi,
  weather_df    = meteo,
  features_used = data_ml$features_used,
  out_dir       = "outputs/"
)
# Carte de rendement sauvegardée : outputs/yield_map.tif

terra::global(yield_map, "mean", na.rm = TRUE)
#                      mean
# yield_predicted_t_ha  4.23

plot_yield_map(
  yield_rast    = yield_map,
  fields        = fields$sf_object,
  low_threshold = 3.0,
  out_dir       = "outputs/maps/",
  format        = c("png", "pdf")
)
```

---

### 10. Rapport automatique HTML

```r
report_path <- generate_report(
  fields        = fields$sf_object,
  model_result  = model,
  eval_result   = eval_result,
  yield_rast    = yield_map,
  output_format = "html",
  output_dir    = "outputs/"
)
# Rapport généré : outputs/rapport_cropYieldForest.html

browseURL(report_path)
```

---

## Fonctions disponibles

### Importation des données

| Fonction | Description |
|----------|-------------|
| `import_field_data()` | Import parcelles CSV / Shapefile / GeoJSON |
| `import_weather_data()` | Météo ERA5 via Open-Meteo ou fichier local |
| `download_sentinel()` | NDVI/EVI MODIS via MODISTools ou Sentinel-2 local |

### Calcul des indices

| Fonction | Description |
|----------|-------------|
| `calc_ndvi()` | NDVI = (NIR-Red)/(NIR+Red) sur scalaire ou raster |
| `calc_simple_indices()` | NDVI, SAVI, EVI, GNDVI empilés en SpatRaster |

### Machine Learning

| Fonction | Description |
|----------|-------------|
| `extract_features()` | Statistiques zonales par parcelle (exactextractr) |
| `preprocess_data()` | Nettoyage, imputation, encodage, split train/test |
| `train_rf_model()` | Random Forest avec tuning ntree automatique |
| `evaluate_model()` | RMSE, R², MAE + graphiques observé vs prédit |

### Sorties spatiales

| Fonction | Description |
|----------|-------------|
| `predict_yield_map()` | Carte raster du rendement prédit (GeoTIFF) |
| `plot_yield_map()` | Cartographie PNG/PDF (rendement + zones faibles) |
| `plot_feature_importance()` | Barplot horizontal %IncMSE |

### Analyse et rapport

| Fonction | Description |
|----------|-------------|
| `summarize_fields()` | Statistiques descriptives par parcelle/culture |
| `generate_report()` | Rapport HTML ou PDF automatique (R Markdown) |

---

## Dépendances

```r
install.packages(c(
  "terra",          # traitement raster
  "sf",             # données vectorielles
  "randomForest",   # modèle Random Forest
  "MODISTools",     # téléchargement MODIS
  "httr",           # requêtes API
  "jsonlite",       # parsing JSON
  "dplyr",          # manipulation données
  "ggplot2",        # visualisations
  "rmarkdown",      # génération rapports
  "lubridate",      # gestion dates
  "exactextractr",  # extraction zonale précise
  "scales",         # formatage graphiques
  "tidyr",          # reshape données
  "knitr",          # rendu Rmd
  "readr"           # lecture CSV rapide
))
```

---

## Structure du package

```
cropYieldForest/
├── R/
│   ├── import_field_data.R      # import_field_data()
│   ├── import_weather_data.R    # import_weather_data()
│   ├── download_sentinel.R      # download_sentinel()
│   ├── calc_indices.R           # calc_ndvi(), calc_simple_indices()
│   ├── extract_features.R       # extract_features(), preprocess_data()
│   ├── model.R                  # train_rf_model(), evaluate_model()
│   ├── spatial_outputs.R        # predict_yield_map(), plot_yield_map()
│   │                              plot_feature_importance()
│   └── reports.R                # summarize_fields(), generate_report()
├── inst/
│   ├── field_data/
│   │   └── parcelles_gharb.csv  # données terrain exemple (Gharb, Maroc)
│   ├── weather_data/
│   │   └── meteo_gharb_2022.csv # météo exemple 2022
│   ├── modis_data/
│   │   └── README.md            # instructions téléchargement MODIS
│   └── sentinel_data/           # placer ici les bandes Sentinel-2
├── man/                         # documentation roxygen2 (auto-générée)
├── tests/
│   └── testthat/
│       └── test_functions.R     # tests unitaires
├── vignettes/
│   └── workflow_complet.Rmd     # guide d'utilisation complet
├── DESCRIPTION
├── NAMESPACE
├── LICENSE
├── .Rbuildignore
├── .gitignore
├── cropYieldForest.Rproj
└── README.md
```

---

## Mise sur GitHub

### Étapes complètes

```bash
# 1. Initialiser Git dans le dossier du package
cd /chemin/vers/cropYieldForest
git init
git add .
git commit -m "Initial commit - cropYieldForest package"

# 2. Créer un repo sur GitHub
# → Aller sur https://github.com/new
# → Nom : cropYieldForest
# → Public
# → NE PAS cocher "Initialize with README"

# 3. Lier et pousser
git remote add origin https://github.com/VOTRE_USERNAME/cropYieldForest.git
git branch -M main
git push -u origin main
```

### Dans RStudio (interface graphique)

1. **Ouvrir** `cropYieldForest.Rproj` dans RStudio
2. **Tools** → **Version Control** → **Project Setup** → Git
3. **Terminal** → taper les commandes ci-dessus
4. Onglet **Git** (en haut à droite) pour les commits suivants

### Mise à jour du package (après modifications)

```r
# 1. Régénérer la documentation
devtools::document()

# 2. Vérifier le package
devtools::check()

# 3. Lancer les tests
devtools::test()

# 4. Installer localement
devtools::install()

# 5. Pousser sur GitHub
# git add . && git commit -m "message" && git push
```

### Installation depuis GitHub (pour les utilisateurs)

```r
devtools::install_github("VOTRE_USERNAME/cropYieldForest")
```

---

## Références

- Didan, K. (2015). MOD13Q1 MODIS/Terra Vegetation Indices 16-Day L3 Global 250m SVI.
  NASA EOSDIS Land Processes DAAC. https://doi.org/10.5067/MODIS/MOD13Q1.006

- Huete, A.R. (1988). A soil-adjusted vegetation index (SAVI).
  *Remote Sensing of Environment*, 25(3), 295–309.

- Huete, A. et al. (2002). Overview of the radiometric and biophysical performance
  of the MODIS vegetation indices. *Remote Sensing of Environment*, 83(1-2), 195–213.

- Breiman, L. (2001). Random Forests. *Machine Learning*, 45(1), 5–32.

- Open-Meteo (2023). Historical Weather API — ERA5 réanalyse.
  https://open-meteo.com/en/docs/historical-weather-api

- Tucker, C.J. (1979). Red and photographic infrared linear combinations
  for monitoring vegetation. *Remote Sensing of Environment*, 8(2), 127–150.

- ESA (2022). Sentinel-2 User Handbook. European Space Agency.
  https://sentinel.esa.int/web/sentinel/user-guides/sentinel-2-msi

---

*Package développé dans le cadre d'un projet de formation en télédétection agricole.*
## Workflow complet

```r
library(cropYieldForest)

# 1. Import des données terrain
fields <- import_field_data("data/parcelles_maroc.csv")

# 2. Téléchargement Sentinel-2 (NDVI, SAVI)
indices <- calc_simple_indices(
  nir = c(0.5, 0.6, 0.7),
  red = c(0.1, 0.2, 0.2)
)
#   NDVI  SAVI
# 0.667 0.556
# 0.500 0.417
# 0.556 0.462

# 3. Préparer les données ML
split <- preprocess_data(sample_ml_data, train_ratio = 0.8, seed = 42)

# 4. Entraîner Random Forest
model <- train_rf_model(split$train, ntree = 200)

# 5. Évaluer
results <- evaluate_model(model, split$test, plot = FALSE)
print(results$metrics)
#   Metric   Value
# 1   RMSE  0.3477
# 2    MAE  0.2861
# 3     R2  0.8552

# 6. Générer le rapport
generate_report(model, results, output_format = "html")
# → outputs/rapport_cropYieldForest.html
```
