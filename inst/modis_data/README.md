# Données MODIS — Instructions de téléchargement

Ce dossier contient les données NDVI/EVI téléchargées depuis MODIS
via la fonction `download_sentinel()` du package `cropYieldForest`.

## Source officielle

- **Produit** : MOD13Q1 v006 — MODIS/Terra Vegetation Indices 16-Day L3 Global 250m
- **URL LP DAAC** : https://lpdaac.usgs.gov/products/mod13q1v006/
- **Référence** : Didan, K. (2015). MOD13Q1 MODIS/Terra Vegetation Indices.
  NASA EOSDIS Land Processes DAAC.

## Téléchargement automatique via R

```r
library(cropYieldForest)

# Téléchargement NDVI MODIS pour la plaine du Gharb (Maroc), 2022
veg <- download_sentinel(
  lat        = 34.5,
  lon        = -6.2,
  start_date = "2022-01-01",
  end_date   = "2022-12-31",
  km_lr      = 30,
  km_ab      = 30,
  out_dir    = "inst/modis_data/"
)
```

## Téléchargement manuel (alternative)

1. Aller sur https://appeears.earthdatacloud.nasa.gov/
2. Créer un compte NASA Earthdata gratuit
3. "Submit" → "Point & Polygon"
4. Sélectionner le produit : MOD13Q1
5. Bandes : 250m_16_days_NDVI, 250m_16_days_EVI
6. Zone : définir la bbox de votre région
7. Télécharger les fichiers .tif résultants dans ce dossier

## Structure attendue des fichiers

```
inst/modis_data/
├── ndvi_modis.tif     ← généré par download_sentinel()
├── evi_modis.tif      ← généré par download_sentinel()
└── README.md          ← ce fichier
```

## Pour Sentinel-2 (haute résolution, 10m)

Téléchargement manuel depuis : https://dataspace.copernicus.eu/
- Bandes requises : B03 (vert), B04 (rouge), B08 (NIR)
- Format : GeoTIFF, niveau 2A (réflectance de surface)
- Placer dans : inst/sentinel_data/

```r
# Utilisation des bandes Sentinel-2 locales
veg <- download_sentinel(
  sentinel_dir = "inst/sentinel_data/T29SPP_20220601T105021/"
)
```
