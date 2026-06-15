Résultats générés par le package
# Outputs — cropYieldForest

Ce dossier reçoit automatiquement les fichiers générés par le workflow :

## Fichiers générés
- `ndvi_modis.tif`        ← téléchargé par download_sentinel()
- `evi_modis.tif`         ← téléchargé par download_sentinel()
- `obs_vs_pred.png`       ← généré par evaluate_model()
- `residuals.png`         ← généré par evaluate_model()
- `yield_map.tif`         ← généré par predict_yield_map()
- `maps/yield_map.png`    ← généré par plot_yield_map()
- `maps/yield_map.pdf`    ← généré par plot_yield_map()
- `rapport_cropYieldForest.html` ← généré par generate_report()