library(terra)
library(ggplot2)
library(tidyterra)

# --- Charger les rasters ---
ndvi <- rast("outputs/ndvi_modis.tif")
yield <- rast("outputs/yield_map.tif")

# --- Carte NDVI (palette rouge-jaune-vert) ---
p_ndvi <- ggplot() +
  geom_spatraster(data = ndvi) +
  scale_fill_gradientn(
    colours = c("#a50026", "#f46d43", "#fee08b", "#66bd63", "#1a9850"),
    na.value = "white",
    name = "NDVI"
  ) +
  labs(title = "NDVI MODIS", subtitle = "cropYieldForest - Gharb") +
  theme_minimal()

ggsave("outputs/maps/ndvi_map.png", p_ndvi, width = 8, height = 6, dpi = 150)

# --- Carte de rendement (déjà existante, on la régénère proprement si besoin) ---
p_yield <- ggplot() +
  geom_spatraster(data = yield) +
  scale_fill_viridis_c(name = "Rendement (t/ha)", na.value = "white") +
  labs(title = "Carte de rendement prédit", subtitle = "cropYieldForest - Gharb") +
  theme_minimal()

ggsave("outputs/maps/yield_map.png", p_yield, width = 8, height = 6, dpi = 150)

cat("Cartes générées : outputs/maps/ndvi_map.png et outputs/maps/yield_map.png\n")