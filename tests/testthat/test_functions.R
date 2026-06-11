library(testthat)
library(cropYieldForest)

# ── Tests calc_ndvi ───────────────────────────────────────────────────────────

test_that("calc_ndvi retourne des valeurs entre -1 et 1", {
  ndvi <- calc_ndvi(nir = 0.8, red = 0.1)
  expect_true(ndvi >= -1 && ndvi <= 1)
  expect_equal(round(ndvi, 5), round((0.8 - 0.1) / (0.8 + 0.1), 5))
})

test_that("calc_ndvi fonctionne sur vecteurs", {
  nir <- c(0.8, 0.6, 0.4)
  red <- c(0.1, 0.2, 0.3)
  ndvi <- calc_ndvi(nir, red)
  expect_length(ndvi, 3)
  expect_true(all(ndvi >= -1 & ndvi <= 1))
})

# ── Tests import_field_data ───────────────────────────────────────────────────

test_that("import_field_data lit le CSV fourni", {
  path <- system.file("field_data/parcelles_gharb.csv",
                      package = "cropYieldForest")
  skip_if(!file.exists(path), "Fichier de données manquant")
  result <- import_field_data(path)
  expect_type(result, "list")
  expect_true("sf_object" %in% names(result))
  expect_true("dataframe" %in% names(result))
  expect_true(nrow(result$dataframe) > 0)
})

test_that("import_field_data renvoie un objet sf", {
  path <- system.file("field_data/parcelles_gharb.csv",
                      package = "cropYieldForest")
  skip_if(!file.exists(path))
  result <- import_field_data(path)
  expect_true(inherits(result$sf_object, "sf"))
})

# ── Tests import_weather_data ─────────────────────────────────────────────────

test_that("import_weather_data lit le CSV météo local", {
  path <- system.file("weather_data/meteo_gharb_2022.csv",
                      package = "cropYieldForest")
  skip_if(!file.exists(path))
  meteo <- import_weather_data(local_file = path, aggregate = "daily")
  expect_s3_class(meteo, "data.frame")
  expect_true("date" %in% names(meteo))
  expect_true("temp_mean" %in% names(meteo))
  expect_true(nrow(meteo) > 0)
})

test_that("import_weather_data agrège correctement en mensuel", {
  path <- system.file("weather_data/meteo_gharb_2022.csv",
                      package = "cropYieldForest")
  skip_if(!file.exists(path))
  meteo <- import_weather_data(local_file = path, aggregate = "monthly")
  expect_true(nrow(meteo) <= 12)
})

# ── Tests preprocess_data ─────────────────────────────────────────────────────

test_that("preprocess_data sépare train/test correctement", {
  df <- data.frame(
    yield      = runif(100, 2, 8),
    ndvi_mean  = runif(100, 0.2, 0.8),
    ndvi_max   = runif(100, 0.4, 0.9),
    temp_mean  = runif(100, 15, 30)
  )
  result <- preprocess_data(df, target = "yield", exclude_cols = c(),
                            test_ratio = 0.2, seed = 42)
  expect_true(result$n_train > 0)
  expect_true(result$n_test  > 0)
  expect_equal(result$n_train + result$n_test, 100)
})

# ── Tests train_rf_model ──────────────────────────────────────────────────────

test_that("train_rf_model retourne un objet randomForest", {
  set.seed(42)
  df <- data.frame(
    yield     = runif(50, 2, 8),
    ndvi_mean = runif(50, 0.2, 0.8),
    ndvi_max  = runif(50, 0.4, 0.9),
    temp      = runif(50, 15, 30)
  )
  data_ml <- preprocess_data(df, target = "yield", exclude_cols = c(),
                             test_ratio = 0.2, seed = 42)
  model <- train_rf_model(data_ml, ntree_values = c(50, 100))
  expect_true(inherits(model$model, "randomForest"))
  expect_true(!is.null(model$test_metrics$RMSE))
  expect_true(model$test_metrics$R2 <= 1)
})

# ── Tests summarize_fields ────────────────────────────────────────────────────

test_that("summarize_fields retourne un data.frame avec les bonnes colonnes", {
  df <- data.frame(
    yield = c(3.5, 4.2, 2.8, 5.1),
    crop  = c("wheat", "barley", "wheat", "corn")
  )
  result <- summarize_fields(df, group_by_crop = TRUE)
  expect_s3_class(result, "data.frame")
  expect_true("yield_mean" %in% names(result))
  expect_true("n" %in% names(result))
})
