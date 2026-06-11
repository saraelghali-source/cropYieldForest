#' Entraînement du modèle Random Forest pour la prédiction de rendement
#'
#' Entraîne un modèle Random Forest sur le jeu d'entraînement avec tuning
#' du nombre d'arbres (ntree). Utilise une validation croisée simple
#' train/test et calcule les métriques de performance.
#'
#' @param data_ml Liste issue de \code{preprocess_data} (avec $train et $test).
#' @param target Nom de la colonne cible (défaut: "yield").
#' @param ntree_values Vecteur de valeurs ntree à tester (défaut: c(100, 300, 500)).
#' @param mtry Nombre de variables à tirer par nœud. NULL = valeur par défaut
#'   (floor(sqrt(p)) pour classification, p/3 pour régression).
#' @param importance Calculer l'importance des variables ? (défaut: TRUE).
#' @param seed Graine aléatoire (défaut: 42).
#'
#' @return Liste avec :
#'   \describe{
#'     \item{model}{Objet \code{randomForest} final (meilleur ntree).}
#'     \item{best_ntree}{Valeur de ntree retenue (minimise le RMSE test).}
#'     \item{importance}{data.frame d'importance des variables.}
#'     \item{train_metrics}{Métriques sur le jeu d'entraînement.}
#'     \item{test_metrics}{Métriques sur le jeu de test.}
#'     \item{tuning_results}{data.frame des résultats de tuning.}
#'   }
#'
#' @examples
#' \dontrun{
#' data_ml <- preprocess_data(features, target = "yield")
#' model   <- train_rf_model(data_ml, ntree_values = c(100, 300, 500))
#'
#' cat("Meilleur ntree :", model$best_ntree, "\n")
#' cat("RMSE test      :", round(model$test_metrics$RMSE, 3), "\n")
#' cat("R²   test      :", round(model$test_metrics$R2, 3), "\n")
#' }
#'
#' @export
train_rf_model <- function(data_ml,
                           target       = "yield",
                           ntree_values = c(100, 300, 500),
                           mtry         = NULL,
                           importance   = TRUE,
                           seed         = 42) {

  if (!is.list(data_ml) || !"train" %in% names(data_ml)) {
    stop("data_ml doit être la sortie de preprocess_data().")
  }

  train <- data_ml$train
  test  <- data_ml$test

  if (!target %in% names(train)) {
    stop("Colonne cible '", target, "' introuvable dans data_ml$train.")
  }

  formula <- as.formula(paste(target, "~ ."))

  # ── Tuning ntree ──────────────────────────────────────────────────────────────
  tuning <- data.frame(ntree = ntree_values, RMSE_test = NA_real_, R2_test = NA_real_)

  message("Tuning ntree : ", paste(ntree_values, collapse = ", "))

  for (i in seq_along(ntree_values)) {
    set.seed(seed)
    nt <- ntree_values[i]
    message("  ntree = ", nt, " ...")

    args_rf <- list(formula = formula, data = train, ntree = nt,
                    importance = importance)
    if (!is.null(mtry)) args_rf$mtry <- mtry

    rf_tmp <- tryCatch(
      do.call(randomForest::randomForest, args_rf),
      error = function(e) stop("Erreur randomForest (ntree=", nt, ") : ", e$message)
    )

    pred_test    <- stats::predict(rf_tmp, test)
    obs_test     <- test[[target]]
    tuning$RMSE_test[i] <- sqrt(mean((obs_test - pred_test)^2, na.rm = TRUE))
    tuning$R2_test[i]   <- .calc_r2(obs_test, pred_test)
  }

  best_idx  <- which.min(tuning$RMSE_test)
  best_ntree <- tuning$ntree[best_idx]
  message("Meilleur ntree : ", best_ntree,
          " (RMSE=", round(tuning$RMSE_test[best_idx], 4), ")")

  # ── Entraînement final ────────────────────────────────────────────────────────
  set.seed(seed)
  args_final <- list(formula = formula, data = train, ntree = best_ntree,
                     importance = importance)
  if (!is.null(mtry)) args_final$mtry <- mtry
  model_final <- do.call(randomForest::randomForest, args_final)

  # ── Métriques ─────────────────────────────────────────────────────────────────
  pred_train <- stats::predict(model_final, train)
  pred_test  <- stats::predict(model_final, test)

  train_metrics <- .calc_metrics(train[[target]], pred_train)
  test_metrics  <- .calc_metrics(test[[target]],  pred_test)

  message("Performance train → RMSE=", round(train_metrics$RMSE, 4),
          " | R²=", round(train_metrics$R2, 4),
          " | MAE=", round(train_metrics$MAE, 4))
  message("Performance test  → RMSE=", round(test_metrics$RMSE, 4),
          " | R²=", round(test_metrics$R2, 4),
          " | MAE=", round(test_metrics$MAE, 4))

  # ── Importance des variables ───────────────────────────────────────────────────
  imp_df <- NULL
  if (importance) {
    imp_mat <- randomForest::importance(model_final)
    imp_df  <- data.frame(
      variable             = rownames(imp_mat),
      IncMSE               = imp_mat[, "%IncMSE"],
      IncNodePurity        = imp_mat[, "IncNodePurity"],
      stringsAsFactors     = FALSE
    )
    imp_df <- imp_df[order(-imp_df$IncMSE), ]
    rownames(imp_df) <- NULL
    message("Top 5 variables : ",
            paste(head(imp_df$variable, 5), collapse = ", "))
  }

  list(
    model          = model_final,
    best_ntree     = best_ntree,
    importance     = imp_df,
    train_metrics  = train_metrics,
    test_metrics   = test_metrics,
    tuning_results = tuning
  )
}


#' Évaluation des performances du modèle
#'
#' Calcule les métriques de régression (RMSE, R², MAE) et produit les
#' visualisations de validation (observé vs prédit, résidus).
#'
#' @param model_result Liste issue de \code{train_rf_model}.
#' @param test_data data.frame de test (sinon utilise model_result$test_metrics).
#' @param target Nom de la colonne cible (défaut: "yield").
#' @param show_plots Afficher les graphiques ? (défaut: TRUE).
#' @param out_dir Dossier de sortie pour les graphiques (défaut: "outputs/").
#'
#' @return Liste avec :
#'   \describe{
#'     \item{performance}{data.frame des métriques (RMSE, R², MAE).}
#'     \item{predictions}{data.frame observé vs prédit.}
#'     \item{plots}{Liste des objets ggplot.}
#'   }
#'
#' @examples
#' \dontrun{
#' model  <- train_rf_model(data_ml)
#' eval   <- evaluate_model(model, data_ml$test)
#' eval$performance
#' #      RMSE    R2   MAE
#' # 1  0.412  0.87  0.31
#' }
#'
#' @export
evaluate_model <- function(model_result,
                           test_data  = NULL,
                           target     = "yield",
                           show_plots = TRUE,
                           out_dir    = "outputs/") {

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  if (!is.list(model_result) || !"model" %in% names(model_result)) {
    stop("model_result doit être la sortie de train_rf_model().")
  }

  # Prédictions
  if (!is.null(test_data) && target %in% names(test_data)) {
    observed  <- test_data[[target]]
    predicted <- stats::predict(model_result$model, test_data)
  } else {
    message("test_data non fourni. Utilisation des métriques de model_result.")
    return(list(performance = as.data.frame(model_result$test_metrics),
                predictions = NULL, plots = NULL))
  }

  metrics <- .calc_metrics(observed, predicted)
  pred_df <- data.frame(observed = observed, predicted = predicted,
                        residual = observed - predicted)

  plots <- list()

  # Graphique 1 : Observé vs Prédit
  p1 <- ggplot2::ggplot(pred_df, ggplot2::aes(x = observed, y = predicted)) +
    ggplot2::geom_point(alpha = 0.6, color = "#2E86AB", size = 2) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red",
                         linetype = "dashed", linewidth = 1) +
    ggplot2::labs(
      title    = "Rendement observé vs prédit",
      subtitle = paste0("R² = ", round(metrics$R2, 3),
                        " | RMSE = ", round(metrics$RMSE, 3)),
      x        = "Rendement observé (t/ha)",
      y        = "Rendement prédit (t/ha)"
    ) +
    ggplot2::theme_minimal(base_size = 13)

  # Graphique 2 : Résidus
  p2 <- ggplot2::ggplot(pred_df, ggplot2::aes(x = predicted, y = residual)) +
    ggplot2::geom_point(alpha = 0.6, color = "#E84855", size = 2) +
    ggplot2::geom_hline(yintercept = 0, color = "black",
                        linetype = "dashed", linewidth = 0.8) +
    ggplot2::labs(
      title = "Graphique des résidus",
      x     = "Rendement prédit (t/ha)",
      y     = "Résidu (observé − prédit)"
    ) +
    ggplot2::theme_minimal(base_size = 13)

  plots[["obs_vs_pred"]] <- p1
  plots[["residuals"]]   <- p2

  if (show_plots) {
    print(p1)
    print(p2)
  }

  ggplot2::ggsave(file.path(out_dir, "obs_vs_pred.png"), p1, width = 7, height = 6, dpi = 150)
  ggplot2::ggsave(file.path(out_dir, "residuals.png"),   p2, width = 7, height = 6, dpi = 150)
  message("Graphiques sauvegardés dans : ", out_dir)

  list(
    performance = as.data.frame(metrics),
    predictions = pred_df,
    plots       = plots
  )
}


# ── Fonctions internes ────────────────────────────────────────────────────────

#' @keywords internal
.calc_r2 <- function(obs, pred) {
  ss_res <- sum((obs - pred)^2, na.rm = TRUE)
  ss_tot <- sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
  1 - ss_res / ss_tot
}

#' @keywords internal
.calc_metrics <- function(obs, pred) {
  list(
    RMSE = sqrt(mean((obs - pred)^2, na.rm = TRUE)),
    R2   = .calc_r2(obs, pred),
    MAE  = mean(abs(obs - pred), na.rm = TRUE)
  )
}
