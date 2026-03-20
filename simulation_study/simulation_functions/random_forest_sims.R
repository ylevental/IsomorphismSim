#' Random Forest Simulation Functions
#'
#' Experiment 1: Variance decomposition validation
#' Demonstrates that Var[f_rf] = rho*sigma^2 + (1-rho)*sigma^2/M
#' holds empirically and that rho varies with m_try/p as predicted.
#'
#' Authors: Ernest Fokoué, Gregory Babbitt, Yuval Levental

library(ranger)

#' Compute empirical variance decomposition for random forests
#'
#' @param n_train Training set size
#' @param n_test Test set size
#' @param p Number of features
#' @param m_try_values Vector of m_try values to test
#' @param n_trees Number of trees per forest
#' @param n_replicates Number of Monte Carlo replicates
#' @param signal_strength Signal strength for data generation
#' @param noise_sd Noise standard deviation
#' @return data.frame with columns: m_try, rep, rho, sigma2,
#'         ensemble_var, pred_error, theoretical_var
variance_decomposition_experiment <- function(
    n_train = 500,
    n_test = 1000,
    p = 50,
    m_try_values = c(1, 2, 5, 10, 20, 50),
    n_trees = 500,
    n_replicates = 100,
    signal_strength = 2,
    noise_sd = 1
) {
  results <- expand.grid(
    m_try = m_try_values,
    rep   = 1:n_replicates
  )
  results$rho             <- NA_real_
  results$sigma2          <- NA_real_
  results$ensemble_var    <- NA_real_
  results$pred_error      <- NA_real_
  results$theoretical_var <- NA_real_

  for (i in seq_len(nrow(results))) {
    data <- generate_data(n_train + n_test, p, signal_strength, noise_sd)
    train_idx <- 1:n_train
    test_idx  <- (n_train + 1):(n_train + n_test)

    X_train    <- data$X[train_idx, ]
    y_train    <- data$y[train_idx]
    X_test     <- data$X[test_idx, ]
    f_true_test <- data$f_true[test_idx]

    rf <- ranger(
      y ~ .,
      data         = cbind(y = y_train, X_train),
      num.trees    = n_trees,
      mtry         = results$m_try[i],
      keep.inbag   = TRUE,
      write.forest = TRUE
    )

    # Individual tree predictions on test set (n_test x n_trees matrix)
    tree_preds <- predict(rf, data = X_test, predict.all = TRUE)$predictions

    # Variance of individual trees (sigma^2)
    sigma2_est <- mean(apply(tree_preds, 2, var))

    # Pairwise correlation (rho) — subsample for speed when n_trees large
    if (n_trees > 100) {
      idx <- sample(n_trees, 100)
      cor_sub <- cor(tree_preds[, idx])
    } else {
      cor_sub <- cor(tree_preds)
    }
    rho_est <- mean(cor_sub[lower.tri(cor_sub)])

    ensemble_preds  <- rowMeans(tree_preds)
    ensemble_var_est <- var(ensemble_preds)
    theoretical_var  <- rho_est * sigma2_est +
      (1 - rho_est) * sigma2_est / n_trees

    results$sigma2[i]          <- sigma2_est
    results$rho[i]             <- rho_est
    results$ensemble_var[i]    <- ensemble_var_est
    results$theoretical_var[i] <- theoretical_var
    results$pred_error[i]      <- mean((ensemble_preds - f_true_test)^2)

    if (i %% 50 == 0) cat("  RF experiment:", i, "/", nrow(results), "\n")
  }

  return(results)
}
