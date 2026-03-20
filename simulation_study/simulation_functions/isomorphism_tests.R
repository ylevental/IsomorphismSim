#' Isomorphism Testing Functions
#'
#' Experiments 3–5: Direct isomorphism validation, optimal decorrelation,
#' and sensitivity analysis across signal/noise conditions.
#'
#' Authors: Ernest Fokoué, Gregory Babbitt, Yuval Levental

library(ranger)

# ── Experiment 3: Direct Isomorphism Test ───────────────────────────────────

#' Compare random forest and ant colony under matched parameters
#'
#' @param n_replicates Number of Monte Carlo replicates per theta
#' @param p_vals Vector of theta values (m_try/p or p_explore)
#' @return data.frame with theta, system, correlation, ensemble_var
isomorphism_test <- function(
    n_replicates = 50,
    p_vals       = c(0.1, 0.3, 0.5, 0.7, 0.9)
) {
  p <- 50
  results <- data.frame()

  for (theta in p_vals) {
    cat("    theta =", theta, "\n")

    # ── Random forest side ──
    rf_cors <- rf_vars <- numeric(n_replicates)
    for (r in seq_len(n_replicates)) {
      data <- generate_data(n = 500, p = p)
      rf <- ranger(
        y ~ ., data = cbind(y = data$y[1:400], data$X[1:400, ]),
        num.trees = 200,
        mtry = max(1, round(theta * p))
      )
      preds_all <- predict(rf, data = data$X[401:500, ],
                            predict.all = TRUE)$predictions
      cm <- cor(preds_all)
      rf_cors[r] <- mean(cm[lower.tri(cm)])
      rf_vars[r] <- var(rowMeans(preds_all))
    }

    results <- rbind(results, data.frame(
      theta        = theta,
      system       = "Random Forest",
      correlation  = mean(rf_cors),
      cor_sd       = sd(rf_cors),
      ensemble_var = mean(rf_vars),
      var_sd       = sd(rf_vars)
    ))

    # ── Ant colony side ──
    ant_prefs_matrix <- matrix(NA, nrow = 50, ncol = n_replicates)
    for (r in seq_len(n_replicates)) {
      sim <- simulate_ant_colony(n_ants = 50, p_explore = theta)
      ant_prefs_matrix[, r] <- sim$ant_preferences[, which.max(c(10, 8, 6, 4, 2))]
    }
    ant_cm <- cor(ant_prefs_matrix, use = "pairwise.complete.obs")
    ant_cor <- mean(ant_cm[lower.tri(ant_cm)], na.rm = TRUE)
    ant_var <- var(colMeans(ant_prefs_matrix), na.rm = TRUE)

    results <- rbind(results, data.frame(
      theta        = theta,
      system       = "Ant Colony",
      correlation  = ant_cor,
      cor_sd       = NA,
      ensemble_var = ant_var,
      var_sd       = NA
    ))
  }
  return(results)
}

# ── Experiment 4: Optimal Decorrelation ─────────────────────────────────────

#' Optimal decorrelation experiment
#'
#' @param ensemble_sizes Vector of ensemble sizes (trees / ants)
#' @param theta_values Vector of theta values
#' @param n_replicates Monte Carlo replicates
#' @return data.frame with M, theta, system, performance, se
optimal_decorrelation_experiment <- function(
    ensemble_sizes = c(10, 30, 100),
    theta_values   = seq(0.1, 0.9, by = 0.1),
    n_replicates   = 30
) {
  p <- 50
  results <- data.frame()

  for (M in ensemble_sizes) {
    for (theta in theta_values) {
      cat("    M =", M, ", theta =", theta, "\n")

      # Random forest performance (MSE)
      rf_errors <- replicate(n_replicates, {
        data <- generate_data(n = 500, p = p)
        rf <- ranger(
          y ~ ., data = cbind(y = data$y[1:400], data$X[1:400, ]),
          num.trees = M,
          mtry = max(1, round(theta * p))
        )
        preds <- predict(rf, data = data$X[401:500, ])$predictions
        mean((preds - data$f_true[401:500])^2)
      })

      # Ant colony performance (accuracy)
      ant_acc <- replicate(n_replicates, {
        sim <- simulate_ant_colony(n_ants = M, p_explore = theta)
        as.numeric(sim$decision == which.max(c(10, 8, 6, 4, 2)))
      })

      results <- rbind(results, data.frame(
        M           = M,
        theta       = theta,
        system      = "Random Forest",
        performance = mean(rf_errors),
        se          = sd(rf_errors) / sqrt(n_replicates)
      ))

      results <- rbind(results, data.frame(
        M           = M,
        theta       = theta,
        system      = "Ant Colony",
        performance = 1 - mean(ant_acc),
        se          = sd(ant_acc) / sqrt(n_replicates)
      ))
    }
  }
  return(results)
}

# ── Experiment 5: Sensitivity Analysis ──────────────────────────────────────

#' Sensitivity analysis across signal strengths and noise levels
#'
#' @param signal_strengths Vector of signal multipliers
#' @param noise_levels Vector of noise standard deviations
#' @param n_replicates Monte Carlo replicates
#' @return data.frame with signal, noise, theta, system, correlation, cor_sd
sensitivity_analysis <- function(
    signal_strengths = c(0.5, 1, 2, 5),
    noise_levels     = c(0.5, 1, 2, 4),
    n_replicates     = 20
) {
  p <- 50
  results <- data.frame()

  for (signal in signal_strengths) {
    for (noise in noise_levels) {
      for (theta in c(0.2, 0.5, 0.8)) {
        cat("    signal =", signal, ", noise =", noise, ", theta =", theta, "\n")

        # Random forest correlation
        rf_cors <- replicate(n_replicates, {
          data <- generate_data(n = 500, p = p,
                                 signal_strength = signal, noise_sd = noise)
          rf <- ranger(
            y ~ ., data = cbind(y = data$y[1:400], data$X[1:400, ]),
            num.trees = 100,
            mtry = max(1, round(theta * p))
          )
          preds_all <- predict(rf, data = data$X[401:500, ],
                                predict.all = TRUE)$predictions
          cm <- cor(preds_all)
          mean(cm[lower.tri(cm)])
        })

        # Ant colony correlation
        ant_prefs <- matrix(NA, nrow = 50, ncol = n_replicates)
        site_quals <- c(10, 8, 6, 4, 2) * signal
        for (r in seq_len(n_replicates)) {
          sim <- simulate_ant_colony(
            n_ants         = 50,
            p_explore      = theta,
            site_qualities = site_quals,
            noise_sd       = noise
          )
          ant_prefs[, r] <- sim$ant_preferences[, which.max(site_quals)]
        }
        ant_cm <- cor(ant_prefs, use = "pairwise.complete.obs")
        ant_cor <- mean(ant_cm[lower.tri(ant_cm)], na.rm = TRUE)

        results <- rbind(results, data.frame(
          signal      = signal,
          noise       = noise,
          theta       = theta,
          system      = "Random Forest",
          correlation = mean(rf_cors, na.rm = TRUE),
          cor_sd      = sd(rf_cors, na.rm = TRUE)
        ))

        results <- rbind(results, data.frame(
          signal      = signal,
          noise       = noise,
          theta       = theta,
          system      = "Ant Colony",
          correlation = ant_cor,
          cor_sd      = NA
        ))
      }
    }
  }
  return(results)
}
