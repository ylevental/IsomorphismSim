#' Data Generation Functions for Isomorphism Simulation Study
#'
#' Functions to generate synthetic data with controlled signal strength
#' for testing the ant colony / random forest isomorphism.
#'
#' Authors: Ernest Fokoué, Gregory Babbitt, Yuval Levental
#' Date: 2026

#' Simulate regression data with controlled signal strength
#'
#' @param n Number of observations
#' @param p Number of features
#' @param signal_strength Multiplier for the true signal
#' @param noise_sd Standard deviation of additive Gaussian noise
#' @return List with X (data.frame), y (response), f_true (true function values)
generate_data <- function(n = 1000, p = 50, signal_strength = 1, noise_sd = 1) {
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(X) <- paste0("X", 1:p)

  # True function: depends only on first 5 features (sparse signal)
  f_true <- signal_strength * (X[, 1] + X[, 2] * X[, 3] + sin(X[, 4]) + X[, 5]^2)

  y <- f_true + rnorm(n, 0, noise_sd)

  return(list(X = as.data.frame(X), y = y, f_true = f_true))
}

#' Generate classification data for binary tasks
#'
#' @param n Number of observations
#' @param p Number of features
#' @param signal_strength Signal multiplier
#' @param noise_sd Noise standard deviation
#' @return List with X, y (binary), f_true (latent)
generate_classification_data <- function(n = 1000, p = 50,
                                          signal_strength = 1, noise_sd = 1) {
  data <- generate_data(n, p, signal_strength, noise_sd)
  prob <- plogis(data$f_true)
  data$y_class <- rbinom(n, 1, prob)
  data$prob <- prob
  return(data)
}
