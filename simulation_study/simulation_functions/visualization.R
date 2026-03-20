#' Visualization Functions
#'
#' Generate all manuscript figures for the isomorphism paper.
#' Figures 1–5 follow publication-quality ggplot2 styling.
#'
#' Authors: Ernest Fokoué, Gregory Babbitt, Yuval Levental

library(ggplot2)
library(ggpubr)
library(gridExtra)
library(viridis)

# ── Common theme ────────────────────────────────────────────────────────────

theme_manuscript <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", hjust = 0.5, size = base_size + 2),
      plot.subtitle    = element_text(hjust = 0.5, color = "grey40"),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold")
    )
}

# ── Figure 1: Isomorphism Schematic (conceptual) ───────────────────────────

#' Create the isomorphism schematic showing the mapping between systems
#'
#' @param rf_results Results from Experiment 1
#' @param colony_results Results from Experiment 2
#' @return ggplot object
create_isomorphism_schematic <- function(rf_results, colony_results) {
  # Summarise RF
  rf_summary <- rf_results |>
    dplyr::group_by(m_try) |>
    dplyr::summarise(
      rho_mean      = mean(rho, na.rm = TRUE),
      m_try_ratio   = dplyr::first(m_try) / 50,
      .groups = "drop"
    )

  # Summarise colony
  colony_cor <- colony_results |>
    dplyr::group_by(p_explore) |>
    dplyr::summarise(cor_avg = mean(cor_mean, na.rm = TRUE), .groups = "drop")

  # Theoretical
  theta_seq <- seq(0, 1, length.out = 100)
  rho_max <- max(c(rf_summary$rho_mean, colony_cor$cor_avg), na.rm = TRUE)
  theoretical <- data.frame(
    theta       = theta_seq,
    correlation = rho_max * (1 - theta_seq),
    system      = "Theoretical"
  )

  # Flip RF: θ = 1 − m_try/p so that higher θ = more decorrelation
  rf_emp <- data.frame(
    theta       = 1 - rf_summary$m_try_ratio,
    correlation = rf_summary$rho_mean,
    system      = "Random Forest"
  )

  ant_emp <- data.frame(
    theta       = colony_cor$p_explore,
    correlation = colony_cor$cor_avg,
    system      = "Ant Colony"
  )

  plot_data <- rbind(theoretical, rf_emp, ant_emp)

  # Use a single combined aesthetic (shape = NA for Theoretical) to merge legends
  ggplot(plot_data, aes(x = theta, y = correlation,
                         color = system, linetype = system, shape = system)) +
    geom_line(data = subset(plot_data, system == "Theoretical"),
              linewidth = 1.2) +
    geom_point(data = subset(plot_data, system != "Theoretical"),
               size = 3.5) +
    geom_line(data = subset(plot_data, system != "Theoretical"),
              linewidth = 0.8) +
    scale_color_manual(values = c(
      "Theoretical"   = "black",
      "Random Forest" = "#2E7D32",
      "Ant Colony"    = "#BF360C"
    )) +
    scale_linetype_manual(values = c(
      "Theoretical"   = "solid",
      "Random Forest" = "dashed",
      "Ant Colony"    = "dotted"
    )) +
    scale_shape_manual(values = c("Theoretical" = NA,
                                   "Random Forest" = 16,
                                   "Ant Colony" = 17)) +
    guides(color = guide_legend(override.aes = list(
             shape = c(17, 16, NA),
             linetype = c("dotted", "dashed", "solid")
           ))) +
    labs(
      title    = "The Isomorphism: Correlation Decay with Decorrelation Parameter",
      subtitle = expression("Both systems follow " ~ rho == rho[max](1 - theta)),
      x = expression(theta ~ "(1 - m_try/p  or  p_explore)"),
      y = expression("Pairwise Correlation " ~ rho)
    ) +
    theme_manuscript()
}

# ── Figure 2: Variance Decomposition ───────────────────────────────────────

#' Plot RF correlation vs m_try/p and theoretical vs empirical variance
#'
#' @param rf_results Results from Experiment 1
#' @return ggplot object
plot_variance_decomposition <- function(rf_results) {
  rf_summary <- rf_results |>
    dplyr::group_by(m_try) |>
    dplyr::summarise(
      rho_mean            = mean(rho),
      rho_sd              = sd(rho),
      sigma2_mean         = mean(sigma2),
      ensemble_var_mean   = mean(ensemble_var),
      theoretical_var_mean = mean(theoretical_var),
      m_try_ratio         = dplyr::first(m_try) / 50,
      .groups = "drop"
    )

  p1 <- ggplot(rf_summary, aes(x = m_try_ratio, y = rho_mean)) +
    geom_point(size = 3, color = "#2E7D32") +
    geom_line(color = "#2E7D32", linewidth = 0.8) +
    geom_errorbar(aes(ymin = rho_mean - 2 * rho_sd,
                      ymax = rho_mean + 2 * rho_sd),
                  width = 0.02, color = "#2E7D32") +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                linetype = "dashed", color = "red") +
    labs(
      title = "A. Correlation vs Feature Subsampling",
      x     = expression(m[try] / p),
      y     = expression(rho)
    ) +
    theme_manuscript()

  p2 <- ggplot(rf_summary, aes(x = ensemble_var_mean, y = theoretical_var_mean)) +
    geom_point(size = 3, color = "#1565C0") +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(
      title = "B. Theoretical vs Empirical Variance",
      x     = "Empirical Ensemble Variance",
      y     = expression(rho * sigma^2 + (1 - rho) * sigma^2 / M)
    ) +
    theme_manuscript()

  ggarrange(p1, p2, ncol = 2, common.legend = FALSE)
}

# ── Figure 3: Correlation Decay Comparison ─────────────────────────────────

#' Plot correlation decay in both systems
#'
#' @param iso_results Results from Experiment 3
#' @return ggplot object
plot_correlation_decay <- function(iso_results) {
  # Flip RF theta: θ = 1 − m_try/p
  plot_data <- iso_results
  plot_data$theta[plot_data$system == "Random Forest"] <-
    1 - plot_data$theta[plot_data$system == "Random Forest"]
  plot_data$cor_sd[is.na(plot_data$cor_sd)] <- 0

  ggplot(plot_data, aes(x = theta, y = correlation,
                         color = system, shape = system, fill = system)) +
    geom_point(size = 3.5) +
    geom_line(linewidth = 0.8) +
    geom_ribbon(aes(ymin = correlation - 1.96 * cor_sd,
                    ymax = correlation + 1.96 * cor_sd),
                alpha = 0.15, color = NA) +
    scale_color_manual(values = c("Random Forest" = "#2E7D32",
                                   "Ant Colony"    = "#BF360C")) +
    scale_fill_manual(values = c("Random Forest" = "#2E7D32",
                                  "Ant Colony"    = "#BF360C")) +
    scale_shape_manual(values = c("Random Forest" = 16, "Ant Colony" = 17)) +
    labs(
      title = "Isomorphism Validation: Correlation Decay in Both Systems",
      x     = expression(theta ~ "(1 - m_try/p  or  p_explore)"),
      y     = expression("Pairwise Correlation " ~ rho)
    ) +
    theme_manuscript()
}

# ── Figure 4: Optimal Decorrelation ────────────────────────────────────────

#' Plot performance vs theta for different ensemble sizes
#'
#' @param optimal_results Results from Experiment 4
#' @return ggplot object
plot_optimal_decorrelation <- function(optimal_results) {
  ggplot(optimal_results, aes(x = theta, y = performance,
                               color = factor(M), group = M)) +
    geom_point(size = 2) +
    geom_line(linewidth = 0.7) +
    geom_errorbar(aes(ymin = performance - 1.96 * se,
                      ymax = performance + 1.96 * se),
                  width = 0.02, alpha = 0.5) +
    facet_wrap(~ system, scales = "free_y") +
    scale_color_viridis_d(option = "D", end = 0.85) +
    labs(
      title = "Optimal Decorrelation: Performance vs Exploration Parameter",
      x     = expression(theta),
      y     = "Error (lower is better)",
      color = "Ensemble Size (M)"
    ) +
    theme_manuscript() +
    theme(legend.position = "right")
}

# ── Figure 5: Sensitivity Heat-map ─────────────────────────────────────────

#' Plot sensitivity analysis heatmap
#'
#' @param sensitivity_results Results from Experiment 5
#' @return ggplot object
plot_sensitivity_heatmap <- function(sensitivity_results) {
  sens_rf <- subset(sensitivity_results, system == "Random Forest")

  ggplot(sens_rf, aes(x = factor(signal), y = factor(noise),
                       fill = correlation)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.2f", correlation)),
              color = "white", size = 3.5) +
    facet_wrap(~ paste("theta =", theta), ncol = 3) +
    scale_fill_viridis(option = "C", direction = -1,
                       name = expression(rho)) +
    labs(
      title = "Sensitivity: Tree Correlation Across Signal/Noise Conditions",
      x     = "Signal Strength",
      y     = "Noise Level"
    ) +
    theme_manuscript() +
    theme(legend.position = "right")
}

# ── Colony accuracy plot (supplementary) ────────────────────────────────────

#' Plot colony accuracy vs colony size
#'
#' @param colony_results Results from Experiment 2
#' @return ggplot object
plot_colony_accuracy <- function(colony_results) {
  ggplot(colony_results, aes(x = n_ants, y = accuracy_mean,
                              color = factor(p_explore),
                              group = p_explore)) +
    geom_point(size = 3) +
    geom_line(linewidth = 0.7) +
    scale_color_viridis_d(option = "D", end = 0.9) +
    labs(
      title = "Ant Colony: Decision Accuracy vs Colony Size",
      x     = "Number of Ants (N)",
      y     = "Probability of Correct Decision",
      color = expression(p[explore])
    ) +
    theme_manuscript() +
    theme(legend.position = "right")
}

# ── Save all manuscript figures ─────────────────────────────────────────────

#' Generate and save all five manuscript figures
#'
#' @param rf_results Experiment 1 results
#' @param colony_results Experiment 2 results
#' @param iso_results Experiment 3 results
#' @param optimal_results Experiment 4 results
#' @param sensitivity_results Experiment 5 results
#' @param output_dir Directory for saved figures
save_manuscript_figures <- function(rf_results, colony_results, iso_results,
                                    optimal_results, sensitivity_results,
                                    output_dir = "manuscript_figures") {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  fig1 <- create_isomorphism_schematic(rf_results, colony_results)
  ggsave(file.path(output_dir, "figure1_isomorphism_schematic.pdf"),
         fig1, width = 10, height = 6)

  fig2 <- plot_variance_decomposition(rf_results)
  ggsave(file.path(output_dir, "figure2_variance_decomposition.pdf"),
         fig2, width = 12, height = 5)

  fig3 <- plot_correlation_decay(iso_results)
  ggsave(file.path(output_dir, "figure3_correlation_decay.pdf"),
         fig3, width = 10, height = 6)

  fig4 <- plot_optimal_decorrelation(optimal_results)
  ggsave(file.path(output_dir, "figure4_optimal_decorrelation.pdf"),
         fig4, width = 12, height = 6)

  fig5 <- plot_sensitivity_heatmap(sensitivity_results)
  ggsave(file.path(output_dir, "figure5_sensitivity_heatmap.pdf"),
         fig5, width = 12, height = 6)

  cat("All figures saved to:", output_dir, "\n")

  invisible(list(fig1 = fig1, fig2 = fig2, fig3 = fig3,
                 fig4 = fig4, fig5 = fig5))
}
