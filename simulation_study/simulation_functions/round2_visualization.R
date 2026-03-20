#' Round 2 Visualization Functions
#'
#' Figures for the revised experiments exploring task complexity,
#' time horizons, and the Emergence Function E(t).
#'
#' Authors: Ernest Fokoué, Gregory Babbitt, Yuval Levental

library(ggplot2)
library(viridis)

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

# ── Figure 6: Task Complexity — Correlation vs p_explore by n_sites ─────────

plot_complexity_correlation <- function(complexity_results) {
  ggplot(complexity_results,
         aes(x = p_explore, y = correlation,
             color = factor(n_sites), group = n_sites)) +
    geom_point(size = 2.5) +
    geom_line(linewidth = 0.7) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
    facet_wrap(~ paste("Quality gap =", quality_gap), ncol = 3) +
    scale_color_viridis_d(option = "D", end = 0.85) +
    labs(
      title = "Task Complexity: Ant Correlation vs Exploration Probability",
      subtitle = "More sites and tighter gaps should reveal the correlation decay",
      x     = expression(p[explore]),
      y     = expression("Pairwise Correlation " ~ rho),
      color = "Number of Sites"
    ) +
    theme_manuscript() +
    theme(legend.position = "right")
}

# ── Figure 7: Time Horizon — Correlation vs p_explore by n_steps ────────────

plot_time_horizon_correlation <- function(time_results) {
  ggplot(time_results,
         aes(x = p_explore, y = correlation,
             color = factor(n_steps), group = n_steps)) +
    geom_point(size = 2.5) +
    geom_line(linewidth = 0.7) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
    scale_color_viridis_d(option = "C", end = 0.85) +
    labs(
      title = "Time Pressure: Ant Correlation vs Exploration Probability",
      subtitle = paste0("n_sites = ", time_results$n_sites[1],
                        ", quality_gap = ", time_results$quality_gap[1]),
      x     = expression(p[explore]),
      y     = expression("Pairwise Correlation " ~ rho),
      color = "Time Steps"
    ) +
    theme_manuscript() +
    theme(legend.position = "right")
}

# ── Figure 8: Emergence Function E(t) ──────────────────────────────────────

plot_emergence <- function(emergence_results) {
  p1 <- ggplot(emergence_results,
               aes(x = time, y = emergence_mean,
                   color = factor(p_explore), fill = factor(p_explore))) +
    geom_line(linewidth = 0.8) +
    geom_ribbon(aes(ymin = emergence_mean - emergence_sd,
                    ymax = pmin(emergence_mean + emergence_sd, 1)),
                alpha = 0.1, color = NA) +
    scale_color_viridis_d(option = "D", end = 0.85) +
    scale_fill_viridis_d(option = "D", end = 0.85) +
    labs(
      title = "A. Emergence Function E(t)",
      subtitle = "Transition from chaos to order (Knar, 2025)",
      x     = "Time Step",
      y     = "E(t)",
      color = expression(p[explore])
    ) +
    theme_manuscript() +
    theme(legend.position = "right")

  p2 <- ggplot(emergence_results,
               aes(x = time, y = alignment_mean,
                   color = factor(p_explore), fill = factor(p_explore))) +
    geom_line(linewidth = 0.8) +
    geom_ribbon(aes(ymin = pmax(alignment_mean - alignment_sd, 0),
                    ymax = pmin(alignment_mean + alignment_sd, 1)),
                alpha = 0.1, color = NA) +
    scale_color_viridis_d(option = "D", end = 0.85) +
    scale_fill_viridis_d(option = "D", end = 0.85) +
    labs(
      title = "B. Alignment: Fraction of Ants Preferring Best Site",
      x     = "Time Step",
      y     = "Fraction Aligned",
      color = expression(p[explore])
    ) +
    theme_manuscript() +
    theme(legend.position = "right")

  ggpubr::ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend = "right")
}

# ── Figure 9: Regime Search Heatmap ─────────────────────────────────────────

plot_regime_heatmap <- function(regime_results) {
  # For each n_sites × n_steps combo, compute the slope of rho vs p_explore
  # A negative slope = the isomorphism is visible
  slopes <- regime_results |>
    dplyr::group_by(n_sites, n_steps) |>
    dplyr::summarise(
      rho_range = max(correlation) - min(correlation),
      rho_slope = coef(lm(correlation ~ p_explore))[2],
      mean_accuracy = mean(accuracy),
      .groups = "drop"
    )

  ggplot(slopes, aes(x = factor(n_sites), y = factor(n_steps),
                      fill = rho_range)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.3f", rho_range)),
              color = "white", size = 4) +
    scale_fill_viridis(option = "B", direction = -1,
                        name = expression(Delta * rho)) +
    labs(
      title = "Regime Search: Where Does Correlation Respond to p_explore?",
      subtitle = expression("Larger" ~ Delta * rho ~ "= stronger isomorphism signal"),
      x = "Number of Sites",
      y = "Time Steps"
    ) +
    theme_manuscript() +
    theme(legend.position = "right")
}

# ── Figure 10: Correlation decay at best regime ────────────────────────────

plot_best_regime_decay <- function(regime_results) {
  # Find regime with largest rho_range
  slopes <- regime_results |>
    dplyr::group_by(n_sites, n_steps) |>
    dplyr::summarise(
      rho_range = max(correlation) - min(correlation),
      .groups = "drop"
    )
  best <- slopes[which.max(slopes$rho_range), ]

  best_data <- regime_results |>
    dplyr::filter(n_sites == best$n_sites, n_steps == best$n_steps)

  # Theoretical overlay
  rho_max <- max(best_data$correlation, na.rm = TRUE)
  th <- seq(0, 1, length.out = 100)
  theory <- data.frame(p_explore = th, correlation = rho_max * (1 - th))

  ggplot() +
    geom_line(data = theory, aes(p_explore, correlation),
              linewidth = 1, color = "black") +
    geom_point(data = best_data, aes(p_explore, correlation),
               color = "#BF360C", size = 3.5) +
    geom_line(data = best_data, aes(p_explore, correlation),
              color = "#BF360C", linewidth = 0.8, linetype = "dashed") +
    labs(
      title = "Ant Colony Correlation Decay at Best Regime",
      subtitle = sprintf("n_sites = %d, n_steps = %d",
                          best$n_sites, best$n_steps),
      x = expression(p[explore]),
      y = expression("Pairwise Correlation " ~ rho)
    ) +
    theme_manuscript()
}

# ── Save all round 2 figures ────────────────────────────────────────────────

save_round2_figures <- function(complexity_results, time_results,
                                 emergence_results, regime_results,
                                 output_dir = "manuscript_figures_r2") {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  fig6 <- plot_complexity_correlation(complexity_results)
  ggsave(file.path(output_dir, "figure6_task_complexity.pdf"),
         fig6, width = 14, height = 6)

  fig7 <- plot_time_horizon_correlation(time_results)
  ggsave(file.path(output_dir, "figure7_time_horizon.pdf"),
         fig7, width = 10, height = 6)

  fig8 <- plot_emergence(emergence_results)
  ggsave(file.path(output_dir, "figure8_emergence_function.pdf"),
         fig8, width = 14, height = 6)

  fig9 <- plot_regime_heatmap(regime_results)
  ggsave(file.path(output_dir, "figure9_regime_heatmap.pdf"),
         fig9, width = 8, height = 6)

  fig10 <- plot_best_regime_decay(regime_results)
  ggsave(file.path(output_dir, "figure10_best_regime_decay.pdf"),
         fig10, width = 10, height = 6)

  cat("Round 2 figures saved to:", output_dir, "\n")

  invisible(list(fig6 = fig6, fig7 = fig7, fig8 = fig8,
                 fig9 = fig9, fig10 = fig10))
}
