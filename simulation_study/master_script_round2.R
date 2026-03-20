#!/usr/bin/env Rscript
#
# master_script_round2.R — Finding the Isomorphism Regime
#
# Round 2 experiments: task complexity, time horizons, E(t) emergence.
#
# Authors: Ernest Fokoué, Gregory Babbitt, Yuval Levental
#
# Usage:
#   Rscript master_script_round2.R            # full run
#   Rscript master_script_round2.R --quick    # fast demo

args <- commandArgs(trailingOnly = TRUE)
quick_mode <- "--quick" %in% args

required_packages <- c("ranger", "tidyverse", "ggpubr", "gridExtra", "viridis")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)
}

set.seed(2026)

# Source functions
source("simulation_functions/data_generation.R")
source("simulation_functions/ant_colony_sims_v2.R")
source("simulation_functions/round2_experiments.R")
source("simulation_functions/round2_visualization.R")

timestamp_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
results_dir   <- paste0("results_round2_", timestamp_str)
dir.create(results_dir)

cat("\n", strrep("=", 60), "\n")
cat(" ROUND 2: FINDING THE ISOMORPHISM REGIME\n")
cat(" Mode:", ifelse(quick_mode, "QUICK DEMO", "FULL"), "\n")
cat(strrep("=", 60), "\n\n")

# ═════════════════════════════════════════════════════════════════════════════
# Experiment 6: Task Complexity Sweep
# ═════════════════════════════════════════════════════════════════════════════

cat("--- Experiment 6: Task Complexity Sweep ---\n")
complexity_results <- task_complexity_sweep(
  n_sites_values   = if (quick_mode) c(5, 20, 50) else c(5, 10, 20, 30, 50),
  quality_gaps     = if (quick_mode) c(2, 8) else c(2, 4, 8),
  p_explore_values = seq(0.1, 0.9, by = ifelse(quick_mode, 0.4, 0.2)),
  n_ants           = 50,
  n_steps          = 100,
  noise_sd         = 2,
  n_replicates     = ifelse(quick_mode, 10, 30)
)
saveRDS(complexity_results, file.path(results_dir, "exp6_task_complexity.rds"))
cat("  Done.\n\n")

# ═════════════════════════════════════════════════════════════════════════════
# Experiment 7: Time Horizon Sweep
# ═════════════════════════════════════════════════════════════════════════════

cat("--- Experiment 7: Time Horizon Sweep ---\n")
time_results <- time_horizon_sweep(
  n_steps_values   = if (quick_mode) c(10, 30, 100) else c(10, 20, 30, 50, 100),
  p_explore_values = seq(0.1, 0.9, by = ifelse(quick_mode, 0.4, 0.2)),
  n_sites          = 20,
  quality_gap      = 2,
  n_ants           = 50,
  noise_sd         = 2,
  n_replicates     = ifelse(quick_mode, 10, 30)
)
saveRDS(time_results, file.path(results_dir, "exp7_time_horizon.rds"))
cat("  Done.\n\n")

# ═════════════════════════════════════════════════════════════════════════════
# Experiment 8: Emergence Function E(t)
# ═════════════════════════════════════════════════════════════════════════════

cat("--- Experiment 8: Emergence Function E(t) ---\n")
emergence_results <- emergence_experiment(
  p_explore_values = if (quick_mode) c(0.1, 0.5, 0.9) else c(0.1, 0.3, 0.5, 0.7, 0.9),
  n_sites          = 20,
  quality_gap      = 2,
  n_ants           = 50,
  n_steps          = ifelse(quick_mode, 80, 150),
  noise_sd         = 2,
  n_replicates     = ifelse(quick_mode, 10, 30)
)
saveRDS(emergence_results, file.path(results_dir, "exp8_emergence.rds"))
cat("  Done.\n\n")

# ═════════════════════════════════════════════════════════════════════════════
# Experiment 9: Combined Regime Search
# ═════════════════════════════════════════════════════════════════════════════

cat("--- Experiment 9: Regime Search ---\n")
regime_results <- regime_search(
  n_sites_values   = if (quick_mode) c(10, 30) else c(10, 20, 50),
  n_steps_values   = if (quick_mode) c(20, 100) else c(20, 50, 100),
  p_explore_values = seq(0.1, 0.9, by = ifelse(quick_mode, 0.4, 0.2)),
  quality_gap      = 2,
  n_ants           = 50,
  noise_sd         = 2,
  n_replicates     = ifelse(quick_mode, 10, 30)
)
saveRDS(regime_results, file.path(results_dir, "exp9_regime_search.rds"))
cat("  Done.\n\n")

# ═════════════════════════════════════════════════════════════════════════════
# Generate Figures
# ═════════════════════════════════════════════════════════════════════════════

cat("--- Generating Round 2 Figures ---\n")
figs_dir <- file.path(results_dir, "manuscript_figures_r2")
figs <- save_round2_figures(
  complexity_results, time_results, emergence_results, regime_results,
  output_dir = figs_dir
)

# Combined PDF
pdf(file.path(results_dir, "round2_plots.pdf"), width = 12, height = 8)
for (f in figs) print(f)
dev.off()
cat("  Done.\n\n")

# ═════════════════════════════════════════════════════════════════════════════
# Summary Report
# ═════════════════════════════════════════════════════════════════════════════

cat("--- Writing Round 2 Summary ---\n")
sink(file.path(results_dir, "round2_summary.txt"))
cat(strrep("=", 60), "\n")
cat("  ROUND 2: FINDING THE ISOMORPHISM REGIME\n")
cat(strrep("=", 60), "\n\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Mode:", ifelse(quick_mode, "Quick demo", "Full"), "\n\n")

cat("EXPERIMENT 6: Task Complexity Sweep\n", strrep("-", 48), "\n")
complexity_summary <- complexity_results |>
  group_by(n_sites, quality_gap) |>
  summarise(
    rho_range  = max(correlation) - min(correlation),
    rho_at_0.1 = correlation[p_explore == min(p_explore)],
    rho_at_0.9 = correlation[p_explore == max(p_explore)],
    accuracy   = mean(accuracy),
    .groups    = "drop"
  )
print(complexity_summary)
cat("\n")

cat("EXPERIMENT 7: Time Horizon Sweep\n", strrep("-", 48), "\n")
time_summary <- time_results |>
  group_by(n_steps) |>
  summarise(
    rho_range  = max(correlation) - min(correlation),
    rho_at_0.1 = correlation[p_explore == min(p_explore)],
    rho_at_0.9 = correlation[p_explore == max(p_explore)],
    accuracy   = mean(accuracy),
    .groups    = "drop"
  )
print(time_summary)
cat("\n")

cat("EXPERIMENT 9: Regime Search\n", strrep("-", 48), "\n")
regime_summary <- regime_results |>
  group_by(n_sites, n_steps) |>
  summarise(
    rho_range = max(correlation) - min(correlation),
    rho_slope = coef(lm(correlation ~ p_explore))[2],
    .groups = "drop"
  )
print(regime_summary)

cat("\nBest regime (largest rho_range):\n")
best <- regime_summary[which.max(regime_summary$rho_range), ]
print(best)
cat("\n")

cat("INTERPRETATION\n", strrep("-", 48), "\n")
cat("If rho_range > 0.05, the isomorphism signal is detectable.\n")
cat("If rho_slope is negative, correlation decays with p_explore as predicted.\n")
cat("The best regime should show rho ~ 0 at high p_explore and rho > 0 at low p_explore.\n")
sink()

cat("\n", strrep("=", 60), "\n")
cat(" ROUND 2 COMPLETE — results in:", results_dir, "\n")
cat(strrep("=", 60), "\n\n")
