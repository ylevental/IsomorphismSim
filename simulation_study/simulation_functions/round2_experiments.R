#' Round 2 Experiments — CORRECTED Correlation Measurement
#'
#' The key fix: correlation is now measured WITHIN a single colony
#' (between ants' preference vectors over sites), analogous to how
#' RF correlation is measured within a single forest (between trees'
#' prediction vectors over test points).
#'
#' Previously, correlation was measured ACROSS independent colony runs,
#' which is zero by construction since ants are exchangeable across runs.
#'
#' Authors: Ernest Fokoué, Gregory Babbitt, Yuval Levental

library(dplyr)

#' Compute within-colony ant correlation
#'
#' Each ant has a preference vector over n_sites (analogous to a tree's
#' predictions over test points). We correlate these vectors between ants.
#'
#' @param ant_preferences n_ants × n_sites matrix from simulate_ant_colony_v2
#' @return Mean pairwise correlation between ants
within_colony_correlation <- function(ant_preferences) {
  # Only use ants that visited at least 2 sites (have nonzero variance)
  visited <- rowSums(ant_preferences != 0) >= 2
  if (sum(visited) < 2) return(NA_real_)

  prefs <- ant_preferences[visited, , drop = FALSE]

  # Correlate ROWS (ants) across COLUMNS (sites)
  # Transpose so cor() computes ant-ant correlation
  cm <- cor(t(prefs), use = "pairwise.complete.obs")
  mean(cm[lower.tri(cm)], na.rm = TRUE)
}

# ── Experiment 6: Task Complexity Sweep ─────────────────────────────────────

task_complexity_sweep <- function(
    n_sites_values   = c(5, 10, 20, 30, 50),
    quality_gaps     = c(2, 4, 8),
    p_explore_values = seq(0.1, 0.9, by = 0.2),
    n_ants           = 50,
    n_steps          = 100,
    noise_sd         = 2,
    n_replicates     = 30
) {
  results <- data.frame()

  for (ns in n_sites_values) {
    for (qg in quality_gaps) {
      sq <- make_site_qualities(ns, qg, best_quality = 10)

      for (pe in p_explore_values) {
        cat("  n_sites =", ns, ", gap =", qg, ", p_explore =", pe, "\n")

        rhos <- numeric(n_replicates)
        decs <- integer(n_replicates)

        for (r in seq_len(n_replicates)) {
          sim <- simulate_ant_colony_v2(
            n_ants = n_ants, n_sites = ns, site_qualities = sq,
            p_explore = pe, n_steps = n_steps, noise_sd = noise_sd
          )
          decs[r] <- as.integer(sim$correct)
          rhos[r] <- within_colony_correlation(sim$ant_preferences)
        }

        results <- rbind(results, data.frame(
          n_sites      = ns,
          quality_gap  = qg,
          p_explore    = pe,
          n_steps      = n_steps,
          correlation  = mean(rhos, na.rm = TRUE),
          cor_sd       = sd(rhos, na.rm = TRUE),
          accuracy     = mean(decs),
          accuracy_sd  = sd(decs)
        ))
      }
    }
  }
  results
}

# ── Experiment 7: Time Horizon Sweep ────────────────────────────────────────

time_horizon_sweep <- function(
    n_steps_values   = c(10, 20, 30, 50, 100),
    p_explore_values = seq(0.1, 0.9, by = 0.2),
    n_sites          = 20,
    quality_gap      = 2,
    n_ants           = 50,
    noise_sd         = 2,
    n_replicates     = 30
) {
  sq <- make_site_qualities(n_sites, quality_gap, best_quality = 10)
  results <- data.frame()

  for (ns in n_steps_values) {
    for (pe in p_explore_values) {
      cat("  n_steps =", ns, ", p_explore =", pe, "\n")

      rhos <- numeric(n_replicates)
      decs <- integer(n_replicates)

      for (r in seq_len(n_replicates)) {
        sim <- simulate_ant_colony_v2(
          n_ants = n_ants, n_sites = n_sites, site_qualities = sq,
          p_explore = pe, n_steps = ns, noise_sd = noise_sd
        )
        decs[r] <- as.integer(sim$correct)
        rhos[r] <- within_colony_correlation(sim$ant_preferences)
      }

      results <- rbind(results, data.frame(
        n_steps     = ns,
        p_explore   = pe,
        n_sites     = n_sites,
        quality_gap = quality_gap,
        correlation = mean(rhos, na.rm = TRUE),
        cor_sd      = sd(rhos, na.rm = TRUE),
        accuracy    = mean(decs),
        accuracy_sd = sd(decs)
      ))
    }
  }
  results
}

# ── Experiment 8: Emergence Function E(t) ───────────────────────────────────

emergence_experiment <- function(
    p_explore_values = c(0.1, 0.3, 0.5, 0.7, 0.9),
    n_sites          = 20,
    quality_gap      = 2,
    n_ants           = 50,
    n_steps          = 150,
    noise_sd         = 2,
    n_replicates     = 30
) {
  sq <- make_site_qualities(n_sites, quality_gap, best_quality = 10)
  results <- data.frame()

  for (pe in p_explore_values) {
    cat("  E(t) for p_explore =", pe, "\n")

    all_emergence  <- matrix(NA, n_replicates, n_steps)
    all_alignment  <- matrix(NA, n_replicates, n_steps)

    for (r in seq_len(n_replicates)) {
      sim <- simulate_ant_colony_v2(
        n_ants = n_ants, n_sites = n_sites, site_qualities = sq,
        p_explore = pe, n_steps = n_steps, noise_sd = noise_sd,
        quorum_threshold = 1e6
      )
      len <- length(sim$emergence)
      all_emergence[r, seq_len(len)] <- sim$emergence
      all_alignment[r, seq_len(len)] <- sim$alignment
    }

    for (t in seq_len(n_steps)) {
      e_vals <- all_emergence[, t]
      a_vals <- all_alignment[, t]
      if (all(is.na(e_vals))) next

      results <- rbind(results, data.frame(
        time           = t,
        p_explore      = pe,
        emergence_mean = mean(e_vals, na.rm = TRUE),
        emergence_sd   = sd(e_vals, na.rm = TRUE),
        alignment_mean = mean(a_vals, na.rm = TRUE),
        alignment_sd   = sd(a_vals, na.rm = TRUE)
      ))
    }
  }
  results
}

# ── Experiment 9: Combined Regime Search ────────────────────────────────────

regime_search <- function(
    n_sites_values   = c(10, 20, 50),
    n_steps_values   = c(20, 50, 100),
    p_explore_values = seq(0.1, 0.9, by = 0.2),
    quality_gap      = 2,
    n_ants           = 50,
    noise_sd         = 2,
    n_replicates     = 30
) {
  results <- data.frame()

  for (ns in n_sites_values) {
    sq <- make_site_qualities(ns, quality_gap, best_quality = 10)
    for (nt in n_steps_values) {
      for (pe in p_explore_values) {
        cat("  sites =", ns, ", steps =", nt, ", p_explore =", pe, "\n")

        rhos <- numeric(n_replicates)
        decs <- integer(n_replicates)

        for (r in seq_len(n_replicates)) {
          sim <- simulate_ant_colony_v2(
            n_ants = n_ants, n_sites = ns, site_qualities = sq,
            p_explore = pe, n_steps = nt, noise_sd = noise_sd
          )
          decs[r] <- as.integer(sim$correct)
          rhos[r] <- within_colony_correlation(sim$ant_preferences)
        }

        results <- rbind(results, data.frame(
          n_sites     = ns,
          n_steps     = nt,
          p_explore   = pe,
          quality_gap = quality_gap,
          correlation = mean(rhos, na.rm = TRUE),
          cor_sd      = sd(rhos, na.rm = TRUE),
          accuracy    = mean(decs),
          accuracy_sd = sd(decs)
        ))
      }
    }
  }
  results
}
