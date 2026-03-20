#' Ant Colony Simulation — Round 2
#'
#' Modified to track the Emergence Function E(t) and support
#' higher task complexity (more sites, tighter quality gaps).
#'
#' Authors: Ernest Fokoué, Gregory Babbitt, Yuval Levental

#' Generate site quality vectors with controlled difficulty
#'
#' @param n_sites Number of candidate sites
#' @param quality_gap Difference between best and worst site
#' @param best_quality Quality of the best site
#' @return Numeric vector of site qualities (descending)
make_site_qualities <- function(n_sites = 5, quality_gap = 8, best_quality = 10) {
  worst <- best_quality - quality_gap
  seq(best_quality, worst, length.out = n_sites)
}

#' Agent-based ant colony simulation with Emergence Function tracking
#'
#' @param n_ants Number of ants
#' @param n_sites Number of candidate nest sites
#' @param site_qualities Numeric vector of true site qualities
#' @param p_explore Exploration probability (decorrelation parameter)
#' @param n_steps Maximum simulation steps
#' @param noise_sd Observation noise
#' @param quorum_threshold Recruitment threshold for decision
#' @param alpha Learning rate
#' @param beta Pheromone evaporation rate
#' @param gamma Recruitment strength
#' @return List with decision, preferences, histories, and E(t) trajectory
simulate_ant_colony_v2 <- function(
    n_ants           = 50,
    n_sites          = 5,
    site_qualities   = c(10, 8, 6, 4, 2),
    p_explore        = 0.3,
    n_steps          = 100,
    noise_sd         = 2,
    quorum_threshold = 20,
    alpha            = 0.1,
    beta             = 0.05,
    gamma            = 0.2
) {
  ant_states      <- matrix(0, nrow = n_ants, ncol = n_sites)
  ant_preferences <- matrix(0, nrow = n_ants, ncol = n_sites)
  pheromone       <- rep(0, n_sites)
  recruitment     <- rep(0, n_sites)
  decision        <- NA

  # Tracking arrays
  phero_history   <- matrix(NA, n_steps, n_sites)
  recruit_history <- matrix(NA, n_steps, n_sites)
  emergence       <- numeric(n_steps)     # E(t)
  alignment       <- numeric(n_steps)     # fraction of ants at best site

  best_site <- which.max(site_qualities)

  for (t in seq_len(n_steps)) {
    ant_choices <- integer(n_ants)

    for (i in seq_len(n_ants)) {
      if (runif(1) < p_explore) {
        site <- sample(n_sites, 1)
      } else {
        if (sum(pheromone) > 0) {
          site <- sample(n_sites, 1, prob = pheromone / sum(pheromone))
        } else {
          site <- sample(n_sites, 1)
        }
      }

      ant_choices[i] <- site
      observation <- site_qualities[site] + rnorm(1, 0, noise_sd)
      ant_states[i, site] <- ant_states[i, site] + 1
      n_visits <- ant_states[i, site]
      current_mean <- ant_preferences[i, site]
      ant_preferences[i, site] <- (current_mean * (n_visits - 1) + observation) / n_visits

      if (ant_preferences[i, site] > mean(site_qualities)) {
        recruitment[site] <- recruitment[site] +
          gamma * (ant_preferences[i, site] - mean(site_qualities))
      }
    }

    pheromone <- pheromone * (1 - beta) + recruitment * alpha

    # ── Emergence Function E(t) ──
    # Fraction of ants whose current best-estimated site is the true best
    ant_best_sites <- apply(ant_preferences, 1, function(row) {
      if (all(row == 0)) return(NA)
      which.max(row)
    })
    alignment[t] <- mean(ant_best_sites == best_site, na.rm = TRUE)

    # E(t) based on recruitment concentration (normalized entropy-based)
    r_norm <- recruitment / max(sum(recruitment), 1e-10)
    r_pos  <- r_norm[r_norm > 0]
    max_entropy <- log(n_sites)
    if (length(r_pos) > 0 && max_entropy > 0) {
      current_entropy <- -sum(r_pos * log(r_pos))
      emergence[t] <- 1 - current_entropy / max_entropy
    } else {
      emergence[t] <- 0
    }

    phero_history[t, ]   <- pheromone
    recruit_history[t, ] <- recruitment

    if (max(recruitment) > quorum_threshold) {
      decision <- which.max(recruitment)
      break
    }

    recruitment <- recruitment * 0.8
  }

  steps_used <- t
  if (is.na(decision)) decision <- which.max(recruitment)

  list(
    decision          = decision,
    correct           = decision == best_site,
    steps_used        = steps_used,
    ant_preferences   = ant_preferences,
    ant_states        = ant_states,
    final_recruitment = recruitment,
    phero_history     = phero_history[seq_len(steps_used), , drop = FALSE],
    recruit_history   = recruit_history[seq_len(steps_used), , drop = FALSE],
    emergence         = emergence[seq_len(steps_used)],
    alignment         = alignment[seq_len(steps_used)]
  )
}
