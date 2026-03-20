#' Ant Colony Simulation Functions
#'
#' Experiment 2: Agent-based ant colony simulation demonstrating
#' that the same variance decomposition holds for biological swarms.
#'
#' Authors: Ernest Fokoué, Gregory Babbitt, Yuval Levental

#' Agent-based ant colony simulation
#'
#' @param n_ants Number of ants in colony
#' @param n_sites Number of potential nest sites
#' @param site_qualities True quality values for each site
#' @param p_explore Probability an ant explores independently vs follows pheromones
#' @param n_steps Number of simulation time steps
#' @param noise_sd Standard deviation of observation noise
#' @param quorum_threshold Recruitment threshold for decision
#' @param alpha Learning rate
#' @param beta Pheromone evaporation rate
#' @param gamma Recruitment strength
#' @return List with history, final_preferences, decision, ant_states, ant_preferences
simulate_ant_colony <- function(
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
  history         <- list()

  for (t in seq_len(n_steps)) {
    for (i in seq_len(n_ants)) {
      if (runif(1) < p_explore) {
        site <- sample(n_sites, 1)
      } else {
        if (sum(pheromone) > 0) {
          probs <- pheromone / sum(pheromone)
          site <- sample(n_sites, 1, prob = probs)
        } else {
          site <- sample(n_sites, 1)
        }
      }

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

    if (max(recruitment) > quorum_threshold) {
      decision <- which.max(recruitment)
      break
    }

    history[[t]] <- list(
      time            = t,
      pheromone       = pheromone,
      recruitment     = recruitment,
      ant_preferences = ant_preferences,
      ant_states      = ant_states
    )

    recruitment <- recruitment * 0.8
  }

  if (is.na(decision)) decision <- which.max(recruitment)

  return(list(
    history           = history,
    final_preferences = colMeans(ant_preferences),
    final_recruitment = recruitment,
    decision          = decision,
    ant_states        = ant_states,
    ant_preferences   = ant_preferences
  ))
}

#' Run colony variance decomposition experiment
#'
#' @param n_ants_values Vector of colony sizes
#' @param p_explore_values Vector of exploration probabilities
#' @param n_replicates Number of Monte Carlo replicates
#' @param n_sites Number of candidate nest sites
#' @param site_qualities True quality vector
#' @return data.frame with accuracy, variance, and correlation estimates
colony_variance_experiment <- function(
    n_ants_values    = c(10, 20, 30, 50, 100),
    p_explore_values = seq(0, 1, by = 0.2),
    n_replicates     = 50,
    n_sites          = 5,
    site_qualities   = c(10, 8, 6, 4, 2)
) {
  results <- expand.grid(
    n_ants    = n_ants_values,
    p_explore = p_explore_values,
    stringsAsFactors = FALSE
  )
  results$accuracy_mean  <- NA_real_
  results$accuracy_sd    <- NA_real_
  results$var_mean       <- NA_real_
  results$cor_mean       <- NA_real_

  best_site <- which.max(site_qualities)

  for (i in seq_len(nrow(results))) {
    decisions <- integer(n_replicates)
    pref_matrix <- matrix(NA, nrow = results$n_ants[i], ncol = n_replicates)

    for (r in seq_len(n_replicates)) {
      sim <- simulate_ant_colony(
        n_ants         = results$n_ants[i],
        p_explore      = results$p_explore[i],
        n_sites        = n_sites,
        site_qualities = site_qualities
      )
      decisions[r] <- as.integer(sim$decision == best_site)
      pref_matrix[, r] <- sim$ant_preferences[, best_site]
    }

    results$accuracy_mean[i] <- mean(decisions)
    results$accuracy_sd[i]   <- sd(decisions)

    within_rep_vars <- apply(pref_matrix, 2, var, na.rm = TRUE)
    results$var_mean[i] <- mean(within_rep_vars, na.rm = TRUE)

    if (results$n_ants[i] >= 2 && n_replicates > 1) {
      cor_mat <- cor(pref_matrix, use = "pairwise.complete.obs")
      results$cor_mean[i] <- mean(cor_mat[lower.tri(cor_mat)], na.rm = TRUE)
    }

    if (i %% 10 == 0) cat("  Colony experiment:", i, "/", nrow(results), "\n")
  }

  return(results)
}
