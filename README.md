# IsomorphismSim

**Simulation Framework for the Ant Colony / Random Forest Isomorphism**

> Fokoué, E., Babbitt, G., & Levental, Y. (2026). *Decorrelation, Diversity, and Emergent Intelligence: The Isomorphism Between Social Insect Colonies and Ensemble Machine Learning.* Rochester Institute of Technology.

## Overview

This R package provides agent-based ant colony simulations, random forest variance decomposition experiments, and direct isomorphism tests demonstrating that social insect colonies and ensemble machine learning implement identical variance reduction strategies through decorrelation of identical units.

Both systems obey the same variance decomposition:

**Var[ensemble] = ρσ² + (1−ρ)σ²/M**

| Component | Random Forest | Ant Colony |
|-----------|--------------|------------|
| Unit | Decision tree | Individual ant |
| Decorrelation | Random feature selection (1 − m_try/p) | Stochastic exploration (p_explore) |
| Aggregation | Prediction averaging | Recruitment + quorum sensing |
| Correlation | Between trees' prediction vectors | Between ants' preference vectors |

## Installation

```r
# install.packages("devtools")
devtools::install_github("ylevental/IsomorphismSim")
```

## Quick Start

```r
library(IsomorphismSim)

# Simulate an ant colony
sim <- simulate_ant_colony(n_ants = 50, p_explore = 0.3, n_sites = 20,
                           site_qualities = seq(10, 2, length.out = 20))

# Measure within-colony correlation (the key metric)
rho <- within_colony_correlation(sim$ant_preferences)
cat("Within-colony correlation:", rho, "\n")

# Launch the interactive Shiny app
launch_app()
```

## Key Functions

### Core

- `generate_data()` — Synthetic regression data with controlled signal/noise
- `simulate_ant_colony()` — Agent-based colony simulation with pheromone dynamics
- `within_colony_correlation()` — Correct within-colony correlation measurement (analogous to tree-tree correlation in a forest)

### Experiments

- `variance_decomposition_experiment()` — Validates Var = ρσ² + (1−ρ)σ²/M for random forests
- `colony_variance_experiment()` — Sweeps colony size × p_explore
- `isomorphism_test()` — Direct comparison under matched θ values
- `optimal_decorrelation_experiment()` — Finds optimal θ for each ensemble size
- `sensitivity_analysis()` — Tests robustness across signal/noise conditions

### Visualization

- `create_isomorphism_schematic()` — Figure 1: correlation decay in both systems
- `plot_variance_decomposition()` — Figure 2: theoretical vs empirical variance
- `plot_correlation_decay()` — Figure 3: direct isomorphism validation
- `plot_optimal_decorrelation()` — Figure 4: performance vs θ
- `plot_sensitivity_heatmap()` — Figure 5: robustness heatmap

### Interactive

- `launch_app()` — Shiny app with colony simulation, isomorphism sweep, and variance decomposition demo

## Reproducing the Paper

The `simulation_study/` folder contains the full experiment pipeline:

```bash
cd simulation_study

# Round 1: RF variance decomposition + original experiments
Rscript master_script.R --quick    # ~15 min demo
Rscript master_script.R            # full run (~2-6 hours)

# Round 2: Task complexity, time horizons, emergence function, regime search
Rscript master_script_round2.R --quick
Rscript master_script_round2.R
```

Pre-computed round 2 results (`.rds` files) are included in `simulation_study/results_round2/`. All 10 manuscript figures are in `manuscript_figures/`.

## Methodological Note

The correct way to measure ant colony correlation is **within-colony** — correlating ants' preference vectors over sites within a single colony run, analogous to correlating trees' prediction vectors over test points within a single forest. The across-replicate measurement (correlating the same ant index across independent runs) is zero by construction and does not capture the isomorphism.

The isomorphism signal is strongest under task complexity (20–50 sites with small quality gaps) and time pressure (20–50 steps), where ants must rely on pheromone-based social information rather than converging independently.

## Vignette

```r
# After installation:
browseVignettes("IsomorphismSim")

# Or render from source:
rmarkdown::render("vignettes/isomorphism-tutorial.Rmd")
```

## Citation

```bibtex
@article{fokoue2026decorrelation,
  title={Decorrelation, Diversity, and Emergent Intelligence: The Isomorphism Between Social Insect Colonies and Ensemble Machine Learning},
  author={Fokou{\'e}, Ernest and Babbitt, Gregory and Levental, Yuval},
  year={2026},
  institution={Rochester Institute of Technology}
}
```

## License

MIT
