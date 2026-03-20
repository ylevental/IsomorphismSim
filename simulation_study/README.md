# Simulation Study

Standalone scripts for reproducing all experiments from the paper.

## Round 1 (Experiments 1–5)

```bash
Rscript master_script.R --quick    # ~15 min
Rscript master_script.R            # full (~2-6 hours)
```

## Round 2 (Experiments 6–9)

```bash
Rscript master_script_round2.R --quick
Rscript master_script_round2.R
```

## Pre-computed Results

`results_round2/` contains `.rds` files from the full round 2 run. Load with `readRDS()`.
