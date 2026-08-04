# Economic model of thalassaemia screening in Thailand

This R project evaluates four thalassaemia screening strategies against no
screening:

1. Post-conception screening with or without abortion.
2. Pre-conception screening using CBC and Hb typing with targeted DNA analysis.
3. Pre-conception screening using universal DNA analysis.
4. Combined pre- and post-conception screening.

The primary outcome is the incremental cost in Thai baht (THB) per severe
thalassaemia birth averted. The project includes base-case cost-effectiveness,
deterministic sensitivity analysis (DSA), probabilistic sensitivity analysis
(PSA), and budget-impact analysis. Exploratory prevalence analyses are retained
in the legacy archive pending methodological review.

## Project structure

```text
analysis/          Supported analysis entry points, numbered in run order
R/                 Shared economic and plotting functions
data/model_inputs/ Model CSV inputs and supporting source material
outputs/figures/   Generated figures
outputs/tables/    Generated result tables
tests/             Lightweight reproducibility checks
archive/           Exploratory and superseded scripts retained for reference
```

The files in `archive/` are not part of the supported workflow and may rely on
older assumptions or incomplete APIs.

## Requirements

Use R 4.4 or later with these packages installed:

```r
install.packages(c(
  "dplyr", "ggplot2", "ggpubr", "here", "purrr", "rdecision",
  "readr", "scales", "stringr", "tibble", "tidyr", "tidyverse"
))
```

Open `Econ_Model_Thalassaemia.Rproj` before running an analysis. Scripts use
`here::here()` and can therefore be run from RStudio or from the project root.

## Supported workflow

Run individual analyses from the project root:

```sh
Rscript analysis/01_baseline_analysis.R
Rscript analysis/02_deterministic_sensitivity_analysis.R
Rscript analysis/03_probabilistic_sensitivity_analysis.R
Rscript analysis/04_extract_uncertainty_results.R
Rscript analysis/05_budget_impact_analysis.R
```

The uncertainty extraction script runs the four PSA models with a reproducible
seed and writes both DSA parameter bounds and PSA 95% uncertainty intervals.
The default is 1,000 PSA simulations. It can be changed for a session with:

```r
options(
  thalassaemia.psa_iterations = 10000L,
  thalassaemia.psa_seed = 20260804L
)
source("analysis/04_extract_uncertainty_results.R")
```

## Main outputs

- `outputs/tables/base_case_results.csv`
- `outputs/tables/DSA_parameter_bounds.csv`
- `outputs/tables/PSA_95_uncertainty_intervals.csv`
- `outputs/tables/budget_impact_results.csv`
- `outputs/figures/PSA_scatter_plot.png`
- `outputs/figures/combined_tornado_plots.png`
- `outputs/figures/combined_tornado_plots.pdf`

PSA births averted are stored as proportions. The PSA table reports the
deterministic baseline, simulation median, and empirical 2.5th and 97.5th
percentiles. When the incremental-effect distribution crosses zero, as for
Strategy 1, the ICER percentile interval is unstable and should be interpreted
alongside the cost-effectiveness plane and incremental cost/effect results.

## Baseline validation

Run the verification script after regenerating results:

```sh
Rscript tests/verify_results.R
```

It checks the four published base-case costs, effects, and ICERs and validates
the expected structure of the DSA and PSA output tables.
