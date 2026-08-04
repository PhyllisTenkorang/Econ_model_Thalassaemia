# Economic model of thalassaemia screening in Thailand

This project evaluates the cost-effectiveness and budget impact of four
thalassaemia screening strategies in Thailand against "no screening":

1. Post-conception screening.
2. Pre-conception CBC and Hb typing with targeted DNA analysis.
3. Pre-conception universal DNA analysis.
4. Combined pre- and post-conception screening.

The primary outcome is the incremental screening cost per severe thalassaemia
birth averted (in Thai Baht).

## Quick start

Open `Econ_Model_Thalassaemia.Rproj`, install the packages listed in the
[workflow vignette](vignettes/02-running-analyses.Rmd), and run:

```sh
Rscript analysis/01_baseline_analysis.R
Rscript analysis/02_deterministic_sensitivity_analysis.R
Rscript analysis/03_probabilistic_sensitivity_analysis.R
Rscript analysis/04_extract_uncertainty_results.R
Rscript analysis/05_budget_impact_analysis.R
Rscript analysis/06_prevalence_analysis.R
Rscript tests/verify_results.R
```

Generated tables and figures are written to `outputs/tables/` and
`outputs/figures/`.

## Project structure

```text
analysis/          Supported analysis scripts
R/                 Shared functions
data/model_inputs/ Model inputs and source material
outputs/           Generated tables and figures
tests/             Reproducibility checks
vignettes/         Detailed model and analysis documentation
archive/           Earlier project scripts retained for reference
```

## Documentation

- [Model overview](vignettes/01-model-overview.Rmd)
- [Running the analyses](vignettes/02-running-analyses.Rmd)
- [Sensitivity analysis](vignettes/03-sensitivity-analysis.Rmd)
- [Carrier-prevalence analysis](vignettes/04-prevalence-analysis.Rmd)

The main numerical outputs are validated by `tests/verify_results.R`.
