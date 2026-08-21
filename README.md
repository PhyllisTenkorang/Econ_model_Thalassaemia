# Economic model of thalassaemia screening strategies in Thailand

This repository contains decision tree models evaluating four
thalassaemia screening strategies in Thailand against **no screening**:

1. Post-conception screening
2. Pre-conception screening using CBC and Hb typing with targeted DNA analysis
3. Pre-conception screening using universal DNA analysis
4. Combined pre-conception and post-conception screening

The primary outcome is the incremental screening cost in Thai Baht (THB) per
severe thalassaemia birth averted. The analysis also reports benefit-cost
ratios and net cost savings under three hospital care scenarios. Budget impact
and carrier prevalence threshold analyses are also provided.

## Analytical perspective and assumptions

- **Perspective:** Healthcare payer; only direct medical costs are included.
- **Price year for screening costs:** 2025 THB.
- **Price year for management costs:** 2024 THB, treated as comparable with 2025 THB
  because a 2025 Thai GDP deflator was unavailable.
- **Life expectancy:** 30 years for patients with severe thalassaemia.
- **Discount rate:** 3% annually for future management costs.
- **Individual carrier prevalence:** 16% in the base case.
- **PSA carrier prevalence:** One shared `Beta(160, 840)` draw is applied to
  both partners within each simulation. Couple probabilities are derived as
  $p^2$, $2p(1-p)$, and $(1-p)^2$.

## Quick start

Open `Econ_Model_Thalassaemia.Rproj`, install the packages listed in the
[workflow vignette](vignettes/02-running-analyses.Rmd), and run the supported
workflow from the repository root:

```sh
Rscript analysis/01_baseline_analysis.R
Rscript analysis/02_deterministic_sensitivity_analysis.R
Rscript analysis/03_probabilistic_sensitivity_analysis.R
Rscript analysis/04_extract_uncertainty_results.R
Rscript analysis/05_budget_impact_analysis.R
Rscript analysis/06_prevalence_analysis.R
Rscript tests/verify_results.R
```

## Project structure

```text
analysis/          Executable analysis workflows
R/                 Shared model, economic, analysis, and plotting functions
data/model_inputs/ Decision tree inputs, parameter metadata, and source material
outputs/tables/    Generated numerical results
outputs/figures/   Generated publication figures
tests/             Reproducibility and regression checks
vignettes/         Detailed model and analysis documentation
```

Decision tree structures are read from the CSV inputs by
`R/model_builders.R`. Base case, DSA, and PSA parameters are defined centrally
in `R/model_parameters.R`, ensuring that all supported analyses use the same
model specification.

## Documentation

- [Model overview](vignettes/01-model-overview.Rmd)
- [Running the analyses](vignettes/02-running-analyses.Rmd)
- [Deterministic and probabilistic sensitivity analyses](vignettes/03-sensitivity-analysis.Rmd)
- [Carrier prevalence analysis](vignettes/04-prevalence-analysis.Rmd)

Run `Rscript tests/verify_results.R` after regenerating outputs. The checks
validate the deterministic results, DSA and PSA tables, shared carrier
prevalence implementation, BCR calculations, and prevalence analyses.
