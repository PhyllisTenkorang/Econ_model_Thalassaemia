# Export DSA parameter bounds and PSA 95% uncertainty intervals

library(here)
library(rdecision)

source(here("R", "model_parameters.R"))
source(here("R", "model_builders.R"))
source(here("R", "analysis_functions.R"))

# This creates `models`, `baseline_results`, and `psa_results` using the
# reproducible seed and simulation count configured by the PSA script.
source(here("analysis", "03_probabilistic_sensitivity_analysis.R"))

psa_intervals <- summarise_psa_results(psa_results, baseline_results)
rownames(psa_intervals) <- NULL

dsa_models <- build_all_strategy_trees(
  mode = "dsa",
  data_dir = here("data", "model_inputs")
)
dsa_bounds <- do.call(rbind, lapply(dsa_models, extract_dsa_bounds))
dsa_bounds <- dsa_bounds[order(dsa_bounds$strategy, dsa_bounds$parameter), ]
rownames(dsa_bounds) <- NULL

output_dir <- here("outputs", "tables")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(
  dsa_bounds,
  file.path(output_dir, "DSA_parameter_bounds.csv"),
  row.names = FALSE
)
write.csv(
  psa_intervals,
  file.path(output_dir, "PSA_95_uncertainty_intervals.csv"),
  row.names = FALSE
)

print(dsa_bounds, row.names = FALSE)
print(psa_intervals, row.names = FALSE)
