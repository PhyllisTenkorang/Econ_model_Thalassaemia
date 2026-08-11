# Deterministic base-case analysis

library(here)
library(rdecision)

source(here("R", "model_parameters.R"))
source(here("R", "model_builders.R"))

models <- build_all_strategy_trees(
  mode = "base",
  data_dir = here("data", "model_inputs")
)
base_case_results <- do.call(rbind, lapply(models, evaluate_incremental_results))
rownames(base_case_results) <- NULL

output_dir <- here("outputs", "tables")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(
  base_case_results,
  file.path(output_dir, "base_case_results.csv"),
  row.names = FALSE
)

print(base_case_results, row.names = FALSE)
