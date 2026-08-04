library(here)

baseline_file <- here("outputs", "tables", "base_case_results.csv")
dsa_file <- here("outputs", "tables", "DSA_parameter_bounds.csv")
psa_file <- here("outputs", "tables", "PSA_95_uncertainty_intervals.csv")

stopifnot(file.exists(baseline_file), file.exists(dsa_file), file.exists(psa_file))

baseline <- read.csv(baseline_file, check.names = FALSE)
expected_cost <- c(677.300096, 933.6, 6000, 978.16256)
expected_effect <- c(0.001989632, 0.0032, 0.0032, 0.00444352)
expected_icer <- expected_cost / expected_effect

stopifnot(
  nrow(baseline) == 4L,
  isTRUE(all.equal(baseline[["Incremental Costs"]], expected_cost, tolerance = 1e-10)),
  isTRUE(all.equal(
    baseline[["Proportion of severe Thalassaemia births averted"]],
    expected_effect,
    tolerance = 1e-10
  )),
  isTRUE(all.equal(baseline[["ICER"]], expected_icer, tolerance = 1e-10))
)

dsa <- read.csv(dsa_file, check.names = FALSE)
stopifnot(
  all(c(
    "strategy", "parameter", "base_value",
    "lower_2.5_percentile", "upper_97.5_percentile"
  ) %in% names(dsa)),
  all(dsa$lower_2.5_percentile <= dsa$base_value),
  all(dsa$base_value <= dsa$upper_97.5_percentile)
)

psa <- read.csv(psa_file, check.names = FALSE)
stopifnot(
  nrow(psa) == 12L,
  length(unique(psa$strategy)) == 4L,
  all(c("baseline", "median", "lower_95_UI", "upper_95_UI") %in% names(psa))
)

message("All baseline, DSA, and PSA result checks passed.")
