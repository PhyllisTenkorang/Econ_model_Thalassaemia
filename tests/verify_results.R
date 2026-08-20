library(here)

source(here("R", "economic_functions.R"))
library(rdecision)
source(here("R", "model_parameters.R"))

psa_parameters <- create_model_parameters("psa")
stopifnot(identical(
  psa_parameters$probabilities$woman_trait,
  psa_parameters$probabilities$partner_trait
))

management_cost_thresholds <- calculate_management_cost_thresholds()
legacy_thresholds <- calculate_wtp_thresholds()
stopifnot(
  identical(
    names(management_cost_thresholds),
    c("lower_level", "average", "higher_level")
  ),
  isTRUE(all.equal(
    unname(unlist(management_cost_thresholds)),
    unname(unlist(legacy_thresholds)),
    tolerance = 1e-12
  ))
)

baseline_file <- here("outputs", "tables", "base_case_results.csv")
dsa_file <- here("outputs", "tables", "DSA_parameter_bounds.csv")
psa_file <- here("outputs", "tables", "PSA_95_uncertainty_intervals.csv")
bcr_file <- here("outputs", "tables", "BCR_PSA_results.csv")
prevalence_file <- here("outputs", "tables", "prevalence_analysis_results.csv")
threshold_file <- here("outputs", "tables", "prevalence_thresholds.csv")
dsa_threshold_file <- here("outputs", "tables", "DSA_thresholds.csv")

stopifnot(
  file.exists(baseline_file), file.exists(dsa_file), file.exists(psa_file),
  file.exists(bcr_file),
  file.exists(prevalence_file), file.exists(threshold_file),
  file.exists(dsa_threshold_file)
)

baseline <- read.csv(baseline_file, check.names = FALSE)
expected_cost <- c(677.300096, 933.6, 6000, 978.16256)
expected_effect <- c(0.001989632, 0.0032, 0.0032, 0.00444352)
expected_icer <- expected_cost / expected_effect

stopifnot(
  nrow(baseline) == 4L,
  isTRUE(all.equal(baseline$incremental_cost_thb, expected_cost, tolerance = 1e-10)),
  isTRUE(all.equal(
    baseline$births_averted_proportion,
    expected_effect,
    tolerance = 1e-10
  )),
  isTRUE(all.equal(
    baseline$icer_thb_per_birth_averted,
    expected_icer,
    tolerance = 1e-10
  ))
)

dsa <- read.csv(dsa_file, check.names = FALSE)
stopifnot(
  all(c(
    "strategy", "parameter", "base_value",
    "lower_2.5_percentile", "upper_97.5_percentile"
  ) %in% names(dsa)),
  all(dsa$lower_2.5_percentile <= dsa$base_value),
  all(dsa$base_value <= dsa$upper_97.5_percentile),
  !any(dsa$parameter %in% c(
    "Probability of man having trait",
    "Probability of woman having trait"
  ))
)

dsa_thresholds <- read.csv(dsa_threshold_file, check.names = FALSE)
stopifnot(
  nrow(dsa_thresholds) == 12L,
  sum(is.na(dsa_thresholds$threshold)) == 2L,
  all(is.finite(dsa_thresholds$average_lifetime_management_cost_thb))
)

psa <- read.csv(psa_file, check.names = FALSE)
stopifnot(
  nrow(psa) == 12L,
  length(unique(psa$strategy)) == 4L,
  all(c("baseline", "median", "lower_95_UI", "upper_95_UI") %in% names(psa)),
  all(psa$simulations_used == 10000L)
)
strategy_1_effect <- psa[
  psa$strategy == "Strategy 1: Post-conception screening" &
    psa$outcome == "Severe thalassaemia births averted",
]
stopifnot(
  nrow(strategy_1_effect) == 1L,
  strategy_1_effect$lower_95_UI >= -1e-12
)

bcr <- read.csv(bcr_file, check.names = FALSE)
required_bcr_columns <- c(
  "strategy", "management_cost_scenario", "lifetime_management_cost_thb",
  "simulations", "base_case_bcr", "median_bcr", "lower_bcr_95_UI",
  "upper_bcr_95_UI", "probability_bcr_gt_1",
  "base_case_net_cost_saving_thb", "median_net_cost_saving_thb",
  "lower_net_cost_saving_95_UI", "upper_net_cost_saving_95_UI"
)
stopifnot(
  nrow(bcr) == 12L,
  length(unique(bcr$strategy)) == 4L,
  length(unique(bcr$management_cost_scenario)) == 3L,
  all(required_bcr_columns %in% names(bcr)),
  all(bcr$simulations == 10000L),
  all(is.finite(bcr$base_case_bcr)),
  all(bcr$base_case_bcr > 0),
  all(bcr$lower_bcr_95_UI <= bcr$median_bcr),
  all(bcr$median_bcr <= bcr$upper_bcr_95_UI),
  all(bcr$probability_bcr_gt_1 >= 0),
  all(bcr$probability_bcr_gt_1 <= 1)
)
bcr_baseline <- baseline[match(bcr$strategy, baseline$strategy), ]
stopifnot(isTRUE(all.equal(
  bcr$base_case_bcr,
  bcr$lifetime_management_cost_thb /
    bcr_baseline$icer_thb_per_birth_averted,
  tolerance = 1e-10
)))

prevalence <- read.csv(prevalence_file, check.names = FALSE)
at_baseline <- prevalence[prevalence$individual_carrier_prevalence == 0.16, ]
stopifnot(
  nrow(at_baseline) == 4L,
  all(c(
    "net_cost_saving_thb",
    "cost_saving_at_average_management_cost"
  ) %in% names(at_baseline)),
  isTRUE(all.equal(at_baseline$incremental_cost_thb, expected_cost, tolerance = 1e-10)),
  isTRUE(all.equal(
    at_baseline$births_averted_proportion,
    expected_effect,
    tolerance = 1e-10
  )),
  all(abs(
    at_baseline$both_carriers_probability +
      at_baseline$one_carrier_probability +
      at_baseline$neither_carrier_probability - 1
  ) < 1e-12)
)

thresholds <- read.csv(threshold_file, check.names = FALSE)
stopifnot(
  nrow(thresholds) == 4L,
  all(is.finite(thresholds$average_lifetime_management_cost_thb))
)

message("All baseline, DSA, PSA, BCR, and prevalence result checks passed.")
