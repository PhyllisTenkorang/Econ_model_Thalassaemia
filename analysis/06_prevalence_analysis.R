# Carrier-prevalence scenario and threshold analysis
#
# p is individual carrier prevalence. Couple probabilities assume random mating:
# both carriers = p^2, one carrier = 2p(1-p), neither carrier = (1-p)^2.

library(here)

source(here("R", "economic_functions.R"))

costs <- c(
  cbc_person = 390,
  cbc_couple = 780,
  dna = 6000,
  pnd = 5500,
  abortion = 3000
)

probabilities <- c(
  early_presentation = 0.80,
  pnd = 0.58,
  abortion = 0.67,
  reconsideration = 0.50,
  severe_birth = 0.25
)

management_cost_thresholds <- calculate_management_cost_thresholds()

evaluate_prevalence <- function(p) {
  stopifnot(is.numeric(p), length(p) == 1L, p > 0, p < 1)

  both_carriers <- p^2
  one_carrier <- 2 * p * (1 - p)
  neither_carrier <- (1 - p)^2

  # Strategy 1: post-conception screening.
  s1_cost <- costs[["cbc_person"]] + p * costs[["cbc_person"]] +
    probabilities[["early_presentation"]] * both_carriers * (
      costs[["dna"]] + probabilities[["pnd"]] * (
        costs[["pnd"]] +
          probabilities[["severe_birth"]] * probabilities[["abortion"]] *
          costs[["abortion"]]
      )
    ) +
    (1 - probabilities[["early_presentation"]]) * both_carriers * costs[["dna"]]
  s1_effect <- both_carriers * probabilities[["severe_birth"]] *
    probabilities[["early_presentation"]] * probabilities[["pnd"]] *
    probabilities[["abortion"]]

  # Strategy 2: pre-conception CBC/Hb typing with targeted DNA analysis.
  s2_cost <- costs[["cbc_couple"]] + both_carriers * costs[["dna"]]
  s2_effect <- both_carriers * probabilities[["severe_birth"]] *
    probabilities[["reconsideration"]]

  # Strategy 3: pre-conception universal DNA analysis.
  s3_cost <- costs[["dna"]]
  s3_effect <- s2_effect

  # Strategy 4: combined pre- and post-conception screening.
  follow_up_cost <- costs[["dna"]] +
    (1 - probabilities[["reconsideration"]]) * probabilities[["pnd"]] * (
      costs[["pnd"]] +
        probabilities[["severe_birth"]] * probabilities[["abortion"]] *
        costs[["abortion"]]
    )
  s4_cost <- costs[["cbc_couple"]] + both_carriers * follow_up_cost
  s4_effect <- both_carriers * probabilities[["severe_birth"]] * (
    probabilities[["reconsideration"]] +
      (1 - probabilities[["reconsideration"]]) * probabilities[["pnd"]] *
      probabilities[["abortion"]]
  )

  strategy <- c(
    "Strategy 1: Post-conception screening",
    "Strategy 2: Pre-conception screening, targeted DNA",
    "Strategy 3: Pre-conception screening, universal DNA",
    "Strategy 4: Combined screening"
  )
  incremental_cost <- c(s1_cost, s2_cost, s3_cost, s4_cost)
  births_averted <- c(s1_effect, s2_effect, s3_effect, s4_effect)

  data.frame(
    individual_carrier_prevalence = p,
    both_carriers_probability = both_carriers,
    one_carrier_probability = one_carrier,
    neither_carrier_probability = neither_carrier,
    strategy = strategy,
    incremental_cost_thb = incremental_cost,
    births_averted_proportion = births_averted,
    icer_thb_per_birth_averted = incremental_cost / births_averted,
    net_cost_saving_thb =
      management_cost_thresholds$average * births_averted - incremental_cost,
    cost_saving_at_average_management_cost =
      management_cost_thresholds$average * births_averted >= incremental_cost,
    stringsAsFactors = FALSE
  )
}

prevalence_grid <- getOption(
  "thalassaemia.prevalence_grid",
  sort(unique(c(seq(0.01, 0.30, by = 0.01), 0.16)))
)
prevalence_results <- do.call(rbind, lapply(prevalence_grid, evaluate_prevalence))
rownames(prevalence_results) <- NULL

find_threshold <- function(strategy_name, interval = c(0.001, 0.50)) {
  objective <- function(p) {
    result <- evaluate_prevalence(p)
    result$icer_thb_per_birth_averted[result$strategy == strategy_name] -
      management_cost_thresholds$average
  }

  endpoint_values <- vapply(interval, objective, numeric(1L))
  if (any(!is.finite(endpoint_values)) || prod(sign(endpoint_values)) > 0) {
    return(NA_real_)
  }

  stats::uniroot(objective, interval = interval)$root
}

strategies <- unique(prevalence_results$strategy)
thresholds <- vapply(strategies, find_threshold, numeric(1L))
prevalence_thresholds <- data.frame(
  strategy = strategies,
  individual_carrier_prevalence_threshold = unname(thresholds),
  both_carriers_probability_at_threshold = unname(thresholds)^2,
  average_lifetime_management_cost_thb =
    management_cost_thresholds$average,
  stringsAsFactors = FALSE
)

# Regression check: p = 0.16 must reproduce the main deterministic model.
baseline_at_16_percent <- evaluate_prevalence(0.16)
stopifnot(
  isTRUE(all.equal(
    baseline_at_16_percent$incremental_cost_thb,
    c(677.300096, 933.6, 6000, 978.16256),
    tolerance = 1e-10
  )),
  isTRUE(all.equal(
    baseline_at_16_percent$births_averted_proportion,
    c(0.001989632, 0.0032, 0.0032, 0.00444352),
    tolerance = 1e-10
  ))
)

output_dir <- here("outputs", "tables")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(
  prevalence_results,
  file.path(output_dir, "prevalence_analysis_results.csv"),
  row.names = FALSE
)
write.csv(
  prevalence_thresholds,
  file.path(output_dir, "prevalence_thresholds.csv"),
  row.names = FALSE
)

print(prevalence_thresholds, row.names = FALSE)
