# Extract DSA parameter bounds and PSA 95% uncertainty intervals
#
# DSA bounds are the 2.5th and 97.5th percentiles used by rdecision's
# tornado analysis. PSA intervals are empirical 2.5th and 97.5th percentiles
# across the simulations, with the median included as the central estimate.

# PSA.R constructs all four decision trees and runs the PSA simulations.
# Run it from the project root so its relative paths resolve correctly.
library(here)

# PSA.R prints and saves a diagnostic plot as a side effect. Redirect those
# plots to temporary files so extracting tables does not modify project plots.
project_here <- here::here
here <- function(...) {
  parts <- list(...)
  if (length(parts) > 1L && identical(parts[[1L]], "outputs") &&
      identical(parts[[2L]], "figures")) {
    return(file.path(tempdir(), parts[[length(parts)]]))
  }
  do.call(project_here, parts)
}
grDevices::pdf(file.path(tempdir(), "PSA_extraction_plots.pdf"))
source(project_here("analysis", "03_probabilistic_sensitivity_analysis.R"))
grDevices::dev.off()

baseline_results <- function(es, intervention, comparator) {
  delta_cost <- es[es[, 2L] == intervention, "Cost"] -
    es[es[, 2L] == comparator, "Cost"]
  delta_effect <- es[es[, 2L] == intervention, "Utility"] -
    es[es[, 2L] == comparator, "Utility"]

  c(
    delta_cost = delta_cost,
    delta_utility = delta_effect,
    ICER = delta_cost / delta_effect
  )
}

strategy_specs <- list(
  list(
    strategy = "Strategy 1: Post-conception screening",
    tree = dt,
    index = e44,
    ref = e45,
    psa = psa,
    baseline = baseline_results(es, "Test woman", "No further testing")
  ),
  list(
    strategy = "Strategy 2: Pre-conception screening, targeted DNA",
    tree = dt2,
    index = e64,
    ref = e65,
    psa = psa2,
    baseline = baseline_results(
      es2, "Screen couple with \nCBC + Hb typing", "No screening"
    )
  ),
  list(
    strategy = "Strategy 3: Pre-conception screening, universal DNA",
    tree = dt3,
    index = e84,
    ref = e85,
    psa = psa3,
    baseline = baseline_results(
      es3, "Screen couple with \nDNA analysis", "No screening"
    )
  ),
  list(
    strategy = "Strategy 4: Combined pre- and post-conception screening",
    tree = dt4,
    index = e111,
    ref = e112,
    psa = psa4,
    baseline = baseline_results(
      es4, "Screen couple with \nCBC + Hb typing", "No screening"
    )
  )
)

# Exact bounds passed by rdecision to each one-way sensitivity analysis.
dsa_bounds <- do.call(
  rbind,
  lapply(strategy_specs, function(x) {
    tab <- x$tree$tornado(
      index = x$index,
      ref = x$ref,
      outcome = "ICER",
      draw = FALSE
    )
    modvars <- x$tree$modvar_table()

    data.frame(
      strategy = x$strategy,
      parameter = tab$Description,
      base_value = modvars$Mean[match(tab$Description, modvars$Description)],
      lower_2.5_percentile = tab$LL,
      upper_97.5_percentile = tab$UL,
      stringsAsFactors = FALSE
    )
  })
)

# In DSA.R these prevalence quantities are fixed numeric inputs, not ModVar
# objects. They are probabilistic in PSA.R but are not independently varied in
# the deterministic tornado analyses, so exclude them from the DSA table.
dsa_fixed_parameters <- c(
  "Probability of one partner having trait",
  "Probability of both partners having trait"
)
dsa_bounds <- dsa_bounds[!dsa_bounds$parameter %in% dsa_fixed_parameters, ]

# A parameter can appear in more than one strategy. Keep strategy-specific rows
# because the table is intended to document the inputs to each analysis.
dsa_bounds <- dsa_bounds[order(dsa_bounds$strategy, dsa_bounds$parameter), ]
rownames(dsa_bounds) <- NULL

summarise_psa <- function(spec) {
  measures <- c(
    delta_cost = "Incremental cost (THB)",
    delta_utility = "Severe thalassaemia births averted",
    ICER = "ICER (THB per severe thalassaemia birth averted)"
  )

  do.call(
    rbind,
    lapply(names(measures), function(variable) {
      values <- spec$psa[[variable]]
      values <- values[is.finite(values)]

      qs <- stats::quantile(
        values,
        probs = c(0.025, 0.5, 0.975),
        names = FALSE,
        na.rm = TRUE,
        type = 7
      )

      data.frame(
        strategy = spec$strategy,
        outcome = unname(measures[[variable]]),
        simulations_used = length(values),
        baseline = unname(spec$baseline[[variable]]),
        median = qs[2L],
        lower_95_UI = qs[1L],
        upper_95_UI = qs[3L],
        stringsAsFactors = FALSE
      )
    })
  )
}

psa_intervals <- do.call(rbind, lapply(strategy_specs, summarise_psa))
rownames(psa_intervals) <- NULL

results_dir <- project_here("outputs", "tables")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

utils::write.csv(
  dsa_bounds,
  file = file.path(results_dir, "DSA_parameter_bounds.csv"),
  row.names = FALSE
)

utils::write.csv(
  psa_intervals,
  file = file.path(results_dir, "PSA_95_uncertainty_intervals.csv"),
  row.names = FALSE
)

cat("\nDSA parameter bounds (2.5th and 97.5th percentiles):\n")
print(dsa_bounds, row.names = FALSE)

cat("\nPSA empirical 95% uncertainty intervals:\n")
print(psa_intervals, row.names = FALSE)
