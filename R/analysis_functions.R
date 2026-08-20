safe_threshold <- function(model, parameter, lower, upper, lambda, tol = 0.01) {
  tryCatch(
    model$tree$threshold(
      index = model$index,
      ref = model$ref,
      outcome = "ICER",
      mvd = parameter,
      a = lower,
      b = upper,
      tol = tol,
      lambda = lambda,
      nmax = 1000L
    ),
    error = function(e) NA_real_
  )
}

extract_dsa_bounds <- function(model) {
  tornado <- model$tree$tornado(
    index = model$index,
    ref = model$ref,
    outcome = "ICER",
    draw = FALSE
  )
  variables <- model$tree$modvar_table()

  data.frame(
    strategy = model$strategy,
    parameter = tornado$Description,
    base_value = variables$Mean[match(tornado$Description, variables$Description)],
    lower_2.5_percentile = tornado$LL,
    upper_97.5_percentile = tornado$UL,
    stringsAsFactors = FALSE
  )
}

run_psa_model <- function(model, simulations) {
  evaluated <- model$tree$evaluate(setvars = "random", by = "run", N = simulations)
  incremental_cost <- evaluated[[paste0("Cost.", model$intervention)]] -
    evaluated[[paste0("Cost.", model$comparator)]]
  births_averted <- evaluated[[paste0("Utility.", model$intervention)]] -
    evaluated[[paste0("Utility.", model$comparator)]]

  data.frame(
    strategy = model$strategy,
    run = seq_len(simulations),
    incremental_cost_thb = incremental_cost,
    births_averted_proportion = births_averted,
    icer_thb_per_birth_averted = incremental_cost / births_averted,
    stringsAsFactors = FALSE
  )
}

summarise_psa_results <- function(psa_results, baseline_results) {
  outcomes <- c(
    incremental_cost_thb = "Incremental cost (THB)",
    births_averted_proportion = "Severe thalassaemia births averted",
    icer_thb_per_birth_averted =
      "ICER (THB per severe thalassaemia birth averted)"
  )

  rows <- lapply(unique(psa_results$strategy), function(strategy_name) {
    strategy_psa <- psa_results[psa_results$strategy == strategy_name, ]
    strategy_base <- baseline_results[baseline_results$strategy == strategy_name, ]

    do.call(rbind, lapply(names(outcomes), function(variable) {
      values <- strategy_psa[[variable]]
      values <- values[is.finite(values)]
      interval <- stats::quantile(values, c(0.025, 0.5, 0.975), names = FALSE)

      data.frame(
        strategy = strategy_name,
        outcome = unname(outcomes[[variable]]),
        simulations_used = length(values),
        baseline = strategy_base[[variable]],
        median = interval[[2L]],
        lower_95_UI = interval[[1L]],
        upper_95_UI = interval[[3L]],
        stringsAsFactors = FALSE
      )
    }))
  })

  do.call(rbind, rows)
}

summarise_bcr_results <- function(
  psa_results,
  baseline_results,
  management_cost_thresholds
) {
  required_columns <- c(
    "strategy",
    "incremental_cost_thb",
    "births_averted_proportion"
  )
  stopifnot(
    all(required_columns %in% names(psa_results)),
    all(required_columns %in% names(baseline_results))
  )

  management_costs <- unlist(management_cost_thresholds)
  expected_scenarios <- c("lower_level", "average", "higher_level")
  stopifnot(
    identical(names(management_costs), expected_scenarios),
    all(is.finite(management_costs)),
    all(management_costs > 0)
  )

  valid_psa <- with(
    psa_results,
    is.finite(incremental_cost_thb) &
      incremental_cost_thb > 0 &
      is.finite(births_averted_proportion) &
      births_averted_proportion > 0
  )
  if (!all(valid_psa)) {
    stop(
      sum(!valid_psa),
      " PSA draws have non-positive or non-finite incremental costs or effects; ",
      "classify dominance before calculating BCR uncertainty intervals."
    )
  }

  scenario_labels <- c(
    lower_level = "Lower-level hospital care",
    average = "Average hospital care",
    higher_level = "Higher-level hospital care"
  )

  rows <- lapply(unique(psa_results$strategy), function(strategy_name) {
    strategy_psa <- psa_results[psa_results$strategy == strategy_name, ]
    strategy_base <- baseline_results[
      baseline_results$strategy == strategy_name,
    ]
    stopifnot(nrow(strategy_base) == 1L)

    do.call(rbind, lapply(expected_scenarios, function(scenario_name) {
      management_cost <- management_costs[[scenario_name]]
      bcr <- management_cost * strategy_psa$births_averted_proportion /
        strategy_psa$incremental_cost_thb
      net_cost_saving <-
        management_cost * strategy_psa$births_averted_proportion -
        strategy_psa$incremental_cost_thb
      bcr_interval <- stats::quantile(
        bcr,
        c(0.025, 0.5, 0.975),
        names = FALSE
      )
      net_saving_interval <- stats::quantile(
        net_cost_saving,
        c(0.025, 0.5, 0.975),
        names = FALSE
      )

      data.frame(
        strategy = strategy_name,
        management_cost_scenario = unname(scenario_labels[[scenario_name]]),
        lifetime_management_cost_thb = management_cost,
        simulations = length(bcr),
        base_case_bcr = management_cost *
          strategy_base$births_averted_proportion /
          strategy_base$incremental_cost_thb,
        median_bcr = bcr_interval[[2L]],
        lower_bcr_95_UI = bcr_interval[[1L]],
        upper_bcr_95_UI = bcr_interval[[3L]],
        probability_bcr_gt_1 = mean(bcr > 1),
        base_case_net_cost_saving_thb = management_cost *
          strategy_base$births_averted_proportion -
          strategy_base$incremental_cost_thb,
        median_net_cost_saving_thb = net_saving_interval[[2L]],
        lower_net_cost_saving_95_UI = net_saving_interval[[1L]],
        upper_net_cost_saving_95_UI = net_saving_interval[[3L]],
        stringsAsFactors = FALSE
      )
    }))
  })

  do.call(rbind, rows)
}
