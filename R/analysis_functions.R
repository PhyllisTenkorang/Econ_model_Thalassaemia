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
