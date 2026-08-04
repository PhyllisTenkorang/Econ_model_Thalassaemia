library(dplyr)
library(tidyr)
library(tibble)
library(purrr)

# PSA on Cost-Effectiveness Plane
source("functions.R")


## Define WTP threshold
exchange_rate_2005 <- 40.22  # Example exchange rate USD to THB in 2005
inflation_rate_thb <- 166.22/111.2  # 164.8/111.2  # 2005 to 2023 -> 2024 average inflation rate in Thailand (World Bank GDP deflator)
discount_rate <- 0.03  # 3% discount rate
years <- 30  # Lifetime in years

wtp_base <- calculate_lifetime_cost(cost_usd_2005 = 562.76,
                                    exchange_rate = exchange_rate_2005,
                                    inflation_rate = inflation_rate_thb,
                                    discount_rate = discount_rate,
                                    years = years)

wtp_low <- calculate_lifetime_cost(cost_usd_2005 = 224.90,
                                    exchange_rate = exchange_rate_2005,
                                    inflation_rate = inflation_rate_thb,
                                    discount_rate = discount_rate,
                                    years = years)

wtp_high <- calculate_lifetime_cost(cost_usd_2005 = 782.70,
                                    exchange_rate = exchange_rate_2005,
                                    inflation_rate = inflation_rate_thb,
                                    discount_rate = discount_rate,
                                    years = years)

wtp <- list(
  low = wtp_low,
  base = wtp_base,
  high = wtp_high
)

# Alias to match requested naming
wtp$wtp_base <- wtp_base


# Threshold specs from DSA.R
threshold_specs <- list(
  "Strategy 1" = tribble(
    ~mvd,                                              ~a,    ~b,     ~lambda,
    "CBC & Hb typing cost per person",                 50,    1000,   wtp$wtp_base,
    "Probability of couple agreeing to PND",           0.10,  0.90,   wtp$wtp_base,
    "Probability of couple agreeing to abortion",      0.10,  0.90,   wtp$wtp_base,
    "Probability of early presentation",               0.10,  0.90,   wtp$wtp_base
  ),
  "Strategy 2" = tribble(
    ~mvd,                                              ~a,    ~b,     ~lambda,
    "CBC & Hb typing cost per couple",                 50,    5000,   wtp$wtp_base,
    "DNA analysis cost",                               10,    100000, wtp$wtp_base,
    "Probability of couple reconsidering decision to conceive", 0.10, 0.99, wtp$wtp_base
  ),
  "Strategy 3" = tribble(
    ~mvd,                                              ~a,    ~b,     ~lambda,
    "DNA analysis cost",                               500,   10000,  wtp$wtp_base
  ),
  "Strategy 4" = tribble(
    ~mvd,                                              ~a,    ~b,     ~lambda,
    # Add/adjust specs for S4 if desired:
    "DNA analysis cost",                               50,    100000, wtp$wtp_base,
    "Probability of couple reconsidering decision to conceive", 0.00, 1.00, wtp$wtp_base
  )
)

# Map strategies to their decision trees and index/ref edges
get_tree <- function(strategy) {
  get0(switch(strategy,
              "Strategy 1" = "dt",
              "Strategy 2" = "dt2",
              "Strategy 3" = "dt3",
              "Strategy 4" = "dt4"),
       inherits = TRUE)
}

edge_map <- list(
  "Strategy 1" = list(index = get0("e44", inherits = TRUE), ref = get0("e45", inherits = TRUE)),
  "Strategy 2" = list(index = get0("e64", inherits = TRUE), ref = get0("e65", inherits = TRUE)),
  "Strategy 3" = list(index = get0("e84", inherits = TRUE), ref = get0("e85", inherits = TRUE)),
  "Strategy 4" = list(index = get0("e111", inherits = TRUE), ref = get0("e112", inherits = TRUE))
)

safe_threshold <- function(tree, index, ref, mvd, a, b, lambda, tol = 0.01, nmax = 1000L) {
  if (is.null(tree) || is.null(index) || is.null(ref)) return(NA_real_)
  res <- try(tree$threshold(index = index, ref = ref, outcome = "ICER",
                            mvd = mvd, a = a, b = b, tol = tol,
                            lambda = lambda, nmax = nmax),
             silent = TRUE)
  tryCatch({
    if (is.numeric(res)) as.numeric(res)
    else if (is.data.frame(res) && "threshold" %in% names(res)) as.numeric(res$threshold[1])
    else if (is.list(res) && !is.null(res$threshold)) as.numeric(res$threshold)
    else NA_real_
  }, error = function(e) NA_real_)
}

# Build long summary
strategies <- names(threshold_specs)
threshold_long <- map_dfr(strategies, function(s) {
  tree <- get_tree(s)
  edges <- edge_map[[s]]
  specs <- threshold_specs[[s]]
  pmap_dfr(specs, function(mvd, a, b, lambda) {
    thr <- safe_threshold(tree, edges$index, edges$ref, mvd, a, b, lambda)
    tibble(
      strategy = s,
      parameter_label = mvd,
      a = a,
      b = b,
      lambda = lambda,
      threshold = thr
    )
  })
})

# Wide table by strategy
threshold_wide <- threshold_long |>
  select(parameter_label, strategy, threshold) |>
  pivot_wider(names_from = strategy, values_from = threshold) |>
  arrange(parameter_label)

threshold_wide
