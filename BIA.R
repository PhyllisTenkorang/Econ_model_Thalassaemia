# ==============================================================================
# Budget Impact Analysis for Thalassaemia Screening Strategies
# ==============================================================================
# Define the four screening strategies with their associated costs and 
# eligible populations
tabs <- data.frame(
  strategies = c("Strategy 1", "Strategy 2", "Strategy 3", "Strategy 4"),
  cost_per_unit = c(677.30, 933.60, 6000.00, 978.16),  # Cost per person screened (THB)
  eligible_pop = c(250000, 12800000, 12800000, 12800000)  # Number of eligible individuals
)

# Coverage scenarios: 25%, 50%, and 75% of eligible population
coverage <- c(0.25, 0.50, 0.75)

# Exchange rate for THB to USD conversion
exchange_rate <- 32.5  # THB per USD

# ------------------------------------------------------------------------------
# Cost calculation function
# ------------------------------------------------------------------------------
calculate_total_cost <- function(cost_per_unit, eligible_pop, coverage) {
  total_cost_thb <- cost_per_unit * eligible_pop * coverage
  return(total_cost_thb)
}

# ------------------------------------------------------------------------------
# Generate results for all strategy-coverage combinations
# ------------------------------------------------------------------------------
results <- data.frame()

# Nested loop: iterate through each strategy and coverage level
for (i in 1:nrow(tabs)) {
  for (cov in coverage) {
    # Calculate total cost for this strategy-coverage combination
    total_cost <- calculate_total_cost(
      tabs$cost_per_unit[i],
      tabs$eligible_pop[i],
      cov)
    
    # Append results to data frame
    results <- rbind(results, data.frame(
      strategy = tabs$strategies[i],
      coverage = cov,
      total_cost_local = total_cost  # Total cost in THB
    ))
  }
}

# Add USD equivalent column by converting THB to USD
results <- within(results, {
  total_cost_usd <- total_cost_local / exchange_rate
})

print(results)
