# Calculate lifetime treatment costs using annual costs converted from 2005 USD
# to 2005 THB and adjusted to 2024 THB.
calculate_lifetime_cost <- function(cost_usd_2005, exchange_rate_2005, inflation_rate_thb, discount_rate, years){
  # Convert cost from USD to THB in 2005
  cost_thb_2005 <- cost_usd_2005 * exchange_rate_2005
  # Adjust cost for inflation to 2024 THB
  cost_thb_2024 <- cost_thb_2005 * inflation_rate_thb
  # Calculate the present value of lifetime costs
  lifetime_cost <- 0
  for (year in 0:(years - 1)) {
    discounted_cost <- cost_thb_2024 / ((1 + discount_rate) ^ year)
    lifetime_cost <- lifetime_cost + discounted_cost
  }
  return(lifetime_cost)
}

# WTP thresholds used consistently across the main economic model.
calculate_wtp_thresholds <- function(
  exchange_rate_2005 = 40.22,
  inflation_rate_thb = 166.22 / 111.2,
  discount_rate = 0.03,
  years = 30L,
  annual_cost_usd_2005 = c(low = 224.90, base = 562.76, high = 782.70)
) {
  values <- vapply(
    annual_cost_usd_2005,
    calculate_lifetime_cost,
    numeric(1L),
    exchange_rate_2005 = exchange_rate_2005,
    inflation_rate_thb = inflation_rate_thb,
    discount_rate = discount_rate,
    years = years
  )

  as.list(values)
}
