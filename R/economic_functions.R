# Calculate lifetime cost of thalassemia treatment from USD in 2005 to THB in 2025
calculate_lifetime_cost <- function(cost_usd_2005, exchange_rate_2005, inflation_rate_thb, discount_rate, years){
  # Convert cost from USD to THB in 2005
  cost_thb_2005 <- cost_usd_2005 * exchange_rate_2005
  # Adjust cost for inflation to 2023 THB
  cost_thb_2023 <- cost_thb_2005 * inflation_rate_thb
  # Calculate the present value of lifetime costs
  lifetime_cost <- 0
  for (year in 0:(years - 1)) {
    discounted_cost <- cost_thb_2023 / ((1 + discount_rate) ^ year)
    lifetime_cost <- lifetime_cost + discounted_cost
  }
  return(lifetime_cost)
}
