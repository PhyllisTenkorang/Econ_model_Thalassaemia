gamma_parameter <- function(description, mean, sd, mode) {
  if (mode == "base") return(mean)
  rdecision::GammaModVar$new(
    description,
    "THB",
    shape = mean^2 / sd^2,
    scale = sd^2 / mean
  )
}

beta_parameter <- function(description, alpha, beta, mode, uncertain = TRUE) {
  mean <- alpha / (alpha + beta)
  if (mode == "base" || !uncertain) return(mean)
  rdecision::BetaModVar$new(description, "", alpha = alpha, beta = beta)
}

create_model_parameters <- function(mode = c("base", "dsa", "psa")) {
  mode <- match.arg(mode)

  p_one_partner_trait <- beta_parameter(
    "Probability of one partner having trait", 268.8, 731.2, mode,
    uncertain = mode == "psa"
  )
  p_both_partners_trait <- beta_parameter(
    "Probability of both partners having trait", 25.6, 974.4, mode,
    uncertain = mode == "psa"
  )
  p_both_partners_healthy <- if (mode == "psa") {
    rdecision::ExprModVar$new(
      "Probability of both partners not having trait",
      "",
      rlang::quo((1 - p_one_partner_trait) - p_both_partners_trait)
    )
  } else {
    0.7056
  }

  list(
    costs = list(
      `1` = gamma_parameter("CBC & Hb typing cost per person", 390, 78, mode),
      `2` = gamma_parameter("CBC & Hb typing cost per couple", 780, 156, mode),
      `3` = gamma_parameter("DNA analysis cost", 6000, 1200, mode),
      `4` = gamma_parameter("PND cost", 5500, 1100, mode),
      `5` = gamma_parameter("Abortion cost", 3000, 600, mode)
    ),
    probabilities = list(
      early_presentation = beta_parameter(
        "Probability of early presentation", 80, 20, mode
      ),
      woman_trait = beta_parameter(
        "Probability of woman having trait", 160, 840, mode,
        uncertain = mode == "psa"
      ),
      partner_trait = beta_parameter(
        "Probability of man having trait", 160, 840, mode,
        uncertain = mode == "psa"
      ),
      one_partner_trait = p_one_partner_trait,
      both_partners_trait = p_both_partners_trait,
      both_partners_healthy = p_both_partners_healthy,
      pnd = beta_parameter(
        "Probability of couple agreeing to PND", 58, 42, mode
      ),
      abortion = beta_parameter(
        "Probability of couple agreeing to abortion", 67, 33, mode
      ),
      reconsideration = beta_parameter(
        "Probability of couple reconsidering decision to conceive", 50, 50, mode
      )
    )
  )
}
