## Decision Tree for Post-Conception Screening +/- Abortion for Thalassaemia prevention

# Load required libraries
library(rdecision)
library(ggplot2)

#Costs
cost_CBC_Hb <- GammaModVar$new(
  "CBC & Hb typing cost per person", "THB",
  shape = (390 ^ 2L) / (78 ^ 2L),
  scale = (78 ^ 2L) / 390)

cost_CBC_Hb2 <- GammaModVar$new(
  "CBC & Hb typing cost per couple", "THB",
  shape = (780 ^ 2L) / (156 ^ 2L),
  scale = (156 ^ 2L) / 780)

cost_DNA_analysis <- GammaModVar$new(
  "DNA analysis cost", "THB",
  shape = (6000 ^ 2L) / (1200 ^ 2L),
  scale = (1200 ^ 2L) / 6000)

cost_PND <- GammaModVar$new(
  "PND cost", "THB",
  shape = (5500 ^ 2L) / (1100 ^ 2L),
  scale = (1100 ^ 2L) / 5500)

cost_abortion <- GammaModVar$new(
  "Abortion cost", "THB",
  shape = (3000 ^ 2L) / (600 ^ 2L),
  scale = (600 ^ 2L) / 3000)


# Probabilties
p_early_presentation <- BetaModVar$new(
  "Probability of early presentation", "", alpha = 80, beta = 20)

p_thalassaemia_trait_W <- BetaModVar$new(
  "Probability of woman having trait", "", alpha = 160, beta = 840)

p_thalassaemia_trait_M <- BetaModVar$new(
  "Probability of man having trait", "", alpha = 160, beta = 840)

p_one_partner_trait <- BetaModVar$new(
  "Probability of one partner having trait", "", alpha = 269, beta = 731)

p_both_partners_trait <- BetaModVar$new(
  "Probability of both partners having trait", "", alpha = 26, beta = 974)

p_both_partners_healthy <- ExprModVar$new(
  "Probability of both partners not having trait", "", 
  rlang::quo((1-p_one_partner_trait) - p_both_partners_trait))

p_PND <- BetaModVar$new(
  "Probability of couple agreeing to PND", "", 
  alpha = 58, beta = 42)

p_abortion <- BetaModVar$new(
  "Probability of couple agreeing to abortion", "", 
  alpha = 67, beta = 33)

p_reconsideration <- BetaModVar$new(
  "Probability of couple reconsidering decision to conceive", "", 
  alpha = 50, beta = 50)

p_C <- 0.50                          # probability of carrier baby
p_T <- 0.25                          # probability of severe thalassaemia baby


# Define outcomes
T <- "Severe Thalassaemia baby"
H <- "Non-carier baby"
C <- "Carrier baby"

# Decision Tree for post-conception screening

# Define the post-conception screening branch
ta <- LeafNode$new("No baby", utility = 1.0)
tb <- LeafNode$new("T1", utility = 0.0)
c13 <- ChanceNode$new()
e1 <- Reaction$new(c13, ta, p = p_abortion, cost_abortion, label = "Abortion")
e2 <- Reaction$new(c13, tb, p = NA_real_, cost = 0.0, label = "No Abortion")

tc <- LeafNode$new("H1", utility = 1.0)
td <- LeafNode$new("C1", utility = 1.0)
c11 <- ChanceNode$new()
e3 <- Reaction$new(c11, tc, p = NA_real_)
e4 <- Reaction$new(c11, td, p = p_C)
e5 <- Reaction$new(c11, c13, p = p_T, label = "T")

te <- LeafNode$new("H2", utility = 1.0)
tf <- LeafNode$new("C2", utility = 1.0)
tg <- LeafNode$new("T2", utility = 0.0)
c12 <- ChanceNode$new()
e6 <- Reaction$new(c12, te, p = NA_real_)
e7 <- Reaction$new(c12, tf, p = p_C)
e8 <- Reaction$new(c12, tg, p = p_T)

c8 <- ChanceNode$new("DNA analysis")
e9 <- Reaction$new(c8, c11, p = p_PND, cost = cost_PND,
                   label = "PND")
e10 <- Reaction$new(c8, c12, p = NA_real_, cost = 0.0,
                    label = "No PND")

th <- LeafNode$new("H3", utility = 1.0)
ti <- LeafNode$new("C3", utility = 1.0)
c9 <- ChanceNode$new()
e11 <- Reaction$new(c9, th, p = NA_real_)
e12 <- Reaction$new(c9, ti, p = p_C)

c6 <- ChanceNode$new("Test male")
e13 <- Reaction$new(c6, c8, p = p_thalassaemia_trait_M,
                    cost = cost_DNA_analysis, label = "Carrier partner")
e14 <- Reaction$new(c6, c9, p = NA_real_, cost = 0.0,
                    label = "Non-carrier partner")

tj <- LeafNode$new("H5", utility = 1.0)
tk <- LeafNode$new("C4", utility = 1.0)
c10 <- ChanceNode$new()
e15 <- Reaction$new(c10, tj, p = NA_real_)
e16 <- Reaction$new(c10, tk, p = p_C)

tl <- LeafNode$new("H4", utility = 1.0)
c7 <- ChanceNode$new()
e17 <- Reaction$new(c7, tl, p = NA_real_, label = "Non-carrier partner")
e18 <- Reaction$new(c7, c10, p = p_thalassaemia_trait_M,
                    label = "Carrier partner")

c4 <- ChanceNode$new()
e19 <- Reaction$new(c4, c6, p = p_thalassaemia_trait_W, cost = cost_CBC_Hb,
                    label = "Carrier woman")
e20 <- Reaction$new(c4, c7, p = NA_real_, cost = 0.0,
                    label = "Non-carrier woman")

tm <- LeafNode$new("T3", utility = 0.0)
tn <- LeafNode$new("C5", utility = 1.0)
to <- LeafNode$new("H6", utility = 1.0)
c16 <- ChanceNode$new("DNA analysis")
e21 <- Reaction$new(c16, tm, p = p_T)
e22 <- Reaction$new(c16, tn, p = p_C)
e23 <- Reaction$new(c16, to, p = NA_real_)

tp <- LeafNode$new("H7", utility = 1.0)
tq <- LeafNode$new("C6", utility = 1.0)
c17 <- ChanceNode$new()
e24 <- Reaction$new(c17, tp, p = NA_real_)
e25 <- Reaction$new(c17, tq, p = p_C)

c14 <- ChanceNode$new("Test male")
e26 <- Reaction$new(c14, c16, p = p_thalassaemia_trait_M,
                    cost = cost_DNA_analysis, label = "Carrier partner")
e27 <- Reaction$new(c14, c17, p = NA_real_, cost = 0.0,
                    label = "Non-carrier partner")

tr <- LeafNode$new("H9", utility = 1.0)
ts <- LeafNode$new("C7", utility = 1.0)
c18 <- ChanceNode$new()
e28 <- Reaction$new(c18, tr, p = NA_real_)
e29 <- Reaction$new(c18, ts, p = p_C)

tt <- LeafNode$new("H8", utility = 1.0)
c15 <- ChanceNode$new()
e30 <- Reaction$new(c15, tt, p = NA_real_, label = "Non-carrier partner")
e31 <- Reaction$new(c15, c18, p = p_thalassaemia_trait_M,
                    label = "Carrier partner")

c5 <- ChanceNode$new()
e32 <- Reaction$new(c5, c14, p = p_thalassaemia_trait_W, cost = cost_CBC_Hb,
                    label = "Carrier woman")
e33 <- Reaction$new(c5, c15, p = NA_real_, cost = 0.0,
                    label = "Non-carrier woman")

c2 <- ChanceNode$new()
e34 <- Reaction$new(c2, c4, p = p_early_presentation,
                    label = "Early presentation")
e35 <- Reaction$new(c2, c5, p = NA_real_,
                    label = "Late presentation")

# Define no post-conception screening branch
tu <- LeafNode$new("H11", utility = 1.0)
tv <- LeafNode$new("C8", utility = 1.0)
c19 <- ChanceNode$new()
e36 <- Reaction$new(c19, tu, p = NA_real_)
e37 <- Reaction$new(c19, tv, p = p_C)

tw <- LeafNode$new("H12", utility = 1.0)
tx <- LeafNode$new("C9", utility = 1.0)
ty <- LeafNode$new("T4", utility = 0.0)
c20 <- ChanceNode$new()
e38 <- Reaction$new(c20, tw, p = NA_real_)
e39 <- Reaction$new(c20, tx, p = p_C)
e40 <- Reaction$new(c20, ty, p = p_T)

tz <- LeafNode$new("H10", utility = 1.0)
c3 <- ChanceNode$new()
e41 <- Reaction$new(c3, tz, p = p_both_partners_healthy, 
                    label = "Both non-carriers")
e42 <- Reaction$new(c3, c19, p = p_one_partner_trait,
                    label = "One carrier")
e43 <- Reaction$new(c3, c20, p = p_both_partners_trait,
                    label = "Both carriers")

# Define decision node
d1 <- DecisionNode$new("Potential carriers already pregnant")
e44 <- Action$new(d1, c2, cost_CBC_Hb, label = "Test woman")
e45 <- Action$new(d1, c3, 0.0, label = "No further testing")

# Create lists of nodes and edges
V <- list(
  d1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14,
  c15, c16, c17, c18, c19, c20, ta, tb, tc, td, te, tf, tg, th, ti, 
  tj, tk, tl,tm, tn, to, tp, tq, tr, ts, tt, tu, tv, tw, tx, ty, tz)

E <- list(
  e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, 
  e16, e17, e18, e19, e20, e21, e22, e23, e24, e25, e26, e27, e28, e29,
  e30, e31, e32, e33, e34, e35, e36, e37, e38, e39, e40, e41, e42, e43, 
  e44, e45)

# Create and draw the decision tree
dt <- DecisionTree$new(V, E)

# Evaluate
es <- dt$evaluate(by = "strategy")
ep <- dt$evaluate(by = "path")

# Probabilistic sensitivity analysis
N <- 1000L
psa <- dt$evaluate(setvars = "random", by = "run", N = N)
psa[ , "delta_cost"] <- psa[ , "Cost.Test woman"] - psa[ , "Cost.No further testing"]
psa[ , "delta_utility"] <- psa[ , "Utility.Test woman"] - psa[ , "Utility.No further testing"]
psa[, "ICER"] <- psa[ , "delta_cost"] / psa[ , "delta_utility"]



## Decision Tree for proposed pre-conception screening (Option 1)

# Define branch for pre-conception screening
t1 <- LeafNode$new("T5", utility = 0.0)
t2 <- LeafNode$new("C11", utility = 1.0)
t3 <- LeafNode$new("H15", utility = 1.0)
c52 <- ChanceNode$new()
e46 <- Reaction$new(c52, t1, p = p_T)
e47 <- Reaction$new(c52, t2, p = p_C)
e48 <- Reaction$new(c52, t3, p = NA_real_)

t4 <- LeafNode$new("No baby \nor alternatives", utility = 1.0)
c51 <- ChanceNode$new("DNA analysis")
e49 <- Reaction$new(c51, t4, p = p_reconsideration,
                    label = "Reconsideration")
e50 <- Reaction$new(c51, c52, p = NA_real_, 
                    label = "No Reconsideration")

t5 <- LeafNode$new("H14", utility = 1.0)
t6 <- LeafNode$new("C10", utility = 1.0)
c50 <- ChanceNode$new()
e51 <- Reaction$new(c50, t5, p = NA_real_)
e52 <- Reaction$new(c50, t6, p = p_C)

t7 <- LeafNode$new("H13", utility = 1.0)
c48 <- ChanceNode$new()
e53 <- Reaction$new(c48, t7, p = p_both_partners_healthy, cost = 0.0, 
                    label = "Both non-carriers")
e54 <- Reaction$new(c48, c50, p = p_one_partner_trait, cost = 0.0,
                    label = "One carrier")
e55 <- Reaction$new(c48, c51, p = p_both_partners_trait, cost = cost_DNA_analysis,
                    label = "Both carriers")

# Define branch for no pre-conception screening
t8 <- LeafNode$new("H17", utility = 1.0)
t9 <- LeafNode$new("C12", utility = 1.0)
c53 <- ChanceNode$new()
e56 <- Reaction$new(c53, t8, p = NA_real_)
e57 <- Reaction$new(c53, t9, p = p_C)

t10 <- LeafNode$new("H18", utility = 1.0)
t11 <- LeafNode$new("C13", utility = 1.0)
t12 <- LeafNode$new("T6", utility = 0.0)
c54 <- ChanceNode$new()
e58 <- Reaction$new(c54, t10, p = NA_real_)
e59 <- Reaction$new(c54, t11, p = p_C)
e60 <- Reaction$new(c54, t12, p = p_T)

t13 <- LeafNode$new("H16", utility = 1.0)
c49 <- ChanceNode$new()
e61 <- Reaction$new(c49, t13, p = p_both_partners_healthy, label = "Both non-carriers")
e62 <- Reaction$new(c49, c53, p = p_one_partner_trait,
                    label = "One carrier")
e63 <- Reaction$new(c49, c54, p = p_both_partners_trait,
                    label = "Both carriers")

# Define decision node
d2 <- DecisionNode$new("Decision to conceive")
e64 <- Action$new(d2, c48, cost = cost_CBC_Hb2,
                  label = "Screen couple with \nCBC + Hb typing")
e65 <- Action$new(d2, c49, cost = 0,
                  label = "No screening")

# Create lists of nodes and edges
V <- list(
  d2, c48, c49, c50, c51, c52, c53,c54,
  t1, t2, t3, t4, t5, t6, t7, t8, t9, 
  t10, t11, t12, t13)

E <- list(
  e46, e47, e48, e49, e50, e51, e52, e53, e54, e55, 
  e56, e57, e58, e59, e60, e61, e62, e63, e64, e65)

# Create and draw the decision tree
dt2 <- DecisionTree$new(V, E)

# Evaluate
es2 <- dt2$evaluate(by = "strategy")
ep2 <- dt2$evaluate(by = "path")

# Probabilistic sensitivity analysis
N <- 1000L
psa2 <- dt2$evaluate(setvars = "random", by = "run", N = N)
psa2[ , "delta_cost"] <- psa2[ , "Cost.Screen couple with \nCBC + Hb typing"] - 
  psa2[ , "Cost.No screening"]
psa2[ , "delta_utility"] <- psa2[ , "Utility.Screen couple with \nCBC + Hb typing"] - 
  psa2[ , "Utility.No screening"]
psa2[, "ICER"] <- psa2[ , "delta_cost"] / psa2[ , "delta_utility"]



## Decision Tree for proposed pre-conception screening (Option 2)

# Define branch for pre-conception screening
t14 <- LeafNode$new("T7", utility = 0.0)
t15 <- LeafNode$new("C15", utility = 1.0)
t16 <- LeafNode$new("H21", utility = 1.0)
c73 <- ChanceNode$new()
e66 <- Reaction$new(c73, t14, p = p_T)
e67 <- Reaction$new(c73, t15, p = p_C)
e68 <- Reaction$new(c73, t16, p = NA_real_)

t17 <- LeafNode$new("No baby or alternatives", utility = 1.0)
c72 <- ChanceNode$new()
e69 <- Reaction$new(c72, t17, p = p_reconsideration,
                    label = "Reconsideration")
e70 <- Reaction$new(c72, c73, p = NA_real_,
                    label = "No Reconsideration")

t18 <- LeafNode$new("H20", utility = 1.0)
t19 <- LeafNode$new("C14", utility = 1.0)
c71 <- ChanceNode$new()
e71 <- Reaction$new(c71, t18, p = NA_real_)
e72 <- Reaction$new(c71, t19, p = p_C)

t20 <- LeafNode$new("H19", utility = 1.0)
c69 <- ChanceNode$new()
e73 <- Reaction$new(c69, t20, p = p_both_partners_healthy, 
                    label = "Both non-carriers")
e74 <- Reaction$new(c69, c71, p = p_one_partner_trait, 
                    label = "One carrier")
e75 <- Reaction$new(c69, c72, p = p_both_partners_trait, 
                    label = "Both carriers")

# Define branch for no pre-conception screening
t21 <- LeafNode$new("H23", utility = 1.0)
t22 <- LeafNode$new("C16", utility = 1.0)
c74 <- ChanceNode$new()
e76 <- Reaction$new(c74, t21, p = NA_real_)
e77 <- Reaction$new(c74, t22, p = p_C)

t23 <- LeafNode$new("H24", utility = 1.0)
t24 <- LeafNode$new("C17", utility = 1.0)
t25 <- LeafNode$new("T8", utility = 0.0)
c75 <- ChanceNode$new()
e78 <- Reaction$new(c75, t23, p = NA_real_)
e79 <- Reaction$new(c75, t24, p = p_C)
e80 <- Reaction$new(c75, t25, p = p_T)

t26 <- LeafNode$new("H22", utility = 1.0)
c70 <- ChanceNode$new()
e81 <- Reaction$new(c70, t26, p = p_both_partners_healthy, 
                    label = "Both non-carriers")
e82 <- Reaction$new(c70, c74, p = p_one_partner_trait,
                    label = "One carrier")
e83 <- Reaction$new(c70, c75, p = p_both_partners_trait,
                    label = "Both carriers")

# Define decision node
d3 <- DecisionNode$new("Decision to conceive")
e84 <- Action$new(d3, c69, cost = cost_DNA_analysis,
                  label = "Screen couple with \nDNA analysis")
e85 <- Action$new(d3, c70, cost = 0,
                  label = "No screening")

# Create lists of nodes and edges
V <- list(
  d3, c69, c70, c71, c72, c73, c74, c75,
  t14, t15, t16, t17, t18, t19, t20, 
  t21, t22, t23, t24, t25, t26)

E <- list(
  e66, e67, e68, e69, e70, e71, e72, e73, e74, e75,
  e76, e77, e78, e79, e80, e81, e82, e83, e84, e85)

# Create and draw the decision tree
dt3 <- DecisionTree$new(V, E)

# Evaluate
es3 <- dt3$evaluate(by = "strategy")
ep3 <- dt3$evaluate(by = "path")

# Probabilistic sensitivity analysis
N <- 1000L
psa3 <- dt3$evaluate(setvars = "random", by = "run", N = N)
psa3[ , "delta_cost"] <- psa3[ , "Cost.Screen couple with \nDNA analysis"] - 
  psa3[ , "Cost.No screening"]
psa3[ , "delta_utility"] <- psa3[ , "Utility.Screen couple with \nDNA analysis"] - 
  psa3[ , "Utility.No screening"]
psa3[, "ICER"] <- psa3[ , "delta_cost"] / psa3[ , "delta_utility"]



## Decision Tree for a combination of pre and post-conception screening +/- abortion

# Define branch for screening
t27 <- LeafNode$new("No baby", utility = 1.0)
t28 <- LeafNode$new("T9", utility = 0.0)
c97 <- ChanceNode$new()
e86 <- Reaction$new(c97, t27, p = p_abortion, cost_abortion,
                    label = "Abortion")
e87 <- Reaction$new(c97, t28, p = NA_real_, cost = 0.0,
                    label = "No Abortion")

t29 <- LeafNode$new("C19", utility = 1.0)
t30 <- LeafNode$new("H27", utility = 1.0)
c95 <- ChanceNode$new()
e88 <- Reaction$new(c95, c97, p = p_T, label = "T")
e89 <- Reaction$new(c95, t29, p = p_C)
e90 <- Reaction$new(c95, t30, p = NA_real_)

t31 <- LeafNode$new("T10", utility = 0.0)
t32 <- LeafNode$new("C20", utility = 1.0)
t33 <- LeafNode$new("H28", utility = 1.0)
c96 <- ChanceNode$new()
e91 <- Reaction$new(c96, t31, p = p_T)
e92 <- Reaction$new(c96, t32, p = p_C)
e93 <- Reaction$new(c96, t33, p = NA_real_)

c94 <- ChanceNode$new("Pregnant")
e94 <- Reaction$new(c94, c96, p = NA_real_, cost = 0.0,
                    label = "No PND")
e95 <- Reaction$new(c94, c95, p = p_PND, cost = cost_PND,
                    label = "PND")

t34 <- LeafNode$new("No baby or alternatives", utility = 1.0)
c93 <- ChanceNode$new("DNA analysis")
e96 <- Reaction$new(c93, t34, p = p_reconsideration,
                    label = "Reconsideration")
e97 <- Reaction$new(c93, c94, p = NA_real_,
                    label = "No Reconsideration")

t35 <- LeafNode$new("H26", utility = 1.0)
t36 <- LeafNode$new("C18", utility = 1.0)
c92 <- ChanceNode$new()
e98 <- Reaction$new(c92, t35, p = NA_real_)
e99 <- Reaction$new(c92, t36, p = p_C)

t37 <- LeafNode$new("H25", utility = 1.0)
c90 <- ChanceNode$new()
e100 <- Reaction$new(c90, t37, p = p_both_partners_healthy, cost = 0.0,
                     label = "Both non-carriers")
e101 <- Reaction$new(c90, c92, p = p_one_partner_trait, cost = 0.0,
                     label = "One carrier")
e102 <- Reaction$new(c90, c93, p = p_both_partners_trait, cost = cost_DNA_analysis,
                     label = "Both carriers")

# Define branch for no screening
t38 <- LeafNode$new("H30", utility = 1.0)
t39 <- LeafNode$new("C21", utility = 1.0)
c98 <- ChanceNode$new()
e103 <- Reaction$new(c98, t38, p = NA_real_)
e104 <- Reaction$new(c98, t39, p = p_C)

t40 <- LeafNode$new("H31", utility = 1.0)
t41 <- LeafNode$new("C22", utility = 1.0)
t42 <- LeafNode$new("T11", utility = 0.0)
c99 <- ChanceNode$new()
e105 <- Reaction$new(c99, t40, p = NA_real_)
e106 <- Reaction$new(c99, t41, p = p_C)
e107 <- Reaction$new(c99, t42, p = p_T)

t43 <- LeafNode$new("H29", utility = 1.0)
c91 <- ChanceNode$new()
e108 <- Reaction$new(c91, t43, p = p_both_partners_healthy, 
                     label = "Both non-carriers")
e109 <- Reaction$new(c91, c98, p = p_one_partner_trait,
                     label = "One carrier")
e110 <- Reaction$new(c91, c99, p = p_both_partners_trait,
                     label = "Both carriers")

# Define decision node
d4 <- DecisionNode$new("Decision to conceive")
e111 <- Action$new(d4, c90, cost = cost_CBC_Hb2,
                   label = "Screen couple with \nCBC + Hb typing")
e112 <- Action$new(d4, c91, cost = 0,
                   label = "No screening")

# Create lists of nodes and edges
V <- list(
  d4, c90, c91, c92, c93, c94, c95, c96, c97, c98,
  c99, t27, t28, t29, t30, t31, t32, t33, t34, t35, 
  t36, t37, t38, t39, t40, t41, t42, t43)

E <- list(
  e86, e87, e88, e89, e90, e91, e92, e93, e94, e95,
  e96, e97, e98, e99, e100, e101, e102, e103, e104, 
  e105, e106, e107, e108, e109, e110, e111, e112)

# Create and draw the decision tree
dt4 <- DecisionTree$new(V, E)

# Evaluate
es4 <- dt4$evaluate(by = "strategy")
ep4 <- dt4$evaluate(by = "path")

# Probabilistic sensitivity analysis
N <- 1000L
psa4 <- dt4$evaluate(setvars = "random", by = "run", N = N)
psa4[ , "delta_cost"] <- psa4[ , "Cost.Screen couple with \nCBC + Hb typing"] - 
  psa4[ , "Cost.No screening"]
psa4[ , "delta_utility"] <- psa4[ , "Utility.Screen couple with \nCBC + Hb typing"] - 
  psa4[ , "Utility.No screening"]
psa4[, "ICER"] <- psa4[ , "delta_cost"] / psa4[ , "delta_utility"]


# PSA on Cost-Effectiveness Plane

## Define WTP threshold
wtp <- 677205.9772

plot_PSA <- function(data, wtp, xlim, ylim){
  
  G <- ggplot(data, aes(x = delta_utility, y = delta_cost)) +
    geom_point(alpha = 0.5, color = "blue") +  
    geom_abline(intercept = 0, slope = wtp, color = "red", linetype = "dashed", size = 1) + 
    labs(
      x = "Proportion of severe Thalassaemia cases averted",
      y = "Incremental costs",
      caption = "Red dashed line represents lifetime costs of treatment"
    ) +
    theme_bw(base_size = 13) +
    scale_x_continuous(limits = c(0, xlim), expand = c(0,0))+
    scale_y_continuous(limits = c(0, ylim), expand = c(0,0)) 
  return(G)
}

plot_PSA(data = psa, wtp = wtp, xlim = 0.015, ylim = 12500)
plot_PSA(data = psa2, wtp = wtp, xlim = 0.015, ylim = 15000)
plot_PSA(data = psa3, wtp = wtp, xlim = 0.015, ylim = 25000)
plot_PSA(data = psa4, wtp = wtp, xlim = 0.015, ylim = 15000)


































































































