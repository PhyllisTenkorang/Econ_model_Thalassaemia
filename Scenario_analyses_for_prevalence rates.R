## Decision Tree for Post-Conception Screening for thalassaemia prevention

# Load required libraries
library(rdecision)

#Costs
cost_CBC_Hb <- 390

cost_CBC_Hb2 <- 780

cost_DNA_analysis <- 6000

cost_PND <- 5500

cost_abortion <- 3000


# Probabilties
p_early_presentation <- 0.8

p_thalassaemia_trait_W <- 0.10

p_thalassaemia_trait_M <- 0.10

p_one_partner_trait <- 0.18

p_both_partners_trait <- 0.01

p_both_partners_healthy <- 0.81

p_PND <- 0.58

p_abortion <- 0.67

p_reconsideration <- 0.50

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

# ICER
inc_cost <- es$Cost[2] - es$Cost[1]
T_averted <- ep$Probability[6] - (ep$Probability[24] + ep$Probability[25] + ep$Probability[26])
ICER <- inc_cost / T_averted



## Decision Tree for proposed pre-conception screening (Option 1)
p_thalassaemia_trait_W <- 0.10

p_thalassaemia_trait_M <- 0.10

p_one_partner_trait <- 0.18

p_both_partners_trait <- 0.01

p_both_partners_healthy <- 0.81

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

# ICER2
inc_cost2 <- es2$Cost[2] - es2$Cost[1]
T_averted2 <- ep2$Probability[6] - ep2$Probability[13]
ICER2 <- inc_cost2 / T_averted2



## Decision Tree for a combination of pre and post-conception screening
p_thalassaemia_trait_W <- 0.085

p_thalassaemia_trait_M <- 0.085

p_one_partner_trait <- 0.15555

p_both_partners_trait <- 0.007225

p_both_partners_healthy <- 0.837225

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

# ICER4
inc_cost4 <- es4$Cost[2] - es4$Cost[1]
T_averted4 <- ep4$Probability[6] - (ep4$Probability[16] + ep4$Probability[17])
ICER4 <- inc_cost4 / T_averted4














































































