library(rdecision)
library(dplyr)

# 1. Setup base parameters and WTP
source("functions.R")
# Re-calculate WTP base to ensure we have the correct target lambda
exchange_rate_2005 <- 40.22
inflation_rate_thb <- 166.22/111.2
discount_rate <- 0.03
years <- 30
lambda <- calculate_lifetime_cost(562.76, exchange_rate_2005, inflation_rate_thb, discount_rate, years)

# Global fixed parameters (costs/uptake probs) from your previous scripts
cost_CBC_Hb <- 390
cost_CBC_Hb2 <- 780
cost_DNA_analysis <- 6000
cost_PND <- 5500
cost_abortion <- 3000
p_early_presentation <- 0.8
p_PND <- 0.58
p_abortion <- 0.67
p_reconsideration <- 0.50
p_C <- 0.50
p_T <- 0.25

# 2. Function to build & evaluate a specific strategy at a given prevalence p
# Returns: ICER (numeric) for that prevalence
get_icer_for_p <- function(p, strategy) {
  # Derived probabilities from prevalence p (random mating)
  # p = p_thal_W = p_thal_M
  p_both_partners_healthy <- (1 - p)^2
  p_both_partners_trait   <- p^2
  p_one_partner_trait     <- 2 * p * (1 - p)
  
  # Re-build tree nodes/edges with these new P values
  # (Copying the minimal structure needed from your attached code)
  
  if (strategy == "S1") {
    # -- Strategy 1: Post-conception --
    # Nodes
    ta <- LeafNode$new("No baby", utility = 1.0)
    tb <- LeafNode$new("T1", utility = 0.0)
    c13 <- ChanceNode$new()
    e1 <- Reaction$new(c13, ta, p = p_abortion, cost = cost_abortion)
    e2 <- Reaction$new(c13, tb, p = NA_real_, cost = 0.0)
    
    tc <- LeafNode$new("H1", utility = 1.0)
    td <- LeafNode$new("C1", utility = 1.0)
    c11 <- ChanceNode$new()
    e3 <- Reaction$new(c11, tc, p = NA_real_)
    e4 <- Reaction$new(c11, td, p = p_C)
    e5 <- Reaction$new(c11, c13, p = p_T)
    
    te <- LeafNode$new("H2", utility = 1.0)
    tf <- LeafNode$new("C2", utility = 1.0)
    tg <- LeafNode$new("T2", utility = 0.0)
    c12 <- ChanceNode$new()
    e6 <- Reaction$new(c12, te, p = NA_real_)
    e7 <- Reaction$new(c12, tf, p = p_C)
    e8 <- Reaction$new(c12, tg, p = p_T)
    
    c8 <- ChanceNode$new()
    e9 <- Reaction$new(c8, c11, p = p_PND, cost = cost_PND)
    e10 <- Reaction$new(c8, c12, p = NA_real_, cost = 0.0)
    
    th <- LeafNode$new("H3", utility = 1.0)
    ti <- LeafNode$new("C3", utility = 1.0)
    c9 <- ChanceNode$new()
    e11 <- Reaction$new(c9, th, p = NA_real_)
    e12 <- Reaction$new(c9, ti, p = p_C)
    
    c6 <- ChanceNode$new()
    e13 <- Reaction$new(c6, c8, p = p, cost = cost_DNA_analysis)
    e14 <- Reaction$new(c6, c9, p = NA_real_, cost = 0.0)
    
    tj <- LeafNode$new("H5", utility = 1.0)
    tk <- LeafNode$new("C4", utility = 1.0)
    c10 <- ChanceNode$new()
    e15 <- Reaction$new(c10, tj, p = NA_real_)
    e16 <- Reaction$new(c10, tk, p = p_C)
    
    tl <- LeafNode$new("H4", utility = 1.0)
    c7 <- ChanceNode$new()
    e17 <- Reaction$new(c7, tl, p = NA_real_)
    e18 <- Reaction$new(c7, c10, p = p)
    
    c4 <- ChanceNode$new()
    e19 <- Reaction$new(c4, c6, p = p, cost = cost_CBC_Hb)
    e20 <- Reaction$new(c4, c7, p = NA_real_, cost = 0.0)
    
    tm <- LeafNode$new("T3", utility = 0.0)
    tn <- LeafNode$new("C5", utility = 1.0)
    to <- LeafNode$new("H6", utility = 1.0)
    c16 <- ChanceNode$new()
    e21 <- Reaction$new(c16, tm, p = p_T)
    e22 <- Reaction$new(c16, tn, p = p_C)
    e23 <- Reaction$new(c16, to, p = NA_real_)
    
    tp <- LeafNode$new("H7", utility = 1.0)
    tq <- LeafNode$new("C6", utility = 1.0)
    c17 <- ChanceNode$new()
    e24 <- Reaction$new(c17, tp, p = NA_real_)
    e25 <- Reaction$new(c17, tq, p = p_C)
    
    c14 <- ChanceNode$new()
    e26 <- Reaction$new(c14, c16, p = p, cost = cost_DNA_analysis)
    e27 <- Reaction$new(c14, c17, p = NA_real_, cost = 0.0)
    
    tr <- LeafNode$new("H9", utility = 1.0)
    ts <- LeafNode$new("C7", utility = 1.0)
    c18 <- ChanceNode$new()
    e28 <- Reaction$new(c18, tr, p = NA_real_)
    e29 <- Reaction$new(c18, ts, p = p_C)
    
    tt <- LeafNode$new("H8", utility = 1.0)
    c15 <- ChanceNode$new()
    e30 <- Reaction$new(c15, tt, p = NA_real_)
    e31 <- Reaction$new(c15, c18, p = p)
    
    c5 <- ChanceNode$new()
    e32 <- Reaction$new(c5, c14, p = p, cost = cost_CBC_Hb)
    e33 <- Reaction$new(c5, c15, p = NA_real_, cost = 0.0)
    
    c2 <- ChanceNode$new()
    e34 <- Reaction$new(c2, c4, p = p_early_presentation)
    e35 <- Reaction$new(c2, c5, p = NA_real_)
    
    # Comparator branch (No Screening S1)
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
    e41 <- Reaction$new(c3, tz, p = p_both_partners_healthy)
    e42 <- Reaction$new(c3, c19, p = p_one_partner_trait)
    e43 <- Reaction$new(c3, c20, p = p_both_partners_trait)
    
    d1 <- DecisionNode$new("S1 Decision")
    act_scr <- Action$new(d1, c2, cost = cost_CBC_Hb, label = "Screen")
    act_no  <- Action$new(d1, c3, cost = 0.0, label = "No Screen")
    
    dt <- DecisionTree$new(
      V = list(d1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20,
               ta, tb, tc, td, te, tf, tg, th, ti, tj, tk, tl, tm, tn, to, tp, tq, tr, ts, tt, tu, tv, tw, tx, ty, tz),
      E = list(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18,e19,e20,e21,e22,e23,e24,e25,e26,e27,
               e28,e29,e30,e31,e32,e33,e34,e35,e36,e37,e38,e39,e40,e41,e42,e43,act_scr,act_no)
    )
    
    es <- dt$evaluate()
    # Cost diff = Screen - NoScreen
    # Effect diff = (Prob T averted)
    # Actually easier to use evaluate output order
    cost_scr <- es$Cost[es[,2] == "Screen"]
    cost_no  <- es$Cost[es[,2] == "No Screen"]
    # Check tree output or just manually pull the prob of severe thal T
    # T outcomes in Screen: tb, e5->c13, e8->c12, tm, ty
    # But evaluating fully is safer. Let's rely on standard evaluate structure
    # assuming index order is consistent (Screen is 1st or 2nd)
    
    # Alternatively path analysis:
    ep <- dt$evaluate(by = "path")
    # Sum prob of T outcomes for each strategy
    # Screen nodes leading to T: tb, tg, tm
    # No Screen nodes leading to T: ty
    prob_T_scr <- sum(ep$Probability[ep$Leaf %in% c("T1","T2","T3") & ep[,2] == "Screen"])
    prob_T_no  <- sum(ep$Probability[ep$Leaf %in% c("T4") & ep[,2] == "No Screen"])
    
    dT <- prob_T_no - prob_T_scr
    dC <- cost_scr - cost_no
    return(dC / dT)
  }
  
  if (strategy == "S2") {
    # -- Strategy 2: Pre-conception Targeted PND --
    t1 <- LeafNode$new("T5", utility=0.0); t2 <- LeafNode$new("C11", utility=1.0); t3 <- LeafNode$new("H15", utility=1.0)
    c52 <- ChanceNode$new()
    e46<-Reaction$new(c52,t1,p=p_T); e47<-Reaction$new(c52,t2,p=p_C); e48<-Reaction$new(c52,t3,p=NA_real_)
    
    t4 <- LeafNode$new("No baby", utility=1.0)
    c51 <- ChanceNode$new()
    e49<-Reaction$new(c51,t4,p=p_reconsideration); e50<-Reaction$new(c51,c52,p=NA_real_)
    
    t5 <- LeafNode$new("H14", utility=1.0); t6 <- LeafNode$new("C10", utility=1.0)
    c50 <- ChanceNode$new()
    e51<-Reaction$new(c50,t5,p=NA_real_); e52<-Reaction$new(c50,t6,p=p_C)
    
    t7 <- LeafNode$new("H13", utility=1.0)
    c48 <- ChanceNode$new()
    e53<-Reaction$new(c48,t7,p=p_both_partners_healthy,cost=0.0)
    e54<-Reaction$new(c48,c50,p=p_one_partner_trait,cost=0.0)
    e55<-Reaction$new(c48,c51,p=p_both_partners_trait,cost=cost_DNA_analysis)
    
    # Comparator S2
    t8<-LeafNode$new("H17", utility=1.0); t9<-LeafNode$new("C12", utility=1.0)
    c53<-ChanceNode$new(); e56<-Reaction$new(c53,t8,p=NA_real_); e57<-Reaction$new(c53,t9,p=p_C)
    
    t10<-LeafNode$new("H18", utility=1.0); t11<-LeafNode$new("C13", utility=1.0); t12<-LeafNode$new("T6", utility=0.0)
    c54<-ChanceNode$new(); e58<-Reaction$new(c54,t10,p=NA_real_); e59<-Reaction$new(c54,t11,p=p_C); e60<-Reaction$new(c54,t12,p=p_T)
    
    t13<-LeafNode$new("H16", utility=1.0)
    c49<-ChanceNode$new()
    e61<-Reaction$new(c49,t13,p=p_both_partners_healthy)
    e62<-Reaction$new(c49,c53,p=p_one_partner_trait)
    e63<-Reaction$new(c49,c54,p=p_both_partners_trait)
    
    d2 <- DecisionNode$new("S2 Decision")
    e64 <- Action$new(d2, c48, cost=cost_CBC_Hb2, label="Screen")
    e65 <- Action$new(d2, c49, cost=0.0, label="No Screen")
    
    dt2 <- DecisionTree$new(V=list(d2,c48,c49,c50,c51,c52,c53,c54,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13),
                            E=list(e46,e47,e48,e49,e50,e51,e52,e53,e54,e55,e56,e57,e58,e59,e60,e61,e62,e63,e64,e65))
    
    es2 <- dt2$evaluate()
    ep2 <- dt2$evaluate(by="path")
    
    cost_scr <- es2$Cost[es2[,2]=="Screen"]
    cost_no  <- es2$Cost[es2[,2]=="No Screen"]
    prob_T_scr <- sum(ep2$Probability[ep2$Leaf=="T5" & ep2[,2]=="Screen"])
    prob_T_no  <- sum(ep2$Probability[ep2$Leaf=="T6" & ep2[,2]=="No Screen"])
    
    return((cost_scr - cost_no)/(prob_T_no - prob_T_scr))
  }
  
  if (strategy == "S3") {
     # -- Strategy 3: Pre-conception Universal DNA --
     t14<-LeafNode$new("T7",utility=0.0); t15<-LeafNode$new("C15",utility=1.0); t16<-LeafNode$new("H21",utility=1.0)
     c73<-ChanceNode$new(); e66<-Reaction$new(c73,t14,p=p_T); e67<-Reaction$new(c73,t15,p=p_C); e68<-Reaction$new(c73,t16,p=NA_real_)
     
     t17<-LeafNode$new("No baby", utility=1.0)
     c72<-ChanceNode$new(); e69<-Reaction$new(c72,t17,p=p_reconsideration); e70<-Reaction$new(c72,c73,p=NA_real_)
     
     t18<-LeafNode$new("H20",utility=1.0); t19<-LeafNode$new("C14",utility=1.0)
     c71<-ChanceNode$new(); e71<-Reaction$new(c71,t18,p=NA_real_); e72<-Reaction$new(c71,t19,p=p_C)
     
     t20<-LeafNode$new("H19",utility=1.0)
     c69<-ChanceNode$new()
     e73<-Reaction$new(c69,t20,p=p_both_partners_healthy)
     e74<-Reaction$new(c69,c71,p=p_one_partner_trait)
     e75<-Reaction$new(c69,c72,p=p_both_partners_trait)
     
     # Comparator S3
     t21<-LeafNode$new("H23",utility=1.0); t22<-LeafNode$new("C16",utility=1.0)
     c74<-ChanceNode$new(); e76<-Reaction$new(c74,t21,p=NA_real_); e77<-Reaction$new(c74,t22,p=p_C)
     
     t23<-LeafNode$new("H24",utility=1.0); t24<-LeafNode$new("C17",utility=1.0); t25<-LeafNode$new("T8",utility=0.0)
     c75<-ChanceNode$new(); e78<-Reaction$new(c75,t23,p=NA_real_); e79<-Reaction$new(c75,t24,p=p_C); e80<-Reaction$new(c75,t25,p=p_T)
     
     t26<-LeafNode$new("H22",utility=1.0)
     c70<-ChanceNode$new()
     e81<-Reaction$new(c70,t26,p=p_both_partners_healthy)
     e82<-Reaction$new(c70,c74,p=p_one_partner_trait)
     e83<-Reaction$new(c70,c75,p=p_both_partners_trait)
     
     d3 <- DecisionNode$new("S3 Decision")
     e84 <- Action$new(d3, c69, cost=cost_DNA_analysis, label="Screen")
     e85 <- Action$new(d3, c70, cost=0.0, label="No Screen")
     
     dt3 <- DecisionTree$new(V=list(d3,c69,c70,c71,c72,c73,c74,c75,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26),
                             E=list(e66,e67,e68,e69,e70,e71,e72,e73,e74,e75,e76,e77,e78,e79,e80,e81,e82,e83,e84,e85))
     
     es3 <- dt3$evaluate()
     ep3 <- dt3$evaluate(by="path")
     
     cost_scr <- es3$Cost[es3[,2]=="Screen"]
     cost_no  <- es3$Cost[es3[,2]=="No Screen"]
     prob_T_scr <- sum(ep3$Probability[ep3$Leaf=="T7" & ep3[,2]=="Screen"])
     prob_T_no  <- sum(ep3$Probability[ep3$Leaf=="T8" & ep3[,2]=="No Screen"])

     return((cost_scr - cost_no)/(prob_T_no - prob_T_scr))
  }
  
  if (strategy == "S4") {
    # -- Strategy 4: Combo --
    t27<-LeafNode$new("No baby",utility=1.0); t28<-LeafNode$new("T9",utility=0.0)
    c97<-ChanceNode$new(); e86<-Reaction$new(c97,t27,p=p_abortion,cost=cost_abortion); e87<-Reaction$new(c97,t28,p=NA_real_,cost=0.0)
    
    t29<-LeafNode$new("C19",utility=1.0); t30<-LeafNode$new("H27",utility=1.0)
    c95<-ChanceNode$new(); e88<-Reaction$new(c95,c97,p=p_T); e89<-Reaction$new(c95,t29,p=p_C); e90<-Reaction$new(c95,t30,p=NA_real_)
    
    t31<-LeafNode$new("T10",utility=0.0); t32<-LeafNode$new("C20",utility=1.0); t33<-LeafNode$new("H28",utility=1.0)
    c96<-ChanceNode$new(); e91<-Reaction$new(c96,t31,p=p_T); e92<-Reaction$new(c96,t32,p=p_C); e93<-Reaction$new(c96,t33,p=NA_real_)
    
    c94<-ChanceNode$new(); e94<-Reaction$new(c94,c96,p=NA_real_,cost=0.0); e95<-Reaction$new(c94,c95,p=p_PND,cost=cost_PND)
    
    t34<-LeafNode$new("No baby",utility=1.0)
    c93<-ChanceNode$new(); e96<-Reaction$new(c93,t34,p=p_reconsideration); e97<-Reaction$new(c93,c94,p=NA_real_)
    
    t35<-LeafNode$new("H26",utility=1.0); t36<-LeafNode$new("C18",utility=1.0)
    c92<-ChanceNode$new(); e98<-Reaction$new(c92,t35,p=NA_real_); e99<-Reaction$new(c92,t36,p=p_C)
    
    t37<-LeafNode$new("H25",utility=1.0)
    c90<-ChanceNode$new()
    e100<-Reaction$new(c90,t37,p=p_both_partners_healthy,cost=0.0)
    e101<-Reaction$new(c90,c92,p=p_one_partner_trait,cost=0.0)
    e102<-Reaction$new(c90,c93,p=p_both_partners_trait,cost=cost_DNA_analysis)
    
    # Comparator S4 (No Screen) - same structure as S4/NoScreen in DSA.R
    t38<-LeafNode$new("H30",utility=1.0); t39<-LeafNode$new("C21",utility=1.0)
    c98<-ChanceNode$new(); e103<-Reaction$new(c98,t38,p=NA_real_); e104<-Reaction$new(c98,t39,p=p_C)
    
    t40<-LeafNode$new("H31",utility=1.0); t41<-LeafNode$new("C22",utility=1.0); t42<-LeafNode$new("T11",utility=0.0)
    c99<-ChanceNode$new(); e105<-Reaction$new(c99,t40,p=NA_real_); e106<-Reaction$new(c99,t41,p=p_C); e107<-Reaction$new(c99,t42,p=p_T)
    
    t43<-LeafNode$new("H29",utility=1.0)
    c91<-ChanceNode$new()
    e108<-Reaction$new(c91,t43,p=p_both_partners_healthy)
    e109<-Reaction$new(c91,c98,p=p_one_partner_trait)
    e110<-Reaction$new(c91,c99,p=p_both_partners_trait)
    
    d4 <- DecisionNode$new("S4 Decision")
    e111 <- Action$new(d4, c90, cost=cost_CBC_Hb2, label="Screen")
    e112 <- Action$new(d4, c91, cost=0.0, label="No Screen")
    
    dt4 <- DecisionTree$new(V=list(d4,c90,c91,c92,c93,c94,c95,c96,c97,c98,c99,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43),
                            E=list(e86,e87,e88,e89,e90,e91,e92,e93,e94,e95,e96,e97,e98,e99,e100,e101,e102,e103,e104,e105,e106,e107,e108,e109,e110,e111,e112))
    
    es4 <- dt4$evaluate()
    ep4 <- dt4$evaluate(by="path")
    
    cost_scr <- es4$Cost[es4[,2]=="Screen"]
    cost_no  <- es4$Cost[es4[,2]=="No Screen"]
    # T outcomes in S4 Screen path: T9(t28), T10(t31)
    # T outcomes in S4 NoScreen path: T11(t42)
    prob_T_scr <- sum(ep4$Probability[ep4$Leaf %in% c("T9","T10") & ep4[,2]=="Screen"])
    prob_T_no  <- sum(ep4$Probability[ep4$Leaf %in% c("T11") & ep4[,2]=="No Screen"])

    return((cost_scr - cost_no)/(prob_T_no - prob_T_scr))
  }
}

# 3. Solver: Find p in [0.01, 0.99] such that ICER(p) = lambda
strategies <- c("S1", "S2", "S3", "S4")
results <- tibble(Strategy = strategies, Threshold_Prevalence = NA_real_)

for (i in seq_along(strategies)) {
  s <- strategies[i]
  # Define objective: f(p) = ICER(p) - lambda
  f <- function(p) { get_icer_for_p(p, s) - lambda }
  
  # Try finding root
  # Check signs at endpoints first to avoid error
  y_low  <- try(f(0.01), silent=TRUE)
  y_high <- try(f(0.99), silent=TRUE)
  
  # Check if results are errors, empty, or NA before using them in logic
  if (inherits(y_low, "try-error") || inherits(y_high, "try-error")) {
    results$Threshold_Prevalence[i] <- NA
  } else if (length(y_low) == 1 && length(y_high) == 1 && 
             !is.na(y_low) && !is.na(y_high) && 
             sign(y_low) != sign(y_high)) {
    
    root <- uniroot(f, c(0.01, 0.99))$root
    results$Threshold_Prevalence[i] <- root
    
  } else {
    results$Threshold_Prevalence[i] <- NA # No root in range or invalid endpoints
  }
}
print(results)
