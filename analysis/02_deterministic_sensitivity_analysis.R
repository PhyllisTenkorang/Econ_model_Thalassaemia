# Deterministic sensitivity and threshold analyses

library(dplyr)
library(ggplot2)
library(ggpubr)
library(here)
library(rdecision)
library(scales)

source(here("R", "economic_functions.R"))
source(here("R", "model_parameters.R"))
source(here("R", "model_builders.R"))
source(here("R", "analysis_functions.R"))
source(here("R", "plot_functions.R"))

models <- build_all_strategy_trees(
  mode = "dsa",
  data_dir = here("data", "model_inputs")
)
wtp <- calculate_wtp_thresholds()

plot_specs <- list(
  list(
    title = "Strategy 1: Post-conception screening",
    xmin = 100000, xmax = 500000, exclude = "Abortion cost"
  ),
  list(
    title = "Strategy 2: Pre-conception screening\nwith targeted DNA analysis",
    xmin = 50000, xmax = 500000, exclude = character()
  ),
  list(
    title = "Strategy 3: Pre-conception screening\nwith universal DNA analysis",
    xmin = 500000, xmax = 3000000, exclude = character()
  ),
  list(
    title = "Strategy 4: Combined pre- and\npost-conception screening",
    xmin = 50000, xmax = 350000, exclude = "Abortion cost"
  )
)

tornado_plots <- Map(function(model, spec) {
  plot_tornado_labeled(
    dt = model$tree,
    index = model$index,
    ref = model$ref,
    outcome = "ICER",
    Label = spec$title,
    xmin = spec$xmin,
    xmax = spec$xmax,
    exclude_parameters = spec$exclude
  )
}, models, plot_specs)

combined_tornado <- ggarrange(
  plotlist = tornado_plots,
  ncol = 2,
  nrow = 2,
  align = "hv",
  common.legend = TRUE,
  legend = "bottom"
)
if (interactive()) print(combined_tornado)

figure_dir <- here("outputs", "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(
  file.path(figure_dir, "combined_tornado_plots.pdf"),
  combined_tornado,
  width = 18,
  height = 12,
  bg = "white"
)
ggsave(
  file.path(figure_dir, "combined_tornado_plots.png"),
  combined_tornado,
  width = 14,
  height = 11,
  dpi = 350,
  bg = "white"
)

dsa_bounds <- do.call(rbind, lapply(models, extract_dsa_bounds))
dsa_bounds <- dsa_bounds[order(dsa_bounds$strategy, dsa_bounds$parameter), ]
rownames(dsa_bounds) <- NULL

threshold_specs <- data.frame(
  strategy_id = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 4),
  parameter = c(
    "CBC & Hb typing cost per person",
    "Probability of couple agreeing to PND",
    "Probability of couple agreeing to abortion",
    "Probability of early presentation",
    "CBC & Hb typing cost per couple",
    "DNA analysis cost",
    "Probability of couple reconsidering decision to conceive",
    "DNA analysis cost",
    "Probability of couple reconsidering decision to conceive",
    "Probability of couple reconsidering decision to conceive",
    "DNA analysis cost",
    "CBC & Hb typing cost per couple"
  ),
  lower = c(50, 0.10, 0.10, 0.10, 50, 10, 0.10, 500, 0.10, 0, 50, 50),
  upper = c(1000, 0.90, 0.90, 0.90, 5000, 100000, 0.99, 10000, 0.99, 1, 100000, 5000),
  stringsAsFactors = FALSE
)

threshold_specs$threshold <- vapply(seq_len(nrow(threshold_specs)), function(i) {
  result <- safe_threshold(
    model = models[[threshold_specs$strategy_id[[i]]]],
    parameter = threshold_specs$parameter[[i]],
    lower = threshold_specs$lower[[i]],
    upper = threshold_specs$upper[[i]],
    lambda = wtp$base,
    tol = if (threshold_specs$strategy_id[[i]] == 4L && i == 10L) 1e-8 else 0.01
  )
  if (length(result) == 0L || !is.finite(result[[1L]])) NA_real_ else result[[1L]]
}, numeric(1L))
threshold_specs$strategy <- strategy_metadata$strategy[threshold_specs$strategy_id]
threshold_specs$wtp_base_thb <- wtp$base

table_dir <- here("outputs", "tables")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(dsa_bounds, file.path(table_dir, "DSA_parameter_bounds.csv"), row.names = FALSE)
write.csv(
  threshold_specs[c("strategy", "parameter", "lower", "upper", "wtp_base_thb", "threshold")],
  file.path(table_dir, "DSA_thresholds.csv"),
  row.names = FALSE
)

print(dsa_bounds, row.names = FALSE)
print(threshold_specs, row.names = FALSE)
