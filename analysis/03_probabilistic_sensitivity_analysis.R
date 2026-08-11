# Probabilistic sensitivity analysis

library(dplyr)
library(ggplot2)
library(here)
library(rdecision)
library(scales)

source(here("R", "economic_functions.R"))
source(here("R", "model_parameters.R"))
source(here("R", "model_builders.R"))
source(here("R", "analysis_functions.R"))

set.seed(getOption("thalassaemia.psa_seed", 20260804L))
simulations <- as.integer(getOption("thalassaemia.psa_iterations", 1000L))

models <- build_all_strategy_trees(
  mode = "psa",
  data_dir = here("data", "model_inputs")
)
baseline_results <- do.call(rbind, lapply(models, evaluate_incremental_results))
psa_results <- do.call(rbind, lapply(models, run_psa_model, simulations = simulations))
rownames(psa_results) <- NULL

wtp <- calculate_wtp_thresholds()
strategy_labels <- c(
  "Strategy 1: Post-conception screening" = "Strategy 1: Post-conception",
  "Strategy 2: Pre-conception screening, targeted DNA" =
    "Strategy 2: Pre-conception, targeted",
  "Strategy 3: Pre-conception screening, universal DNA" =
    "Strategy 3: Pre-conception, universal",
  "Strategy 4: Combined screening" = "Strategy 4: Combined screening"
)
strategy_colors <- c("#EA5B6F", "#F79A19", "#3338A0", "#9112BC")
names(strategy_colors) <- names(strategy_labels)

medians <- psa_results |>
  group_by(strategy) |>
  summarise(
    median_cost = median(incremental_cost_thb),
    median_effect = median(births_averted_proportion),
    .groups = "drop"
  )

cost_effectiveness <- psa_results |>
  group_by(strategy) |>
  summarise(
    probability = mean(incremental_cost_thb < wtp$base * births_averted_proportion),
    probability_low = mean(incremental_cost_thb < wtp$low * births_averted_proportion),
    probability_high = mean(incremental_cost_thb < wtp$high * births_averted_proportion),
    .groups = "drop"
  ) |>
  mutate(
    label = paste0(
      "Probability cost-effective: ", round(probability * 100),
      "% [Range: ", round(probability_low * 100), "% to ",
      round(probability_high * 100), "%]"
    ),
    x = -Inf,
    y = Inf
  )

psa_plot <- ggplot(
  psa_results,
  aes(x = births_averted_proportion, y = incremental_cost_thb, color = strategy)
) +
  geom_point(size = 1.5, alpha = 0.15) +
  stat_ellipse(type = "norm", level = 0.8, linewidth = 0.7) +
  geom_point(
    data = medians,
    aes(x = median_effect, y = median_cost, fill = strategy),
    size = 4,
    shape = 21,
    color = "white",
    stroke = 1.2
  ) +
  geom_abline(intercept = 0, slope = wtp$base, color = "gray30", linewidth = 1.2) +
  geom_abline(
    intercept = 0,
    slope = wtp$low,
    color = "gray30",
    linetype = "21",
    linewidth = 1,
    alpha = 0.5
  ) +
  geom_abline(
    intercept = 0,
    slope = wtp$high,
    color = "gray30",
    linetype = "21",
    linewidth = 1,
    alpha = 0.5
  ) +
  geom_text(
    data = cost_effectiveness,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 4.25,
    hjust = -0.025,
    vjust = 1.8
  ) +
  scale_color_manual(values = strategy_colors, labels = strategy_labels, name = NULL) +
  scale_fill_manual(values = strategy_colors, labels = strategy_labels, name = NULL) +
  scale_x_continuous(
    labels = function(x) comma(x * 1000),
    expand = c(0.002, 0),
    limits = c(-0.0005, 0.008)
  ) +
  scale_y_continuous(labels = label_comma(), expand = c(0, 0), limits = c(0, 12000)) +
  facet_wrap(~strategy, ncol = 2, labeller = as_labeller(strategy_labels)) +
  labs(
    x = "Severe thalassaemia births averted per 1,000 screened",
    y = "Incremental cost (THB)"
  ) +
  theme_bw(base_size = 18) +
  theme(legend.position = "none")

if (interactive()) print(psa_plot)
figure_dir <- here("outputs", "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(
  file.path(figure_dir, "PSA_scatter_plot.png"),
  psa_plot,
  width = 10,
  height = 8,
  dpi = 350
)
