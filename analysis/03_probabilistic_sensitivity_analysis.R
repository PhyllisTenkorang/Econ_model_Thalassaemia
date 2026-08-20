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

strategy_1_psa <- psa_results[
  psa_results$strategy == "Strategy 1: Post-conception screening",
]
stopifnot(all(strategy_1_psa$births_averted_proportion >= -1e-12))

management_cost_thresholds <- calculate_management_cost_thresholds()
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
    probability = mean(
      incremental_cost_thb <
        management_cost_thresholds$average * births_averted_proportion
    ),
    probability_lower_level = mean(
      incremental_cost_thb <
        management_cost_thresholds$lower_level * births_averted_proportion
    ),
    probability_higher_level = mean(
      incremental_cost_thb <
        management_cost_thresholds$higher_level * births_averted_proportion
    ),
    .groups = "drop"
  ) |>
  mutate(
    label = paste0(
      "Probability cost-saving: ", round(probability * 100),
      "% [Hospital-care scenarios: ",
      round(probability_lower_level * 100), "% to ",
      round(probability_higher_level * 100), "%]"
    ),
    x = -Inf,
    y = Inf
  )

figure_dir <- here("outputs", "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
publication_labels <- c(
  "Strategy 1: Post-conception screening" = "Strategy 1",
  "Strategy 2: Pre-conception screening, targeted DNA" =
    "Strategy 2",
  "Strategy 3: Pre-conception screening, universal DNA" =
    "Strategy 3",
  "Strategy 4: Combined screening" = "Strategy 4"
)
publication_colors <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7")
names(publication_colors) <- names(publication_labels)

publication_psa <- psa_results |>
  mutate(births_averted_per_1000 = births_averted_proportion * 1000)
publication_medians <- medians |>
  mutate(median_effect_per_1000 = median_effect * 1000)
publication_ce <- cost_effectiveness |>
  mutate(label = paste0(
    "Probability cost-saving: ", round(probability * 100),
    "%  (hospital-care scenarios: ",
    round(probability_lower_level * 100),
    "% to ", round(probability_higher_level * 100), "%)"
  ))

threshold_descriptions <- c(
  "Lower-level hospital care:",
  "Average lifetime cost of managing severe thalassaemia:",
  "Higher-level hospital care:"
)
threshold_labels <- paste(
  threshold_descriptions,
  scales::label_number(accuracy = 0.01, big.mark = ",")(
    c(
      management_cost_thresholds$lower_level,
      management_cost_thresholds$average,
      management_cost_thresholds$higher_level
    )
  )
)

management_cost_lines <- data.frame(
  threshold = factor(
    threshold_labels,
    levels = threshold_labels
  ),
  slope = c(
    management_cost_thresholds$lower_level,
    management_cost_thresholds$average,
    management_cost_thresholds$higher_level
  ) / 1000,
  intercept = 0
)

publication_plot <- ggplot(
  publication_psa,
  aes(x = births_averted_per_1000, y = incremental_cost_thb, color = strategy)
) +
  geom_abline(
    data = management_cost_lines,
    aes(slope = slope, intercept = intercept, linetype = threshold),
    color = "grey30",
    linewidth = 0.65
  ) +
  geom_point(size = 1.15, alpha = 0.16) +
  stat_ellipse(
    type = "norm",
    level = 0.95,
    segments = 401,
    linewidth = 0.8,
    show.legend = FALSE
  ) +
  geom_point(
    data = publication_medians,
    aes(x = median_effect_per_1000, y = median_cost, fill = strategy),
    shape = 21,
    size = 2.35,
    color = "white",
    stroke = 0.9,
    show.legend = FALSE
  ) +
  geom_text(
    data = publication_ce,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.06,
    vjust = 1.45,
    size = 2.55,
    color = "grey15"
  ) +
  facet_wrap(~strategy, ncol = 2, labeller = as_labeller(publication_labels)) +
  scale_color_manual(values = publication_colors, guide = "none") +
  scale_fill_manual(values = publication_colors, guide = "none") +
  scale_linetype_manual(
    values = setNames(
      c("dotted", "solid", "dashed"),
      levels(management_cost_lines$threshold)
    ),
    name = "Lifetime management-cost thresholds (THB):",
    guide = guide_legend(
      nrow = 3,
      byrow = TRUE,
      title.position = "top",
      title.hjust = 0
    )
  ) +
  scale_x_continuous(
    breaks = seq(0, 8, 2),
    labels = label_number(),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_y_continuous(
    breaks = seq(0, 12000, 3000),
    labels = label_comma(),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  coord_cartesian(xlim = c(0, 8), ylim = c(0, 12000)) +
  labs(
    x = "Severe thalassaemia births averted per 1,000 couples screened",
    y = "Incremental costs (THB)"
  ) +
  theme_classic(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "grey94", color = "grey35", linewidth = 0.4),
    strip.text = element_text(face = "bold", hjust = 0, margin = margin(5, 6, 5, 6)),
    panel.border = element_rect(color = "grey35", fill = NA, linewidth = 0.4),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(8, "pt"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.justification = "left",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8.5),
    legend.key.width = unit(22, "pt"),
    legend.margin = margin(t = -2, b = 0),
    plot.margin = margin(8, 10, 8, 8)
  )

ggsave(
  file.path(figure_dir, "PSA_scatter_plot_publication.png"),
  publication_plot,
  width = 180,
  height = 155,
  units = "mm",
  dpi = 600,
  bg = "white"
)
ggsave(
  file.path(figure_dir, "PSA_scatter_plot_publication.pdf"),
  publication_plot,
  width = 180,
  height = 155,
  units = "mm",
  bg = "white"
)
