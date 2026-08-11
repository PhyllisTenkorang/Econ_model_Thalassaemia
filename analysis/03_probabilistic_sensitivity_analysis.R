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

# Publication-style alternative. The original figure above is retained for
# direct comparison.
publication_labels <- c(
  "Strategy 1: Post-conception screening" = "A  Post-conception",
  "Strategy 2: Pre-conception screening, targeted DNA" =
    "B  Targeted pre-conception",
  "Strategy 3: Pre-conception screening, universal DNA" =
    "C  Universal pre-conception",
  "Strategy 4: Combined screening" = "D  Combined screening"
)
publication_colors <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7")
names(publication_colors) <- names(publication_labels)

publication_psa <- psa_results |>
  mutate(births_averted_per_1000 = births_averted_proportion * 1000)
publication_medians <- medians |>
  mutate(median_effect_per_1000 = median_effect * 1000)
publication_base <- baseline_results |>
  mutate(base_effect_per_1000 = births_averted_proportion * 1000)
publication_ce <- cost_effectiveness |>
  mutate(label = paste0("P(cost-effective) = ", round(probability * 100), "%"))

wtp_lines <- data.frame(
  threshold = factor(
    c("Lower WTP", "Primary WTP", "Upper WTP"),
    levels = c("Lower WTP", "Primary WTP", "Upper WTP")
  ),
  slope = c(wtp$low, wtp$base, wtp$high) / 1000,
  intercept = 0
)
wtp_labels <- transform(
  wtp_lines,
  strategy = "Strategy 4: Combined screening",
  x = 6.35,
  y = slope * 6.35,
  label = paste0(
    threshold, ": ", scales::comma(round(slope * 1000)),
    " THB/birth averted"
  )
)

publication_plot <- ggplot(
  publication_psa,
  aes(x = births_averted_per_1000, y = incremental_cost_thb, color = strategy)
) +
  geom_hline(yintercept = 0, color = "grey45", linewidth = 0.35) +
  geom_vline(xintercept = 0, color = "grey45", linewidth = 0.35) +
  geom_abline(
    data = wtp_lines,
    aes(slope = slope, intercept = intercept, linetype = threshold),
    color = "grey30",
    linewidth = 0.65
  ) +
  geom_point(size = 1.15, alpha = 0.16) +
  stat_density_2d(
    contour_var = "ndensity",
    breaks = c(0.15, 0.5),
    linewidth = 0.65,
    show.legend = FALSE
  ) +
  geom_point(
    data = publication_medians,
    aes(x = median_effect_per_1000, y = median_cost, fill = strategy),
    shape = 21,
    size = 3.2,
    color = "white",
    stroke = 0.9,
    show.legend = FALSE
  ) +
  geom_point(
    data = publication_base,
    aes(x = base_effect_per_1000, y = incremental_cost_thb),
    shape = 4,
    size = 4.2,
    stroke = 1,
    show.legend = FALSE
  ) +
  geom_text(
    data = publication_ce,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.08,
    vjust = 1.45,
    size = 3.2,
    color = "grey15"
  ) +
  geom_label(
    data = wtp_labels,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 1,
    vjust = -0.2,
    size = 2.35,
    label.size = 0,
    label.padding = unit(0.08, "lines"),
    color = "grey25",
    fill = scales::alpha("white", 0.78)
  ) +
  facet_wrap(~strategy, ncol = 2, labeller = as_labeller(publication_labels)) +
  scale_color_manual(values = publication_colors, guide = "none") +
  scale_fill_manual(values = publication_colors, guide = "none") +
  scale_linetype_manual(
    values = c("Lower WTP" = "dotted", "Primary WTP" = "solid", "Upper WTP" = "dashed"),
    guide = "none"
  ) +
  scale_x_continuous(breaks = seq(-2, 8, 2), labels = label_number()) +
  scale_y_continuous(
    breaks = seq(0, 12000, 3000),
    labels = label_comma(),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  coord_cartesian(xlim = c(-2.5, 8), ylim = c(0, 12000)) +
  labs(
    x = "Severe thalassaemia births averted per 1,000 screened",
    y = "Incremental cost (THB)",
    caption = paste0(
      "Points represent PSA simulations; contours show kernel-density levels.\n",
      "Circles denote PSA medians; crosses denote deterministic base-case estimates."
    )
  ) +
  theme_classic(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "grey94", color = "grey35", linewidth = 0.4),
    strip.text = element_text(face = "bold", hjust = 0, margin = margin(5, 6, 5, 6)),
    panel.border = element_rect(color = "grey35", fill = NA, linewidth = 0.4),
    panel.spacing = unit(8, "pt"),
    axis.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8.5, hjust = 0, color = "grey25"),
    plot.margin = margin(8, 10, 8, 8)
  )

ggsave(
  file.path(figure_dir, "PSA_scatter_plot_publication.png"),
  publication_plot,
  width = 180,
  height = 145,
  units = "mm",
  dpi = 600,
  bg = "white"
)
ggsave(
  file.path(figure_dir, "PSA_scatter_plot_publication.pdf"),
  publication_plot,
  width = 180,
  height = 145,
  units = "mm",
  bg = "white"
)
