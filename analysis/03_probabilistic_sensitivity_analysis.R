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
    x = "Severe thalassaemia births averted per 1,000 couples screened",
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
    "Probability cost-effective: ", round(probability * 100),
    "%  (range: ", round(probability_low * 100),
    "% to ", round(probability_high * 100), "%)"
  ))

# Return the boundary of the smallest kernel-density region containing 95% of
# the estimated joint distribution of incremental effects and costs.
kde_hdr_contour <- function(
  data,
  probability = 0.95,
  grid_size = 300L,
  bandwidth_adjustment = 1.35
) {
  x <- data$births_averted_per_1000
  y <- data$incremental_cost_thb
  x_padding <- diff(range(x)) * 0.1
  y_padding <- diff(range(y)) * 0.1
  density <- MASS::kde2d(
    x,
    y,
    n = grid_size,
    h = c(
      MASS::bandwidth.nrd(x),
      MASS::bandwidth.nrd(y)
    ) * bandwidth_adjustment,
    lims = c(
      min(x) - x_padding, max(x) + x_padding,
      min(y) - y_padding, max(y) + y_padding
    )
  )
  ordered_density <- sort(as.vector(density$z), decreasing = TRUE)
  cutoff <- ordered_density[
    which(cumsum(ordered_density) / sum(ordered_density) >= probability)[1L]
  ]
  boundaries <- contourLines(density$x, density$y, density$z, levels = cutoff)

  do.call(rbind, lapply(seq_along(boundaries), function(i) {
    data.frame(
      births_averted_per_1000 = boundaries[[i]]$x,
      incremental_cost_thb = boundaries[[i]]$y,
      contour_group = paste(data$strategy[[1L]], i, sep = "_"),
      strategy = data$strategy[[1L]],
      stringsAsFactors = FALSE
    )
  }))
}

publication_contours <- publication_psa |>
  group_split(strategy) |>
  lapply(kde_hdr_contour) |>
  bind_rows()

wtp_lines <- data.frame(
  threshold = factor(
    paste0(
      c("Lower", "Average", "Upper"), ": ",
      scales::comma(round(c(wtp$low, wtp$base, wtp$high)))
    ),
    levels = paste0(
      c("Lower", "Average", "Upper"), ": ",
      scales::comma(round(c(wtp$low, wtp$base, wtp$high)))
    )
  ),
  slope = c(wtp$low, wtp$base, wtp$high) / 1000,
  intercept = 0
)

publication_plot <- ggplot(
  publication_psa,
  aes(x = births_averted_per_1000, y = incremental_cost_thb, color = strategy)
) +
  geom_abline(
    data = wtp_lines,
    aes(slope = slope, intercept = intercept, linetype = threshold),
    color = "grey30",
    linewidth = 0.65
  ) +
  geom_point(size = 1.15, alpha = 0.16) +
  geom_path(
    data = publication_contours,
    aes(group = contour_group),
    linewidth = 0.8,
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
      levels(wtp_lines$threshold)
    ),
    name = "Cost-effectiveness thresholds (THB):",
    guide = guide_legend(
      nrow = 3,
      byrow = TRUE,
      title.position = "top",
      title.hjust = 0
    )
  ) +
  scale_x_continuous(breaks = seq(-2, 8, 2), labels = label_number()) +
  scale_y_continuous(
    breaks = seq(0, 12000, 3000),
    labels = label_comma(),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  coord_cartesian(xlim = c(-2.5, 8), ylim = c(0, 12000)) +
  labs(
    x = "Severe thalassaemia births averted per 1,000 couples screened",
    y = "Incremental cost (THB)",
    caption = paste0(
      "Smaller coloured points represent PSA simulations\n",
      "Each contour encloses the 95% joint uncertainty region\n",
      "Larger white circles denote median incremental costs and outcomes from the PSA"
    )
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
    plot.caption = element_text(size = 8.5, hjust = 0, color = "grey25"),
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
