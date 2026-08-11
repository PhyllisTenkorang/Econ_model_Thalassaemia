# get_action_by_label <- function(tree, label) {
#   dns <- tree$decision_nodes(what = "node")
#   for (d in dns) {
#     acts <- tree$actions(d)
#     labs <- vapply(acts, function(a) a$label(), FUN.VALUE = character(1))
#     i <- which(labs == label)
#     if (length(i)) return(acts[[i[1]]])
#   }
#   stop(sprintf("No action with label '%s' found.", label))
# }


format_val <- function(x) {
  if (is.numeric(x)) {
    ifelse(x <= 1, scales::number(x, accuracy = 0.001),
                  scales::number(x, accuracy = 1, big.mark = ","))
  } else {
    as.character(x)
  }
}

plot_tornado_labeled <- function(
  dt,
  index,
  ref,
  outcome = "ICER",
  Label,
  xmax,
  xmin,
  exclude_parameters = character()
) {
  dsa <- dt$tornado(index = index, ref = ref, outcome = outcome, draw = FALSE)

  es <- dt$evaluate(by = "strategy")
  control_label <- ref$label()
  intervention_label <- index$label()
  baseline_icer <- (es[es[,2] == control_label, "Cost"] - es[es[,2] == intervention_label, "Cost"]) /
                   (es[es[,2] == control_label, "Utility"] - es[es[,2] == intervention_label, "Utility"])

  plot_data <- dsa |>
    filter(!Description %in% c(
      "Probability of man having trait",
      "Probability of woman having trait",
      exclude_parameters
    )) |>
    mutate(
      low = pmin(outcome.min, outcome.max),
      high = pmax(outcome.min, outcome.max),
      range = high - low,
      lower_fmt = format_val(LL),
      upper_fmt = format_val(UL),
      lower_hjust = ifelse(outcome.min <= outcome.max, 1.05, -0.05),
      upper_hjust = ifelse(outcome.max <= outcome.min, 1.05, -0.05)
    ) |>
    arrange(range) |>
    mutate(
      Description = factor(Description, levels = Description)
    )

  label_data <- bind_rows(
    plot_data |>
      transmute(
        Description,
        x = outcome.min,
        label = lower_fmt,
        hjust = lower_hjust,
        bound = "Lower bound"
      ),
    plot_data |>
      transmute(
        Description,
        x = outcome.max,
        label = upper_fmt,
        hjust = upper_hjust,
        bound = "Upper bound"
      )
  )

  icer_text <- paste0("Estimated ICER:\n",
                      scales::number(baseline_icer, accuracy = 1, big.mark = ","),
                      " THB")
  span <- xmax - xmin

  ggplot(plot_data, aes(y = Description)) +
    geom_segment(aes(x = low, xend = high, yend = Description), 
                    linewidth = 8, alpha = 0.8, col = '#2a9d8f') +
    geom_vline(xintercept = baseline_icer, linetype = '31', col = '#C40C0C') +
    geom_text(
      data = label_data,
      aes(x = x, label = label, hjust = hjust, color = bound),
      size = 5,
      show.legend = TRUE,
      key_glyph = "point"
    ) +
    annotate("text",
             x = -Inf,
             y = -Inf,
             label = icer_text,
             hjust = -0.1, vjust = -0.1, size = 5, color = "grey30") +
    scale_x_continuous(expand = expansion(mult = c(0.08, 0.08)), 
                      limits = c(xmin, xmax), labels = label_comma()) +
    scale_color_manual(
      values = c("Lower bound" = "#0072B2", "Upper bound" = "#D55E00"),
      name = "Parameter value"
    ) +
    scale_y_discrete(labels = scales::label_wrap(30)) +
    labs(
      x = "Cost per severe thalassaemia\nbirth averted (THB)",
      y = NULL,
      title = Label
    ) +
    theme_bw(base_size = 16) +
    theme(
      plot.title = element_text(size = 18),
      axis.title = element_text(size = 16),
      axis.text.y = element_text(size = 15),
      legend.position = "bottom",
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12)
    )
}


# Example call for your first tree:
# plot_tornado_labeled(dt, e44, e45, outcome = "ICER",
#                      Label = "Strategy 1: Post-conception screening",
#                      xmax = 500000, xmin = 200000)
