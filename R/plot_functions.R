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

plot_tornado_labeled <- function(dt, index, ref, outcome = "ICER", Label, xmax, xmin) {
  dsa <- dt$tornado(index = index, ref = ref, outcome = outcome, draw = FALSE)

  es <- dt$evaluate(by = "strategy")
  control_label <- ref$label()
  intervention_label <- index$label()
  baseline_icer <- (es[es[,2] == control_label, "Cost"] - es[es[,2] == intervention_label, "Cost"]) /
                   (es[es[,2] == control_label, "Utility"] - es[es[,2] == intervention_label, "Utility"])

  # Extract parameter bounds (not ICERs)
  param_low  <- dsa$LL
  param_high <- dsa$UL

  plot_data <- dsa |>
    mutate(
      low = pmin(outcome.min, outcome.max),
      high = pmax(outcome.min, outcome.max),
      range = high - low
    ) |>
    arrange(range) |>
    mutate(
      Description = factor(Description, levels = Description),
      a_fmt = rev(format_val(.env$param_low)),
      b_fmt = rev(format_val(.env$param_high))
    ) |>
    filter(!Description  %in% c("Probability of man having trait",
                                "Probability of woman having trait"
  ))

  icer_text <- paste0("Estimated ICER:\n",
                      scales::number(baseline_icer, accuracy = 1, big.mark = ","),
                      " THB")
  span <- xmax - xmin

  ggplot(plot_data, aes(y = Description)) +
    geom_segment(aes(x = low, xend = high, yend = Description), 
                    linewidth = 8, alpha = 0.8, col = '#2a9d8f') +
    geom_vline(xintercept = baseline_icer, linetype = '31', col = '#C40C0C') +
    geom_text(aes(x = low,  label = a_fmt), hjust = 1.05, size = 5) +
    geom_text(aes(x = high, label = b_fmt), hjust = -0.05, size = 5) +
    annotate("text",
             x = -Inf,
             y = -Inf,
             label = icer_text,
             hjust = -0.1, vjust = -0.1, size = 5, color = "grey30") +
    scale_x_continuous(expand = expansion(mult = c(0.08, 0.08)), 
                      limits = c(xmin, xmax), labels = label_comma()) +
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
      axis.text.y = element_text(size = 15)
    )
}


# Example call for your first tree:
# plot_tornado_labeled(dt, e44, e45, outcome = "ICER",
#                      Label = "Strategy 1: Post-conception screening",
#                      xmax = 500000, xmin = 200000)
