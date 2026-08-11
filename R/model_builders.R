strategy_metadata <- data.frame(
  strategy_id = 1:4,
  strategy = c(
    "Strategy 1: Post-conception screening",
    "Strategy 2: Pre-conception screening, targeted DNA",
    "Strategy 3: Pre-conception screening, universal DNA",
    "Strategy 4: Combined screening"
  ),
  intervention = c(
    "Test woman",
    "Screen couple with CBC & Hb typing",
    "Screen couple with DNA analysis",
    "Screen couple with CBC & Hb typing"
  ),
  comparator = c(
    "No further testing",
    "No screening",
    "No screening",
    "No screening"
  ),
  stringsAsFactors = FALSE
)

probability_for_edge <- function(edge, parameters) {
  description <- edge[["p_description"]]
  distribution <- edge[["p_distribution"]]

  if (is.na(description) || !nzchar(description)) {
    return(if (is.na(edge[["probability"]])) NA_real_ else edge[["probability"]])
  }

  probability_map <- list(
    "Probability of early presentation" = parameters$probabilities$early_presentation,
    "Probability of woman having trait" = parameters$probabilities$woman_trait,
    "Probability of partner having trait" = parameters$probabilities$partner_trait,
    "Probability of one partner having trait" = parameters$probabilities$one_partner_trait,
    "Probability of both partners having trait" = parameters$probabilities$both_partners_trait,
    "Probability of both partners not having trait" = parameters$probabilities$both_partners_healthy,
    "Probability of couple agreeing to PND" = parameters$probabilities$pnd,
    "Probability of couple agreeing to abortion" = parameters$probabilities$abortion,
    "Probability of couple reconsidering decision to conceive" = parameters$probabilities$reconsideration
  )

  if (description %in% names(probability_map)) return(probability_map[[description]])

  # Constant terminal probabilities are read directly from the model input.
  if (!is.na(distribution) && identical(distribution, "Constant")) {
    return(edge[["probability"]])
  }

  # All other described rows are complementary branches at their chance node.
  NA_real_
}

cost_for_edge <- function(cost_id, parameters) {
  if (is.na(cost_id) || !nzchar(as.character(cost_id))) return(0)
  ids <- strsplit(as.character(cost_id), ";", fixed = TRUE)[[1L]]
  costs <- lapply(ids, function(id) parameters$costs[[id]])
  if (length(costs) == 1L) return(costs[[1L]])
  Reduce(`+`, costs)
}

build_strategy_tree <- function(strategy_id, parameters, data_dir) {
  nodes <- read.csv(
    file.path(data_dir, paste0("nodes", strategy_id, ".csv")),
    fileEncoding = "UTF-8-BOM",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  edges <- read.csv(
    file.path(data_dir, paste0("edges", strategy_id, ".csv")),
    fileEncoding = "UTF-8-BOM",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  nodes$label[is.na(nodes$label)] <- ""
  edges$action[is.na(edges$action)] <- ""

  node_objects <- lapply(seq_len(nrow(nodes)), function(i) {
    row <- nodes[i, ]
    switch(
      row$type,
      decision = rdecision::DecisionNode$new(row$label),
      chance = rdecision::ChanceNode$new(row$label),
      leaf = rdecision::LeafNode$new(
        row$label,
        utility = if (grepl("^T[0-9]+$", row$label)) 0 else 1
      ),
      stop("Unknown node type: ", row$type)
    )
  })
  names(node_objects) <- as.character(nodes$node_id)

  edge_objects <- lapply(seq_len(nrow(edges)), function(i) {
    row <- edges[i, ]
    from_node <- node_objects[[as.character(row[["from"]])]]
    to_node <- node_objects[[as.character(row[["to"]])]]
    edge_cost <- cost_for_edge(row[["cost_id"]], parameters)

    if (inherits(from_node, "DecisionNode")) {
      rdecision::Action$new(
        from_node,
        to_node,
        cost = edge_cost,
        label = row$action
      )
    } else {
      rdecision::Reaction$new(
        from_node,
        to_node,
        cost = edge_cost,
        label = row$action,
        p = probability_for_edge(row, parameters)
      )
    }
  })

  decision_edges <- which(vapply(
    edge_objects,
    inherits,
    logical(1L),
    what = "Action"
  ))
  metadata <- strategy_metadata[strategy_metadata$strategy_id == strategy_id, ]

  list(
    strategy_id = strategy_id,
    strategy = metadata$strategy,
    intervention = metadata$intervention,
    comparator = metadata$comparator,
    tree = rdecision::DecisionTree$new(node_objects, edge_objects),
    index = edge_objects[[decision_edges[[1L]]]],
    ref = edge_objects[[decision_edges[[2L]]]]
  )
}

build_all_strategy_trees <- function(mode = c("base", "dsa", "psa"), data_dir) {
  mode <- match.arg(mode)
  parameters <- create_model_parameters(mode)
  lapply(1:4, build_strategy_tree, parameters = parameters, data_dir = data_dir)
}

evaluate_incremental_results <- function(model) {
  evaluated <- model$tree$evaluate(by = "strategy")
  intervention_row <- evaluated[, 2L] == model$intervention
  comparator_row <- evaluated[, 2L] == model$comparator
  incremental_cost <- evaluated[intervention_row, "Cost"] -
    evaluated[comparator_row, "Cost"]
  births_averted <- evaluated[intervention_row, "Utility"] -
    evaluated[comparator_row, "Utility"]

  data.frame(
    strategy = model$strategy,
    incremental_cost_thb = incremental_cost,
    births_averted_proportion = births_averted,
    icer_thb_per_birth_averted = incremental_cost / births_averted,
    stringsAsFactors = FALSE
  )
}
