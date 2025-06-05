# flexible_tree_2.R
# This script builds a decision tree using data from CSV files and the rdecision package.
# It reads nodes, edges, and costs from CSVs, constructs the tree, and draws it.

# -----------------------------
# Load required libraries
# -----------------------------
library(readr)        # For reading CSV files
library(purrr)        # For functional programming tools (e.g., pmap)
library(rdecision)    # For decision tree classes and methods
library(stringr)      # For string manipulation
library(tidyverse)    # For data manipulation and piping


# -----------------------------
# Set up data directory and read CSVs
# -----------------------------
# Define the directory containing the data files
# Read nodes, edges, and costs data from CSV files

data_dir <- "Data/"   # Directory containing the data files
nodes <- read_csv(file.path(data_dir, "nodes.csv"))      # Node definitions
edges <- read_csv(file.path(data_dir, "edges.csv"))      # Edge definitions
costs <- read_csv(file.path(data_dir, "costs.csv"))      # Cost definitions

# Ensure all node labels are non-NA
nodes <- nodes %>% 
  mutate(label = if_else(is.na(label), "", label))

# -----------------------------
# Create node objects for each row in the nodes data frame
# -----------------------------
# Each node is created as a DecisionNode, ChanceNode, or LeafNode based on its type
node_objs <- pmap(
  nodes,
  function(node_id, type, label, ...) {
    if (type == "decision") {
      DecisionNode$new(label)   # Create a DecisionNode
    } else if (type == "chance") {
      ChanceNode$new(label)     # Create a ChanceNode
    } else if (type == "leaf") {
      LeafNode$new(label)       # Create a LeafNode (terminal node)
    }
  }
)
names(node_objs) <- as.character(nodes$node_id)  # Name the list by node_id for easy lookup

# -----------------------------
# Create edge objects (Action or Reaction) for each row in the edges data frame
# -----------------------------
Edge_list <- list()
for (i in 1:nrow(edges)) {
  edge <- edges[i, ]
  from_node <- node_objs[[as.character(edge[["from"]])]]   # Source node object
  to_node <- node_objs[[as.character(edge[["to"]])]]       # Target node object
  
  # Parse cost IDs (can be multiple, separated by ";"), sum their costs
  action_cost_ids <-  edge$cost_id %>% str_split(";") %>% unlist() %>% as.numeric()
  cost_action <- ifelse(all(is.na(action_cost_ids)), 0,   sum(costs$cost[action_cost_ids])) # Total cost for this action
  
  # If the edge starts from a DecisionNode, create an Action; if from a ChanceNode, create a Reaction
  if(grepl("Decision", class(from_node)[1])){
    action_label <- edge$action                                # Edge label
    Edge_list[[i]] <- Action$new(from_node, to_node, cost = cost_action, label = action_label)
  }
  if(grepl("Chance", class(from_node)[1])){
    action_label <- edge$reaction                                # Edge label
    # Handle probability assignment for Reaction edges
    if(grepl("Const", edge$p_distribution)){
      probability_action <- ConstModVar$new(description = edge$p_description, units = "", const = edge$p_params1)
    } else {
      writeLines("Your codes need to be modified to include more distribution types!!!")
      stop()
    }
    
    Edge_list[[i]] <- Reaction$new(from_node, to_node, cost = 0, label = action_label, p = probability_action)
  }
  
}

# -----------------------------
# Build the decision tree using the node and edge objects
# -----------------------------
# V: list of nodes, E: list of edges
decision_tree <- DecisionTree$new(
  V = node_objs,
  E = Edge_list
)

# -----------------------------
# Draw and evaluate the decision tree
# -----------------------------
decision_tree$draw(border = TRUE, fontsize = 10)   # Draw the tree
decision_tree$modvar_table()                       # Show table of model variables
decision_tree$evaluate()                           # Evaluate the tree
