## Post-conception screening

# Load necessary libraries
library(readr)        # For reading CSV files
library(purrr)        # For functional programming tools (e.g., pmap)
library(rdecision)    # For decision tree classes and methods
library(stringr)      # For string manipulation

# Set up data directory and read CSVs
# Define the directory containing the data files
# Read nodes, edges, and costs data from CSV files

data_dir <- "Data/"                                       # Directory containing the data files
nodes <- read_csv(file.path(data_dir, "nodes1.csv"))      # Node definitions
edges <- read_csv(file.path(data_dir, "edges1.csv"))      # Edge definitions
costs <- read_csv(file.path(data_dir, "costs1.csv"))      # Cost definitions

# Create node objects for each row in the nodes data frame
# Each node is created as a DecisionNode, ChanceNode, or LeafNode based on its type
node_objs <- pmap(
  nodes,
  function(node_id, type, label, ...) {
    if (type == "decision") {
      DecisionNode$new(label)                       # Create a DecisionNode
    } else if (type == "chance") {
      ChanceNode$new(label)                         # Create a ChanceNode
    } else if (type == "leaf") {
      LeafNode$new(label)                           # Create a LeafNode (terminal node)
    }
  }
)
names(node_objs) <- as.character(nodes$node_id)    # Name the list by node_id for easy lookup

# Create edge objects (Action or Reaction) for each row in the edges data frame
Edge_list <- list()
for (i in 1:nrow(edges)) {
  edge <- edges[i, ]
  from_node <- node_objs[[as.character(edge[["from"]])]]     # Source node object
  to_node <- node_objs[[as.character(edge[["to"]])]]         # Target node object
  action_label <- edge$action                                # Edge label
  
  # Parse cost IDs (can be multiple, separated by ";"), sum their costs
  action_cost_ids <-  edge$cost_id %>% str_split(";") %>% unlist() %>% as.numeric()
  cost_action <- ifelse(all(is.na(action_cost_ids)), 0, sum(costs$cost[action_cost_ids]))   # Total cost for this action
  probability_action <- edge$probability                                                    # Probability for chance edges
  
  # If the edge starts from a DecisionNode, create an Action; if from a ChanceNode, create a Reaction
  if(grepl("Decision", class(from_node)[1])){
    Edge_list[[i]] <- Action$new(from_node, to_node, cost = cost_action, label = action_label)
  }
  if(grepl("Chance", class(from_node)[1])){
    Edge_list[[i]] <- Reaction$new(from_node, to_node, cost = 0, label = action_label, p = probability_action)
  }
  
}

# Build the decision tree using the node and edge objects
# V: list of nodes, E: list of edges
decision_tree <- DecisionTree$new(
  V = node_objs,
  E = Edge_list
)

# Draw the decision tree
decision_tree$draw(border = TRUE, fontsize = 10)
