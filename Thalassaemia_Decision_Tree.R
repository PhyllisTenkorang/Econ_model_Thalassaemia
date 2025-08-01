## Post-conception screening

# Load necessary libraries
library(readr)        # For reading CSV files
library(purrr)        # For functional programming tools (e.g., pmap)
library(rdecision)    # For decision tree classes and methods
library(stringr)      # For string manipulation
library(tidyverse)

# Set up data directory and read CSVs
# Define the directory containing the data files
# Read nodes, edges, and costs data from CSV files

data_dir <- "Data/"                                       # Directory containing the data files
nodes <- read_csv(file.path(data_dir, "nodes1.csv"))      # Node definitions
edges <- read_csv(file.path(data_dir, "edges1.csv"))      # Edge definitions
costs <- read_csv(file.path(data_dir, "costs1.csv"))      # Cost definitions

nodes <- nodes %>% 
  mutate(label = if_else(is.na(label), "", label))

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

edges <- edges %>% 
  mutate(action = if_else(is.na(action), "", action))

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
    Edge_list[[i]] <- Reaction$new(from_node, to_node, cost = cost_action, label = action_label, p = probability_action)
  }
}

# Build the decision tree using the node and edge objects
# V: list of nodes, E: list of edges
decision_tree <- DecisionTree$new(
  V = node_objs,
  E = Edge_list
)

# Draw the decision tree
decision_tree$draw(border = TRUE, fontsize = 11)

# Evaluate
es <- decision_tree$evaluate(by = "strategy")
ep <- decision_tree$evaluate(by = "path")

# ICER
inc_cost <- es$Cost[2] - es$Cost[1]
T_averted <- ep$Probability[6] - (ep$Probability[24] + ep$Probability[25] + ep$Probability[26])
ICER <- inc_cost / T_averted



## Pre-conception screening (Option 1)

# Set up data directory and read CSVs
# Define the directory containing the data files
# Read nodes, edges, and costs data from CSV files

data_dir <- "Data/"                                       # Directory containing the data files
nodes <- read_csv(file.path(data_dir, "nodes2.csv"))      # Node definitions
edges <- read_csv(file.path(data_dir, "edges2.csv"))      # Edge definitions
costs <- read_csv(file.path(data_dir, "costs1.csv"))      # Cost definitions

nodes <- nodes %>% 
  mutate(label = if_else(is.na(label), "", label))

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

edges <- edges %>% 
  mutate(action = if_else(is.na(action), "", action))

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
    Edge_list[[i]] <- Reaction$new(from_node, to_node, cost = cost_action, label = action_label, p = probability_action)
  }
}

# Build the decision tree using the node and edge objects
# V: list of nodes, E: list of edges
decision_tree2 <- DecisionTree$new(
  V = node_objs,
  E = Edge_list
)

# Draw the decision tree
decision_tree2$draw(border = TRUE, fontsize = 11.5)

# Evaluate
es2 <- decision_tree2$evaluate(by = "strategy")
ep2 <- decision_tree2$evaluate(by = "path")

# ICER2
inc_cost2 <- es2$Cost[2] - es2$Cost[1]
T_averted2 <- ep2$Probability[6] - ep2$Probability[13]
ICER2 <- inc_cost2 / T_averted2



## Pre-conception screening (Option 2)

# Set up data directory and read CSVs
# Define the directory containing the data files
# Read nodes, edges, and costs data from CSV files

data_dir <- "Data/"                                       # Directory containing the data files
nodes <- read_csv(file.path(data_dir, "nodes3.csv"))      # Node definitions
edges <- read_csv(file.path(data_dir, "edges3.csv"))      # Edge definitions
costs <- read_csv(file.path(data_dir, "costs1.csv"))      # Cost definitions

nodes <- nodes %>% 
  mutate(label = if_else(is.na(label), "", label))

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

edges <- edges %>% 
  mutate(action = if_else(is.na(action), "", action))

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
    Edge_list[[i]] <- Reaction$new(from_node, to_node, cost = cost_action, label = action_label, p = probability_action)
  }
}

# Build the decision tree using the node and edge objects
# V: list of nodes, E: list of edges
decision_tree3 <- DecisionTree$new(
  V = node_objs,
  E = Edge_list
)

# Draw the decision tree
decision_tree3$draw(border = TRUE, fontsize = 12)

# Evaluate
es3 <- decision_tree3$evaluate(by = "strategy")
ep3 <- decision_tree3$evaluate(by = "path")

# ICER3
inc_cost3 <- es3$Cost[2] - es3$Cost[1]
T_averted3 <- ep3$Probability[6] - ep3$Probability[13]
ICER3 <- inc_cost3 / T_averted3



## Combination of Pre-conception and Post-conception screening

# Set up data directory and read CSVs
# Define the directory containing the data files
# Read nodes, edges, and costs data from CSV files

data_dir <- "Data/"                                       # Directory containing the data files
nodes <- read_csv(file.path(data_dir, "nodes4.csv"))      # Node definitions
edges <- read_csv(file.path(data_dir, "edges4.csv"))      # Edge definitions
costs <- read_csv(file.path(data_dir, "costs1.csv"))      # Cost definitions

nodes <- nodes %>% 
  mutate(label = if_else(is.na(label), "", label))

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

edges <- edges %>% 
  mutate(action = if_else(is.na(action), "", action))

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
    Edge_list[[i]] <- Reaction$new(from_node, to_node, cost = cost_action, label = action_label, p = probability_action)
  }
}

# Build the decision tree using the node and edge objects
# V: list of nodes, E: list of edges
decision_tree4 <- DecisionTree$new(
  V = node_objs,
  E = Edge_list
)

# Draw the decision tree
decision_tree4$draw(border = TRUE, fontsize = 11)

# Evaluate
es4 <- decision_tree4$evaluate(by = "strategy")
ep4 <- decision_tree4$evaluate(by = "path")

# ICER4
inc_cost4 <- es4$Cost[2] - es4$Cost[1]
T_averted4 <- ep4$Probability[6] - (ep4$Probability[16] + ep4$Probability[17])
ICER4 <- inc_cost4 / T_averted4



## Presentation on CEA plane
All_res<-matrix(NA, ncol=3, nrow=4)
All_res[1,]<-c(inc_cost, T_averted, ICER)
All_res[2,]<-c(inc_cost2, T_averted2, ICER2)
All_res[3,]<-c(inc_cost3, T_averted3, ICER3)
All_res[4,]<-c(inc_cost4, T_averted4, ICER4)
colnames(All_res)<-c("Incremental Costs", "Proportion of severe Thalassaemia births averted", "ICER")
rownames(All_res)<-c("Post-conception screening with/without abortion", 
                     "Pre-conception screening (Option 1)", 
                     "Pre-conception screening (Option 2)",
                     "Combination of pre and post-conception screening")
All_res 


## Plot to see CEA results on ICER plane

plot(All_res[,2], All_res[,1], 
     xlim=c(0, 0.005), ylim=c(0, 8000),
     ylab="Incremental costs (THB)", 
     xlab="Proportion of severe Thalassaemia births averted", 
     pch=20,
     col=c("black", "blue", "green", "violet"), 
     main="CEA of thalassaemia screening strategies versus no screening in Thailand", 
     cex.main=1.5, cex.lab=1.2, cex.axis=1.1)
text(All_res[,2], All_res[,1],
     labels=c("Post-conception","Pre-conception 1", "Pre-conception 2", "Combination"), 
     cex=0.9, pos=3, col="black")
abline(a=0, b=677206, lty=1.5, col='red')  # CET
grid()
legend("topright", 
       legend = c("CET - Lifetime cost of managing severe Thalassaemia"),
       col = c("red"), 
       lty = 1.5)


