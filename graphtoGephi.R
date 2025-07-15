install.packages("igraph")
library(igraph)

# Load the saved graph
g <- readRDS("movie_graph_full.rds")

# Check it loaded correctly
print(g)

summary(g)

# Check if 'community' exists as a vertex attribute
if("community" %in% vertex_attr_names(g)) {  # CORRECTED LINE
  # Communities are stored in the graph
  communities <- V(g)$community
  print("Communities found in graph attributes!")
  print(paste("Number of communities:", length(unique(communities))))
} else {
  print("Communities not found in graph attributes!")
  # You'll need to recalculate communities
}
summary(components(g))

# Export to Gephi format
# write_graph(g, "movie_graph_gephi.graphml", format = "graphml")

print("Graph exported for Gephi! File: movie_graph_gephi.graphml")
