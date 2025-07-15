install.packages("igraph")
library(igraph)

# Load the saved graph
g <- readRDS("movie_graph_full.rds")

# Check it loaded correctly
print(g)

# Export to Gephi format
write_graph(g, "movie_graph_gephi.graphml", format = "graphml")

print("Graph exported for Gephi! File: movie_graph_gephi.graphml")
