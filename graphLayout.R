library(igraph)

# Load the graph
g <- readRDS("c:/Users/ferr/Documents/ACSAI/A2SEM2/Data-Quattrociocchi/Project/movie_graph_full.rds")
summary(g)
print(g)

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
## ---- PLOT ----

# Create layout using DrL algorithm
layout_drl <- layout_with_drl(g,
                              weights = E(g)$weight,
                              options = drl_defaults$default)

# save layout for later use
saveRDS(layout_drl, "saved_drl_layout.rds")

# Basic plot with DrL layout
plot(g, 
     layout = layout_drl,
     vertex.color = communities,
     vertex.size = sqrt(degree(g)) * 0.5,  # Adjust size multiplier as needed
     vertex.label = NA,
     edge.width = 0.1,  # Thin edges for large graphs
     edge.color = rgb(0.5, 0.5, 0.5, 0.1),  # Semi-transparent gray
     main = "Movie Network - Leiden Communities (DrL Layout)")

# For better visualization of large graphs:

# Enhanced plot with community colors
# n_communities <- length(unique(communities))
# if(n_communities <= 12) {
#   colors <- brewer.pal(min(n_communities, 12), "Set3")
# } else {
#   colors <- rainbow(n_communities)
# }

# # Plot with better aesthetics
# plot(g, 
#      layout = layout_drl,
#      vertex.color = colors[communities],
#      vertex.size = sqrt(degree(g)) * 0.3,
#      vertex.frame.color = NA,  # Remove vertex borders
#      vertex.label = NA,
#      edge.width = E(g)$weight / max(E(g)$weight) * 0.5,  # Weight-based width
#      edge.color = rgb(0.2, 0.2, 0.2, 0.05),  # Very transparent
#      edge.curved = 0,  # Straight edges for performance
#      main = "Movie Communities via Rating Patterns")

# Add legend
legend("topright", 
       legend = paste("Community", 1:min(n_communities, 15)),
       col = colors[1:min(n_communities, 15)],
       pch = 19,
       cex = 0.7,
       bg = "white")

