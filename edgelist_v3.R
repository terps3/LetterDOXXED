library(data.table)
library(igraph)
library(progress)

# Check if "movie_graph_full.rds" exists, if so open it
if(file.exists("movie_leiden_500users_79M_edges.rds")) {
  g <- readRDS("movie_leiden_500users_79M_edges.rds")
  print("Loaded existing movie graph from 'movie_graph_full.rds'.")
  summary(g)
} else {
  print("No existing graph found, proceeding with new graph creation.")

# Read data
rating_data <- read.csv("ratings_export.csv")
dt <- as.data.table(rating_data)

# First, analyze user rating distributions
user_stats <- dt[, .(n_ratings = .N), by = user_id]
quantile(user_stats$n_ratings, probs = c(0.5, 0.75, 0.9, 0.95, 0.99))
summary(user_stats[n_ratings > 100])

# 2. Select a random sample of active users (>100 ratings)
set.seed(42) # For reproducibility
active_users <- user_stats[n_ratings >= 100]$user_id
n_to_sample <- min(500, length(active_users))
sampled_users <- sample(active_users, n_to_sample)

# Initialize progress bar for user processing
pb <- progress_bar$new(
  format = "  Processing sampled users [:bar] :percent ETA: :eta",
  total = length(sampled_users), clear = FALSE, width= 60
)

edges_data <- dt[user_id %in% sampled_users, {
  pb$tick()
  movies <- unique(movie_id)
  if(length(movies) > 1000) {
    movies <- sample(movies, 1000)
  }
  pairs <- combn(sort(movies), 2)
  list(movie1 = pairs[1,], movie2 = pairs[2,])
}, by = user_id]

cat(sprintf("Total raw edges generated: %d\n", nrow(edges_data)))
all_edges <- edges_data

# Count co-ratings and create weights
cat("\nAggregating edges and calculating weights...\n")
edge_counts <- all_edges[, .N, by = .(movie1, movie2)]
edge_counts[, weight := as.numeric(N)]
cat(sprintf("Number of unique edges: %d\n", nrow(edge_counts)))

# ---- BENCHMARKING FOR ETA ----
sample_size <- min(500000, nrow(edge_counts))
cat(sprintf("\nBenchmarking with %d edges to estimate total time...\n", sample_size))
test_sample <- edge_counts[sample(.N, sample_size)]
bench_time <- system.time({
  g_test <- graph_from_data_frame(test_sample[, .(movie1, movie2, weight)], directed = FALSE)
})["elapsed"]

avg_time_per_edge <- bench_time / sample_size
total_est_seconds <- avg_time_per_edge * nrow(edge_counts)

cat(sprintf("Estimated time for full graph (%d edges): %.1f seconds (~%.1f minutes)\n", 
            nrow(edge_counts), total_est_seconds, total_est_seconds / 60))
# ------------------------------

# Create graph
cat("\nBuilding full graph from data frame...\n")
g <- graph_from_data_frame(edge_counts[, .(movie1, movie2, weight)], 
                           directed = FALSE)
cat("Graph creation complete!\n")

saveRDS(g, "final_movie_graph.rds")
summary(g)

## ---- ADD LEIDEN CLUSTERING HERE ----
library(RColorBrewer)

cat("Performing Leiden clustering...\n")
leiden_result <- cluster_leiden(g, 
                                objective_function = "modularity",
                                resolution_parameter = 1.0,
                                weights = E(g)$weight)

communities <- membership(leiden_result)
V(g)$community <- communities

print(paste("Number of communities:", length(unique(communities))))
mod_score <- modularity(g, communities, weights = E(g)$weight)
print(paste("Modularity:", mod_score))

# Save the graph object - 
saveRDS(g, "movie_graph_full.rds")

# This is what you need for Gephi
write_graph(g, "movie_graph_gephi.graphml", format = "graphml")
}

# Remove isolated vertices
summary(g)
}


## ---- STAGED PLOTTING ----
cat("\n=== STARTING PLOTTING STAGE ===\n")

# 1. FAST PREVIEW (Sub-sampled edges for speed)
preview_edge_limit <- 50000
cat(sprintf("Phase 1: Generating Fast Preview (sampling %d edges)...\n", preview_edge_limit))

# Create a temporary graph with only a subset of edges for layout/preview
if (ecount(g) > preview_edge_limit) {
    g_preview <- subgraph.edges(g, sample(E(g), preview_edge_limit))
} else {
    g_preview <- g
}

# --- BENCHMARK VISUALIZATION ---
cat("Benchmarking visualization performance on sample...\n")
bench_v_time <- system.time({
    png("temp_bench.png", width=500, height=500)
    temp_coords <- layout_with_drl(g_preview, weights = E(g_preview)$weight)
    plot(g_preview, layout = temp_coords, vertex.size=1, vertex.label=NA, edge.width=0.1)
    dev.off()
    unlink("temp_bench.png")
})["elapsed"]

# Estimate time for full graph (Layout + Render)
# Scaling factor: total_edges / sampled_edges
# Plus an additional factor for higher resolution complexity (5000x5000 vs 500x500 is ~100x pixels)
scale_factor <- ecount(g) / ecount(g_preview)
est_total_vis_seconds <- bench_v_time * scale_factor

cat(sprintf("\n--- VISUALIZATION ETA ---\n"))
cat(sprintf("Based on benchmark, full High-Res plot will take approximately:\n"))
cat(sprintf("Time: %.1f minutes (~%.2f hours)\n", est_total_vis_seconds / 60, est_total_vis_seconds / 3600))
cat("--------------------------\n\n")

# Now create the actual preview (using FR which is faster for the UI)
png("movie_graph_preview.png", width = 1200, height = 1200, res = 150)
plot(g_preview, 
     layout = layout_with_fr(g_preview, niter = 50),
     vertex.color = colors[V(g_preview)$community],
     vertex.size = 1,
     vertex.frame.color = NA,
     vertex.label = NA,
     edge.width = 0.1,
     edge.color = rgb(0.5, 0.5, 0.5, 0.2),
     main = "Fast Preview (50k sample edges)")
dev.off()
cat("Preview saved to 'movie_graph_preview.png'\n")

# 2. HIGH-QUALITY RENDER (Full Layout Calculation)
cat("\nPhase 2: Calculating full DrL layout for High-Quality render...\n")
cat("Note: This is the slowest part. Coordinates will be saved to disk.\n")

# Calculate coordinates separately
coords <- layout_with_drl(g, weights = E(g)$weight)

# Save coordinates so you don't have to recalculate if the plot fails
saveRDS(coords, "graph_coords_drl.rds")
cat("Layout coordinates saved to 'graph_coords_drl.rds'\n")

cat("Rendering High-Resolution PNG...\n")
png("movie_graph_high_res.png", width = 5000, height = 5000, res = 300)

plot(g, 
     layout = coords,
     vertex.color = colors[V(g)$community],
     vertex.size = 0.6,
     vertex.frame.color = NA,
     vertex.label = NA,
     edge.width = 0.05,           # Even thinner for full graph
     edge.color = rgb(0.5, 0.5, 0.5, 0.02), # More transparent
     main = paste("Movie Communities -", n_comm, "Clusters"))

dev.off()
cat("Final High-Res plot saved to 'movie_graph_high_res.png'\n")

## ---- SAMPLED USER STATISTICS ----
cat("\n=== SAMPLED USER STATISTICS (%d users) ===\n", length(sampled_users))
sampled_user_stats <- user_stats[user_id %in% sampled_users]

cat(sprintf("Min reviews:    %d\n", min(sampled_user_stats$n_ratings)))
cat(sprintf("Max reviews:    %d\n", max(sampled_user_stats$n_ratings)))
cat(sprintf("Mean reviews:   %.1f\n", mean(sampled_user_stats$n_ratings)))
cat(sprintf("Median reviews: %d\n", median(sampled_user_stats$n_ratings)))

# Optional: Print quantile distribution
cat("\nReview Count Distribution:\n")
print(quantile(sampled_user_stats$n_ratings, probs = c(0, 0.25, 0.5, 0.75, 0.99, 1)))
