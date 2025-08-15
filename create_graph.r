library(igraph)

  print("No existing graph found, proceeding with new graph creation.")

# Read data
rating_data <- read.csv("~/ACSAI/A2SEM2/Data-Quattrociocchi/Project/ratings_export/ratings_export.csv")
dt <- as.data.table(rating_data)

# First, analyze user rating distributions
user_stats <- dt[, .(n_ratings = .N), by = user_id]
quantile(user_stats$n_ratings, probs = c(0.5, 0.75, 0.9, 0.95, 0.99))
summary(user_stats[n_ratings > 6000])
# Stratified approach: different handling based on user activity
edges_list <- list()

# 1. Process users with reasonable number of ratings (2-100)
# commented out normal_users

#normal_users <- user_stats[n_ratings >= 2 & n_ratings <= 100]$user_id
#edges_normal <- dt[user_id %in% normal_users, {
#  movies <- unique(movie_id)
#  if(length(movies) >= 2) {
#    pairs <- combn(sort(movies), 2)
#    list(movie1 = pairs[1,], movie2 = pairs[2,])
#  }
#}, by = user_id]

# 2. For power users (>100 ratings), sample their connections
power_users <- user_stats[n_ratings > 6000]$user_id
edges_power <- dt[user_id %in% power_users, {
  movies <- unique(movie_id)
  # Random sample to keep computation manageable
  if(length(movies) > 1000) {
    movies <- sample(movies, 1000)
  }
  pairs <- combn(sort(movies), 2)
  list(movie1 = pairs[1,], movie2 = pairs[2,])
}, by = user_id]

# Combine all edges
# all_edges <- rbind(edges_normal, edges_power)
all_edges <- edges_power

# Count co-ratings and create weights
edge_counts <- all_edges[, .N, by = .(movie1, movie2)]
edge_counts[, weight := 1/N]  # Keep N for now, can transform later

# Create graph
g <- graph_from_data_frame(edge_counts[, .(movie1, movie2, weight)], 
                           directed = FALSE)
# saverds
saveRDS(g, "final_movie_graph.rds")
summary(g)