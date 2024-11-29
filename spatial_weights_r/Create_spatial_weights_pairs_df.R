## SETUP ----

# Install required packages
install.packages(c("sf", "spdep", "dplyr", "tidyr", "readr", "ggplot2"))

# Load libraries
library(sf)
library(spdep)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

# Path to hexagons file
gpk_path <- "uber_hex_res09.gpkg"

# Layers in gpkg
layers <- st_layers(gpk_path)
print(layers)

# Layer name from the st_layers output
layer_name <- "uber_hex_res09"

# Read layer to sf object
sf_data <- st_read(gpk_path, layer = layer_name)

# Inspect
print(head(sf_data))


## CREATE SPATIAL WEIGHTS ----

# Compute centroids for Knn
centroids <- st_centroid(sf_data)

# Extract coordinates of the centroids for Knn
centroid_coords <- st_coordinates(centroids)

# Create Rook contiguity neighbours
rook_nb <- poly2nb(sf_data, queen = FALSE)

# ...Queen
queen_nb <- poly2nb(sf_data, queen = TRUE)

# ...K-Nearest (K=6)
knn_6 <- knearneigh(centroid_coords, k = 6)
knn6_nb <- knn2nb(knn_6)

# Check if any polygons have no neighbours
any(sapply(rook_nb, length) == 0)
any(sapply(queen_nb, length) == 0)
any(sapply(knn6_nb, length) == 0)

## CREATE SOURCE NEIGH LISTS ----

nb_to_pairs <- function(nb_list) {
  # Create a df of all pairs (source, neighbour)
  pairs <- lapply(seq_along(nb_list), function(i) {
    if (length(nb_list[[i]]) == 0) return(NULL)
    data.frame(source = rep(i, length(nb_list[[i]])),
               neighbor = nb_list[[i]])
  })
  
  # Combine all pairs into a single data frame
  pairs_df <- do.call(rbind, pairs)
  
  # Return the dataframe as is, keeping both (source, neighbor) and (neighbor, source)
  return(pairs_df)
}

# Extract neighbour pairs
rook_pairs_df <- nb_to_pairs(rook_nb)
queen_pairs_df <- nb_to_pairs(queen_nb)
knn6_pairs_df <- nb_to_pairs(knn6_nb)

## VIEW OUTPUTS ----

# Calculate # neighbours for each polygon
rook_neighbours <- card(rook_nb)  
queen_neighbours <- card(queen_nb) 
knn6_neighbours <- card(knn6_nb)  

# Summarise # neighbours
rook_summary <- as.data.frame(table(rook_neighbours))
queen_summary <- as.data.frame(table(queen_neighbours))
knn6_summary <- as.data.frame(table(knn6_neighbours))

# Rename columns for clarity
colnames(rook_summary) <- c("Neighbours #", "Count")
colnames(queen_summary) <- c("Neighbours #", "Count")
colnames(knn6_summary) <- c("Neighbours #", "Count")

# Merge summaries by the number of neighbors
summary_table <- merge(rook_summary, queen_summary, by = "Neighbours #", all = TRUE, suffixes = c("_Rook", "_Queen"))
summary_table <- merge(summary_table, knn6_summary, by = "Neighbours #", all = TRUE)

# Rename columns for clarity
colnames(summary_table) <- c("Neighbours", "Rook_count", "Queen_count", "KNN6_count")

# Replace NA with 0 for missing counts
summary_table[is.na(summary_table)] <- 0

# View the summary table
print(summary_table)

## SAVE PAIR LIST FOR SW CALCULATIONS ----

# Save dataframe of pairs to use 
write.csv(knn6_pairs_df, "knn6_pairs.csv" , row.names = FALSE)


