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

# ...K-Nearest (K=6)
knn_6 <- knearneigh(centroid_coords, k = 6)
knn6_nb <- knn2nb(knn_6)

# Check if any polygons have no neighbours
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
knn6_pairs_df <- nb_to_pairs(knn6_nb)

## SAVE PAIR LIST FOR SW CALCULATIONS ----

# Save dataframe of pairs to use 
write.csv(knn6_pairs_df, "knn6_pairs.csv", row.names = FALSE)

