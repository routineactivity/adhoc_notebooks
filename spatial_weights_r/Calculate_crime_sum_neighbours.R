# Load libraries
library(sf)
library(dplyr)

# Read csv of source-neighbour relationships
knn6_pairs <- read.csv("knn6_pairs.csv")

# Read the hexagons with crime geopackage
polygons <- st_read("pseudo_hex_data.gpkg")

# Calculate the sum of pseudo_crime for neighbouring polygons
# Step 1: Join neighbour data to polygon data to get neighbour pseudo_crime values
neighbour_crime <- knn6_pairs %>%
  left_join(polygons, by = c("neighbor" = "id")) %>% # Match neighbor to polygon id
  group_by(source) %>% # Group by source
  summarise(neighbour_crime_sum = sum(pseudo_crime, na.rm = TRUE)) # Sum crimes for neighbors

# Step 2: Join the crime sum back to the polygon data
polygons <- polygons %>%
  left_join(neighbour_crime, by = c("id" = "source"))

# Step 3: If necessary, replace NA with 0 for polygons without neighbors
polygons$neighbour_crime_sum[is.na(polygons$neighbour_crime_sum)] <- 0

# Save the result to a new GeoPackage or inspect it
st_write(polygons, "polygons_with_neigh_crimecounts.gpkg", delete_layer = TRUE)

# Inspect the result
head(polygons)
