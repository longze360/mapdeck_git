# Administrative Sampling Examples
#
# This file demonstrates how to use the spatial_sample_administrative
# function for various administrative boundary sampling scenarios.

library(sf)
library(R6)

# Load necessary sampling functions
# Note: Adjust the path if running from a different directory.
# The '../' assumes this script is run from the 'examples' directory.
source('R/concurrent-processor.R')
source('R/spatial-sampling-engine.R')
source('R/administrative-sampler.R')

# ============================================================================
# Example 1: Basic Proportional Sampling
# ============================================================================

cat("=== Example 1: Proportional Sampling (Default) ===\n")

# Create sample administrative polygons
create_admin_regions <- function() {
  coords_list <- list(
    matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE),
    matrix(c(1, 0, 3, 0, 3, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE),
    matrix(c(0, 1, 0, 3, 1, 3, 1, 1, 0, 1), ncol = 2, byrow = TRUE)
  )
  polygons <- lapply(coords_list, function(x) st_polygon(list(x)))
  admin_data <- data.frame(
    admin_id = c("A", "B", "C"),
    population = c(1000, 2500, 1500)
  )
  admin_sf <- st_sf(admin_data, geometry = st_sfc(polygons), crs = 4326)

  # Add area for clarity
  admin_sf$area_sqkm <- as.numeric(st_area(admin_sf) / 1e6)

  return(admin_sf)
}

admin_sf <- create_admin_regions()

cat("Administrative Regions:\n")
print(admin_sf)

# Perform proportional sampling (proportional to area)
prop_samples <- spatial_sample_administrative(
  admin_sf,
  total_samples = 100,
  seed = 123
)

cat("\nProportional Sampling Results (by area):\n")
prop_counts <- table(prop_samples$admin_id)
print(prop_counts)

total_area <- sum(admin_sf$area_sqkm)
for(id in names(prop_counts)) {
    region_area <- admin_sf$area_sqkm[admin_sf$admin_id == id]
    expected <- round(100 * (region_area / total_area))
    cat(sprintf("  Region %s: %d samples (Area: %.2f, Expected: %d)\n",
        id, prop_counts[id], region_area, expected))
}


# ============================================================================
# Example 2: Equal Allocation Sampling
# ============================================================================

cat("\n=== Example 2: Equal Allocation Sampling ===\n")

equal_samples <- spatial_sample_administrative(
  admin_sf,
  total_samples = 100,
  allocation_method = "equal",
  seed = 123
)

cat("Equal Allocation Results:\n")
equal_counts <- table(equal_samples$admin_id)
print(equal_counts)


# ============================================================================
# Example 3: Custom Allocation (by Population)
# ============================================================================

cat("\n=== Example 3: Custom Allocation (by Population) ===\n")

custom_samples <- spatial_sample_administrative(
  admin_sf,
  total_samples = 100,
  allocation_method = "custom",
  weight_column = "population",
  seed = 123
)

cat("Custom Allocation Results (by population):\n")
custom_counts <- table(custom_samples$admin_id)
print(custom_counts)

total_pop <- sum(admin_sf$population)
for(id in names(custom_counts)) {
    region_pop <- admin_sf$population[admin_sf$admin_id == id]
    expected <- round(100 * (region_pop / total_pop))
    cat(sprintf("  Region %s: %d samples (Population: %d, Expected: %d)\n",
        id, custom_counts[id], region_pop, expected))
}


# ============================================================================
# Example 4: Concurrent Processing
# ============================================================================

cat("\n=== Example 4: Concurrent Processing ===\n")

# Note: Concurrency is most effective with a larger number of polygons.
# This is for demonstration purposes.
concurrent_samples <- spatial_sample_administrative(
  admin_sf,
  total_samples = 500,
  concurrent = TRUE,
  max_workers = 2,
  progress = TRUE,
  seed = 123
)

cat("\nConcurrent Sampling Results:\n")
concurrent_counts <- table(concurrent_samples$admin_id)
print(concurrent_counts)
cat(sprintf("Total samples generated: %d\n", nrow(concurrent_samples)))


cat("\n=== All Examples Complete ===\n")
