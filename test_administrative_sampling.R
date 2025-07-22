#!/usr/bin/env Rscript

# Test script for administrative sampling accuracy
library(R6)
library(sf)

# Source the required files
source('R/spatial-sampling/concurrent-processor.R')
source('R/spatial-sampling-engine.R')
source('R/spatial-sampling/administrative-sampler.R')

# Create test administrative polygons
create_test_admin_polygons <- function() {
  # Create three simple rectangular polygons
  coords_list <- list(
    # Polygon A: 1x1 square
    matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE),
    # Polygon B: 2x1 rectangle (larger area)
    matrix(c(1, 0, 3, 0, 3, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE),
    # Polygon C: 1x1.5 rectangle
    matrix(c(0, 1, 2, 1, 2, 2.5, 0, 2.5, 0, 1), ncol = 2, byrow = TRUE)
  )

  polygons <- lapply(coords_list, function(x) sf::st_polygon(list(x)))

  admin_data <- data.frame(
    admin_id = c("A", "B", "C"),
    area = c(1, 2, 1.5)  # Areas for reference
  )

  admin_sf <- sf::st_sf(admin_data, geometry = sf::st_sfc(polygons), crs = 4326)

  list(
    polygons = admin_sf,
    coords_list = coords_list
  )
}

# Test administrative sampling accuracy
cat("Testing administrative sampling accuracy...\n")
admin_data <- create_test_admin_polygons()

# Test proportional allocation accuracy
cat("\n1. Testing proportional allocation accuracy...\n")
samples <- spatial_sample_administrative(
  admin_data$polygons,
  total_samples = 300,
  allocation_method = "proportional",
  concurrent = TRUE,
  progress = FALSE,
  seed = 123
)

sample_counts <- table(samples$admin_id)
cat("Sample counts by admin unit:\n")
print(sample_counts)

# Calculate expected proportions based on area
total_area <- sum(admin_data$polygons$area)
expected_props <- admin_data$polygons$area / total_area
expected_counts <- round(expected_props * 300)
names(expected_counts) <- admin_data$polygons$admin_id

cat("Expected counts based on area:\n")
print(expected_counts)

# Test equal allocation accuracy
cat("\n2. Testing equal allocation accuracy...\n")
equal_samples <- spatial_sample_administrative(
  admin_data$polygons,
  total_samples = 300,
  allocation_method = "equal",
  concurrent = TRUE,
  progress = FALSE,
  seed = 456
)

equal_counts <- table(equal_samples$admin_id)
cat("Equal allocation sample counts:\n")
print(equal_counts)

# Should be 100 each (300/3)
expected_equal <- rep(100, 3)
names(expected_equal) <- admin_data$polygons$admin_id
cat("Expected equal counts:\n")
print(expected_equal)

# Test custom allocation accuracy
cat("\n3. Testing custom allocation accuracy...\n")
admin_data$polygons$population <- c(1000, 3000, 2000)  # Custom weights
custom_samples <- spatial_sample_administrative(
  admin_data$polygons,
  total_samples = 300,
  allocation_method = "custom",
  weight_column = "population",
  concurrent = TRUE,
  progress = FALSE,
  seed = 789
)

custom_counts <- table(custom_samples$admin_id)
cat("Custom allocation sample counts:\n")
print(custom_counts)

# Calculate expected based on population weights
total_pop <- sum(admin_data$polygons$population)
expected_custom_props <- admin_data$polygons$population / total_pop
expected_custom_counts <- round(expected_custom_props * 300)
names(expected_custom_counts) <- admin_data$polygons$admin_id

cat("Expected counts based on population:\n")
print(expected_custom_counts)

# Test boundary validation
cat("\n4. Testing boundary validation...\n")
validation_samples <- spatial_sample_administrative(
  admin_data$polygons,
  total_samples = 100,
  concurrent = TRUE,
  progress = FALSE,
  seed = 999
)

# Check that all points are within their respective polygons
boundary_violations <- 0
for (i in seq_len(nrow(validation_samples))) {
  point <- validation_samples[i, ]
  admin_id <- point$admin_id
  
  # Find corresponding polygon
  polygon <- admin_data$polygons[admin_data$polygons$admin_id == admin_id, ]
  
  # Check if point is within polygon
  within_result <- sf::st_within(point, polygon, sparse = FALSE)
  if (!any(within_result)) {
    boundary_violations <- boundary_violations + 1
    cat("Boundary violation: Point", i, "not within polygon", admin_id, "\n")
  }
}

cat("Boundary validation results:\n")
cat("  Total points tested:", nrow(validation_samples), "\n")
cat("  Boundary violations:", boundary_violations, "\n")
cat("  Accuracy:", (nrow(validation_samples) - boundary_violations) / nrow(validation_samples) * 100, "%\n")

# Test reproducibility
cat("\n5. Testing reproducibility with seeds...\n")
repro_samples1 <- spatial_sample_administrative(
  admin_data$polygons,
  total_samples = 60,
  concurrent = TRUE,
  progress = FALSE,
  seed = 12345
)

repro_samples2 <- spatial_sample_administrative(
  admin_data$polygons,
  total_samples = 60,
  concurrent = TRUE,
  progress = FALSE,
  seed = 12345
)

counts1 <- table(repro_samples1$admin_id)
counts2 <- table(repro_samples2$admin_id)

cat("Reproducibility test:\n")
cat("  Run 1 counts:", paste(counts1, collapse = ", "), "\n")
cat("  Run 2 counts:", paste(counts2, collapse = ", "), "\n")
cat("  Identical results:", identical(counts1, counts2), "\n")

cat("\n✓ Administrative sampling accuracy tests completed!\n")