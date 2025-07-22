#!/usr/bin/env Rscript

# Test script for concurrent processing functionality
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

# Test 1: Basic concurrent processor creation
cat("Test 1: Creating ConcurrentProcessor...\n")
processor <- ConcurrentProcessor$new()
processor$initialize()
cat("✓ ConcurrentProcessor created successfully\n")
cat("  Max workers:", processor$max_workers, "\n")
cat("  Progress enabled:", processor$progress_enabled, "\n")

# Test 2: Administrative sampler with concurrent processing
cat("\nTest 2: Testing administrative sampling with concurrent processing...\n")
admin_data <- create_test_admin_polygons()

# Test without concurrent processing
samples_sequential <- spatial_sample_administrative(
  admin_data$polygons,
  total_samples = 60,
  concurrent = FALSE,
  progress = FALSE
)

cat("✓ Sequential sampling completed\n")
cat("  Samples generated:", nrow(samples_sequential), "\n")

# Test with concurrent processing
samples_concurrent <- spatial_sample_administrative(
  admin_data$polygons,
  total_samples = 60,
  concurrent = TRUE,
  max_workers = 2,
  progress = FALSE
)

cat("✓ Concurrent sampling completed\n")
cat("  Samples generated:", nrow(samples_concurrent), "\n")

# Test 3: Different allocation methods with concurrent processing
cat("\nTest 3: Testing allocation methods with concurrent processing...\n")

# Proportional allocation
prop_samples <- spatial_sample_administrative(
  admin_data$polygons,
  total_samples = 90,
  allocation_method = "proportional",
  concurrent = TRUE,
  progress = FALSE
)
cat("✓ Proportional allocation:", nrow(prop_samples), "samples\n")

# Equal allocation
equal_samples <- spatial_sample_administrative(
  admin_data$polygons,
  total_samples = 90,
  allocation_method = "equal",
  concurrent = TRUE,
  progress = FALSE
)
cat("✓ Equal allocation:", nrow(equal_samples), "samples\n")

# Custom allocation
admin_data$polygons$weight <- c(1, 3, 2)
custom_samples <- spatial_sample_administrative(
  admin_data$polygons,
  total_samples = 60,
  allocation_method = "custom",
  weight_column = "weight",
  concurrent = TRUE,
  progress = FALSE
)
cat("✓ Custom allocation:", nrow(custom_samples), "samples\n")

# Test 4: Processor configuration
cat("\nTest 4: Testing processor configuration...\n")
processor$set_max_workers(4)
cat("✓ Max workers updated to:", processor$max_workers, "\n")

processor$disable_progress()
cat("✓ Progress disabled:", !processor$progress_enabled, "\n")

processor$enable_progress()
cat("✓ Progress enabled:", processor$progress_enabled, "\n")

# Test 5: Statistics
cat("\nTest 5: Testing processor statistics...\n")
stats <- processor$get_statistics()
cat("✓ Statistics retrieved:\n")
cat("  Max workers:", stats$max_workers, "\n")
cat("  Chunk size:", stats$chunk_size, "\n")
cat("  Cluster active:", stats$cluster_active, "\n")

# Cleanup
processor$cleanup()
cat("\n✓ All tests completed successfully!\n")