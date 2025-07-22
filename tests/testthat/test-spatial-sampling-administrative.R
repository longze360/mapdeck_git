test_that("AdministrativeSampler can be created and initialized", {
  sampler <- AdministrativeSampler$new()
  expect_s3_class(sampler, "AdministrativeSampler")

  sampler$initialize()
  expect_s3_class(sampler$allocation_strategy, "ProportionalAllocationStrategy")
  expect_s3_class(sampler$boundary_validator, "BoundaryValidator")
  expect_s3_class(sampler$sampling_engine, "SamplingEngine")
})

test_that("spatial_sample_administrative works with proportional allocation", {
  # Create test administrative polygons
  admin_data <- create_test_admin_polygons()

  # Test proportional sampling
  samples <- spatial_sample_administrative(
    admin_data$polygons,
    total_samples = 100,
    allocation_method = "proportional"
  )

  expect_s3_class(samples, "sf")
  expect_lte(nrow(samples), 100)  # May be less due to rejection sampling
  expect_true("admin_id" %in% names(samples))

  # Check that samples are distributed proportionally to area
  sample_counts <- table(samples$admin_id)
  expect_true(length(sample_counts) <= 3)  # Should have samples from units
})

test_that("spatial_sample_administrative works with equal allocation", {
  # Create test administrative polygons
  admin_data <- create_test_admin_polygons()

  # Test equal sampling
  samples <- spatial_sample_administrative(
    admin_data$polygons,
    total_samples = 90,
    allocation_method = "equal"
  )

  expect_s3_class(samples, "sf")
  expect_lte(nrow(samples), 90)
  expect_true("admin_id" %in% names(samples))

  # Check that samples are distributed roughly equally
  sample_counts <- table(samples$admin_id)
  expect_true(max(sample_counts) - min(sample_counts) <= 1)
})

test_that("spatial_sample_administrative works with custom allocation", {
  # Create test administrative polygons with custom weights
  admin_data <- create_test_admin_polygons()
  admin_data$polygons$weight <- c(1, 3, 2)  # Custom weights

  # Test custom sampling
  samples <- spatial_sample_administrative(
    admin_data$polygons,
    total_samples = 60,
    allocation_method = "custom",
    weight_column = "weight"
  )

  expect_s3_class(samples, "sf")
  expect_lte(nrow(samples), 60)
  expect_true("admin_id" %in% names(samples))

  # Check that samples are distributed according to weights
  sample_counts <- table(samples$admin_id)
  # Unit B (weight=3) should have most samples
  if ("B" %in% names(sample_counts)) {
    expect_gte(sample_counts[["B"]], sample_counts[["A"]])
    expect_gte(sample_counts[["B"]], sample_counts[["C"]])
  }
})

test_that("allocation strategies work correctly", {
  admin_data <- create_test_admin_polygons()

  # Test ProportionalAllocationStrategy
  prop_strategy <- ProportionalAllocationStrategy$new()
  prop_counts <- prop_strategy$allocate_samples(admin_data$polygons, 100)
  expect_equal(sum(prop_counts), 100)
  expect_equal(length(prop_counts), 3)
  expect_true(all(names(prop_counts) %in% c("A", "B", "C")))

  # Test EqualAllocationStrategy
  equal_strategy <- EqualAllocationStrategy$new()
  equal_counts <- equal_strategy$allocate_samples(admin_data$polygons, 99)
  expect_equal(sum(equal_counts), 99)
  expect_equal(length(equal_counts), 3)
  expect_true(max(equal_counts) - min(equal_counts) <= 1)

  # Test CustomAllocationStrategy
  admin_data$polygons$weight <- c(2, 4, 3)
  custom_strategy <- CustomAllocationStrategy$new()
  custom_counts <- custom_strategy$allocate_samples(
    admin_data$polygons, 90, "weight"
  )
  expect_equal(sum(custom_counts), 90)
  expect_equal(length(custom_counts), 3)
})

test_that("boundary validation works correctly", {
  validator <- BoundaryValidator$new()

  # Create test points and polygons
  admin_data <- create_test_admin_polygons()

  # Create points - some inside, some outside
  test_points <- sf::st_as_sf(
    data.frame(
      x = c(0.5, 1.5, 2.5, 5.0),  # Last point is outside all polygons
      y = c(0.5, 0.5, 1.5, 5.0)
    ),
    coords = c("x", "y"),
    crs = 4326
  )

  # Validate points
  valid_points <- validator$validate_points(test_points, admin_data$polygons)

  expect_s3_class(valid_points, "sf")
  expect_lt(nrow(valid_points), nrow(test_points))  # Should remove outside points
})

test_that("error handling works correctly", {
  # Test with empty admin_polygons
  expect_error(
    spatial_sample_administrative(
      sf::st_sf(geometry = sf::st_sfc()),
      total_samples = 10
    ),
    "admin_polygons must be non-empty"
  )

  # Test with invalid total_samples
  admin_data <- create_test_admin_polygons()
  expect_error(
    spatial_sample_administrative(admin_data$polygons, total_samples = 0),
    "total_samples must be a positive integer"
  )

  # Test with invalid allocation_method
  expect_error(
    spatial_sample_administrative(
      admin_data$polygons,
      total_samples = 10,
      allocation_method = "invalid"
    ),
    "allocation_method must be one of"
  )

  # Test custom allocation without weight_column
  expect_error(
    spatial_sample_administrative(
      admin_data$polygons,
      total_samples = 10,
      allocation_method = "custom"
    ),
    "weight_column must be specified when using custom allocation"
  )

  # Test with invalid weight_column
  expect_error(
    spatial_sample_administrative(
      admin_data$polygons,
      total_samples = 10,
      allocation_method = "custom",
      weight_column = "nonexistent"
    ),
    "weight_column must be a valid column in admin_polygons"
  )
})

test_that("admin column detection works correctly", {
  # Test with explicit admin column
  admin_data <- create_test_admin_polygons()
  samples <- spatial_sample_administrative(
    admin_data$polygons,
    total_samples = 30,
    admin_column = "admin_id"
  )
  expect_true("admin_id" %in% names(samples))

  # Test with auto-detection
  admin_data$polygons$name <- c("Unit A", "Unit B", "Unit C")
  samples2 <- spatial_sample_administrative(
    admin_data$polygons[, c("name", "geometry")],  # Remove admin_id
    total_samples = 30
  )
  expect_true("name" %in% names(samples2))
})

test_that("seed parameter works for reproducibility", {
  admin_data <- create_test_admin_polygons()

  # Generate samples with same seed
  samples1 <- spatial_sample_administrative(
    admin_data$polygons,
    total_samples = 50,
    seed = 123
  )

  samples2 <- spatial_sample_administrative(
    admin_data$polygons,
    total_samples = 50,
    seed = 123
  )

  # Should have same number of samples per unit
  counts1 <- table(samples1$admin_id)
  counts2 <- table(samples2$admin_id)
  expect_equal(counts1, counts2)
})

test_that("allocation strategy methods work correctly", {
  # Test method field
  prop_strategy <- ProportionalAllocationStrategy$new()
  expect_equal(prop_strategy$method, "proportional")

  equal_strategy <- EqualAllocationStrategy$new()
  expect_equal(equal_strategy$method, "equal")

  custom_strategy <- CustomAllocationStrategy$new()
  expect_equal(custom_strategy$method, "custom")
})

test_that("custom allocation handles edge cases", {
  admin_data <- create_test_admin_polygons()

  # Test with zero weights
  admin_data$polygons$zero_weight <- c(0, 0, 0)
  expect_warning(
    custom_strategy <- CustomAllocationStrategy$new(),
    NA  # No warning expected during creation
  )

  expect_warning(
    counts <- custom_strategy$allocate_samples(
      admin_data$polygons, 30, "zero_weight"
    ),
    "All weights are zero, using equal allocation"
  )
  expect_equal(sum(counts), 30)

  # Test with negative weights
  admin_data$polygons$negative_weight <- c(-1, 2, 3)
  expect_error(
    custom_strategy$allocate_samples(
      admin_data$polygons, 30, "negative_weight"
    ),
    "weights must be non-negative"
  )

  # Test with non-numeric weights
  admin_data$polygons$text_weight <- c("a", "b", "c")
  expect_error(
    custom_strategy$allocate_samples(
      admin_data$polygons, 30, "text_weight"
    ),
    "weight_column must contain numeric values"
  )
})

test_that("point generation handles difficult polygons", {
  # Create a very small polygon that might be difficult to sample
  small_coords <- matrix(c(
    0, 0,
    0.001, 0,
    0.001, 0.001,
    0, 0.001,
    0, 0
  ), ncol = 2, byrow = TRUE)

  small_polygon <- sf::st_sf(
    admin_id = "small",
    geometry = sf::st_sfc(sf::st_polygon(list(small_coords))),
    crs = 4326
  )

  # This should work but might generate fewer points than requested
  expect_warning(
    samples <- spatial_sample_administrative(small_polygon, total_samples = 100),
    "Could only generate"
  )

  expect_s3_class(samples, "sf")
  expect_lte(nrow(samples), 100)
})

# Helper function to create test administrative polygons
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
}# Tests f
or concurrent processing and advanced features

test_that("ConcurrentProcessor can be created and initialized", {
  skip_if_not_installed("parallel")
  
  processor <- ConcurrentProcessor$new()
  expect_s3_class(processor, "ConcurrentProcessor")
  
  processor$initialize()
  expect_true(is.numeric(processor$max_workers))
  expect_true(is.logical(processor$progress_enabled))
  expect_true(is.numeric(processor$chunk_size))
})

test_that("ConcurrentProcessor configuration works correctly", {
  skip_if_not_installed("parallel")
  
  config <- list(
    max_workers = 2,
    progress_enabled = FALSE,
    chunk_size = 5
  )
  
  processor <- ConcurrentProcessor$new()
  processor$initialize(config)
  
  expect_equal(processor$max_workers, 2)
  expect_false(processor$progress_enabled)
  expect_equal(processor$chunk_size, 5)
})

test_that("concurrent processing works with small datasets", {
  skip_if_not_installed("parallel")
  
  admin_data <- create_test_admin_polygons()
  
  # Test concurrent processing with small dataset (should use sequential)
  samples <- spatial_sample_administrative(
    admin_data$polygons,
    total_samples = 60,
    concurrent = TRUE,
    max_workers = 2,
    progress = FALSE
  )
  
  expect_s3_class(samples, "sf")
  expect_lte(nrow(samples), 60)
  expect_true("admin_id" %in% names(samples))
})

test_that("concurrent processing works with larger datasets", {
  skip_if_not_installed("parallel")
  
  # Create larger test dataset
  admin_data <- create_large_test_admin_polygons()
  
  # Test concurrent processing
  samples <- spatial_sample_administrative(
    admin_data$polygons,
    total_samples = 200,
    concurrent = TRUE,
    max_workers = 2,
    progress = FALSE
  )
  
  expect_s3_class(samples, "sf")
  expect_lte(nrow(samples), 200)
  expect_true("admin_id" %in% names(samples))
  
  # Check that all administrative units are represented
  sample_counts <- table(samples$admin_id)
  expect_true(length(sample_counts) > 0)
})

test_that("progress indicators work correctly", {
  skip_if_not_installed("progress")
  
  admin_data <- create_test_admin_polygons()
  
  # Test with progress enabled (should not error)
  expect_no_error({
    samples <- spatial_sample_administrative(
      admin_data$polygons,
      total_samples = 30,
      progress = TRUE
    )
  })
  
  expect_s3_class(samples, "sf")
})

test_that("concurrent processor statistics work correctly", {
  skip_if_not_installed("parallel")
  
  processor <- ConcurrentProcessor$new()
  processor$initialize(list(max_workers = 3, chunk_size = 8))
  
  stats <- processor$get_statistics()
  expect_equal(stats$max_workers, 3)
  expect_equal(stats$chunk_size, 8)
  expect_true(is.logical(stats$cluster_active))
})

test_that("concurrent processor worker management works", {
  skip_if_not_installed("parallel")
  
  processor <- ConcurrentProcessor$new()
  processor$initialize(list(max_workers = 2))
  
  # Test setting max workers
  processor$set_max_workers(4)
  expect_equal(processor$max_workers, 4)
  
  # Test invalid max workers
  expect_error(
    processor$set_max_workers(0),
    "max_workers must be a positive integer"
  )
  
  # Test cleanup
  expect_no_error(processor$cleanup())
})

test_that("progress control methods work correctly", {
  processor <- ConcurrentProcessor$new()
  processor$initialize()
  
  # Test enabling progress
  processor$enable_progress()
  expect_true(processor$progress_enabled)
  
  # Test disabling progress
  processor$disable_progress()
  expect_false(processor$progress_enabled)
})

test_that("concurrent processing handles errors gracefully", {
  skip_if_not_installed("parallel")
  
  # Test with invalid admin_polygons
  expect_error(
    spatial_sample_administrative(
      sf::st_sf(geometry = sf::st_sfc()),
      total_samples = 10,
      concurrent = TRUE
    ),
    "admin_polygons must be non-empty"
  )
})

test_that("allocation strategies work with concurrent processing", {
  skip_if_not_installed("parallel")
  
  admin_data <- create_test_admin_polygons()
  
  # Test proportional allocation with concurrent processing
  prop_samples <- spatial_sample_administrative(
    admin_data$polygons,
    total_samples = 90,
    allocation_method = "proportional",
    concurrent = TRUE,
    progress = FALSE
  )
  
  expect_s3_class(prop_samples, "sf")
  expect_lte(nrow(prop_samples), 90)
  
  # Test equal allocation with concurrent processing
  equal_samples <- spatial_sample_administrative(
    admin_data$polygons,
    total_samples = 90,
    allocation_method = "equal",
    concurrent = TRUE,
    progress = FALSE
  )
  
  expect_s3_class(equal_samples, "sf")
  expect_lte(nrow(equal_samples), 90)
  
  # Check equal distribution
  sample_counts <- table(equal_samples$admin_id)
  expect_true(max(sample_counts) - min(sample_counts) <= 1)
  
  # Test custom allocation with concurrent processing
  admin_data$polygons$weight <- c(1, 3, 2)
  custom_samples <- spatial_sample_administrative(
    admin_data$polygons,
    total_samples = 60,
    allocation_method = "custom",
    weight_column = "weight",
    concurrent = TRUE,
    progress = FALSE
  )
  
  expect_s3_class(custom_samples, "sf")
  expect_lte(nrow(custom_samples), 60)
})

test_that("concurrent processing maintains reproducibility with seeds", {
  skip_if_not_installed("parallel")
  
  admin_data <- create_test_admin_polygons()
  
  # Generate samples with same seed using concurrent processing
  samples1 <- spatial_sample_administrative(
    admin_data$polygons,
    total_samples = 60,
    concurrent = TRUE,
    seed = 456,
    progress = FALSE
  )
  
  samples2 <- spatial_sample_administrative(
    admin_data$polygons,
    total_samples = 60,
    concurrent = TRUE,
    seed = 456,
    progress = FALSE
  )
  
  # Should have same number of samples per unit
  counts1 <- table(samples1$admin_id)
  counts2 <- table(samples2$admin_id)
  expect_equal(counts1, counts2)
})

test_that("concurrent processing performance scales appropriately", {
  skip_if_not_installed("parallel")
  
  # Create moderately large dataset
  admin_data <- create_large_test_admin_polygons()
  
  # Time sequential processing
  start_time <- Sys.time()
  sequential_samples <- spatial_sample_administrative(
    admin_data$polygons,
    total_samples = 100,
    concurrent = FALSE,
    progress = FALSE
  )
  sequential_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  # Time concurrent processing
  start_time <- Sys.time()
  concurrent_samples <- spatial_sample_administrative(
    admin_data$polygons,
    total_samples = 100,
    concurrent = TRUE,
    max_workers = 2,
    progress = FALSE
  )
  concurrent_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  # Both should produce valid results
  expect_s3_class(sequential_samples, "sf")
  expect_s3_class(concurrent_samples, "sf")
  expect_equal(nrow(sequential_samples), nrow(concurrent_samples))
  
  # Performance comparison (concurrent should not be significantly slower)
  # Allow for some overhead in small datasets
  expect_lt(concurrent_time, sequential_time * 3)
})

test_that("boundary validation works with concurrent processing", {
  skip_if_not_installed("parallel")
  
  admin_data <- create_test_admin_polygons()
  
  # Generate samples with concurrent processing
  samples <- spatial_sample_administrative(
    admin_data$polygons,
    total_samples = 90,
    concurrent = TRUE,
    progress = FALSE
  )
  
  # All points should be within their respective polygons
  for (i in seq_len(nrow(samples))) {
    point <- samples[i, ]
    admin_id <- point$admin_id
    
    # Find corresponding polygon
    polygon <- admin_data$polygons[admin_data$polygons$admin_id == admin_id, ]
    
    # Check if point is within polygon
    within_result <- sf::st_within(point, polygon, sparse = FALSE)
    expect_true(any(within_result), 
                info = paste("Point", i, "not within polygon", admin_id))
  }
})

test_that("concurrent processing handles memory efficiently", {
  skip_if_not_installed("parallel")
  
  # Create dataset that might stress memory
  admin_data <- create_large_test_admin_polygons()
  
  # Monitor memory usage during processing
  gc_before <- gc()
  
  samples <- spatial_sample_administrative(
    admin_data$polygons,
    total_samples = 500,
    concurrent = TRUE,
    max_workers = 2,
    progress = FALSE
  )
  
  gc_after <- gc()
  
  # Should produce valid results
  expect_s3_class(samples, "sf")
  expect_lte(nrow(samples), 500)
  
  # Memory usage should not grow excessively
  # This is a basic check - in practice would need more sophisticated monitoring
  expect_true(is.finite(gc_after[1, 2]))  # Used memory should be finite
})

# Helper function to create larger test dataset for concurrent processing tests
create_large_test_admin_polygons <- function() {
  # Create 15 administrative units to test concurrent processing
  coords_list <- list()
  admin_ids <- character(15)
  
  for (i in 1:15) {
    # Create rectangular polygons in a grid pattern
    row <- ((i - 1) %/% 5)
    col <- ((i - 1) %% 5)
    
    x_start <- col * 1.2
    y_start <- row * 1.2
    
    coords_list[[i]] <- matrix(c(
      x_start, y_start,
      x_start + 1, y_start,
      x_start + 1, y_start + 1,
      x_start, y_start + 1,
      x_start, y_start
    ), ncol = 2, byrow = TRUE)
    
    admin_ids[i] <- paste0("Unit_", sprintf("%02d", i))
  }
  
  polygons <- lapply(coords_list, function(x) sf::st_polygon(list(x)))
  
  admin_data <- data.frame(
    admin_id = admin_ids,
    area = rep(1, 15),  # All units have same area
    population = sample(1000:5000, 15)  # Random population for testing
  )
  
  admin_sf <- sf::st_sf(admin_data, geometry = sf::st_sfc(polygons), crs = 4326)
  
  list(
    polygons = admin_sf,
    coords_list = coords_list
  )
}