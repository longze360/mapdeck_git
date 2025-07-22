# Comprehensive Spatial Sampling Tests
# Tests for spatial sampling with statistical validation

test_that("SamplingEngine initialization is complete", {
  skip_if_not_installed("R6")
  
  engine <- SamplingEngine$new()
  expect_s3_class(engine, "SamplingEngine")
  expect_s3_class(engine, "R6")
  
  # Test initialization
  engine$initialize()
  expect_false(engine$gpu_enabled)  # Should be FALSE by default
  expect_equal(engine$fallback_threshold, 10000)
  expect_true(is.list(engine$performance_stats))
  expect_true(is.numeric(engine$fallback_threshold))
  
  # Test custom initialization
  custom_config <- list(
    fallback_threshold = 5000,
    gpu_enabled = TRUE,
    max_iterations = 1000
  )
  
  engine$initialize(custom_config)
  expect_equal(engine$fallback_threshold, 5000)
  expect_true(engine$gpu_enabled)
})

test_that("spatial_sample_random works with comprehensive data types", {
  skip_if_not_installed("R6")
  
  # Test with data.frame
  test_data <- create_test_data(200)
  samples <- spatial_sample_random(test_data, n = 100)
  
  expect_s3_class(samples, "data.frame")
  expect_equal(nrow(samples), 100)
  expect_equal(ncol(samples), ncol(test_data))
  expect_true(all(names(samples) == names(test_data)))
  expect_statistical_validity(samples, test_data)
  
  # Test with different coordinate column names
  alt_data <- test_data
  names(alt_data)[1:2] <- c("lon", "lat")
  samples_alt <- spatial_sample_random(alt_data, n = 50)
  expect_equal(nrow(samples_alt), 50)
  
  # Test with x/y coordinates
  xy_data <- test_data
  names(xy_data)[1:2] <- c("x", "y")
  samples_xy <- spatial_sample_random(xy_data, n = 75)
  expect_equal(nrow(samples_xy), 75)
  
  # Test with minimal data
  minimal_data <- data.frame(longitude = runif(10, -1, 1), latitude = runif(10, -1, 1))
  samples_min <- spatial_sample_random(minimal_data, n = 5)
  expect_equal(nrow(samples_min), 5)
  
  # Test parameter validation
  expect_error(spatial_sample_random(test_data, n = 0), "n must be positive")
  expect_error(spatial_sample_random(test_data, n = -1), "n must be positive")
  expect_error(spatial_sample_random(test_data, n = 250), "n cannot be larger than")
  expect_error(spatial_sample_random(data.frame(), n = 10), "Data cannot be empty")
})

test_that("spatial_sample_random works with bounds correctly", {
  skip_if_not_installed("R6")
  
  # Create test data with known distribution
  test_data <- data.frame(
    longitude = runif(1000, -5, 5),
    latitude = runif(1000, -5, 5),
    value = rnorm(1000)
  )
  
  # Test with restrictive bounds
  bounds <- c(-2, -2, 2, 2)
  bounded_samples <- spatial_sample_random(test_data, n = 100, bounds = bounds)
  
  expect_s3_class(bounded_samples, "data.frame")
  expect_lte(nrow(bounded_samples), 100)  # May be less if few points in bounds
  
  # Verify all samples are within bounds
  expect_true(all(bounded_samples$longitude >= bounds[1]))
  expect_true(all(bounded_samples$longitude <= bounds[3]))
  expect_true(all(bounded_samples$latitude >= bounds[2]))
  expect_true(all(bounded_samples$latitude <= bounds[4]))
  
  # Test with bounds that include all data
  wide_bounds <- c(-10, -10, 10, 10)
  wide_samples <- spatial_sample_random(test_data, n = 100, bounds = wide_bounds)
  expect_equal(nrow(wide_samples), 100)
  
  # Test bounds validation
  expect_error(spatial_sample_random(test_data, n = 50, bounds = c(1, 2, 3)), 
               "Bounds must be a numeric vector of length 4")
  expect_error(spatial_sample_random(test_data, n = 50, bounds = c(2, 1, 4, 3)), 
               "Invalid bounds: xmin must be less than xmax")
  expect_error(spatial_sample_random(test_data, n = 50, bounds = c(1, 4, 3, 2)), 
               "Invalid bounds: ymin must be less than ymax")
  
  # Test with bounds that exclude all data
  empty_bounds <- c(10, 10, 11, 11)
  expect_error(spatial_sample_random(test_data, n = 10, bounds = empty_bounds),
               "No data points within specified bounds")
})

test_that("spatial_sample_grid works with comprehensive options", {
  skip_if_not_installed("R6")
  
  # Create grid-like test data
  test_data <- expand.grid(
    longitude = seq(-2, 2, by = 0.1),
    latitude = seq(-2, 2, by = 0.1)
  )
  test_data$value <- rnorm(nrow(test_data))
  test_data$category <- sample(c("A", "B", "C"), nrow(test_data), replace = TRUE)
  
  # Test basic grid sampling
  grid_samples <- spatial_sample_grid(test_data, grid_size = 0.5)
  
  expect_s3_class(grid_samples, "data.frame")
  expect_lt(nrow(grid_samples), nrow(test_data))
  expect_equal(ncol(grid_samples), ncol(test_data))
  
  # Test different aggregation methods
  centroid_samples <- spatial_sample_grid(test_data, grid_size = 0.5, 
                                          aggregation_method = "centroid")
  random_samples <- spatial_sample_grid(test_data, grid_size = 0.5, 
                                        aggregation_method = "random")
  first_samples <- spatial_sample_grid(test_data, grid_size = 0.5, 
                                       aggregation_method = "first")
  
  # All should produce similar number of samples
  expect_true(abs(nrow(centroid_samples) - nrow(random_samples)) <= 2)
  expect_true(abs(nrow(centroid_samples) - nrow(first_samples)) <= 2)
  
  # Test parameter validation
  expect_error(spatial_sample_grid(test_data, grid_size = 0), 
               "grid_size must be positive")
  expect_error(spatial_sample_grid(test_data, grid_size = -1), 
               "grid_size must be positive")
  expect_error(spatial_sample_grid(test_data, grid_size = c(0.5, 1.0)), 
               "grid_size must be a single numeric value")
  expect_error(spatial_sample_grid(test_data, grid_size = 0.5, 
                                   aggregation_method = "invalid"),
               "aggregation_method must be one of")
  
  # Test with different grid sizes
  large_grid <- spatial_sample_grid(test_data, grid_size = 1.0)
  small_grid <- spatial_sample_grid(test_data, grid_size = 0.2)
  
  expect_lt(nrow(large_grid), nrow(small_grid))
})

test_that("spatial_sample_stratified works with comprehensive scenarios", {
  skip_if_not_installed("R6")
  
  # Create test data with known strata distribution
  test_data <- data.frame(
    longitude = runif(500, -1, 1),
    latitude = runif(500, -1, 1),
    category = c(rep("A", 200), rep("B", 150), rep("C", 100), rep("D", 50)),
    value = rnorm(500),
    numeric_strata = sample(1:5, 500, replace = TRUE)
  )
  
  # Test basic stratified sampling
  stratified_samples <- spatial_sample_stratified(test_data, 
                                                   strata_column = "category",
                                                   n_per_stratum = 20)
  
  expect_s3_class(stratified_samples, "data.frame")
  expect_lte(nrow(stratified_samples), 80)  # Max 20 per stratum * 4 strata
  expect_equal(ncol(stratified_samples), ncol(test_data))
  
  # Check strata distribution
  strata_counts <- table(stratified_samples$category)
  expect_true(all(strata_counts <= 20))
  expect_true(length(strata_counts) <= 4)
  
  # Test with numeric strata
  numeric_stratified <- spatial_sample_stratified(test_data,
                                                   strata_column = "numeric_strata",
                                                   n_per_stratum = 10)
  expect_s3_class(numeric_stratified, "data.frame")
  
  # Test with uneven strata (should warn)
  uneven_data <- data.frame(
    longitude = runif(100, -1, 1),
    latitude = runif(100, -1, 1),
    category = c(rep("A", 80), rep("B", 15), rep("C", 5))
  )
  
  expect_warning(
    uneven_samples <- spatial_sample_stratified(uneven_data,
                                                 strata_column = "category",
                                                 n_per_stratum = 20),
    "Requested.*samples but only.*available"
  )
  
  # Should still produce valid results
  expect_s3_class(uneven_samples, "data.frame")
  uneven_counts <- table(uneven_samples$category)
  expect_lte(uneven_counts[["A"]], 20)
  expect_lte(uneven_counts[["B"]], 15)  # Limited by available data
  expect_lte(uneven_counts[["C"]], 5)   # Limited by available data
  
  # Test parameter validation
  expect_error(spatial_sample_stratified(test_data, 
                                         strata_column = "nonexistent",
                                         n_per_stratum = 10),
               "strata_column must be a valid column name")
  expect_error(spatial_sample_stratified(test_data,
                                         strata_column = "category",
                                         n_per_stratum = 0),
               "n_per_stratum must be positive")
  expect_error(spatial_sample_stratified(test_data,
                                         strata_column = c("cat1", "cat2"),
                                         n_per_stratum = 10),
               "strata_column must be a single column name")
})

test_that("create_sampling_engine works with custom configurations", {
  skip_if_not_installed("R6")
  
  # Test default engine
  default_engine <- create_sampling_engine()
  expect_s3_class(default_engine, "SamplingEngine")
  expect_equal(default_engine$fallback_threshold, 10000)
  
  # Test custom configuration
  custom_config <- list(
    fallback_threshold = 5000,
    gpu_enabled = TRUE,
    performance_monitoring = TRUE
  )
  
  custom_engine <- create_sampling_engine(custom_config)
  expect_s3_class(custom_engine, "SamplingEngine")
  expect_equal(custom_engine$fallback_threshold, 5000)
  
  # Test invalid configuration
  expect_error(create_sampling_engine("not_a_list"),
               "Config must be a list")
  expect_error(create_sampling_engine(list(fallback_threshold = -1)),
               "fallback_threshold must be positive")
})

test_that("Performance statistics work comprehensively", {
  skip_if_not_installed("R6")
  
  engine <- create_sampling_engine()
  
  # Initial stats should be zero
  stats <- get_sampling_performance(engine)
  expect_equal(stats$gpu_operations, 0)
  expect_equal(stats$cpu_operations, 0)
  expect_equal(stats$total_samples, 0)
  expect_equal(stats$avg_cpu_time, 0)
  expect_equal(stats$avg_gpu_time, 0)
  
  # Perform operations to generate statistics
  test_data <- create_test_data(100)
  
  # Multiple operations
  spatial_sample_random(test_data, n = 25, engine = engine)
  spatial_sample_grid(test_data, grid_size = 0.5, engine = engine)
  spatial_sample_stratified(test_data, strata_column = "category", 
                            n_per_stratum = 10, engine = engine)
  
  # Check updated statistics
  updated_stats <- get_sampling_performance(engine)
  expect_gt(updated_stats$cpu_operations, 0)
  expect_gt(updated_stats$total_samples, 0)
  expect_gt(updated_stats$avg_cpu_time, 0)
  
  # Test reset statistics
  engine$reset_performance_stats()
  reset_stats <- get_sampling_performance(engine)
  expect_equal(reset_stats$cpu_operations, 0)
  expect_equal(reset_stats$total_samples, 0)
})

test_that("GPU acceleration check works correctly", {
  skip_if_not_installed("R6")
  
  gpu_info <- check_gpu_acceleration()
  
  expect_true(is.list(gpu_info))
  expect_true("available" %in% names(gpu_info))
  expect_true("reason" %in% names(gpu_info))
  expect_true("fallback_threshold" %in% names(gpu_info))
  expect_true("webgl_support" %in% names(gpu_info))
  
  expect_true(is.logical(gpu_info$available))
  expect_true(is.character(gpu_info$reason))
  expect_true(is.numeric(gpu_info$fallback_threshold))
  expect_true(is.logical(gpu_info$webgl_support))
  
  # Test that reason is informative
  expect_true(nchar(gpu_info$reason) > 0)
})

test_that("Error handling works comprehensively", {
  skip_if_not_installed("R6")
  
  # Test with empty data
  expect_error(spatial_sample_random(data.frame(), n = 10),
               "Data cannot be empty")
  
  # Test with invalid n values
  test_data <- create_test_data(50)
  expect_error(spatial_sample_random(test_data, n = 0),
               "n must be positive")
  expect_error(spatial_sample_random(test_data, n = -5),
               "n must be positive")
  expect_error(spatial_sample_random(test_data, n = 100),
               "n cannot be larger than")
  expect_error(spatial_sample_random(test_data, n = c(10, 20)),
               "n must be a single integer")
  expect_error(spatial_sample_random(test_data, n = "ten"),
               "n must be numeric")
  
  # Test with invalid data types
  expect_error(spatial_sample_random("not_data", n = 10),
               "Data must be a data.frame")
  expect_error(spatial_sample_random(list(x = 1:5, y = 1:5), n = 3),
               "Data must be a data.frame")
  expect_error(spatial_sample_random(matrix(1:10, ncol = 2), n = 3),
               "Data must be a data.frame")
  
  # Test with missing coordinate columns
  bad_data <- data.frame(x = 1:10, z = 1:10)
  expect_error(spatial_sample_random(bad_data, n = 5),
               "Could not find coordinate columns")
  
  # Test grid sampling errors
  expect_error(spatial_sample_grid(test_data, grid_size = 0),
               "grid_size must be positive")
  expect_error(spatial_sample_grid(test_data, grid_size = "large"),
               "grid_size must be numeric")
  
  # Test stratified sampling errors
  expect_error(spatial_sample_stratified(test_data, 
                                         strata_column = "nonexistent",
                                         n_per_stratum = 10),
               "strata_column must be a valid column name")
  expect_error(spatial_sample_stratified(test_data,
                                         strata_column = "category",
                                         n_per_stratum = "many"),
               "n_per_stratum must be numeric")
})

test_that("Coordinate detection works with various formats", {
  skip_if_not_installed("R6")
  
  # Test different coordinate column name combinations
  test_cases <- list(
    list(cols = c("longitude", "latitude"), expected = TRUE),
    list(cols = c("lon", "lat"), expected = TRUE),
    list(cols = c("lng", "lat"), expected = TRUE),
    list(cols = c("long", "lat"), expected = TRUE),
    list(cols = c("x", "y"), expected = TRUE),
    list(cols = c("X", "Y"), expected = TRUE),
    list(cols = c("LONGITUDE", "LATITUDE"), expected = TRUE),
    list(cols = c("Longitude", "Latitude"), expected = TRUE),
    list(cols = c("coord_x", "coord_y"), expected = FALSE),
    list(cols = c("east", "north"), expected = FALSE)
  )
  
  for (test_case in test_cases) {
    test_data <- data.frame(
      value = rnorm(20)
    )
    test_data[[test_case$cols[1]]] <- runif(20, -1, 1)
    test_data[[test_case$cols[2]]] <- runif(20, -1, 1)
    
    if (test_case$expected) {
      expect_silent({
        samples <- spatial_sample_random(test_data, n = 10)
        expect_equal(nrow(samples), 10)
      })
    } else {
      expect_error(spatial_sample_random(test_data, n = 10),
                   "Could not find coordinate columns")
    }
  }
})

test_that("Large dataset handling works correctly", {
  skip_if_not_installed("R6")
  
  # Create large dataset
  large_data <- create_test_data(20000)
  
  # Should handle large dataset without errors
  expect_silent({
    random_samples <- spatial_sample_random(large_data, n = 1000)
    expect_equal(nrow(random_samples), 1000)
  })
  
  expect_silent({
    grid_samples <- spatial_sample_grid(large_data, grid_size = 10)
    expect_lt(nrow(grid_samples), nrow(large_data))
  })
  
  expect_silent({
    stratified_samples <- spatial_sample_stratified(large_data, 
                                                     strata_column = "category",
                                                     n_per_stratum = 200)
    expect_lte(nrow(stratified_samples), 600)  # 3 categories * 200
  })
  
  # Test memory efficiency
  memory_before <- gc()[2, 2]
  large_samples <- spatial_sample_random(large_data, n = 5000)
  memory_after <- gc()[2, 2]
  
  # Memory usage should be reasonable
  memory_increase <- memory_after - memory_before
  expect_lt(memory_increase, 100)  # Less than 100MB increase
})

test_that("Statistical validity of sampling methods", {
  skip_if_not_installed("R6")
  
  # Create test data with known statistical properties
  set.seed(123)  # For reproducible tests
  n_pop <- 10000
  test_data <- data.frame(
    longitude = runif(n_pop, -10, 10),
    latitude = runif(n_pop, -10, 10),
    value = rnorm(n_pop, mean = 100, sd = 15),
    category = sample(c("A", "B", "C"), n_pop, replace = TRUE, prob = c(0.5, 0.3, 0.2))
  )
  
  # Test random sampling statistical properties
  random_samples <- spatial_sample_random(test_data, n = 1000)
  
  # Sample mean should be close to population mean
  pop_mean <- mean(test_data$value)
  sample_mean <- mean(random_samples$value)
  expect_true(abs(sample_mean - pop_mean) / pop_mean < 0.05,  # Within 5%
              info = sprintf("Sample mean %.2f too different from population mean %.2f", 
                           sample_mean, pop_mean))
  
  # Sample standard deviation should be close to population sd
  pop_sd <- sd(test_data$value)
  sample_sd <- sd(random_samples$value)
  expect_true(abs(sample_sd - pop_sd) / pop_sd < 0.1,  # Within 10%
              info = sprintf("Sample sd %.2f too different from population sd %.2f", 
                           sample_sd, pop_sd))
  
  # Test stratified sampling preserves strata proportions
  stratified_samples <- spatial_sample_stratified(test_data,
                                                   strata_column = "category",
                                                   n_per_stratum = 100)
  
  # Each stratum should have approximately 100 samples
  strata_counts <- table(stratified_samples$category)
  expect_true(all(strata_counts >= 95 & strata_counts <= 100))
  
  # Test grid sampling spatial distribution
  grid_samples <- spatial_sample_grid(test_data, grid_size = 2.0)
  
  # Grid samples should be more evenly distributed spatially
  # Calculate spatial variance (should be lower than random sampling)
  random_spatial_var <- var(random_samples$longitude) + var(random_samples$latitude)
  grid_spatial_var <- var(grid_samples$longitude) + var(grid_samples$latitude)
  
  # Grid sampling should have more even spatial distribution
  expect_lt(grid_spatial_var, random_spatial_var * 1.5)  # Allow some tolerance
})

test_that("Reproducibility with seeds works correctly", {
  skip_if_not_installed("R6")
  
  test_data <- create_test_data(1000)
  
  # Test random sampling reproducibility
  set.seed(456)
  samples1 <- spatial_sample_random(test_data, n = 100)
  
  set.seed(456)
  samples2 <- spatial_sample_random(test_data, n = 100)
  
  expect_equal(samples1, samples2)
  
  # Test stratified sampling reproducibility
  set.seed(789)
  strat_samples1 <- spatial_sample_stratified(test_data,
                                               strata_column = "category",
                                               n_per_stratum = 50)
  
  set.seed(789)
  strat_samples2 <- spatial_sample_stratified(test_data,
                                               strata_column = "category",
                                               n_per_stratum = 50)
  
  expect_equal(strat_samples1, strat_samples2)
  
  # Test that different seeds produce different results
  set.seed(111)
  diff_samples1 <- spatial_sample_random(test_data, n = 100)
  
  set.seed(222)
  diff_samples2 <- spatial_sample_random(test_data, n = 100)
  
  expect_false(identical(diff_samples1, diff_samples2))
})