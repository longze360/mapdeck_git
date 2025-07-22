test_that("SamplingEngine can be created and initialized", {
  engine <- SamplingEngine$new()
  expect_s3_class(engine, "SamplingEngine")
  
  engine$initialize()
  expect_false(engine$gpu_enabled)  # Should be FALSE by default
  expect_equal(engine$fallback_threshold, 10000)
  expect_is(engine$performance_stats, "list")
})

test_that("spatial_sample_random works with data.frame", {
  # Create test data
  test_data <- data.frame(
    longitude = runif(100, -1, 1),
    latitude = runif(100, -1, 1),
    value = rnorm(100)
  )
  
  # Test random sampling
  samples <- spatial_sample_random(test_data, n = 50)
  
  expect_s3_class(samples, "data.frame")
  expect_equal(nrow(samples), 50)
  expect_equal(ncol(samples), ncol(test_data))
  expect_true(all(names(samples) == names(test_data)))
})

test_that("spatial_sample_random works with bounds", {
  # Create test data
  test_data <- data.frame(
    longitude = runif(100, -2, 2),
    latitude = runif(100, -2, 2),
    value = rnorm(100)
  )
  
  # Define bounds
  bounds <- c(-1, -1, 1, 1)
  
  # Test bounded sampling
  samples <- spatial_sample_random(test_data, n = 20, bounds = bounds)
  
  expect_s3_class(samples, "data.frame")
  expect_lte(nrow(samples), 20)  # May be less if few points in bounds
  
  # Check that all samples are within bounds
  expect_true(all(samples$longitude >= bounds[1] & samples$longitude <= bounds[3]))
  expect_true(all(samples$latitude >= bounds[2] & samples$latitude <= bounds[4]))
})

test_that("spatial_sample_grid works correctly", {
  # Create test data in a grid pattern
  test_data <- expand.grid(
    longitude = seq(-1, 1, by = 0.1),
    latitude = seq(-1, 1, by = 0.1)
  )
  test_data$value <- rnorm(nrow(test_data))
  
  # Test grid sampling
  grid_samples <- spatial_sample_grid(test_data, grid_size = 0.5)
  
  expect_s3_class(grid_samples, "data.frame")
  expect_lt(nrow(grid_samples), nrow(test_data))  # Should reduce number of points
  expect_equal(ncol(grid_samples), ncol(test_data))
})

test_that("spatial_sample_stratified works correctly", {
  # Create test data with strata
  test_data <- data.frame(
    longitude = runif(100, -1, 1),
    latitude = runif(100, -1, 1),
    category = sample(c("A", "B", "C"), 100, replace = TRUE),
    value = rnorm(100)
  )
  
  # Test stratified sampling
  stratified_samples <- spatial_sample_stratified(test_data, 
                                                   strata_column = "category",
                                                   n_per_stratum = 10)
  
  expect_s3_class(stratified_samples, "data.frame")
  expect_lte(nrow(stratified_samples), 30)  # Max 10 per stratum * 3 strata
  expect_equal(ncol(stratified_samples), ncol(test_data))
  
  # Check that we have samples from each stratum
  strata_counts <- table(stratified_samples$category)
  expect_true(all(strata_counts <= 10))
})

test_that("create_sampling_engine works with custom config", {
  config <- list(fallback_threshold = 5000)
  engine <- create_sampling_engine(config)
  
  expect_s3_class(engine, "SamplingEngine")
  expect_equal(engine$fallback_threshold, 5000)
})

test_that("performance statistics work correctly", {
  engine <- create_sampling_engine()
  
  # Initial stats should be zero
  stats <- get_sampling_performance(engine)
  expect_equal(stats$gpu_operations, 0)
  expect_equal(stats$cpu_operations, 0)
  expect_equal(stats$total_samples, 0)
  
  # Perform some operations
  test_data <- data.frame(
    longitude = runif(50, -1, 1),
    latitude = runif(50, -1, 1)
  )
  
  spatial_sample_random(test_data, n = 25, engine = engine)
  
  # Stats should be updated
  stats <- get_sampling_performance(engine)
  expect_gt(stats$cpu_operations, 0)  # Should use CPU for small dataset
  expect_equal(stats$total_samples, 25)
})

test_that("GPU acceleration check works", {
  gpu_info <- check_gpu_acceleration()
  
  expect_is(gpu_info, "list")
  expect_true("available" %in% names(gpu_info))
  expect_true("reason" %in% names(gpu_info))
  expect_true("fallback_threshold" %in% names(gpu_info))
  expect_is(gpu_info$available, "logical")
  expect_is(gpu_info$reason, "character")
})

test_that("error handling works correctly", {
  # Test with empty data
  expect_error(spatial_sample_random(data.frame(), n = 10))
  
  # Test with invalid n
  test_data <- data.frame(longitude = 1:5, latitude = 1:5)
  expect_error(spatial_sample_random(test_data, n = 0))
  expect_error(spatial_sample_random(test_data, n = 10))  # More than available
  
  # Test with invalid bounds
  expect_error(spatial_sample_random(test_data, n = 3, bounds = c(1, 2, 3)))  # Wrong length
  
  # Test with invalid grid size
  expect_error(spatial_sample_grid(test_data, grid_size = 0))
  expect_error(spatial_sample_grid(test_data, grid_size = -1))
  
  # Test with invalid strata column
  expect_error(spatial_sample_stratified(test_data, strata_column = "nonexistent", n_per_stratum = 2))
})

test_that("coordinate detection works correctly", {
  # Test with standard column names
  test_data1 <- data.frame(longitude = 1:5, latitude = 1:5, value = 1:5)
  samples1 <- spatial_sample_random(test_data1, n = 3)
  expect_equal(nrow(samples1), 3)
  
  # Test with alternative column names
  test_data2 <- data.frame(lon = 1:5, lat = 1:5, value = 1:5)
  samples2 <- spatial_sample_random(test_data2, n = 3)
  expect_equal(nrow(samples2), 3)
  
  # Test with x/y column names
  test_data3 <- data.frame(x = 1:5, y = 1:5, value = 1:5)
  samples3 <- spatial_sample_random(test_data3, n = 3)
  expect_equal(nrow(samples3), 3)
})

test_that("aggregation methods work in grid sampling", {
  # Create test data
  test_data <- data.frame(
    longitude = c(0.1, 0.2, 0.6, 0.7),
    latitude = c(0.1, 0.2, 0.6, 0.7),
    value = 1:4
  )
  
  # Test different aggregation methods
  centroid_samples <- spatial_sample_grid(test_data, grid_size = 0.5, 
                                          aggregation_method = "centroid")
  random_samples <- spatial_sample_grid(test_data, grid_size = 0.5, 
                                        aggregation_method = "random")
  first_samples <- spatial_sample_grid(test_data, grid_size = 0.5, 
                                       aggregation_method = "first")
  
  # All should produce 2 samples (2 grid cells)
  expect_equal(nrow(centroid_samples), 2)
  expect_equal(nrow(random_samples), 2)
  expect_equal(nrow(first_samples), 2)
  
  # Test invalid aggregation method
  expect_error(spatial_sample_grid(test_data, grid_size = 0.5, 
                                   aggregation_method = "invalid"))
})