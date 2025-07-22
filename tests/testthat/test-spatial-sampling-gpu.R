test_that("WebGLShaderManager can be created and initialized", {
  skip_if_not_installed("R6")
  
  # Source the WebGL shader manager
  source("R/spatial-sampling/webgl-shaders.R")
  
  manager <- WebGLShaderManager$new()
  expect_s3_class(manager, "WebGLShaderManager")
  
  # Test initialization
  result <- manager$initialize()
  expect_true(result)
  expect_true(manager$initialized)
  expect_not_null(manager$gl_context)
  expect_not_null(manager$shaders)
})

test_that("GPU fallback mechanisms work correctly", {
  # Create engine with GPU disabled
  engine <- SamplingEngine$new()
  engine$initialize()
  
  # Force GPU to be disabled
  engine$gpu_enabled <- FALSE
  
  # Test data
  test_data <- data.frame(
    longitude = runif(100, -1, 1),
    latitude = runif(100, -1, 1),
    value = rnorm(100)
  )
  
  # All operations should fall back to CPU
  random_samples <- engine$spatial_sample_random(test_data, n = 50)
  expect_s3_class(random_samples, "data.frame")
  expect_equal(nrow(random_samples), 50)
  
  grid_samples <- engine$spatial_sample_grid(test_data, grid_size = 0.5)
  expect_s3_class(grid_samples, "data.frame")
  expect_lt(nrow(grid_samples), nrow(test_data))
  
  test_data$category <- sample(c("A", "B"), 100, replace = TRUE)
  stratified_samples <- engine$spatial_sample_stratified(test_data, 
                                                         strata_column = "category",
                                                         n_per_stratum = 10)
  expect_s3_class(stratified_samples, "data.frame")
  expect_lte(nrow(stratified_samples), 20)
  
  # Check that CPU was used
  stats <- engine$get_performance_stats()
  expect_gt(stats$cpu_operations, 0)
  expect_equal(stats$gpu_operations, 0)
})

test_that("GPU threshold switching works correctly", {
  engine <- SamplingEngine$new()
  engine$initialize()
  
  # Set low threshold to force GPU usage attempt
  engine$fallback_threshold <- 10
  engine$gpu_enabled <- TRUE  # Simulate GPU availability
  
  # Small dataset should use CPU
  small_data <- data.frame(
    longitude = runif(5, -1, 1),
    latitude = runif(5, -1, 1)
  )
  
  samples_small <- engine$spatial_sample_random(small_data, n = 3)
  expect_equal(nrow(samples_small), 3)
  
  # Large dataset should attempt GPU (but fall back to CPU in our mock)
  large_data <- data.frame(
    longitude = runif(100, -1, 1),
    latitude = runif(100, -1, 1)
  )
  
  samples_large <- engine$spatial_sample_random(large_data, n = 50)
  expect_equal(nrow(samples_large), 50)
})

test_that("Performance monitoring works correctly", {
  skip_if_not_installed("R6")
  
  # Source the performance monitor
  source("R/spatial-sampling/performance-monitor.R")
  
  monitor <- PerformanceMonitor$new()
  monitor$initialize()
  
  # Test recording operations
  monitor$record_operation("random", "cpu", 1000, 0.1, 100)
  monitor$record_operation("grid", "gpu", 5000, 0.05, 200)
  
  # Check metrics
  summary <- monitor$get_performance_summary()
  expect_equal(summary$total_operations, 2)
  expect_equal(summary$cpu_operations, 1)
  expect_equal(summary$gpu_operations, 1)
  expect_gt(summary$avg_cpu_time, 0)
  expect_gt(summary$avg_gpu_time, 0)
  
  # Test report generation
  report <- monitor$generate_report("text")
  expect_true(is.character(report))
  expect_true(nchar(report) > 0)
})

test_that("Benchmark system works correctly", {
  skip_if_not_installed("R6")
  
  # Source the performance monitor
  source("R/spatial-sampling/performance-monitor.R")
  
  monitor <- PerformanceMonitor$new()
  monitor$initialize()
  
  engine <- SamplingEngine$new()
  engine$initialize()
  
  # Run small benchmark
  results <- monitor$run_benchmark(
    data_sizes = c(50, 100),
    operations = c("random"),
    iterations = 1,
    engine = engine
  )
  
  expect_s3_class(results, "data.frame")
  expect_true(nrow(results) > 0)
  expect_true(all(c("data_size", "operation", "method", "mean_time") %in% names(results)))
})

test_that("GPU error handling works correctly", {
  engine <- SamplingEngine$new()
  engine$initialize()
  
  # Force GPU enabled but simulate failure
  engine$gpu_enabled <- TRUE
  
  # Test data
  test_data <- data.frame(
    longitude = runif(1000, -1, 1),
    latitude = runif(1000, -1, 1)
  )
  
  # Should fall back to CPU on GPU failure
  expect_warning({
    samples <- engine$spatial_sample_random(test_data, n = 100)
  }, "GPU.*failed.*falling back to CPU")
  
  expect_s3_class(samples, "data.frame")
  expect_equal(nrow(samples), 100)
})

test_that("Memory usage tracking works", {
  skip_if_not_installed("R6")
  
  # Source the performance monitor
  source("R/spatial-sampling/performance-monitor.R")
  
  monitor <- PerformanceMonitor$new()
  monitor$initialize()
  
  # Record operation with memory usage
  monitor$record_operation("random", "cpu", 1000, 0.1, 100, memory_used = 1024)
  
  # Check that memory usage is recorded
  ops <- monitor$metrics$operations
  expect_equal(nrow(ops), 1)
  expect_equal(ops$memory_used[1], 1024)
})

test_that("Performance trends are calculated correctly", {
  skip_if_not_installed("R6")
  
  # Source the performance monitor
  source("R/spatial-sampling/performance-monitor.R")
  
  monitor <- PerformanceMonitor$new()
  monitor$initialize()
  
  # Record operations with improving performance
  for (i in 1:5) {
    monitor$record_operation("random", "cpu", 1000, 0.1 - i * 0.01, 100)
  }
  
  summary <- monitor$get_performance_summary()
  expect_equal(summary$performance_trend, "improving")
})

test_that("Coordinate detection works with various formats", {
  # Test with different coordinate column names
  test_cases <- list(
    list(data = data.frame(longitude = 1:5, latitude = 1:5, value = 1:5), expected = TRUE),
    list(data = data.frame(lon = 1:5, lat = 1:5, value = 1:5), expected = TRUE),
    list(data = data.frame(lng = 1:5, lat = 1:5, value = 1:5), expected = TRUE),
    list(data = data.frame(x = 1:5, y = 1:5, value = 1:5), expected = TRUE),
    list(data = data.frame(X = 1:5, Y = 1:5, value = 1:5), expected = TRUE)
  )
  
  for (test_case in test_cases) {
    if (test_case$expected) {
      expect_no_error({
        samples <- spatial_sample_random(test_case$data, n = 3)
        expect_equal(nrow(samples), 3)
      })
    }
  }
})

test_that("Large dataset handling works correctly", {
  # Test with larger dataset to trigger GPU path
  large_data <- data.frame(
    longitude = runif(15000, -180, 180),
    latitude = runif(15000, -90, 90),
    category = sample(c("A", "B", "C"), 15000, replace = TRUE),
    value = rnorm(15000)
  )
  
  # Should handle large dataset without errors
  expect_no_error({
    random_samples <- spatial_sample_random(large_data, n = 1000)
    expect_equal(nrow(random_samples), 1000)
  })
  
  expect_no_error({
    grid_samples <- spatial_sample_grid(large_data, grid_size = 10)
    expect_lt(nrow(grid_samples), nrow(large_data))
  })
  
  expect_no_error({
    stratified_samples <- spatial_sample_stratified(large_data, 
                                                     strata_column = "category",
                                                     n_per_stratum = 100)
    expect_lte(nrow(stratified_samples), 300)
  })
})

test_that("Bounds filtering works with edge cases", {
  # Test data at bounds edges
  test_data <- data.frame(
    longitude = c(-1, -0.5, 0, 0.5, 1),
    latitude = c(-1, -0.5, 0, 0.5, 1),
    value = 1:5
  )
  
  # Bounds that include some points
  bounds <- c(-0.6, -0.6, 0.6, 0.6)
  
  bounded_samples <- spatial_sample_random(test_data, n = 10, bounds = bounds)
  
  # Should only include points within bounds
  expect_true(all(bounded_samples$longitude >= bounds[1]))
  expect_true(all(bounded_samples$longitude <= bounds[3]))
  expect_true(all(bounded_samples$latitude >= bounds[2]))
  expect_true(all(bounded_samples$latitude <= bounds[4]))
  
  # Test with bounds that include no points
  empty_bounds <- c(10, 10, 11, 11)
  expect_error({
    spatial_sample_random(test_data, n = 1, bounds = empty_bounds)
  }, "No data points within specified bounds")
})

test_that("Stratified sampling handles uneven strata correctly", {
  # Create data with uneven strata distribution
  test_data <- data.frame(
    longitude = runif(100, -1, 1),
    latitude = runif(100, -1, 1),
    category = c(rep("A", 70), rep("B", 20), rep("C", 10)),
    value = rnorm(100)
  )
  
  # Request more samples than available in some strata
  expect_warning({
    stratified_samples <- spatial_sample_stratified(test_data, 
                                                     strata_column = "category",
                                                     n_per_stratum = 15)
  }, "Requested.*samples but only.*available")
  
  # Should still produce valid results
  expect_s3_class(stratified_samples, "data.frame")
  expect_gt(nrow(stratified_samples), 0)
  
  # Check strata distribution in results
  result_strata <- table(stratified_samples$category)
  expect_lte(result_strata[["A"]], 15)
  expect_lte(result_strata[["B"]], 15)
  expect_lte(result_strata[["C"]], 10)  # Limited by available data
})

test_that("Performance statistics are accurate", {
  engine <- create_sampling_engine()
  
  # Perform multiple operations
  test_data <- data.frame(
    longitude = runif(1000, -1, 1),
    latitude = runif(1000, -1, 1),
    category = sample(c("A", "B"), 1000, replace = TRUE)
  )
  
  # Multiple random samples
  for (i in 1:3) {
    spatial_sample_random(test_data, n = 100, engine = engine)
  }
  
  # Grid sampling
  spatial_sample_grid(test_data, grid_size = 0.2, engine = engine)
  
  # Stratified sampling
  spatial_sample_stratified(test_data, strata_column = "category", 
                            n_per_stratum = 50, engine = engine)
  
  # Check statistics
  stats <- get_sampling_performance(engine)
  expect_equal(stats$cpu_operations, 5)  # All should use CPU
  expect_equal(stats$gpu_operations, 0)
  expect_gt(stats$total_samples, 0)
  expect_gt(stats$avg_cpu_time, 0)
})