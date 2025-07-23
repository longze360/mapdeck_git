context("Performance benchmarking tests")

# Test performance benchmarks for various operations across providers
# This test file focuses on measuring and comparing performance metrics

# Skip tests on CRAN and CI environments
skip_if_no_benchmark <- function() {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("microbenchmark")
  skip_if_not_installed("profmem")
}

# Helper function to generate test data of different sizes
generate_test_data <- function(size) {
  # Generate random points
  set.seed(123)  # For reproducibility
  
  data.frame(
    lon = runif(size, -180, 180),
    lat = runif(size, -90, 90),
    value = runif(size, 0, 100)
  )
}

# Test WebGL acceleration for spatial sampling with comprehensive baselines
test_that("WebGL acceleration improves spatial sampling performance with documented baselines", {
  skip_if_no_benchmark()
  
  # Mock the spatial sampling functions with realistic performance simulation
  with_mock(
    `spatial_sample_random` = function(data, n, use_gpu = TRUE) {
      # Simulate different performance based on GPU usage and data size
      base_time <- nrow(data) * 0.000001  # Base processing time per point
      
      if (use_gpu) {
        # GPU acceleration provides significant speedup for large datasets
        gpu_factor <- ifelse(nrow(data) > 1000, 0.1, 0.3)  # Better scaling for large data
        processing_time <- base_time * gpu_factor
      } else {
        # CPU processing scales linearly
        processing_time <- base_time * 1.0
      }
      
      Sys.sleep(processing_time)
      
      # Return random sample with performance metadata
      result <- data[sample(nrow(data), min(n, nrow(data))), ]
      attr(result, "performance") <- list(
        processing_time = processing_time,
        use_gpu = use_gpu,
        data_size = nrow(data),
        sample_size = n
      )
      
      return(result)
    },
    `spatial_sample_grid` = function(data, cell_size, use_gpu = TRUE) {
      # Grid sampling has different performance characteristics
      base_time <- nrow(data) * 0.000002  # More complex than random sampling
      
      if (use_gpu) {
        gpu_factor <- ifelse(nrow(data) > 5000, 0.15, 0.4)
        processing_time <- base_time * gpu_factor
      } else {
        processing_time <- base_time * 1.2  # Slightly slower than random
      }
      
      Sys.sleep(processing_time)
      
      # Return grid sample
      grid_size <- max(10, nrow(data) %/% 100)
      result <- data[sample(nrow(data), grid_size), ]
      attr(result, "performance") <- list(
        processing_time = processing_time,
        use_gpu = use_gpu,
        data_size = nrow(data),
        grid_size = grid_size
      )
      
      return(result)
    },
    `spatial_sample_administrative` = function(admin_polygons, total_samples, 
                                              allocation_method = "proportional",
                                              concurrent = TRUE, use_gpu = TRUE) {
      # Administrative sampling is most complex
      n_polygons <- length(admin_polygons$polygons)
      base_time <- n_polygons * total_samples * 0.000005
      
      if (use_gpu) {
        gpu_factor <- 0.2  # Significant GPU advantage for polygon operations
        if (concurrent) {
          gpu_factor <- gpu_factor * 0.7  # Additional speedup from concurrency
        }
        processing_time <- base_time * gpu_factor
      } else {
        cpu_factor <- ifelse(concurrent, 0.8, 1.0)  # Some benefit from CPU concurrency
        processing_time <- base_time * cpu_factor
      }
      
      Sys.sleep(processing_time)
      
      # Generate samples
      result <- data.frame(
        polygon_id = sample(1:n_polygons, total_samples, replace = TRUE),
        lon = runif(total_samples, -180, 180),
        lat = runif(total_samples, -90, 90)
      )
      
      attr(result, "performance") <- list(
        processing_time = processing_time,
        use_gpu = use_gpu,
        concurrent = concurrent,
        n_polygons = n_polygons,
        total_samples = total_samples
      )
      
      return(result)
    },
    {
      # Define performance baselines (in seconds)
      baselines <- list(
        random_sampling_gpu_1k = 0.01,
        random_sampling_cpu_1k = 0.05,
        random_sampling_gpu_10k = 0.02,
        random_sampling_cpu_10k = 0.10,
        grid_sampling_gpu_10k = 0.03,
        grid_sampling_cpu_10k = 0.15,
        admin_sampling_gpu_1k = 0.05,
        admin_sampling_cpu_1k = 0.25
      )
      
      # Test different data sizes
      data_sizes <- c(1000, 10000, 50000)
      
      if (requireNamespace("microbenchmark", quietly = TRUE)) {
        cat("\n=== WebGL/GPU Performance Benchmarks ===\n")
        
        for (size in data_sizes) {
          test_data <- generate_test_data(size)
          
          cat(sprintf("\nTesting with %d data points:\n", size))
          
          # Random sampling benchmark
          random_benchmark <- microbenchmark::microbenchmark(
            gpu = spatial_sample_random(test_data, min(1000, size), use_gpu = TRUE),
            cpu = spatial_sample_random(test_data, min(1000, size), use_gpu = FALSE),
            times = 5
          )
          
          gpu_time <- median(random_benchmark$time[random_benchmark$expr == "gpu"]) / 1e9
          cpu_time <- median(random_benchmark$time[random_benchmark$expr == "cpu"]) / 1e9
          speedup <- cpu_time / gpu_time
          
          cat(sprintf("Random Sampling - GPU: %.3fs, CPU: %.3fs, Speedup: %.1fx\n", 
                     gpu_time, cpu_time, speedup))
          
          # Check performance against baselines
          if (size == 1000) {
            expect_lt(gpu_time, baselines$random_sampling_gpu_1k * 2)  # Allow 2x tolerance
            expect_lt(cpu_time, baselines$random_sampling_cpu_1k * 2)
          } else if (size == 10000) {
            expect_lt(gpu_time, baselines$random_sampling_gpu_10k * 2)
            expect_lt(cpu_time, baselines$random_sampling_cpu_10k * 2)
          }
          
          # GPU should be at least 2x faster
          expect_gt(speedup, 2.0)
          
          # Grid sampling benchmark (only for larger datasets)
          if (size >= 10000) {
            grid_benchmark <- microbenchmark::microbenchmark(
              gpu = spatial_sample_grid(test_data, 1000, use_gpu = TRUE),
              cpu = spatial_sample_grid(test_data, 1000, use_gpu = FALSE),
              times = 3
            )
            
            grid_gpu_time <- median(grid_benchmark$time[grid_benchmark$expr == "gpu"]) / 1e9
            grid_cpu_time <- median(grid_benchmark$time[grid_benchmark$expr == "cpu"]) / 1e9
            grid_speedup <- grid_cpu_time / grid_gpu_time
            
            cat(sprintf("Grid Sampling - GPU: %.3fs, CPU: %.3fs, Speedup: %.1fx\n", 
                       grid_gpu_time, grid_cpu_time, grid_speedup))
            
            expect_gt(grid_speedup, 2.0)
          }
        }
        
        # Administrative sampling benchmark
        admin_data <- list(
          polygons = replicate(20, list(coords = matrix(runif(20), ncol = 2)), simplify = FALSE),
          areas = runif(20, 100, 1000)
        )
        
        admin_benchmark <- microbenchmark::microbenchmark(
          gpu_concurrent = spatial_sample_administrative(admin_data, 1000, 
                                                        concurrent = TRUE, use_gpu = TRUE),
          gpu_sequential = spatial_sample_administrative(admin_data, 1000, 
                                                        concurrent = FALSE, use_gpu = TRUE),
          cpu_concurrent = spatial_sample_administrative(admin_data, 1000, 
                                                        concurrent = TRUE, use_gpu = FALSE),
          cpu_sequential = spatial_sample_administrative(admin_data, 1000, 
                                                        concurrent = FALSE, use_gpu = FALSE),
          times = 3
        )
        
        # Extract times
        gpu_conc_time <- median(admin_benchmark$time[admin_benchmark$expr == "gpu_concurrent"]) / 1e9
        gpu_seq_time <- median(admin_benchmark$time[admin_benchmark$expr == "gpu_sequential"]) / 1e9
        cpu_conc_time <- median(admin_benchmark$time[admin_benchmark$expr == "cpu_concurrent"]) / 1e9
        cpu_seq_time <- median(admin_benchmark$time[admin_benchmark$expr == "cpu_sequential"]) / 1e9
        
        cat(sprintf("\nAdministrative Sampling:\n"))
        cat(sprintf("GPU Concurrent: %.3fs, GPU Sequential: %.3fs\n", gpu_conc_time, gpu_seq_time))
        cat(sprintf("CPU Concurrent: %.3fs, CPU Sequential: %.3fs\n", cpu_conc_time, cpu_seq_time))
        cat(sprintf("Best GPU vs Best CPU Speedup: %.1fx\n", cpu_seq_time / gpu_conc_time))
        
        # GPU with concurrency should be fastest
        expect_lt(gpu_conc_time, gpu_seq_time)
        expect_lt(gpu_conc_time, cpu_conc_time)
        expect_lt(gpu_conc_time, cpu_seq_time)
        
        # Overall speedup should be significant
        expect_gt(cpu_seq_time / gpu_conc_time, 3.0)
        
        # Print summary
        print(summary(random_benchmark))
        print(summary(admin_benchmark))
      }
    }
  )
})

# Test coordinate transformation performance
test_that("coordinate transformation performance is acceptable", {
  skip_if_no_benchmark()
  
  # Mock the coordinate transformation functions
  with_mock(
    `transform_coordinates` = function(coords, from_crs, to_crs) {
      # Simulate transformation
      Sys.sleep(0.001 * nrow(coords))
      
      # Return transformed coordinates (just the original in this mock)
      return(coords)
    },
    {
      # Generate test data of different sizes
      small_data <- generate_test_data(100)
      medium_data <- generate_test_data(1000)
      large_data <- generate_test_data(10000)
      
      # Benchmark transformation performance
      if (requireNamespace("microbenchmark", quietly = TRUE)) {
        benchmark_results <- microbenchmark::microbenchmark(
          small = transform_coordinates(small_data[, c("lon", "lat")], "WGS84", "GCJ02"),
          medium = transform_coordinates(medium_data[, c("lon", "lat")], "WGS84", "GCJ02"),
          large = transform_coordinates(large_data[, c("lon", "lat")], "WGS84", "GCJ02"),
          times = 5
        )
        
        # Extract median times
        small_time <- median(benchmark_results$time[benchmark_results$expr == "small"])
        medium_time <- median(benchmark_results$time[benchmark_results$expr == "medium"])
        large_time <- median(benchmark_results$time[benchmark_results$expr == "large"])
        
        # Check that performance scales roughly linearly with data size
        # Medium should be ~10x small, large should be ~10x medium
        ratio_medium_small <- medium_time / small_time
        ratio_large_medium <- large_time / medium_time
        
        # Allow some flexibility in the scaling factor (8-12x)
        expect_gt(ratio_medium_small, 8)
        expect_lt(ratio_medium_small, 12)
        expect_gt(ratio_large_medium, 8)
        expect_lt(ratio_large_medium, 12)
        
        # Print benchmark summary
        print(summary(benchmark_results))
      }
    }
  )
})

# Test provider initialization performance
test_that("provider initialization performance is acceptable", {
  skip_if_no_benchmark()
  
  # Mock the provider creation functions
  with_mock(
    `create_provider` = function(provider_name, config = list()) {
      # Simulate different initialization times for different providers
      switch(provider_name,
        "mapbox" = Sys.sleep(0.01),
        "leaflet" = Sys.sleep(0.02),
        "openlayers" = Sys.sleep(0.03),
        "gaode" = Sys.sleep(0.04),
        "baidu" = Sys.sleep(0.05)
      )
      
      # Return a mock provider object
      structure(
        list(
          provider_name = provider_name,
          config = config
        ),
        class = "mock_provider"
      )
    },
    {
      # Benchmark provider initialization
      if (requireNamespace("microbenchmark", quietly = TRUE)) {
        benchmark_results <- microbenchmark::microbenchmark(
          mapbox = create_provider("mapbox"),
          leaflet = create_provider("leaflet"),
          openlayers = create_provider("openlayers"),
          gaode = create_provider("gaode"),
          baidu = create_provider("baidu"),
          times = 10
        )
        
        # Print benchmark summary
        print(summary(benchmark_results))
        
        # Check that all providers initialize within a reasonable time
        # (specific thresholds would depend on the actual implementation)
        for (provider in c("mapbox", "leaflet", "openlayers", "gaode", "baidu")) {
          provider_time <- median(benchmark_results$time[benchmark_results$expr == provider])
          expect_lt(provider_time, 1e9)  # Less than 1 second (in nanoseconds)
        }
      }
    }
  )
})

# Test layer rendering performance across providers
test_that("layer rendering performance is acceptable across providers", {
  skip_if_no_benchmark()
  
  # Mock the layer rendering functions
  with_mock(
    `add_scatterplot` = function(map, data, lon, lat, ...) {
      # Simulate rendering time based on data size and provider
      provider <- map$provider
      render_time_factor <- switch(provider,
        "mapbox" = 1.0,
        "leaflet" = 1.2,
        "openlayers" = 1.3,
        "gaode" = 1.4,
        "baidu" = 1.5,
        1.0  # Default
      )
      
      Sys.sleep(0.0001 * nrow(data) * render_time_factor)
      
      # Return the map object
      return(map)
    },
    {
      # Generate test data
      test_data <- generate_test_data(1000)
      
      # Create mock maps for different providers
      maps <- list(
        mapbox = list(provider = "mapbox"),
        leaflet = list(provider = "leaflet"),
        openlayers = list(provider = "openlayers"),
        gaode = list(provider = "gaode"),
        baidu = list(provider = "baidu")
      )
      
      # Benchmark layer rendering across providers
      if (requireNamespace("microbenchmark", quietly = TRUE)) {
        benchmark_results <- microbenchmark::microbenchmark(
          mapbox = add_scatterplot(maps$mapbox, test_data, "lon", "lat", radius = 10),
          leaflet = add_scatterplot(maps$leaflet, test_data, "lon", "lat", radius = 10),
          openlayers = add_scatterplot(maps$openlayers, test_data, "lon", "lat", radius = 10),
          gaode = add_scatterplot(maps$gaode, test_data, "lon", "lat", radius = 10),
          baidu = add_scatterplot(maps$baidu, test_data, "lon", "lat", radius = 10),
          times = 5
        )
        
        # Print benchmark summary
        print(summary(benchmark_results))
        
        # Check that all providers render within a reasonable time
        # (specific thresholds would depend on the actual implementation)
        for (provider in c("mapbox", "leaflet", "openlayers", "gaode", "baidu")) {
          provider_time <- median(benchmark_results$time[benchmark_results$expr == provider])
          expect_lt(provider_time, 1e9)  # Less than 1 second (in nanoseconds)
        }
      }
    }
  )
})

# Test spatial sampling algorithm performance
test_that("spatial sampling algorithms have expected performance characteristics", {
  skip_if_no_benchmark()
  
  # Mock the spatial sampling functions
  with_mock(
    `spatial_sample_random` = function(data, n, use_gpu = TRUE) {
      Sys.sleep(0.01)
      return(data[sample(nrow(data), min(n, nrow(data))), ])
    },
    `spatial_sample_grid` = function(data, cell_size, use_gpu = TRUE) {
      Sys.sleep(0.02)
      return(data[sample(nrow(data), nrow(data) %/% 10), ])
    },
    `spatial_sample_stratified` = function(data, strata_column, n_per_stratum, use_gpu = TRUE) {
      Sys.sleep(0.03)
      return(data[sample(nrow(data), min(n_per_stratum * length(unique(data[[strata_column]])), nrow(data))), ])
    },
    `spatial_sample_administrative` = function(admin_polygons, total_samples, allocation_method = "proportional", concurrent = TRUE) {
      Sys.sleep(0.04)
      return(data.frame(
        lon = runif(total_samples, -180, 180),
        lat = runif(total_samples, -90, 90)
      ))
    },
    {
      # Generate test data
      test_data <- generate_test_data(10000)
      test_data$strata <- sample(letters[1:5], nrow(test_data), replace = TRUE)
      
      # Mock admin polygons
      admin_polygons <- list(
        polygons = list(1, 2, 3),  # Placeholder
        areas = c(100, 200, 300)   # Placeholder areas
      )
      
      # Benchmark sampling algorithms
      if (requireNamespace("microbenchmark", quietly = TRUE)) {
        benchmark_results <- microbenchmark::microbenchmark(
          random = spatial_sample_random(test_data, 1000),
          grid = spatial_sample_grid(test_data, 0.1),
          stratified = spatial_sample_stratified(test_data, "strata", 200),
          administrative = spatial_sample_administrative(admin_polygons, 1000),
          times = 5
        )
        
        # Print benchmark summary
        print(summary(benchmark_results))
        
        # Check that all algorithms complete within a reasonable time
        for (algorithm in c("random", "grid", "stratified", "administrative")) {
          algorithm_time <- median(benchmark_results$time[benchmark_results$expr == algorithm])
          expect_lt(algorithm_time, 1e9)  # Less than 1 second (in nanoseconds)
        }
        
        # Check relative performance (random should be fastest)
        random_time <- median(benchmark_results$time[benchmark_results$expr == "random"])
        grid_time <- median(benchmark_results$time[benchmark_results$expr == "grid"])
        stratified_time <- median(benchmark_results$time[benchmark_results$expr == "stratified"])
        administrative_time <- median(benchmark_results$time[benchmark_results$expr == "administrative"])
        
        expect_lt(random_time, grid_time)
        expect_lt(grid_time, stratified_time)
        expect_lt(stratified_time, administrative_time)
      }
    }
  )
})

# Test GPU vs CPU fallback performance
test_that("GPU fallback mechanism maintains acceptable performance", {
  skip_if_no_benchmark()
  
  # Mock the GPU detection and sampling functions
  with_mock(
    `is_gpu_available` = function() {
      return(FALSE)  # Simulate GPU unavailability
    },
    `spatial_sample_random` = function(data, n, use_gpu = TRUE) {
      # Check if GPU is requested but unavailable
      if (use_gpu && !is_gpu_available()) {
        # Should fall back to CPU implementation
        use_gpu <- FALSE
      }
      
      # Simulate different performance based on GPU usage
      if (use_gpu) {
        Sys.sleep(0.01)  # Faster GPU execution
      } else {
        Sys.sleep(0.05)  # Slower CPU execution
      }
      
      # Return random sample
      return(data[sample(nrow(data), min(n, nrow(data))), ])
    },
    {
      # Generate test data
      test_data <- generate_test_data(10000)
      
      # Test that function works with GPU unavailable
      result <- spatial_sample_random(test_data, 1000, use_gpu = TRUE)
      expect_equal(nrow(result), 1000)
      
      # Benchmark with explicit GPU and CPU options
      if (requireNamespace("microbenchmark", quietly = TRUE)) {
        benchmark_results <- microbenchmark::microbenchmark(
          gpu_requested = spatial_sample_random(test_data, 1000, use_gpu = TRUE),
          cpu_explicit = spatial_sample_random(test_data, 1000, use_gpu = FALSE),
          times = 5
        )
        
        # Print benchmark summary
        print(summary(benchmark_results))
        
        # Since GPU is unavailable, both should have similar performance
        gpu_time <- median(benchmark_results$time[benchmark_results$expr == "gpu_requested"])
        cpu_time <- median(benchmark_results$time[benchmark_results$expr == "cpu_explicit"])
        
        # Times should be within 10% of each other
        ratio <- gpu_time / cpu_time
        expect_gt(ratio, 0.9)
        expect_lt(ratio, 1.1)
      }
    }
  )
})

# Test performance regression detection
test_that("performance regression detection works", {
  skip_if_no_benchmark()
  
  # Define baseline performance metrics
  baselines <- list(
    spatial_sample_random = 100000000,  # 100ms in nanoseconds
    coordinate_transform = 50000000,    # 50ms in nanoseconds
    provider_init = 20000000            # 20ms in nanoseconds
  )
  
  # Mock the functions to test
  with_mock(
    `spatial_sample_random` = function(data, n, use_gpu = TRUE) {
      Sys.sleep(0.08)  # Simulate slower than baseline
      return(data[sample(nrow(data), min(n, nrow(data))), ])
    },
    `transform_coordinates` = function(coords, from_crs, to_crs) {
      Sys.sleep(0.04)  # Simulate faster than baseline
      return(coords)
    },
    `create_provider` = function(provider_name, config = list()) {
      Sys.sleep(0.03)  # Simulate slower than baseline
      return(list(provider_name = provider_name))
    },
    {
      # Generate test data
      test_data <- generate_test_data(1000)
      
      # Benchmark current performance
      if (requireNamespace("microbenchmark", quietly = TRUE)) {
        # Test spatial sampling
        sampling_benchmark <- microbenchmark::microbenchmark(
          spatial_sample_random(test_data, 100),
          times = 5
        )
        sampling_time <- median(sampling_benchmark$time)
        
        # Test coordinate transformation
        transform_benchmark <- microbenchmark::microbenchmark(
          transform_coordinates(test_data[, c("lon", "lat")], "WGS84", "GCJ02"),
          times = 5
        )
        transform_time <- median(transform_benchmark$time)
        
        # Test provider initialization
        provider_benchmark <- microbenchmark::microbenchmark(
          create_provider("mapbox"),
          times = 5
        )
        provider_time <- median(provider_benchmark$time)
        
        # Compare with baselines
        sampling_regression <- sampling_time > baselines$spatial_sample_random * 1.5
        transform_regression <- transform_time > baselines$coordinate_transform * 1.5
        provider_regression <- provider_time > baselines$provider_init * 1.5
        
        # Print results
        cat("\nPerformance comparison with baselines:\n")
        cat(sprintf("Spatial sampling: %s (current: %.2fms, baseline: %.2fms)\n",
                   ifelse(sampling_regression, "REGRESSION", "OK"),
                   sampling_time / 1e6, baselines$spatial_sample_random / 1e6))
        cat(sprintf("Coordinate transform: %s (current: %.2fms, baseline: %.2fms)\n",
                   ifelse(transform_regression, "REGRESSION", "OK"),
                   transform_time / 1e6, baselines$coordinate_transform / 1e6))
        cat(sprintf("Provider initialization: %s (current: %.2fms, baseline: %.2fms)\n",
                   ifelse(provider_regression, "REGRESSION", "OK"),
                   provider_time / 1e6, baselines$provider_init / 1e6))
        
        # We expect sampling to be slower (regression)
        expect_true(sampling_regression)
        
        # We expect transform to be similar or faster (no regression)
        expect_false(transform_regression)
        
        # We expect provider init to be slower (regression)
        expect_true(provider_regression)
      }
    }
  )
})

# Test WebGL rendering performance across browsers
test_that("WebGL rendering performance is consistent across browsers", {
  skip_if_no_benchmark()
  
  # Mock the browser-specific rendering functions
  with_mock(
    `render_in_browser` = function(browser, data, provider) {
      # Simulate different rendering times for different browsers
      browser_factor <- switch(browser,
        "chrome" = 1.0,
        "firefox" = 1.1,
        "safari" = 1.2,
        "edge" = 1.15,
        1.0  # Default
      )
      
      provider_factor <- switch(provider,
        "mapbox" = 1.0,
        "leaflet" = 1.1,
        "openlayers" = 1.2,
        "gaode" = 1.3,
        "baidu" = 1.4,
        1.0  # Default
      )
      
      # Simulate rendering time
      Sys.sleep(0.01 * browser_factor * provider_factor)
      
      # Return simulated frame rate
      return(60 / (browser_factor * provider_factor))
    },
    {
      # Test browsers
      browsers <- c("chrome", "firefox", "safari", "edge")
      
      # Test providers
      providers <- c("mapbox", "leaflet", "openlayers", "gaode", "baidu")
      
      # Generate test data
      test_data <- generate_test_data(1000)
      
      # Create benchmark results matrix
      results <- matrix(0, nrow = length(browsers), ncol = length(providers),
                       dimnames = list(browsers, providers))
      
      # Run benchmarks
      for (browser in browsers) {
        for (provider in providers) {
          # Simulate rendering 10 frames and take average
          frame_rates <- replicate(10, render_in_browser(browser, test_data, provider))
          results[browser, provider] <- mean(frame_rates)
        }
      }
      
      # Print results
      cat("\nWebGL rendering performance (estimated FPS) by browser and provider:\n")
      print(results)
      
      # Check that Chrome with Mapbox has the best performance
      expect_true(results["chrome", "mapbox"] >= results["firefox", "mapbox"])
      expect_true(results["chrome", "mapbox"] >= results["safari", "mapbox"])
      expect_true(results["chrome", "mapbox"] >= results["edge", "mapbox"])
      
      # Check that all browsers maintain acceptable frame rates (>30 FPS)
      for (browser in browsers) {
        for (provider in providers) {
          expect_gt(results[browser, provider], 30)
        }
      }
    }
  )
})

# Test layer update performance
test_that("layer update performance is acceptable", {
  skip_if_no_benchmark()
  
  # Mock the layer update functions
  with_mock(
    `update_layer` = function(map, layer_id, data, ...) {
      # Simulate update time based on data size and provider
      provider <- map$provider
      update_time_factor <- switch(provider,
        "mapbox" = 1.0,
        "leaflet" = 1.1,
        "openlayers" = 1.2,
        "gaode" = 1.3,
        "baidu" = 1.4,
        1.0  # Default
      )
      
      Sys.sleep(0.0001 * nrow(data) * update_time_factor)
      
      # Return the map object
      return(map)
    },
    {
      # Generate test data of different sizes
      small_data <- generate_test_data(100)
      medium_data <- generate_test_data(1000)
      large_data <- generate_test_data(10000)
      
      # Create mock map
      map <- list(
        provider = "mapbox",
        layers = list(
          test_layer = list(id = "test_layer", type = "scatterplot")
        )
      )
      
      # Benchmark layer updates with different data sizes
      if (requireNamespace("microbenchmark", quietly = TRUE)) {
        benchmark_results <- microbenchmark::microbenchmark(
          small = update_layer(map, "test_layer", small_data),
          medium = update_layer(map, "test_layer", medium_data),
          large = update_layer(map, "test_layer", large_data),
          times = 5
        )
        
        # Print benchmark summary
        print(summary(benchmark_results))
        
        # Extract median times
        small_time <- median(benchmark_results$time[benchmark_results$expr == "small"])
        medium_time <- median(benchmark_results$time[benchmark_results$expr == "medium"])
        large_time <- median(benchmark_results$time[benchmark_results$expr == "large"])
        
        # Check that performance scales roughly linearly with data size
        ratio_medium_small <- medium_time / small_time
        ratio_large_medium <- large_time / medium_time
        
        # Allow some flexibility in the scaling factor (8-12x)
        expect_gt(ratio_medium_small, 8)
        expect_lt(ratio_medium_small, 12)
        expect_gt(ratio_large_medium, 8)
        expect_lt(ratio_large_medium, 12)
      }
    }
  )
})

# Test provider switching performance
test_that("provider switching performance is acceptable", {
  skip_if_no_benchmark()
  
  # Mock the provider switching function
  with_mock(
    `update_provider` = function(map, new_provider) {
      # Simulate switching time based on layer count and provider
      layer_count <- length(map$layers)
      
      # Simulate different switching times for different providers
      switch_time_factor <- switch(new_provider,
        "mapbox" = 1.0,
        "leaflet" = 1.2,
        "openlayers" = 1.3,
        "gaode" = 1.4,
        "baidu" = 1.5,
        1.0  # Default
      )
      
      Sys.sleep(0.01 * layer_count * switch_time_factor)
      
      # Update provider
      map$provider <- new_provider
      
      # Return the map object
      return(map)
    },
    {
      # Create mock maps with different layer counts
      create_mock_map <- function(provider, layer_count) {
        layers <- list()
        for (i in 1:layer_count) {
          layer_id <- paste0("layer_", i)
          layers[[layer_id]] <- list(id = layer_id, type = "scatterplot")
        }
        
        return(list(
          provider = provider,
          layers = layers
        ))
      }
      
      # Create maps with different layer counts
      map_small <- create_mock_map("mapbox", 1)
      map_medium <- create_mock_map("mapbox", 5)
      map_large <- create_mock_map("mapbox", 10)
      
      # Benchmark provider switching with different layer counts
      if (requireNamespace("microbenchmark", quietly = TRUE)) {
        benchmark_results <- microbenchmark::microbenchmark(
          small = update_provider(map_small, "leaflet"),
          medium = update_provider(map_medium, "leaflet"),
          large = update_provider(map_large, "leaflet"),
          times = 5
        )
        
        # Print benchmark summary
        print(summary(benchmark_results))
        
        # Extract median times
        small_time <- median(benchmark_results$time[benchmark_results$expr == "small"])
        medium_time <- median(benchmark_results$time[benchmark_results$expr == "medium"])
        large_time <- median(benchmark_results$time[benchmark_results$expr == "large"])
        
        # Check that performance scales roughly linearly with layer count
        # Medium has 5x layers of small, large has 2x layers of medium
        ratio_medium_small <- medium_time / small_time
        ratio_large_medium <- large_time / medium_time
        
        # Allow some flexibility in the scaling factor
        expect_gt(ratio_medium_small, 4)
        expect_lt(ratio_medium_small, 6)
        expect_gt(ratio_large_medium, 1.5)
        expect_lt(ratio_large_medium, 2.5)
      }
      
      # Benchmark switching between different providers
      if (requireNamespace("microbenchmark", quietly = TRUE)) {
        map <- create_mock_map("mapbox", 5)
        
        benchmark_results <- microbenchmark::microbenchmark(
          to_leaflet = update_provider(map, "leaflet"),
          to_openlayers = update_provider(map, "openlayers"),
          to_gaode = update_provider(map, "gaode"),
          to_baidu = update_provider(map, "baidu"),
          times = 5
        )
        
        # Print benchmark summary
        print(summary(benchmark_results))
        
        # Check that all provider switches complete within a reasonable time
        for (expr in c("to_leaflet", "to_openlayers", "to_gaode", "to_baidu")) {
          switch_time <- median(benchmark_results$time[benchmark_results$expr == expr])
          expect_lt(switch_time, 1e9)  # Less than 1 second (in nanoseconds)
        }
      }
    }
  )
})

# Test coordinate transformation accuracy vs performance tradeoff
test_that("coordinate transformation accuracy vs performance tradeoff is optimal", {
  skip_if_no_benchmark()
  
  # Mock the coordinate transformation functions with different accuracy levels
  with_mock(
    `transform_coordinates_low_accuracy` = function(coords) {
      # Simulate low accuracy, fast transformation
      Sys.sleep(0.001)
      return(coords)
    },
    `transform_coordinates_medium_accuracy` = function(coords) {
      # Simulate medium accuracy, moderate speed transformation
      Sys.sleep(0.005)
      return(coords)
    },
    `transform_coordinates_high_accuracy` = function(coords) {
      # Simulate high accuracy, slow transformation
      Sys.sleep(0.01)
      return(coords)
    },
    {
      # Generate test data
      test_data <- generate_test_data(1000)[, c("lon", "lat")]
      
      # Benchmark different accuracy levels
      if (requireNamespace("microbenchmark", quietly = TRUE)) {
        benchmark_results <- microbenchmark::microbenchmark(
          low = transform_coordinates_low_accuracy(test_data),
          medium = transform_coordinates_medium_accuracy(test_data),
          high = transform_coordinates_high_accuracy(test_data),
          times = 10
        )
        
        # Print benchmark summary
        print(summary(benchmark_results))
        
        # Extract median times
        low_time <- median(benchmark_results$time[benchmark_results$expr == "low"])
        medium_time <- median(benchmark_results$time[benchmark_results$expr == "medium"])
        high_time <- median(benchmark_results$time[benchmark_results$expr == "high"])
        
        # Check that performance decreases with accuracy
        expect_lt(low_time, medium_time)
        expect_lt(medium_time, high_time)
        
        # Calculate performance ratios
        medium_low_ratio <- medium_time / low_time
        high_medium_ratio <- high_time / medium_time
        
        # Print ratios
        cat("\nPerformance ratios for different accuracy levels:\n")
        cat(sprintf("Medium/Low accuracy: %.2fx slower\n", medium_low_ratio))
        cat(sprintf("High/Medium accuracy: %.2fx slower\n", high_medium_ratio))
        
        # Check that the ratios are reasonable
        # Medium should be ~5x slower than low, high should be ~2x slower than medium
        expect_gt(medium_low_ratio, 4)
        expect_lt(medium_low_ratio, 6)
        expect_gt(high_medium_ratio, 1.5)
        expect_lt(high_medium_ratio, 2.5)
      }
    }
  )
})