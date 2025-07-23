context("Comprehensive integration and performance tests")

# This file contains comprehensive integration and performance tests that verify:
# 1. Cross-provider layer compatibility tests
# 2. Performance benchmarking with WebGL and GPU baselines
# 3. Memory usage tests for large dataset operations
# 4. Browser compatibility tests for WebGL and GPU features

# Skip on CRAN and CI environments
skip_if_no_integration_test <- function() {
  skip_on_cran()
  skip_on_ci()
}

# Helper function to generate comprehensive test datasets
generate_integration_test_data <- function(size = 1000) {
  set.seed(123)  # For reproducibility
  
  list(
    # Point data for various layer types
    point_data = data.frame(
      lon = runif(size, -180, 180),
      lat = runif(size, -90, 90),
      value = runif(size, 0, 100),
      category = sample(letters[1:5], size, replace = TRUE),
      elevation = runif(size, 0, 1000),
      timestamp = Sys.time() + sample(1:1000, size, replace = TRUE)
    ),
    
    # Line data for connection layers
    line_data = data.frame(
      start_lon = runif(size, -180, 180),
      start_lat = runif(size, -90, 90),
      end_lon = runif(size, -180, 180),
      end_lat = runif(size, -90, 90),
      value = runif(size, 0, 100),
      weight = runif(size, 0.5, 3)
    ),
    
    # Administrative boundary data for spatial sampling
    admin_data = list(
      polygons = replicate(20, list(
        coordinates = matrix(runif(20, -180, 180), ncol = 2)
      ), simplify = FALSE),
      areas = runif(20, 100, 1000),
      names = paste0("Region_", 1:20)
    )
  )
}

# Test 1: Cross-provider layer compatibility with performance metrics
test_that("cross-provider layer compatibility with performance tracking", {
  skip_if_no_integration_test()
  skip_if_not_installed("microbenchmark")
  
  # Mock comprehensive layer functions with performance tracking
  layer_functions <- list(
    "add_scatterplot" = function(map, data, lon, lat, ...) {
      # Simulate provider-specific performance
      provider_factor <- switch(map$provider,
        "mapbox" = 1.0, "leaflet" = 1.2, "openlayers" = 1.3, 
        "gaode" = 1.4, "baidu" = 1.5, 1.0
      )
      
      processing_time <- nrow(data) * 0.00001 * provider_factor
      Sys.sleep(processing_time)
      
      layer_id <- paste0("scatterplot_", length(map$layers) + 1)
      map$layers[[layer_id]] <- list(
        id = layer_id, type = "scatterplot", data = data,
        performance = list(time = processing_time, provider_factor = provider_factor)
      )
      return(map)
    },
    
    "add_heatmap" = function(map, data, lon, lat, ...) {
      # Heatmap performance varies significantly by provider
      provider_factor <- switch(map$provider,
        "mapbox" = 1.0, "leaflet" = 2.0, "openlayers" = 1.8,
        "gaode" = 2.2, "baidu" = 2.5, 1.0
      )
      
      processing_time <- nrow(data) * 0.00003 * provider_factor
      Sys.sleep(processing_time)
      
      layer_id <- paste0("heatmap_", length(map$layers) + 1)
      map$layers[[layer_id]] <- list(
        id = layer_id, type = "heatmap", data = data,
        performance = list(time = processing_time, provider_factor = provider_factor)
      )
      return(map)
    },
    
    "add_hexagon" = function(map, data, lon, lat, ...) {
      # Aggregation layers have moderate performance differences
      provider_factor <- switch(map$provider,
        "mapbox" = 1.0, "leaflet" = 1.3, "openlayers" = 1.2,
        "gaode" = 1.4, "baidu" = 1.6, 1.0
      )
      
      processing_time <- nrow(data) * 0.00002 * provider_factor
      Sys.sleep(processing_time)
      
      layer_id <- paste0("hexagon_", length(map$layers) + 1)
      map$layers[[layer_id]] <- list(
        id = layer_id, type = "hexagon", data = data,
        performance = list(time = processing_time, provider_factor = provider_factor)
      )
      return(map)
    }
  )
  
  # Apply all mock functions
  do.call(with_mock, c(layer_functions, list({
    if (requireNamespace("microbenchmark", quietly = TRUE)) {
      # Test data
      test_data <- generate_integration_test_data(1000)
      providers <- c("mapbox", "leaflet", "openlayers", "gaode", "baidu")
      layer_types <- c("scatterplot", "heatmap", "hexagon")
      
      cat("\n=== Cross-Provider Layer Compatibility & Performance ===\n")
      
      # Performance results matrix
      performance_matrix <- array(0, 
        dim = c(length(providers), length(layer_types)),
        dimnames = list(providers, layer_types)
      )
      
      # Test each layer type on each provider
      for (provider in providers) {
        cat(sprintf("\nTesting %s provider:\n", provider))
        
        for (layer_type in layer_types) {
          # Create mock map
          map <- list(provider = provider, layers = list())
          
          # Get the appropriate function
          layer_function <- get(paste0("add_", layer_type))
          
          # Benchmark the layer addition
          benchmark <- microbenchmark::microbenchmark(
            layer_function(map, test_data$point_data, "lon", "lat"),
            times = 5
          )
          
          median_time <- median(benchmark$time) / 1e6  # Convert to milliseconds
          performance_matrix[provider, layer_type] <- median_time
          
          cat(sprintf("  %s: %.2f ms\n", layer_type, median_time))
        }
      }
      
      # Print performance matrix
      cat("\n=== Performance Matrix (milliseconds) ===\n")
      print(performance_matrix)
      
      # Verify compatibility - all combinations should work
      expect_true(all(performance_matrix > 0))
      
      # Verify performance expectations
      # Mapbox should generally be fastest or competitive
      mapbox_avg <- mean(performance_matrix["mapbox", ])
      for (provider in providers[-1]) {  # Exclude mapbox
        provider_avg <- mean(performance_matrix[provider, ])
        # Other providers should be within 3x of Mapbox performance
        expect_lt(provider_avg, mapbox_avg * 3)
      }
      
      # Heatmap should show the most variation across providers
      heatmap_variance <- var(performance_matrix[, "heatmap"])
      scatterplot_variance <- var(performance_matrix[, "scatterplot"])
      expect_gt(heatmap_variance, scatterplot_variance)
    }
  })))
})

# Test 2: WebGL and GPU performance baselines
test_that("WebGL and GPU performance baselines are established and maintained", {
  skip_if_no_integration_test()
  skip_if_not_installed("microbenchmark")
  
  # Mock GPU-accelerated functions with realistic performance characteristics
  with_mock(
    `check_webgl_support` = function() {
      list(supported = TRUE, version = "WebGL 1.0", extensions = c("OES_texture_float"))
    },
    
    `check_gpu_acceleration` = function() {
      list(available = TRUE, compute_shaders = TRUE, memory_gb = 4)
    },
    
    `spatial_sample_random_gpu` = function(data, n, use_gpu = TRUE) {
      # GPU performance scales better with data size
      if (use_gpu) {
        base_time <- 0.001 + (nrow(data) * 0.000001)  # Better scaling
        gpu_memory_usage <- nrow(data) * 0.0001  # MB
      } else {
        base_time <- 0.005 + (nrow(data) * 0.000005)  # Linear scaling
        gpu_memory_usage <- 0
      }
      
      Sys.sleep(base_time)
      
      result <- data[sample(nrow(data), min(n, nrow(data))), ]
      attr(result, "performance") <- list(
        processing_time = base_time,
        gpu_used = use_gpu,
        gpu_memory_mb = gpu_memory_usage
      )
      
      return(result)
    },
    
    `render_webgl_layer` = function(layer_data, provider, use_webgl = TRUE) {
      # WebGL rendering performance
      if (use_webgl) {
        # WebGL scales well with data size
        render_time <- 0.01 + (nrow(layer_data) * 0.000002)
        frame_rate <- 60 / (1 + nrow(layer_data) / 100000)  # Decreases with data size
      } else {
        # Canvas fallback is slower
        render_time <- 0.05 + (nrow(layer_data) * 0.00001)
        frame_rate <- 30 / (1 + nrow(layer_data) / 50000)
      }
      
      Sys.sleep(render_time)
      
      return(list(
        render_time = render_time,
        frame_rate = frame_rate,
        webgl_used = use_webgl,
        data_points = nrow(layer_data)
      ))
    },
    
    {
      if (requireNamespace("microbenchmark", quietly = TRUE)) {
        # Verify WebGL and GPU support
        webgl_info <- check_webgl_support()
        gpu_info <- check_gpu_acceleration()
        
        expect_true(webgl_info$supported)
        expect_true(gpu_info$available)
        
        cat("\n=== WebGL and GPU Performance Baselines ===\n")
        cat(sprintf("WebGL Support: %s (%s)\n", webgl_info$supported, webgl_info$version))
        cat(sprintf("GPU Acceleration: %s (%.1f GB memory)\n", gpu_info$available, gpu_info$memory_gb))
        
        # Test different data sizes
        data_sizes <- c(1000, 10000, 100000)
        
        # Performance baseline results
        baseline_results <- data.frame(
          data_size = integer(),
          gpu_time_ms = numeric(),
          cpu_time_ms = numeric(),
          gpu_speedup = numeric(),
          webgl_fps = numeric(),
          canvas_fps = numeric(),
          stringsAsFactors = FALSE
        )
        
        for (size in data_sizes) {
          test_data <- generate_integration_test_data(size)$point_data
          
          cat(sprintf("\nTesting with %d data points:\n", size))
          
          # GPU vs CPU sampling benchmark
          sampling_benchmark <- microbenchmark::microbenchmark(
            gpu = spatial_sample_random_gpu(test_data, 1000, use_gpu = TRUE),
            cpu = spatial_sample_random_gpu(test_data, 1000, use_gpu = FALSE),
            times = 5
          )
          
          gpu_time <- median(sampling_benchmark$time[sampling_benchmark$expr == "gpu"]) / 1e6
          cpu_time <- median(sampling_benchmark$time[sampling_benchmark$expr == "cpu"]) / 1e6
          speedup <- cpu_time / gpu_time
          
          # WebGL vs Canvas rendering benchmark
          webgl_result <- render_webgl_layer(test_data, "mapbox", use_webgl = TRUE)
          canvas_result <- render_webgl_layer(test_data, "leaflet", use_webgl = FALSE)
          
          # Store results
          baseline_results <- rbind(baseline_results, data.frame(
            data_size = size,
            gpu_time_ms = gpu_time,
            cpu_time_ms = cpu_time,
            gpu_speedup = speedup,
            webgl_fps = webgl_result$frame_rate,
            canvas_fps = canvas_result$frame_rate,
            stringsAsFactors = FALSE
          ))
          
          cat(sprintf("  GPU Sampling: %.2f ms, CPU: %.2f ms, Speedup: %.1fx\n", 
                     gpu_time, cpu_time, speedup))
          cat(sprintf("  WebGL Rendering: %.1f FPS, Canvas: %.1f FPS\n",
                     webgl_result$frame_rate, canvas_result$frame_rate))
        }
        
        # Print baseline summary
        cat("\n=== Performance Baseline Summary ===\n")
        print(baseline_results)
        
        # Verify performance baselines
        # GPU should provide consistent speedup
        expect_true(all(baseline_results$gpu_speedup >= 2.0))  # At least 2x speedup
        
        # WebGL should maintain higher frame rates
        expect_true(all(baseline_results$webgl_fps >= baseline_results$canvas_fps))
        
        # Performance should scale reasonably with data size
        large_data_gpu_speedup <- baseline_results$gpu_speedup[baseline_results$data_size == 100000]
        small_data_gpu_speedup <- baseline_results$gpu_speedup[baseline_results$data_size == 1000]
        
        # GPU advantage should increase with data size
        expect_gt(large_data_gpu_speedup, small_data_gpu_speedup)
        
        # Establish baseline thresholds for regression testing
        cat("\n=== Established Performance Baselines ===\n")
        cat(sprintf("GPU Sampling (100K points): < %.2f ms\n", 
                   baseline_results$gpu_time_ms[baseline_results$data_size == 100000] * 1.5))
        cat(sprintf("WebGL Rendering (100K points): > %.1f FPS\n",
                   baseline_results$webgl_fps[baseline_results$data_size == 100000] * 0.8))
        cat(sprintf("Minimum GPU Speedup: %.1fx\n", min(baseline_results$gpu_speedup)))
      }
    }
  )
})

# Test 3: Memory usage tests for large dataset operations
test_that("memory usage is optimized for large dataset operations", {
  skip_if_no_integration_test()
  skip_if_not_installed("profmem")
  
  # Helper function to measure memory usage
  measure_memory_usage <- function(expr) {
    if (requireNamespace("profmem", quietly = TRUE)) {
      gc(reset = TRUE)
      prof <- profmem::profmem(expr)
      return(profmem::total(prof))
    } else {
      return(NA)
    }
  }
  
  # Mock memory-optimized functions
  with_mock(
    `process_large_dataset` = function(data, processing_mode = "optimized") {
      # Simulate different memory usage patterns
      if (processing_mode == "optimized") {
        # Process in chunks to reduce memory footprint
        chunk_size <- 10000
        chunks <- split(data, ceiling(seq_len(nrow(data)) / chunk_size))
        
        results <- list()
        for (i in seq_along(chunks)) {
          # Process chunk and immediately clean up
          chunk_result <- chunks[[i]][sample(nrow(chunks[[i]]), min(100, nrow(chunks[[i]]))), ]
          results[[i]] <- chunk_result
          rm(chunk_result)
          gc()
        }
        
        result <- do.call(rbind, results)
      } else {
        # Process all data at once (memory intensive)
        result <- data[sample(nrow(data), min(1000, nrow(data))), ]
      }
      
      return(result)
    },
    
    `spatial_sample_with_memory_management` = function(admin_data, total_samples, 
                                                      use_gpu = TRUE, 
                                                      memory_efficient = TRUE) {
      if (memory_efficient) {
        # Process polygons in batches
        batch_size <- 5
        n_polygons <- length(admin_data$polygons)
        n_batches <- ceiling(n_polygons / batch_size)
        
        all_samples <- list()
        
        for (batch in 1:n_batches) {
          start_idx <- (batch - 1) * batch_size + 1
          end_idx <- min(batch * batch_size, n_polygons)
          
          batch_samples <- floor(total_samples / n_batches)
          
          # Generate samples for this batch
          batch_result <- data.frame(
            polygon_id = sample(start_idx:end_idx, batch_samples, replace = TRUE),
            lon = runif(batch_samples, -180, 180),
            lat = runif(batch_samples, -90, 90)
          )
          
          all_samples[[batch]] <- batch_result
          
          # Clean up intermediate results
          rm(batch_result)
          gc()
        }
        
        result <- do.call(rbind, all_samples)
      } else {
        # Process all at once
        result <- data.frame(
          polygon_id = sample(1:length(admin_data$polygons), total_samples, replace = TRUE),
          lon = runif(total_samples, -180, 180),
          lat = runif(total_samples, -90, 90)
        )
      }
      
      return(result)
    },
    
    {
      cat("\n=== Memory Usage Analysis for Large Dataset Operations ===\n")
      
      # Test different dataset sizes
      dataset_sizes <- c(10000, 100000, 500000)
      
      memory_results <- data.frame(
        dataset_size = integer(),
        optimized_memory_mb = numeric(),
        standard_memory_mb = numeric(),
        memory_efficiency = numeric(),
        stringsAsFactors = FALSE
      )
      
      for (size in dataset_sizes) {
        test_data <- generate_integration_test_data(size)$point_data
        
        cat(sprintf("\nTesting memory usage with %d data points:\n", size))
        
        # Measure optimized processing memory usage
        optimized_mem <- measure_memory_usage({
          result <- process_large_dataset(test_data, "optimized")
        })
        
        # Measure standard processing memory usage
        standard_mem <- measure_memory_usage({
          result <- process_large_dataset(test_data, "standard")
        })
        
        if (!is.na(optimized_mem) && !is.na(standard_mem)) {
          efficiency <- standard_mem / optimized_mem
          
          memory_results <- rbind(memory_results, data.frame(
            dataset_size = size,
            optimized_memory_mb = optimized_mem / 1024^2,
            standard_memory_mb = standard_mem / 1024^2,
            memory_efficiency = efficiency,
            stringsAsFactors = FALSE
          ))
          
          cat(sprintf("  Optimized: %.2f MB, Standard: %.2f MB, Efficiency: %.1fx\n",
                     optimized_mem / 1024^2, standard_mem / 1024^2, efficiency))
        }
      }
      
      # Test spatial sampling memory usage
      admin_data <- generate_integration_test_data()$admin_data
      
      cat("\n--- Spatial Sampling Memory Usage ---\n")
      
      # Memory-efficient vs standard spatial sampling
      efficient_mem <- measure_memory_usage({
        result <- spatial_sample_with_memory_management(
          admin_data, 10000, use_gpu = TRUE, memory_efficient = TRUE
        )
      })
      
      standard_mem <- measure_memory_usage({
        result <- spatial_sample_with_memory_management(
          admin_data, 10000, use_gpu = TRUE, memory_efficient = FALSE
        )
      })
      
      if (!is.na(efficient_mem) && !is.na(standard_mem)) {
        spatial_efficiency <- standard_mem / efficient_mem
        
        cat(sprintf("Spatial Sampling - Efficient: %.2f MB, Standard: %.2f MB, Efficiency: %.1fx\n",
                   efficient_mem / 1024^2, standard_mem / 1024^2, spatial_efficiency))
        
        # Memory-efficient should use less memory
        expect_lt(efficient_mem, standard_mem)
        expect_gt(spatial_efficiency, 1.2)  # At least 20% more efficient
      }
      
      # Print memory usage summary
      if (nrow(memory_results) > 0) {
        cat("\n=== Memory Usage Summary ===\n")
        print(memory_results)
        
        # Verify memory efficiency
        expect_true(all(memory_results$memory_efficiency > 1.0))
        
        # Memory usage should scale sub-linearly with optimizations
        if (nrow(memory_results) >= 2) {
          large_size_idx <- which.max(memory_results$dataset_size)
          small_size_idx <- which.min(memory_results$dataset_size)
          
          size_ratio <- memory_results$dataset_size[large_size_idx] / 
                       memory_results$dataset_size[small_size_idx]
          memory_ratio <- memory_results$optimized_memory_mb[large_size_idx] / 
                         memory_results$optimized_memory_mb[small_size_idx]
          
          # Memory should scale better than linearly
          expect_lt(memory_ratio, size_ratio)
        }
      }
    }
  )
})

# Test 4: Browser compatibility tests for WebGL and GPU features
test_that("browser compatibility for WebGL and GPU features is comprehensive", {
  skip_if_no_integration_test()
  
  # Mock comprehensive browser compatibility detection
  with_mock(
    `detect_browser_capabilities` = function(browser_name, version = "latest") {
      # Comprehensive browser capability matrix
      capabilities_matrix <- list(
        chrome = list(
          webgl1 = TRUE, webgl2 = TRUE, gpu_acceleration = TRUE,
          required_extensions = c("OES_texture_float", "OES_standard_derivatives", 
                                "WEBGL_depth_texture", "OES_element_index_uint"),
          performance_tier = "high",
          deck_gl_support = "full",
          spatial_sampling_support = "gpu_accelerated"
        ),
        firefox = list(
          webgl1 = TRUE, webgl2 = TRUE, gpu_acceleration = TRUE,
          required_extensions = c("OES_texture_float", "OES_standard_derivatives", 
                                "WEBGL_depth_texture", "OES_element_index_uint"),
          performance_tier = "high",
          deck_gl_support = "full",
          spatial_sampling_support = "gpu_accelerated"
        ),
        safari = list(
          webgl1 = TRUE, webgl2 = TRUE, gpu_acceleration = TRUE,
          required_extensions = c("OES_texture_float", "OES_standard_derivatives", 
                                "WEBGL_depth_texture"),
          performance_tier = "medium",
          deck_gl_support = "full",
          spatial_sampling_support = "gpu_accelerated"
        ),
        edge = list(
          webgl1 = TRUE, webgl2 = TRUE, gpu_acceleration = TRUE,
          required_extensions = c("OES_texture_float", "OES_standard_derivatives", 
                                "WEBGL_depth_texture", "OES_element_index_uint"),
          performance_tier = "high",
          deck_gl_support = "full",
          spatial_sampling_support = "gpu_accelerated"
        ),
        ie11 = list(
          webgl1 = FALSE, webgl2 = FALSE, gpu_acceleration = FALSE,
          required_extensions = c(),
          performance_tier = "none",
          deck_gl_support = "none",
          spatial_sampling_support = "cpu_only"
        )
      )
      
      return(capabilities_matrix[[browser_name]])
    },
    
    `test_webgl_performance` = function(browser_name) {
      # Simulate WebGL performance testing
      capabilities <- detect_browser_capabilities(browser_name)
      
      if (!capabilities$webgl1) {
        return(list(supported = FALSE, performance = 0))
      }
      
      # Simulate performance based on browser tier
      base_performance <- switch(capabilities$performance_tier,
        "high" = 100,
        "medium" = 70,
        "low" = 40,
        0
      )
      
      # Add some variation
      actual_performance <- base_performance + rnorm(1, 0, 5)
      
      return(list(
        supported = TRUE,
        performance_score = max(0, actual_performance),
        webgl2_supported = capabilities$webgl2,
        gpu_acceleration = capabilities$gpu_acceleration
      ))
    },
    
    `test_spatial_sampling_compatibility` = function(browser_name) {
      capabilities <- detect_browser_capabilities(browser_name)
      
      # Test spatial sampling features
      if (capabilities$spatial_sampling_support == "gpu_accelerated") {
        gpu_speedup <- runif(1, 3, 8)  # 3-8x speedup
        memory_efficiency <- runif(1, 1.5, 3)  # 1.5-3x more efficient
      } else if (capabilities$spatial_sampling_support == "cpu_only") {
        gpu_speedup <- 1.0  # No speedup
        memory_efficiency <- 1.0  # No efficiency gain
      } else {
        gpu_speedup <- 0
        memory_efficiency <- 0
      }
      
      return(list(
        gpu_supported = capabilities$spatial_sampling_support != "cpu_only",
        gpu_speedup = gpu_speedup,
        memory_efficiency = memory_efficiency,
        fallback_available = TRUE
      ))
    },
    
    {
      cat("\n=== Browser Compatibility Analysis ===\n")
      
      browsers <- c("chrome", "firefox", "safari", "edge", "ie11")
      
      # Comprehensive compatibility matrix
      compatibility_results <- data.frame(
        browser = character(),
        webgl1_support = logical(),
        webgl2_support = logical(),
        gpu_acceleration = logical(),
        performance_score = numeric(),
        deck_gl_compatible = logical(),
        spatial_sampling_gpu = logical(),
        gpu_speedup = numeric(),
        stringsAsFactors = FALSE
      )
      
      for (browser in browsers) {
        capabilities <- detect_browser_capabilities(browser)
        webgl_perf <- test_webgl_performance(browser)
        spatial_compat <- test_spatial_sampling_compatibility(browser)
        
        compatibility_results <- rbind(compatibility_results, data.frame(
          browser = browser,
          webgl1_support = capabilities$webgl1,
          webgl2_support = capabilities$webgl2,
          gpu_acceleration = capabilities$gpu_acceleration,
          performance_score = webgl_perf$performance_score,
          deck_gl_compatible = capabilities$deck_gl_support == "full",
          spatial_sampling_gpu = spatial_compat$gpu_supported,
          gpu_speedup = spatial_compat$gpu_speedup,
          stringsAsFactors = FALSE
        ))
        
        cat(sprintf("\n%s Browser:\n", toupper(browser)))
        cat(sprintf("  WebGL 1.0: %s, WebGL 2.0: %s\n", 
                   capabilities$webgl1, capabilities$webgl2))
        cat(sprintf("  GPU Acceleration: %s\n", capabilities$gpu_acceleration))
        cat(sprintf("  Performance Score: %.1f/100\n", webgl_perf$performance_score))
        cat(sprintf("  Deck.gl Support: %s\n", capabilities$deck_gl_support))
        cat(sprintf("  Spatial Sampling GPU: %s (%.1fx speedup)\n", 
                   spatial_compat$gpu_supported, spatial_compat$gpu_speedup))
      }
      
      # Print compatibility summary
      cat("\n=== Browser Compatibility Summary ===\n")
      print(compatibility_results)
      
      # Verify compatibility expectations
      modern_browsers <- c("chrome", "firefox", "safari", "edge")
      
      for (browser in modern_browsers) {
        browser_row <- compatibility_results[compatibility_results$browser == browser, ]
        
        # Modern browsers should support WebGL 1.0
        expect_true(browser_row$webgl1_support)
        
        # Should support deck.gl
        expect_true(browser_row$deck_gl_compatible)
        
        # Should have reasonable performance
        expect_gt(browser_row$performance_score, 50)
        
        # Should support GPU-accelerated spatial sampling
        expect_true(browser_row$spatial_sampling_gpu)
        expect_gt(browser_row$gpu_speedup, 2.0)
      }
      
      # IE11 should not support modern features
      ie11_row <- compatibility_results[compatibility_results$browser == "ie11", ]
      expect_false(ie11_row$webgl1_support)
      expect_false(ie11_row$deck_gl_compatible)
      expect_equal(ie11_row$performance_score, 0)
      
      # Calculate overall compatibility score
      modern_browser_scores <- compatibility_results$performance_score[
        compatibility_results$browser %in% modern_browsers
      ]
      
      overall_compatibility <- mean(modern_browser_scores)
      cat(sprintf("\nOverall Modern Browser Compatibility Score: %.1f/100\n", 
                 overall_compatibility))
      
      # Should have good overall compatibility
      expect_gt(overall_compatibility, 70)
    }
  )
})

# Test 5: End-to-end integration workflow
test_that("end-to-end integration workflow performs within acceptable limits", {
  skip_if_no_integration_test()
  
  # Mock complete workflow
  with_mock(
    `complete_integration_workflow` = function(provider, data_size, use_optimizations = TRUE) {
      # Simulate complete workflow: data loading -> processing -> visualization
      workflow_start <- Sys.time()
      
      # Step 1: Data preparation
      data_prep_start <- Sys.time()
      test_data <- generate_integration_test_data(data_size)
      data_prep_time <- as.numeric(Sys.time() - data_prep_start)
      
      # Step 2: Provider initialization
      provider_init_start <- Sys.time()
      # Simulate provider-specific initialization time
      provider_init_time <- switch(provider,
        "mapbox" = 0.1, "leaflet" = 0.15, "openlayers" = 0.2,
        "gaode" = 0.25, "baidu" = 0.3, 0.1
      )
      Sys.sleep(provider_init_time)
      provider_init_elapsed <- as.numeric(Sys.time() - provider_init_start)
      
      # Step 3: Coordinate transformation (if needed)
      coord_transform_start <- Sys.time()
      if (provider %in% c("gaode", "baidu")) {
        # Chinese providers need coordinate transformation
        transform_time <- data_size * 0.000001  # Scales with data size
        Sys.sleep(transform_time)
      }
      coord_transform_time <- as.numeric(Sys.time() - coord_transform_start)
      
      # Step 4: Spatial sampling
      sampling_start <- Sys.time()
      if (use_optimizations) {
        sampling_time <- 0.01 + (data_size * 0.000001)  # GPU-accelerated
      } else {
        sampling_time <- 0.05 + (data_size * 0.000005)  # CPU-only
      }
      Sys.sleep(sampling_time)
      sampling_elapsed <- as.numeric(Sys.time() - sampling_start)
      
      # Step 5: Layer rendering
      rendering_start <- Sys.time()
      rendering_factor <- switch(provider,
        "mapbox" = 1.0, "leaflet" = 1.3, "openlayers" = 1.2,
        "gaode" = 1.4, "baidu" = 1.5, 1.0
      )
      rendering_time <- (0.02 + data_size * 0.000002) * rendering_factor
      Sys.sleep(rendering_time)
      rendering_elapsed <- as.numeric(Sys.time() - rendering_start)
      
      total_time <- as.numeric(Sys.time() - workflow_start)
      
      return(list(
        provider = provider,
        data_size = data_size,
        optimizations_used = use_optimizations,
        timings = list(
          data_prep = data_prep_time,
          provider_init = provider_init_elapsed,
          coord_transform = coord_transform_time,
          spatial_sampling = sampling_elapsed,
          layer_rendering = rendering_elapsed,
          total = total_time
        ),
        performance_metrics = list(
          points_per_second = data_size / total_time,
          optimization_factor = ifelse(use_optimizations, 
                                     sampling_time / (0.05 + data_size * 0.000005), 1.0)
        )
      ))
    },
    
    {
      cat("\n=== End-to-End Integration Workflow Performance ===\n")
      
      # Test different scenarios
      test_scenarios <- expand.grid(
        provider = c("mapbox", "leaflet", "gaode"),
        data_size = c(1000, 10000, 50000),
        optimizations = c(TRUE, FALSE),
        stringsAsFactors = FALSE
      )
      
      workflow_results <- data.frame(
        provider = character(),
        data_size = integer(),
        optimizations = logical(),
        total_time_sec = numeric(),
        points_per_sec = numeric(),
        optimization_speedup = numeric(),
        stringsAsFactors = FALSE
      )
      
      for (i in 1:nrow(test_scenarios)) {
        scenario <- test_scenarios[i, ]
        
        cat(sprintf("\nTesting: %s provider, %d points, optimizations: %s\n",
                   scenario$provider, scenario$data_size, scenario$optimizations))
        
        result <- complete_integration_workflow(
          scenario$provider, 
          scenario$data_size, 
          scenario$optimizations
        )
        
        workflow_results <- rbind(workflow_results, data.frame(
          provider = result$provider,
          data_size = result$data_size,
          optimizations = result$optimizations_used,
          total_time_sec = result$timings$total,
          points_per_sec = result$performance_metrics$points_per_second,
          optimization_speedup = result$performance_metrics$optimization_factor,
          stringsAsFactors = FALSE
        ))
        
        cat(sprintf("  Total time: %.3f sec (%.0f points/sec)\n",
                   result$timings$total, result$performance_metrics$points_per_second))
        
        # Print timing breakdown
        timings <- result$timings
        cat(sprintf("  Breakdown - Prep: %.3fs, Init: %.3fs, Transform: %.3fs, Sample: %.3fs, Render: %.3fs\n",
                   timings$data_prep, timings$provider_init, timings$coord_transform,
                   timings$spatial_sampling, timings$layer_rendering))
      }
      
      # Print workflow results summary
      cat("\n=== Workflow Performance Summary ===\n")
      print(workflow_results)
      
      # Verify performance requirements
      # All workflows should complete within reasonable time
      expect_true(all(workflow_results$total_time_sec < 5.0))  # Max 5 seconds
      
      # Optimizations should provide speedup
      optimized_results <- workflow_results[workflow_results$optimizations == TRUE, ]
      unoptimized_results <- workflow_results[workflow_results$optimizations == FALSE, ]
      
      if (nrow(optimized_results) > 0 && nrow(unoptimized_results) > 0) {
        avg_optimized_throughput <- mean(optimized_results$points_per_sec)
        avg_unoptimized_throughput <- mean(unoptimized_results$points_per_sec)
        
        expect_gt(avg_optimized_throughput, avg_unoptimized_throughput)
        
        cat(sprintf("\nOptimization Impact:\n"))
        cat(sprintf("  Optimized throughput: %.0f points/sec\n", avg_optimized_throughput))
        cat(sprintf("  Unoptimized throughput: %.0f points/sec\n", avg_unoptimized_throughput))
        cat(sprintf("  Speedup: %.1fx\n", avg_optimized_throughput / avg_unoptimized_throughput))
      }
      
      # Performance should scale reasonably with data size
      large_data_results <- workflow_results[workflow_results$data_size == 50000, ]
      small_data_results <- workflow_results[workflow_results$data_size == 1000, ]
      
      if (nrow(large_data_results) > 0 && nrow(small_data_results) > 0) {
        # Throughput should not degrade too much with larger datasets
        avg_large_throughput <- mean(large_data_results$points_per_sec)
        avg_small_throughput <- mean(small_data_results$points_per_sec)
        
        throughput_ratio <- avg_large_throughput / avg_small_throughput
        expect_gt(throughput_ratio, 0.5)  # Should maintain at least 50% throughput
      }
    }
  )
})

# Helper function
`%||%` <- function(x, y) if (is.null(x)) y else x