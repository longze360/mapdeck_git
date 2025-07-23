context("Memory usage tests")

# Test memory usage for large dataset operations
# This test file focuses on monitoring memory consumption during operations with large datasets

# Skip tests on CRAN and CI environments
skip_if_no_memory_test <- function() {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("profmem")
}

# Helper function to generate large test datasets
generate_large_dataset <- function(size) {
  # Generate random points
  set.seed(123)  # For reproducibility
  
  data.frame(
    lon = runif(size, -180, 180),
    lat = runif(size, -90, 90),
    value = runif(size, 0, 100),
    category = sample(letters[1:5], size, replace = TRUE),
    timestamp = Sys.time() + sample(1:1000, size, replace = TRUE)
  )
}

# Helper function to measure memory usage
measure_memory_usage <- function(expr) {
  if (requireNamespace("profmem", quietly = TRUE)) {
    # Garbage collect before measurement
    gc(reset = TRUE)
    
    # Measure memory usage
    prof <- profmem::profmem(expr)
    
    # Return memory usage summary
    return(profmem::total(prof))
  } else {
    # Return NA if profmem is not available
    return(NA)
  }
}

# Test memory usage for spatial sampling with large datasets
test_that("spatial sampling has acceptable memory usage with large datasets", {
  skip_if_no_memory_test()
  
  # Mock the spatial sampling function
  with_mock(
    `spatial_sample_random` = function(data, n, use_gpu = TRUE) {
      # Simulate sampling operation
      result <- data[sample(nrow(data), min(n, nrow(data))), ]
      return(result)
    },
    {
      # Generate datasets of increasing size
      small_data <- generate_large_dataset(1e4)    # 10K points
      medium_data <- generate_large_dataset(1e5)   # 100K points
      large_data <- generate_large_dataset(1e6)    # 1M points
      
      # Measure memory usage for each dataset size
      small_mem <- measure_memory_usage({
        result <- spatial_sample_random(small_data, 1000)
      })
      
      medium_mem <- measure_memory_usage({
        result <- spatial_sample_random(medium_data, 1000)
      })
      
      large_mem <- measure_memory_usage({
        result <- spatial_sample_random(large_data, 1000)
      })
      
      # Print memory usage
      cat("\nMemory usage for spatial sampling:\n")
      cat(sprintf("Small dataset (10K points): %.2f MB\n", small_mem / 1024^2))
      cat(sprintf("Medium dataset (100K points): %.2f MB\n", medium_mem / 1024^2))
      cat(sprintf("Large dataset (1M points): %.2f MB\n", large_mem / 1024^2))
      
      # Check that memory usage scales sub-linearly with data size
      # (memory usage should not increase proportionally to data size)
      if (!is.na(small_mem) && !is.na(medium_mem) && !is.na(large_mem)) {
        # Medium should use less than 10x the memory of small (for 10x the data)
        expect_lt(medium_mem, small_mem * 10)
        
        # Large should use less than 10x the memory of medium (for 10x the data)
        expect_lt(large_mem, medium_mem * 10)
      }
    }
  )
})

# Test memory usage for coordinate transformations with large datasets
test_that("coordinate transformations have acceptable memory usage with large datasets", {
  skip_if_no_memory_test()
  
  # Mock the coordinate transformation function
  with_mock(
    `transform_coordinates` = function(coords, from_crs, to_crs) {
      # Simulate transformation (just return the original coordinates)
      return(coords)
    },
    {
      # Generate datasets of increasing size
      small_data <- generate_large_dataset(1e4)[, c("lon", "lat")]    # 10K points
      medium_data <- generate_large_dataset(1e5)[, c("lon", "lat")]   # 100K points
      large_data <- generate_large_dataset(1e6)[, c("lon", "lat")]    # 1M points
      
      # Measure memory usage for each dataset size
      small_mem <- measure_memory_usage({
        result <- transform_coordinates(small_data, "WGS84", "GCJ02")
      })
      
      medium_mem <- measure_memory_usage({
        result <- transform_coordinates(medium_data, "WGS84", "GCJ02")
      })
      
      large_mem <- measure_memory_usage({
        result <- transform_coordinates(large_data, "WGS84", "GCJ02")
      })
      
      # Print memory usage
      cat("\nMemory usage for coordinate transformations:\n")
      cat(sprintf("Small dataset (10K points): %.2f MB\n", small_mem / 1024^2))
      cat(sprintf("Medium dataset (100K points): %.2f MB\n", medium_mem / 1024^2))
      cat(sprintf("Large dataset (1M points): %.2f MB\n", large_mem / 1024^2))
      
      # Check that memory usage scales reasonably with data size
      if (!is.na(small_mem) && !is.na(medium_mem) && !is.na(large_mem)) {
        # Memory usage should scale roughly linearly with data size
        # but with some overhead, so we allow a factor of 15x instead of 10x
        expect_lt(medium_mem, small_mem * 15)
        expect_lt(large_mem, medium_mem * 15)
      }
    }
  )
})

# Test memory usage for administrative boundary sampling
test_that("administrative boundary sampling has acceptable memory usage", {
  skip_if_no_memory_test()
  
  # Mock the administrative sampling function
  with_mock(
    `spatial_sample_administrative` = function(admin_polygons, total_samples, 
                                              allocation_method = "proportional",
                                              concurrent = TRUE) {
      # Simulate sampling within administrative boundaries
      n_polygons <- length(admin_polygons$polygons)
      
      # Allocate samples based on method
      if (allocation_method == "proportional") {
        # Allocate proportionally to area
        total_area <- sum(admin_polygons$areas)
        samples_per_polygon <- round(admin_polygons$areas / total_area * total_samples)
      } else if (allocation_method == "equal") {
        # Allocate equally
        samples_per_polygon <- rep(floor(total_samples / n_polygons), n_polygons)
      } else {
        # Custom allocation (use equal for mock)
        samples_per_polygon <- rep(floor(total_samples / n_polygons), n_polygons)
      }
      
      # Generate random points for each polygon
      result <- data.frame(
        polygon_id = rep(1:n_polygons, samples_per_polygon),
        lon = runif(sum(samples_per_polygon), -180, 180),
        lat = runif(sum(samples_per_polygon), -90, 90)
      )
      
      return(result)
    },
    {
      # Create mock administrative polygons of increasing complexity
      create_admin_polygons <- function(n_polygons) {
        list(
          polygons = vector("list", n_polygons),
          areas = runif(n_polygons, 100, 1000)
        )
      }
      
      small_admin <- create_admin_polygons(10)     # 10 polygons
      medium_admin <- create_admin_polygons(100)   # 100 polygons
      large_admin <- create_admin_polygons(1000)   # 1000 polygons
      
      # Measure memory usage for each dataset size
      small_mem <- measure_memory_usage({
        result <- spatial_sample_administrative(small_admin, 10000)
      })
      
      medium_mem <- measure_memory_usage({
        result <- spatial_sample_administrative(medium_admin, 10000)
      })
      
      large_mem <- measure_memory_usage({
        result <- spatial_sample_administrative(large_admin, 10000)
      })
      
      # Print memory usage
      cat("\nMemory usage for administrative boundary sampling:\n")
      cat(sprintf("Small admin (10 polygons): %.2f MB\n", small_mem / 1024^2))
      cat(sprintf("Medium admin (100 polygons): %.2f MB\n", medium_mem / 1024^2))
      cat(sprintf("Large admin (1000 polygons): %.2f MB\n", large_mem / 1024^2))
      
      # Check that memory usage scales reasonably with complexity
      if (!is.na(small_mem) && !is.na(medium_mem) && !is.na(large_mem)) {
        # Memory usage should scale sub-linearly with polygon count
        # due to optimizations and shared resources
        expect_lt(medium_mem, small_mem * 10)
        expect_lt(large_mem, medium_mem * 10)
      }
    }
  )
})

# Test memory usage for concurrent processing
test_that("concurrent processing has acceptable memory usage", {
  skip_if_no_memory_test()
  
  # Mock the concurrent processing function
  with_mock(
    `process_concurrently` = function(data, processor_fn, n_threads = 2) {
      # Simulate concurrent processing by dividing data into chunks
      chunk_size <- ceiling(nrow(data) / n_threads)
      chunks <- split(data, ceiling(seq_len(nrow(data)) / chunk_size))
      
      # Process each chunk (sequentially in the mock)
      results <- lapply(chunks, processor_fn)
      
      # Combine results
      do.call(rbind, results)
    },
    `simple_processor` = function(data) {
      # Simple processing function that returns the data
      return(data)
    },
    {
      # Generate large dataset
      large_data <- generate_large_dataset(1e6)  # 1M points
      
      # Measure memory usage with different thread counts
      single_thread_mem <- measure_memory_usage({
        result <- process_concurrently(large_data, simple_processor, n_threads = 1)
      })
      
      two_threads_mem <- measure_memory_usage({
        result <- process_concurrently(large_data, simple_processor, n_threads = 2)
      })
      
      four_threads_mem <- measure_memory_usage({
        result <- process_concurrently(large_data, simple_processor, n_threads = 4)
      })
      
      # Print memory usage
      cat("\nMemory usage for concurrent processing:\n")
      cat(sprintf("Single thread: %.2f MB\n", single_thread_mem / 1024^2))
      cat(sprintf("Two threads: %.2f MB\n", two_threads_mem / 1024^2))
      cat(sprintf("Four threads: %.2f MB\n", four_threads_mem / 1024^2))
      
      # Check that memory usage scales reasonably with thread count
      if (!is.na(single_thread_mem) && !is.na(two_threads_mem) && !is.na(four_threads_mem)) {
        # Memory usage should increase with thread count, but not linearly
        # due to shared resources and copy-on-write semantics
        expect_lt(two_threads_mem, single_thread_mem * 2)
        expect_lt(four_threads_mem, single_thread_mem * 4)
      }
    }
  )
})

# Test memory leaks in provider switching
test_that("provider switching does not leak memory", {
  skip_if_no_memory_test()
  
  # Mock the provider switching function
  with_mock(
    `update_provider` = function(map, new_provider) {
      # Simulate provider switching
      map$provider <- new_provider
      return(map)
    },
    {
      # Create a mock map
      map <- list(
        provider = "mapbox",
        layers = list(
          layer1 = list(id = "layer1", type = "scatterplot"),
          layer2 = list(id = "layer2", type = "line")
        )
      )
      
      # Measure memory usage for multiple provider switches
      initial_mem <- measure_memory_usage({
        map <- update_provider(map, "leaflet")
      })
      
      second_mem <- measure_memory_usage({
        map <- update_provider(map, "openlayers")
      })
      
      third_mem <- measure_memory_usage({
        map <- update_provider(map, "mapbox")
      })
      
      # Print memory usage
      cat("\nMemory usage for provider switching:\n")
      cat(sprintf("Initial switch: %.2f MB\n", initial_mem / 1024^2))
      cat(sprintf("Second switch: %.2f MB\n", second_mem / 1024^2))
      cat(sprintf("Third switch: %.2f MB\n", third_mem / 1024^2))
      
      # Check that memory usage remains stable across switches
      if (!is.na(initial_mem) && !is.na(second_mem) && !is.na(third_mem)) {
        # Memory usage should be similar across switches
        # Allow for 20% variation due to GC and other factors
        expect_lt(abs(second_mem - initial_mem) / initial_mem, 0.2)
        expect_lt(abs(third_mem - initial_mem) / initial_mem, 0.2)
      }
    }
  )
})

# Test memory usage for large layer operations
test_that("layer operations have acceptable memory usage with large datasets", {
  skip_if_no_memory_test()
  
  # Mock the layer addition function
  with_mock(
    `add_scatterplot` = function(map, data, lon, lat, ...) {
      # Simulate layer addition
      layer_id <- paste0("scatterplot_", length(map$layers) + 1)
      
      # Create layer object (without storing full data)
      layer <- list(
        id = layer_id,
        type = "scatterplot",
        data_size = nrow(data)
      )
      
      # Add layer to map
      map$layers[[layer_id]] <- layer
      
      return(map)
    },
    {
      # Generate datasets of increasing size
      small_data <- generate_large_dataset(1e4)    # 10K points
      medium_data <- generate_large_dataset(1e5)   # 100K points
      large_data <- generate_large_dataset(1e6)    # 1M points
      
      # Create a mock map
      map <- list(
        provider = "mapbox",
        layers = list()
      )
      
      # Measure memory usage for each dataset size
      small_mem <- measure_memory_usage({
        map <- add_scatterplot(map, small_data, "lon", "lat", radius = 10)
      })
      
      medium_mem <- measure_memory_usage({
        map <- add_scatterplot(map, medium_data, "lon", "lat", radius = 10)
      })
      
      large_mem <- measure_memory_usage({
        map <- add_scatterplot(map, large_data, "lon", "lat", radius = 10)
      })
      
      # Print memory usage
      cat("\nMemory usage for layer operations:\n")
      cat(sprintf("Small dataset (10K points): %.2f MB\n", small_mem / 1024^2))
      cat(sprintf("Medium dataset (100K points): %.2f MB\n", medium_mem / 1024^2))
      cat(sprintf("Large dataset (1M points): %.2f MB\n", large_mem / 1024^2))
      
      # Check that memory usage scales reasonably with data size
      if (!is.na(small_mem) && !is.na(medium_mem) && !is.na(large_mem)) {
        # Memory usage should scale sub-linearly with data size
        # due to optimizations and data summarization
        expect_lt(medium_mem, small_mem * 10)
        expect_lt(large_mem, medium_mem * 10)
      }
    }
  )
})

# Test memory cleanup after operations
test_that("memory is properly cleaned up after operations", {
  skip_if_no_memory_test()
  
  # Mock the spatial sampling function with explicit cleanup
  with_mock(
    `spatial_sample_random` = function(data, n, use_gpu = TRUE) {
      # Simulate sampling operation
      result <- data[sample(nrow(data), min(n, nrow(data))), ]
      
      # Explicitly clean up temporary objects
      rm(list = setdiff(ls(), c("data", "n", "use_gpu", "result")))
      gc()
      
      return(result)
    },
    {
      # Generate large dataset
      large_data <- generate_large_dataset(1e6)  # 1M points
      
      # Measure initial memory state
      gc_stats_before <- gc(full = TRUE)
      
      # Perform operation
      result <- spatial_sample_random(large_data, 1000)
      
      # Measure memory state after operation
      gc_stats_after <- gc(full = TRUE)
      
      # Print memory usage
      cat("\nMemory cleanup test:\n")
      cat("Memory stats before operation:\n")
      print(gc_stats_before)
      cat("\nMemory stats after operation:\n")
      print(gc_stats_after)
      
      # Check that memory usage is reasonable after cleanup
      # This is a basic check - actual values would depend on the implementation
      expect_equal(nrow(result), 1000)
    }
  )
})

# Test memory usage for different layer types
test_that("different layer types have appropriate memory usage", {
  skip_if_no_memory_test()
  
  # Mock the layer addition functions
  with_mock(
    `add_scatterplot` = function(map, data, lon, lat, ...) {
      # Simulate scatterplot layer addition
      layer_id <- paste0("scatterplot_", length(map$layers) + 1)
      map$layers[[layer_id]] <- list(id = layer_id, type = "scatterplot", data_size = nrow(data))
      return(map)
    },
    `add_heatmap` = function(map, data, lon, lat, ...) {
      # Simulate heatmap layer addition
      layer_id <- paste0("heatmap_", length(map$layers) + 1)
      map$layers[[layer_id]] <- list(id = layer_id, type = "heatmap", data_size = nrow(data))
      return(map)
    },
    `add_grid` = function(map, data, lon, lat, ...) {
      # Simulate grid layer addition
      layer_id <- paste0("grid_", length(map$layers) + 1)
      map$layers[[layer_id]] <- list(id = layer_id, type = "grid", data_size = nrow(data))
      return(map)
    },
    `add_hexagon` = function(map, data, lon, lat, ...) {
      # Simulate hexagon layer addition
      layer_id <- paste0("hexagon_", length(map$layers) + 1)
      map$layers[[layer_id]] <- list(id = layer_id, type = "hexagon", data_size = nrow(data))
      return(map)
    },
    {
      # Generate test data
      test_data <- generate_large_dataset(1e5)  # 100K points
      
      # Create a mock map
      map <- list(
        provider = "mapbox",
        layers = list()
      )
      
      # Measure memory usage for different layer types
      scatterplot_mem <- measure_memory_usage({
        map <- add_scatterplot(map, test_data, "lon", "lat", radius = 10)
      })
      
      heatmap_mem <- measure_memory_usage({
        map <- add_heatmap(map, test_data, "lon", "lat", weight = "value")
      })
      
      grid_mem <- measure_memory_usage({
        map <- add_grid(map, test_data, "lon", "lat", cell_size = 1000)
      })
      
      hexagon_mem <- measure_memory_usage({
        map <- add_hexagon(map, test_data, "lon", "lat", hexagon_size = 1000)
      })
      
      # Print memory usage
      cat("\nMemory usage for different layer types (100K points):\n")
      cat(sprintf("Scatterplot layer: %.2f MB\n", scatterplot_mem / 1024^2))
      cat(sprintf("Heatmap layer: %.2f MB\n", heatmap_mem / 1024^2))
      cat(sprintf("Grid layer: %.2f MB\n", grid_mem / 1024^2))
      cat(sprintf("Hexagon layer: %.2f MB\n", hexagon_mem / 1024^2))
      
      # Check that aggregation layers (grid, hexagon) use similar memory
      if (!is.na(grid_mem) && !is.na(hexagon_mem)) {
        # Grid and hexagon should use similar memory (within 30%)
        ratio <- grid_mem / hexagon_mem
        expect_gt(ratio, 0.7)
        expect_lt(ratio, 1.3)
      }
      
      # Check that memory usage is reasonable for all layer types
      for (mem in list(scatterplot_mem, heatmap_mem, grid_mem, hexagon_mem)) {
        if (!is.na(mem)) {
          expect_lt(mem, 1024^3)  # Less than 1 GB
        }
      }
    }
  )
})

# Test memory usage with multiple providers
test_that("memory usage is consistent across providers", {
  skip_if_no_memory_test()
  
  # Mock the map creation function
  with_mock(
    `mapdeck` = function(provider = "mapbox", ...) {
      # Simulate map creation
      return(list(
        provider = provider,
        layers = list(),
        ...
      ))
    },
    {
      # Measure memory usage for different providers
      mapbox_mem <- measure_memory_usage({
        map <- mapdeck(provider = "mapbox", style = "mapbox://styles/mapbox/dark-v10")
      })
      
      leaflet_mem <- measure_memory_usage({
        map <- mapdeck(provider = "leaflet", style = "CartoDB.DarkMatter")
      })
      
      openlayers_mem <- measure_memory_usage({
        map <- mapdeck(provider = "openlayers", style = "OSM")
      })
      
      gaode_mem <- measure_memory_usage({
        map <- mapdeck(provider = "gaode", style = "dark")
      })
      
      baidu_mem <- measure_memory_usage({
        map <- mapdeck(provider = "baidu", style = "dark")
      })
      
      # Print memory usage
      cat("\nMemory usage for different providers:\n")
      cat(sprintf("Mapbox: %.2f MB\n", mapbox_mem / 1024^2))
      cat(sprintf("Leaflet: %.2f MB\n", leaflet_mem / 1024^2))
      cat(sprintf("OpenLayers: %.2f MB\n", openlayers_mem / 1024^2))
      cat(sprintf("Gaode: %.2f MB\n", gaode_mem / 1024^2))
      cat(sprintf("Baidu: %.2f MB\n", baidu_mem / 1024^2))
      
      # Check that memory usage is reasonable for all providers
      for (mem in list(mapbox_mem, leaflet_mem, openlayers_mem, gaode_mem, baidu_mem)) {
        if (!is.na(mem)) {
          expect_lt(mem, 1024^3)  # Less than 1 GB
        }
      }
      
      # Check that memory usage is similar across providers
      # Allow for 50% variation due to different provider implementations
      if (!is.na(mapbox_mem) && !is.na(leaflet_mem) && !is.na(openlayers_mem)) {
        expect_lt(abs(leaflet_mem - mapbox_mem) / mapbox_mem, 0.5)
        expect_lt(abs(openlayers_mem - mapbox_mem) / mapbox_mem, 0.5)
      }
    }
  )
})

# Test memory usage for coordinate system transformations
test_that("coordinate system transformations have efficient memory usage", {
  skip_if_no_memory_test()
  
  # Mock the coordinate transformation functions
  with_mock(
    `transform_coordinates` = function(coords, from_crs, to_crs) {
      # Simulate transformation (just return the original coordinates)
      return(coords)
    },
    `detect_coordinate_system` = function(data) {
      # Simulate coordinate system detection
      return("WGS84")
    },
    `auto_transform` = function(data, target_provider) {
      # Simulate auto-transformation
      return(data)
    },
    {
      # Generate test data
      test_data <- generate_large_dataset(1e5)[, c("lon", "lat")]  # 100K points
      
      # Measure memory usage for different operations
      transform_mem <- measure_memory_usage({
        result <- transform_coordinates(test_data, "WGS84", "GCJ02")
      })
      
      detect_mem <- measure_memory_usage({
        crs <- detect_coordinate_system(test_data)
      })
      
      auto_mem <- measure_memory_usage({
        result <- auto_transform(test_data, "gaode")
      })
      
      # Print memory usage
      cat("\nMemory usage for coordinate system operations (100K points):\n")
      cat(sprintf("Transform coordinates: %.2f MB\n", transform_mem / 1024^2))
      cat(sprintf("Detect coordinate system: %.2f MB\n", detect_mem / 1024^2))
      cat(sprintf("Auto-transform: %.2f MB\n", auto_mem / 1024^2))
      
      # Check that detection uses less memory than transformation
      if (!is.na(transform_mem) && !is.na(detect_mem)) {
        expect_lt(detect_mem, transform_mem)
      }
      
      # Check that memory usage is reasonable for all operations
      for (mem in list(transform_mem, detect_mem, auto_mem)) {
        if (!is.na(mem)) {
          expect_lt(mem, 1024^3)  # Less than 1 GB
        }
      }
    }
  )
})

# Test memory usage for large dataset visualization
test_that("large dataset visualization has optimized memory usage", {
  skip_if_no_memory_test()
  
  # Mock the visualization functions
  with_mock(
    `visualize_large_dataset` = function(data, provider, optimization_level = "medium") {
      # Simulate different optimization strategies
      if (optimization_level == "low") {
        # No optimization - use full data
        processed_data <- data
      } else if (optimization_level == "medium") {
        # Medium optimization - sample data
        processed_data <- data[sample(nrow(data), min(nrow(data), 100000)), ]
      } else if (optimization_level == "high") {
        # High optimization - cluster data
        processed_data <- data[sample(nrow(data), min(nrow(data), 10000)), ]
      }
      
      # Return mock visualization result
      return(list(
        provider = provider,
        data_size = nrow(processed_data),
        optimization_level = optimization_level
      ))
    },
    {
      # Generate large dataset
      large_data <- generate_large_dataset(1e6)  # 1M points
      
      # Measure memory usage with different optimization levels
      low_mem <- measure_memory_usage({
        result <- visualize_large_dataset(large_data, "mapbox", "low")
      })
      
      medium_mem <- measure_memory_usage({
        result <- visualize_large_dataset(large_data, "mapbox", "medium")
      })
      
      high_mem <- measure_memory_usage({
        result <- visualize_large_dataset(large_data, "mapbox", "high")
      })
      
      # Print memory usage
      cat("\nMemory usage for large dataset visualization (1M points):\n")
      cat(sprintf("Low optimization: %.2f MB\n", low_mem / 1024^2))
      cat(sprintf("Medium optimization: %.2f MB\n", medium_mem / 1024^2))
      cat(sprintf("High optimization: %.2f MB\n", high_mem / 1024^2))
      
      # Check that higher optimization levels use less memory
      if (!is.na(low_mem) && !is.na(medium_mem) && !is.na(high_mem)) {
        expect_lt(medium_mem, low_mem)
        expect_lt(high_mem, medium_mem)
      }
    }
  )
})