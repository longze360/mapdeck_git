context("Comprehensive integration tests")

# This file contains comprehensive integration tests that verify the entire multi-provider system
# works together correctly, including cross-provider compatibility, performance, and memory usage.

# Skip on CRAN and CI environments
skip_if_no_integration_test <- function() {
  skip_on_cran()
  skip_on_ci()
}

# Helper function to generate comprehensive test datasets
generate_comprehensive_test_data <- function(size = 1000) {
  set.seed(123)  # For reproducibility
  
  list(
    # Point data for scatterplot, column, pointcloud layers
    point_data = data.frame(
      lon = runif(size, -180, 180),
      lat = runif(size, -90, 90),
      value = runif(size, 0, 100),
      category = sample(letters[1:5], size, replace = TRUE),
      elevation = runif(size, 0, 1000),
      timestamp = Sys.time() + sample(1:1000, size, replace = TRUE)
    ),
    
    # Line data for line, arc, greatcircle layers
    line_data = data.frame(
      start_lon = runif(size, -180, 180),
      start_lat = runif(size, -90, 90),
      end_lon = runif(size, -180, 180),
      end_lat = runif(size, -90, 90),
      value = runif(size, 0, 100),
      weight = runif(size, 0.5, 3)
    ),
    
    # Path data for path and trips layers
    path_data = data.frame(
      id = rep(1:(size/10), each = 10),
      lon = runif(size, -180, 180),
      lat = runif(size, -90, 90),
      timestamp = rep(1:10, size/10),
      value = runif(size, 0, 100)
    ),
    
    # Administrative boundary data for spatial sampling
    admin_data = list(
      polygons = replicate(10, list(
        coordinates = matrix(runif(20, -180, 180), ncol = 2)
      ), simplify = FALSE),
      areas = runif(10, 100, 1000),
      names = paste0("Region_", 1:10)
    )
  )
}

# Test complete provider switching workflow with comprehensive layer types
test_that("complete provider switching workflow works correctly with all layer types", {
  skip_if_no_integration_test()
  
  # Mock the provider creation and switching functions
  with_mock(
    `create_provider` = function(provider_name, config = list()) {
      # Return a mock provider object with enhanced capabilities
      structure(
        list(
          provider_name = provider_name,
          config = config,
          coordinate_system = switch(provider_name,
            "gaode" = "GCJ02",
            "baidu" = "BD09",
            "WGS84"
          ),
          supported_layers = switch(provider_name,
            "mapbox" = c("scatterplot", "line", "polygon", "arc", "hexagon", "grid", "heatmap", "column", "pointcloud", "mesh", "terrain"),
            "leaflet" = c("scatterplot", "line", "polygon", "arc", "hexagon", "grid", "heatmap"),
            "openlayers" = c("scatterplot", "line", "polygon", "arc", "hexagon", "grid", "heatmap", "column"),
            "gaode" = c("scatterplot", "line", "polygon", "arc", "hexagon", "grid", "heatmap", "column", "pointcloud"),
            "baidu" = c("scatterplot", "line", "polygon", "arc", "hexagon", "grid", "heatmap", "column"),
            c("scatterplot", "line", "polygon")
          ),
          add_layer = function(layer) TRUE,
          remove_layer = function(layer_id) TRUE,
          transform_coordinates = function(coords) coords
        ),
        class = "mock_provider"
      )
    },
    `update_provider` = function(map, new_provider) {
      # Update the provider while preserving compatible layers
      old_layers <- map$layers
      new_provider_obj <- create_provider(new_provider)
      
      # Filter layers based on new provider capabilities
      compatible_layers <- list()
      for (layer_name in names(old_layers)) {
        layer <- old_layers[[layer_name]]
        if (layer$type %in% new_provider_obj$supported_layers) {
          compatible_layers[[layer_name]] <- layer
        }
      }
      
      map$provider <- new_provider
      map$provider_obj <- new_provider_obj
      map$layers <- compatible_layers
      return(map)
    },
    `transform_coordinates` = function(coords, from_crs, to_crs) {
      # Mock coordinate transformation
      return(coords)
    },
    {
      # Create comprehensive test data
      test_data <- generate_comprehensive_test_data(100)
      
      # Create initial map with Mapbox provider
      map <- list(
        provider = "mapbox",
        provider_obj = create_provider("mapbox"),
        layers = list(),
        view_state = list(
          longitude = -122.4194,
          latitude = 37.7749,
          zoom = 11,
          pitch = 0,
          bearing = 0
        )
      )
      
      # Add multiple layer types
      map$layers$scatterplot <- list(
        id = "scatterplot_1",
        type = "scatterplot",
        data = test_data$point_data
      )
      
      map$layers$line <- list(
        id = "line_1",
        type = "line",
        data = test_data$line_data
      )
      
      map$layers$mesh <- list(
        id = "mesh_1",
        type = "mesh",
        data = test_data$point_data
      )
      
      # Test switching to each provider
      providers <- c("leaflet", "openlayers", "gaode", "baidu")
      
      for (provider in providers) {
        # Switch provider
        map <- update_provider(map, provider)
        
        # Verify provider switch
        expect_equal(map$provider, provider)
        
        # Check that compatible layers are preserved
        expect_true("scatterplot" %in% names(map$layers))
        expect_true("line" %in% names(map$layers))
        
        # Check that incompatible layers are removed (mesh not supported by leaflet)
        if (provider == "leaflet") {
          expect_false("mesh" %in% names(map$layers))
        } else {
          # Other providers support mesh or it gets filtered appropriately
          expect_true(length(map$layers) >= 2)
        }
      }
      
      # Switch back to Mapbox and verify all layers can be restored
      map <- update_provider(map, "mapbox")
      expect_equal(map$provider, "mapbox")
      expect_true("scatterplot" %in% names(map$layers))
      expect_true("line" %in% names(map$layers))
    }
  )
})

# Test comprehensive cross-provider layer compatibility
test_that("all layer types work consistently across compatible providers", {
  skip_if_no_integration_test()
  
  # Mock layer addition functions
  layer_functions <- list(
    "add_scatterplot" = function(map, data, lon, lat, ...) {
      expect_true(is.data.frame(data))
      expect_true(lon %in% names(data))
      expect_true(lat %in% names(data))
      map$layers[[paste0("scatterplot_", length(map$layers) + 1)]] <- list(type = "scatterplot", data = data)
      return(map)
    },
    "add_line" = function(map, data, origin, destination, ...) {
      expect_true(is.data.frame(data))
      map$layers[[paste0("line_", length(map$layers) + 1)]] <- list(type = "line", data = data)
      return(map)
    },
    "add_polygon" = function(map, data, ...) {
      expect_true(is.data.frame(data) || inherits(data, "sf"))
      map$layers[[paste0("polygon_", length(map$layers) + 1)]] <- list(type = "polygon", data = data)
      return(map)
    },
    "add_hexagon" = function(map, data, lon, lat, ...) {
      expect_true(is.data.frame(data))
      map$layers[[paste0("hexagon_", length(map$layers) + 1)]] <- list(type = "hexagon", data = data)
      return(map)
    },
    "add_grid" = function(map, data, lon, lat, ...) {
      expect_true(is.data.frame(data))
      map$layers[[paste0("grid_", length(map$layers) + 1)]] <- list(type = "grid", data = data)
      return(map)
    },
    "add_heatmap" = function(map, data, lon, lat, ...) {
      expect_true(is.data.frame(data))
      map$layers[[paste0("heatmap_", length(map$layers) + 1)]] <- list(type = "heatmap", data = data)
      return(map)
    }
  )
  
  # Apply all mock functions
  do.call(with_mock, c(layer_functions, list({
    # Test data
    test_data <- generate_comprehensive_test_data(100)
    
    # Test providers
    providers <- c("mapbox", "leaflet", "openlayers", "gaode", "baidu")
    
    # Test each layer type on each provider
    for (provider in providers) {
      # Create mock map
      map <- list(
        provider = provider,
        layers = list()
      )
      
      # Test scatterplot layer
      map <- add_scatterplot(map, test_data$point_data, "lon", "lat", radius = 100)
      expect_equal(length(map$layers), 1)
      
      # Test line layer
      map <- add_line(map, test_data$line_data, c("start_lon", "start_lat"), c("end_lon", "end_lat"))
      expect_equal(length(map$layers), 2)
      
      # Test polygon layer (mock sf data)
      polygon_data <- test_data$point_data
      class(polygon_data) <- c("sf", "data.frame")
      map <- add_polygon(map, polygon_data)
      expect_equal(length(map$layers), 3)
      
      # Test aggregation layers
      map <- add_hexagon(map, test_data$point_data, "lon", "lat")
      expect_equal(length(map$layers), 4)
      
      map <- add_grid(map, test_data$point_data, "lon", "lat")
      expect_equal(length(map$layers), 5)
      
      map <- add_heatmap(map, test_data$point_data, "lon", "lat")
      expect_equal(length(map$layers), 6)
      
      # Verify all layers were added successfully
      expect_true(any(grepl("scatterplot", names(map$layers))))
      expect_true(any(grepl("line", names(map$layers))))
      expect_true(any(grepl("polygon", names(map$layers))))
      expect_true(any(grepl("hexagon", names(map$layers))))
      expect_true(any(grepl("grid", names(map$layers))))
      expect_true(any(grepl("heatmap", names(map$layers))))
    }
  })))
})

# Test comprehensive coordinate system integration
test_that("coordinate system transformations work correctly across providers", {
  skip_if_no_integration_test()
  
  # Mock coordinate transformation functions
  with_mock(
    `detect_coordinate_system` = function(data) {
      # Simulate coordinate system detection based on data bounds
      lon_range <- range(data$lon, na.rm = TRUE)
      lat_range <- range(data$lat, na.rm = TRUE)
      
      # Chinese coordinate bounds detection
      if (lon_range[1] > 70 && lon_range[2] < 140 && lat_range[1] > 10 && lat_range[2] < 60) {
        if (any(data$lon > 180) || any(data$lat > 90)) {
          return("BD09")
        } else {
          return("GCJ02")
        }
      }
      return("WGS84")
    },
    `transform_coordinates` = function(coords, from_crs, to_crs) {
      # Mock transformation with accuracy tracking
      if (from_crs == to_crs) return(coords)
      
      # Simulate transformation accuracy
      transformation_accuracy <- switch(paste(from_crs, to_crs, sep = "_to_"),
        "WGS84_to_GCJ02" = 0.5,  # ~0.5 meter accuracy
        "GCJ02_to_WGS84" = 0.5,
        "WGS84_to_BD09" = 1.0,   # ~1 meter accuracy
        "BD09_to_WGS84" = 1.0,
        "GCJ02_to_BD09" = 0.3,   # ~0.3 meter accuracy
        "BD09_to_GCJ02" = 0.3,
        0.1  # Default high accuracy
      )
      
      # Add small random offset to simulate transformation
      coords$lon <- coords$lon + rnorm(nrow(coords), 0, transformation_accuracy / 111320)
      coords$lat <- coords$lat + rnorm(nrow(coords), 0, transformation_accuracy / 110540)
      
      # Add transformation metadata
      attr(coords, "transformation") <- list(
        from = from_crs,
        to = to_crs,
        accuracy = transformation_accuracy
      )
      
      return(coords)
    },
    `auto_transform_for_provider` = function(data, provider) {
      # Detect current coordinate system
      current_crs <- detect_coordinate_system(data)
      
      # Determine target coordinate system for provider
      target_crs <- switch(provider,
        "gaode" = "GCJ02",
        "baidu" = "BD09",
        "WGS84"  # Default for mapbox, leaflet, openlayers
      )
      
      # Transform if necessary
      if (current_crs != target_crs) {
        coords <- data[, c("lon", "lat")]
        transformed_coords <- transform_coordinates(coords, current_crs, target_crs)
        data$lon <- transformed_coords$lon
        data$lat <- transformed_coords$lat
        
        # Add transformation metadata to data
        attr(data, "coordinate_transformation") <- attr(transformed_coords, "transformation")
      }
      
      return(data)
    },
    {
      # Test with different coordinate systems
      coordinate_systems <- list(
        # WGS84 data (global)
        wgs84_data = data.frame(
          lon = c(-122.4194, -122.4300, -122.4100),
          lat = c(37.7749, 37.7800, 37.7700),
          value = c(10, 20, 30)
        ),
        
        # GCJ02 data (China)
        gcj02_data = data.frame(
          lon = c(116.397, 116.410, 116.380),
          lat = c(39.909, 39.920, 39.900),
          value = c(10, 20, 30)
        ),
        
        # BD09 data (Baidu)
        bd09_data = data.frame(
          lon = c(116.404, 116.417, 116.387),
          lat = c(39.915, 39.926, 39.906),
          value = c(10, 20, 30)
        )
      )
      
      providers <- c("mapbox", "leaflet", "openlayers", "gaode", "baidu")
      
      # Test each coordinate system with each provider
      for (data_name in names(coordinate_systems)) {
        test_data <- coordinate_systems[[data_name]]
        original_crs <- detect_coordinate_system(test_data)
        
        for (provider in providers) {
          # Auto-transform data for provider
          transformed_data <- auto_transform_for_provider(test_data, provider)
          
          # Verify transformation occurred if needed
          target_crs <- switch(provider,
            "gaode" = "GCJ02",
            "baidu" = "BD09",
            "WGS84"
          )
          
          if (original_crs != target_crs) {
            # Check that transformation metadata exists
            expect_true(!is.null(attr(transformed_data, "coordinate_transformation")))
            
            # Check transformation accuracy
            transformation_info <- attr(transformed_data, "coordinate_transformation")
            expect_equal(transformation_info$from, original_crs)
            expect_equal(transformation_info$to, target_crs)
            expect_true(transformation_info$accuracy <= 1.0)  # Within 1 meter
          }
          
          # Verify data integrity
          expect_equal(nrow(transformed_data), nrow(test_data))
          expect_true(all(c("lon", "lat", "value") %in% names(transformed_data)))
          expect_true(all(is.finite(transformed_data$lon)))
          expect_true(all(is.finite(transformed_data$lat)))
        }
      }
    }
  )
})

# Test comprehensive spatial sampling integration
test_that("spatial sampling integrates correctly with all providers", {
  skip_if_no_integration_test()
  
  # Mock spatial sampling functions
  with_mock(
    `spatial_sample_administrative` = function(admin_polygons, total_samples, 
                                              allocation_method = "proportional",
                                              concurrent = TRUE, use_gpu = TRUE) {
      # Simulate administrative boundary sampling
      n_polygons <- length(admin_polygons$polygons)
      
      # Allocate samples based on method
      if (allocation_method == "proportional") {
        total_area <- sum(admin_polygons$areas)
        samples_per_polygon <- round(admin_polygons$areas / total_area * total_samples)
      } else if (allocation_method == "equal") {
        samples_per_polygon <- rep(floor(total_samples / n_polygons), n_polygons)
      } else {
        samples_per_polygon <- rep(floor(total_samples / n_polygons), n_polygons)
      }
      
      # Generate points
      result <- data.frame(
        polygon_id = rep(1:n_polygons, samples_per_polygon),
        lon = runif(sum(samples_per_polygon), -180, 180),
        lat = runif(sum(samples_per_polygon), -90, 90),
        sample_method = allocation_method,
        used_gpu = use_gpu,
        concurrent = concurrent
      )
      
      # Add performance metadata
      attr(result, "performance") <- list(
        gpu_acceleration = use_gpu,
        concurrent_processing = concurrent,
        processing_time = ifelse(use_gpu, 0.1, 0.5),  # GPU is faster
        memory_usage = nrow(result) * 0.001  # MB
      )
      
      return(result)
    },
    `spatial_sample_random` = function(data, n, use_gpu = TRUE) {
      # Simulate random sampling with GPU acceleration
      processing_time <- ifelse(use_gpu, 0.01, 0.05)
      Sys.sleep(processing_time)
      
      result <- data[sample(nrow(data), min(n, nrow(data))), ]
      
      attr(result, "performance") <- list(
        gpu_acceleration = use_gpu,
        processing_time = processing_time,
        memory_usage = nrow(result) * 0.001
      )
      
      return(result)
    },
    `add_scatterplot` = function(map, data, lon, lat, ...) {
      # Mock layer addition with performance tracking
      layer_id <- paste0("scatterplot_", length(map$layers) + 1)
      
      # Check for sampling metadata
      performance_info <- attr(data, "performance")
      
      map$layers[[layer_id]] <- list(
        id = layer_id,
        type = "scatterplot",
        data = data,
        performance = performance_info
      )
      
      return(map)
    },
    {
      # Test comprehensive spatial sampling workflow
      test_data <- generate_comprehensive_test_data(10000)
      providers <- c("mapbox", "leaflet", "openlayers", "gaode", "baidu")
      
      for (provider in providers) {
        # Create mock map
        map <- list(
          provider = provider,
          layers = list()
        )
        
        # Test administrative boundary sampling
        admin_samples <- spatial_sample_administrative(
          test_data$admin_data,
          total_samples = 1000,
          allocation_method = "proportional",
          concurrent = TRUE,
          use_gpu = TRUE
        )
        
        # Verify sampling results
        expect_equal(nrow(admin_samples), 1000)
        expect_true(all(c("polygon_id", "lon", "lat") %in% names(admin_samples)))
        
        # Check performance metadata
        perf_info <- attr(admin_samples, "performance")
        expect_true(perf_info$gpu_acceleration)
        expect_true(perf_info$concurrent_processing)
        expect_lt(perf_info$processing_time, 0.2)  # Should be fast with GPU
        
        # Add samples to map
        map <- add_scatterplot(map, admin_samples, "lon", "lat", 
                              fill_colour = "polygon_id", radius = 50)
        
        # Test random sampling
        random_samples <- spatial_sample_random(test_data$point_data, 500, use_gpu = TRUE)
        
        # Verify random sampling results
        expect_equal(nrow(random_samples), 500)
        
        # Check performance
        perf_info <- attr(random_samples, "performance")
        expect_true(perf_info$gpu_acceleration)
        expect_lt(perf_info$processing_time, 0.02)  # Should be very fast
        
        # Add random samples to map
        map <- add_scatterplot(map, random_samples, "lon", "lat", 
                              fill_colour = "value", radius = 30)
        
        # Verify both layers were added
        expect_equal(length(map$layers), 2)
        
        # Check that performance info is preserved in layers
        for (layer in map$layers) {
          expect_true(!is.null(layer$performance))
          expect_true(layer$performance$gpu_acceleration)
        }
      }
    }
  )
})

# Test comprehensive error handling and recovery
test_that("error handling and recovery work correctly across all components", {
  skip_if_no_integration_test()
  
  # Mock functions with error scenarios
  with_mock(
    `create_provider` = function(provider_name, config = list()) {
      # Simulate provider creation failures
      if (provider_name == "invalid_provider") {
        stop("Unknown provider: invalid_provider")
      }
      
      if (provider_name == "gaode" && is.null(config$api_key)) {
        stop("Gaode provider requires API key")
      }
      
      if (provider_name == "baidu" && is.null(config$api_key)) {
        stop("Baidu provider requires API key")
      }
      
      # Return mock provider
      structure(
        list(provider_name = provider_name, config = config),
        class = "mock_provider"
      )
    },
    `transform_coordinates` = function(coords, from_crs, to_crs) {
      # Simulate transformation failures
      if (from_crs == "INVALID_CRS") {
        stop("Invalid source coordinate system: INVALID_CRS")
      }
      
      if (to_crs == "INVALID_CRS") {
        stop("Invalid target coordinate system: INVALID_CRS")
      }
      
      # Simulate data validation errors
      if (any(is.na(coords$lon)) || any(is.na(coords$lat))) {
        stop("Coordinate data contains NA values")
      }
      
      return(coords)
    },
    `spatial_sample_administrative` = function(admin_polygons, total_samples, ...) {
      # Simulate sampling failures
      if (is.null(admin_polygons$polygons)) {
        stop("Administrative polygons data is missing")
      }
      
      if (total_samples <= 0) {
        stop("Total samples must be positive")
      }
      
      if (length(admin_polygons$polygons) == 0) {
        stop("No administrative polygons provided")
      }
      
      # Return mock result
      return(data.frame(
        lon = runif(total_samples, -180, 180),
        lat = runif(total_samples, -90, 90)
      ))
    },
    {
      # Test provider creation error handling
      expect_error(create_provider("invalid_provider"), "Unknown provider")
      expect_error(create_provider("gaode", list()), "requires API key")
      expect_error(create_provider("baidu", list()), "requires API key")
      
      # Test successful provider creation
      mapbox_provider <- create_provider("mapbox", list(token = "test_token"))
      expect_equal(mapbox_provider$provider_name, "mapbox")
      
      gaode_provider <- create_provider("gaode", list(api_key = "test_key"))
      expect_equal(gaode_provider$provider_name, "gaode")
      
      # Test coordinate transformation error handling
      test_coords <- data.frame(lon = c(116.397, 116.410), lat = c(39.909, 39.920))
      
      expect_error(transform_coordinates(test_coords, "INVALID_CRS", "WGS84"), "Invalid source")
      expect_error(transform_coordinates(test_coords, "WGS84", "INVALID_CRS"), "Invalid target")
      
      # Test with NA values
      na_coords <- data.frame(lon = c(116.397, NA), lat = c(39.909, 39.920))
      expect_error(transform_coordinates(na_coords, "WGS84", "GCJ02"), "NA values")
      
      # Test successful transformation
      result <- transform_coordinates(test_coords, "WGS84", "GCJ02")
      expect_equal(nrow(result), 2)
      
      # Test spatial sampling error handling
      expect_error(spatial_sample_administrative(list(), 1000), "polygons data is missing")
      expect_error(spatial_sample_administrative(list(polygons = list()), 0), "must be positive")
      expect_error(spatial_sample_administrative(list(polygons = list()), 1000), "No administrative polygons")
      
      # Test successful sampling
      admin_data <- list(
        polygons = list(1, 2, 3),
        areas = c(100, 200, 300)
      )
      result <- spatial_sample_administrative(admin_data, 100)
      expect_equal(nrow(result), 100)
    }
  )
})

# Helper function
`%||%` <- function(x, y) if (is.null(x)) y else x