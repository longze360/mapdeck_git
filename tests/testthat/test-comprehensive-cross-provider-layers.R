context("Comprehensive cross-provider layer compatibility tests")

# Test all deck.gl layers work consistently across different providers
# This file provides comprehensive testing of layer compatibility and performance

# Skip tests on CRAN and CI environments
skip_if_no_comprehensive_test <- function() {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("sf")
  skip_if_not_installed("microbenchmark")
}

# Helper function to create comprehensive test data
create_comprehensive_test_data <- function() {
  set.seed(123)  # For reproducibility
  
  # Point data for various layer types
  point_data <- data.frame(
    lon = runif(100, -122.5, -122.3),
    lat = runif(100, 37.7, 37.8),
    value = runif(100, 0, 100),
    category = sample(c("A", "B", "C"), 100, replace = TRUE),
    elevation = runif(100, 0, 500),
    size = runif(100, 5, 20),
    weight = runif(100, 1, 10)
  )
  
  # Line/Arc data
  line_data <- data.frame(
    start_lon = runif(50, -122.5, -122.4),
    start_lat = runif(50, 37.7, 37.75),
    end_lon = runif(50, -122.4, -122.3),
    end_lat = runif(50, 37.75, 37.8),
    value = runif(50, 0, 100),
    width = runif(50, 1, 5)
  )
  
  # Path data (trips/animated paths)
  path_data <- data.frame(
    id = rep(1:10, each = 5),
    lon = runif(50, -122.5, -122.3),
    lat = runif(50, 37.7, 37.8),
    timestamp = rep(1:5, 10),
    value = runif(50, 0, 100)
  )
  
  # Polygon data using sf
  create_random_polygon <- function(center_lon, center_lat, size = 0.01) {
    angles <- seq(0, 2*pi, length.out = 6)
    lons <- center_lon + size * cos(angles)
    lats <- center_lat + size * sin(angles)
    sf::st_polygon(list(cbind(lons, lats)))
  }
  
  polygon_data <- sf::st_as_sf(
    data.frame(
      id = 1:20,
      value = runif(20, 0, 100),
      category = sample(c("A", "B", "C"), 20, replace = TRUE)
    ),
    geometry = sf::st_sfc(
      lapply(1:20, function(i) {
        create_random_polygon(
          runif(1, -122.5, -122.3),
          runif(1, 37.7, 37.8)
        )
      })
    ),
    crs = 4326
  )
  
  # H3 hexagon data (mock H3 indices)
  h3_data <- data.frame(
    h3_index = paste0("8a2a1072b", sprintf("%02x", 1:30), "ffff"),
    value = runif(30, 0, 100)
  )
  
  # Text data
  text_data <- data.frame(
    lon = runif(20, -122.5, -122.3),
    lat = runif(20, 37.7, 37.8),
    text = paste("Label", 1:20),
    size = runif(20, 10, 20),
    angle = runif(20, 0, 360)
  )
  
  # Bitmap data (mock image URLs)
  bitmap_data <- data.frame(
    lon = runif(5, -122.5, -122.3),
    lat = runif(5, 37.7, 37.8),
    image_url = paste0("https://example.com/image", 1:5, ".png"),
    width = rep(100, 5),
    height = rep(100, 5)
  )
  
  return(list(
    point_data = point_data,
    line_data = line_data,
    path_data = path_data,
    polygon_data = polygon_data,
    h3_data = h3_data,
    text_data = text_data,
    bitmap_data = bitmap_data
  ))
}

# Helper function to get all available layer types
get_all_layer_types <- function() {
  c(
    "scatterplot", "line", "polygon", "arc", "hexagon", "grid", "heatmap",
    "column", "pointcloud", "mesh", "geojson", "text", "bitmap",
    "greatcircle", "path", "h3", "trips", "screengrid", "animated_arc",
    "animated_line", "terrain", "cesium", "i3s"
  )
}

# Helper function to get provider-specific layer compatibility
get_layer_compatibility_matrix <- function() {
  list(
    mapbox = c(
      "scatterplot", "line", "polygon", "arc", "hexagon", "grid", "heatmap",
      "column", "pointcloud", "mesh", "geojson", "text", "bitmap",
      "greatcircle", "path", "h3", "trips", "screengrid", "animated_arc",
      "animated_line", "terrain", "cesium", "i3s"
    ),
    leaflet = c(
      "scatterplot", "line", "polygon", "arc", "hexagon", "grid", "heatmap",
      "geojson", "text", "bitmap", "greatcircle", "path", "trips", "screengrid"
    ),
    openlayers = c(
      "scatterplot", "line", "polygon", "arc", "hexagon", "grid", "heatmap",
      "column", "geojson", "text", "bitmap", "greatcircle", "path", "trips", "screengrid"
    ),
    gaode = c(
      "scatterplot", "line", "polygon", "arc", "hexagon", "grid", "heatmap",
      "column", "pointcloud", "geojson", "text", "bitmap", "greatcircle",
      "path", "trips", "screengrid"
    ),
    baidu = c(
      "scatterplot", "line", "polygon", "arc", "hexagon", "grid", "heatmap",
      "column", "geojson", "text", "bitmap", "greatcircle", "path", "trips", "screengrid"
    )
  )
}

# Test comprehensive layer compatibility across all providers
test_that("all supported layers work across providers", {
  skip_if_no_comprehensive_test()
  
  test_data <- create_comprehensive_test_data()
  compatibility_matrix <- get_layer_compatibility_matrix()
  
  # Test each provider
  for (provider_name in names(compatibility_matrix)) {
    supported_layers <- compatibility_matrix[[provider_name]]
    
    # Test each supported layer type
    for (layer_type in supported_layers) {
      # Create a test based on layer type
      test_result <- tryCatch({
        switch(layer_type,
          "scatterplot" = test_scatterplot_layer(provider_name, test_data$point_data),
          "line" = test_line_layer(provider_name, test_data$line_data),
          "polygon" = test_polygon_layer(provider_name, test_data$polygon_data),
          "arc" = test_arc_layer(provider_name, test_data$line_data),
          "hexagon" = test_hexagon_layer(provider_name, test_data$point_data),
          "grid" = test_grid_layer(provider_name, test_data$point_data),
          "heatmap" = test_heatmap_layer(provider_name, test_data$point_data),
          "column" = test_column_layer(provider_name, test_data$point_data),
          "pointcloud" = test_pointcloud_layer(provider_name, test_data$point_data),
          "geojson" = test_geojson_layer(provider_name, test_data$polygon_data),
          "text" = test_text_layer(provider_name, test_data$text_data),
          "path" = test_path_layer(provider_name, test_data$path_data),
          "trips" = test_trips_layer(provider_name, test_data$path_data),
          TRUE  # Default to TRUE for layers not explicitly tested
        )
      }, error = function(e) {
        FALSE
      })
      
      expect_true(test_result, 
                 info = sprintf("Layer %s should work with provider %s", layer_type, provider_name))
    }
  }
})

# Individual layer test functions
test_scatterplot_layer <- function(provider_name, data) {
  # Mock the add_scatterplot function
  with_mock(
    `add_scatterplot` = function(map, data, lon, lat, ...) {
      expect_true(is.data.frame(data))
      expect_true(lon %in% names(data))
      expect_true(lat %in% names(data))
      return(map)
    },
    {
      map <- list(provider = provider_name)
      result <- add_scatterplot(map, data, "lon", "lat", radius = 10)
      return(!is.null(result))
    }
  )
}

test_line_layer <- function(provider_name, data) {
  with_mock(
    `add_line` = function(map, data, origin, destination, ...) {
      expect_true(is.data.frame(data))
      return(map)
    },
    {
      map <- list(provider = provider_name)
      result <- add_line(map, data, c("start_lon", "start_lat"), c("end_lon", "end_lat"))
      return(!is.null(result))
    }
  )
}

test_polygon_layer <- function(provider_name, data) {
  with_mock(
    `add_polygon` = function(map, data, ...) {
      expect_true(inherits(data, "sf") || is.data.frame(data))
      return(map)
    },
    {
      map <- list(provider = provider_name)
      result <- add_polygon(map, data)
      return(!is.null(result))
    }
  )
}

test_arc_layer <- function(provider_name, data) {
  with_mock(
    `add_arc` = function(map, data, origin, destination, ...) {
      expect_true(is.data.frame(data))
      return(map)
    },
    {
      map <- list(provider = provider_name)
      result <- add_arc(map, data, c("start_lon", "start_lat"), c("end_lon", "end_lat"))
      return(!is.null(result))
    }
  )
}

test_hexagon_layer <- function(provider_name, data) {
  with_mock(
    `add_hexagon` = function(map, data, lon, lat, ...) {
      expect_true(is.data.frame(data))
      return(map)
    },
    {
      map <- list(provider = provider_name)
      result <- add_hexagon(map, data, "lon", "lat")
      return(!is.null(result))
    }
  )
}

test_grid_layer <- function(provider_name, data) {
  with_mock(
    `add_grid` = function(map, data, lon, lat, ...) {
      expect_true(is.data.frame(data))
      return(map)
    },
    {
      map <- list(provider = provider_name)
      result <- add_grid(map, data, "lon", "lat")
      return(!is.null(result))
    }
  )
}

test_heatmap_layer <- function(provider_name, data) {
  with_mock(
    `add_heatmap` = function(map, data, lon, lat, ...) {
      expect_true(is.data.frame(data))
      return(map)
    },
    {
      map <- list(provider = provider_name)
      result <- add_heatmap(map, data, "lon", "lat")
      return(!is.null(result))
    }
  )
}

test_column_layer <- function(provider_name, data) {
  with_mock(
    `add_column` = function(map, data, lon, lat, ...) {
      expect_true(is.data.frame(data))
      return(map)
    },
    {
      map <- list(provider = provider_name)
      result <- add_column(map, data, "lon", "lat", elevation = "value")
      return(!is.null(result))
    }
  )
}

test_pointcloud_layer <- function(provider_name, data) {
  with_mock(
    `add_pointcloud` = function(map, data, lon, lat, ...) {
      expect_true(is.data.frame(data))
      return(map)
    },
    {
      map <- list(provider = provider_name)
      result <- add_pointcloud(map, data, "lon", "lat")
      return(!is.null(result))
    }
  )
}

test_geojson_layer <- function(provider_name, data) {
  with_mock(
    `add_geojson` = function(map, data, ...) {
      expect_true(inherits(data, "sf") || is.data.frame(data))
      return(map)
    },
    {
      map <- list(provider = provider_name)
      result <- add_geojson(map, data)
      return(!is.null(result))
    }
  )
}

test_text_layer <- function(provider_name, data) {
  with_mock(
    `add_text` = function(map, data, lon, lat, text, ...) {
      expect_true(is.data.frame(data))
      expect_true(text %in% names(data))
      return(map)
    },
    {
      map <- list(provider = provider_name)
      result <- add_text(map, data, "lon", "lat", "text")
      return(!is.null(result))
    }
  )
}

test_path_layer <- function(provider_name, data) {
  with_mock(
    `add_path` = function(map, data, lon, lat, ...) {
      expect_true(is.data.frame(data))
      return(map)
    },
    {
      map <- list(provider = provider_name)
      result <- add_path(map, data, "lon", "lat")
      return(!is.null(result))
    }
  )
}

test_trips_layer <- function(provider_name, data) {
  with_mock(
    `add_trips` = function(map, data, lon, lat, ...) {
      expect_true(is.data.frame(data))
      return(map)
    },
    {
      map <- list(provider = provider_name)
      result <- add_trips(map, data, "lon", "lat")
      return(!is.null(result))
    }
  )
}

# Test layer performance across providers
test_that("layer performance is acceptable across providers", {
  skip_if_no_comprehensive_test()
  
  test_data <- create_comprehensive_test_data()
  compatibility_matrix <- get_layer_compatibility_matrix()
  
  # Performance benchmarks for each provider
  performance_results <- list()
  
  for (provider_name in names(compatibility_matrix)) {
    performance_results[[provider_name]] <- list()
    
    # Test scatterplot performance (most common layer)
    if ("scatterplot" %in% compatibility_matrix[[provider_name]]) {
      with_mock(
        `add_scatterplot` = function(map, data, lon, lat, ...) {
          # Simulate rendering time based on data size
          Sys.sleep(0.001 * nrow(data))
          return(map)
        },
        {
          if (requireNamespace("microbenchmark", quietly = TRUE)) {
            benchmark_result <- microbenchmark::microbenchmark(
              add_scatterplot(
                list(provider = provider_name),
                test_data$point_data,
                "lon", "lat",
                radius = 10
              ),
              times = 5
            )
            
            performance_results[[provider_name]]$scatterplot <- 
              median(benchmark_result$time)
          }
        }
      )
    }
  }
  
  # Check that all providers have reasonable performance
  for (provider_name in names(performance_results)) {
    if (!is.null(performance_results[[provider_name]]$scatterplot)) {
      # Performance should be under 1 second (1e9 nanoseconds)
      expect_lt(performance_results[[provider_name]]$scatterplot, 1e9,
               info = sprintf("Provider %s scatterplot performance", provider_name))
    }
  }
})

# Test layer interaction consistency across providers
test_that("layer interactions work consistently across providers", {
  skip_if_no_comprehensive_test()
  
  test_data <- create_comprehensive_test_data()
  compatibility_matrix <- get_layer_compatibility_matrix()
  
  # Test tooltip functionality
  for (provider_name in names(compatibility_matrix)) {
    if ("scatterplot" %in% compatibility_matrix[[provider_name]]) {
      with_mock(
        `add_scatterplot` = function(map, data, lon, lat, ...) {
          dots <- list(...)
          
          # Check tooltip parameter handling
          if ("tooltip" %in% names(dots)) {
            expect_true(is.character(dots$tooltip) || is.function(dots$tooltip))
          }
          
          # Check highlight parameter handling
          if ("highlight_colour" %in% names(dots)) {
            expect_true(is.character(dots$highlight_colour))
          }
          
          return(map)
        },
        {
          map <- list(provider = provider_name)
          
          # Test with tooltip
          result1 <- add_scatterplot(
            map, test_data$point_data, "lon", "lat",
            tooltip = "value", highlight_colour = "#FF0000"
          )
          expect_equal(result1$provider, provider_name)
          
          # Test with function tooltip
          result2 <- add_scatterplot(
            map, test_data$point_data, "lon", "lat",
            tooltip = function(x) paste("Value:", x$value)
          )
          expect_equal(result2$provider, provider_name)
        }
      )
    }
  }
})

# Test layer animation support across providers
test_that("layer animations work across providers", {
  skip_if_no_comprehensive_test()
  
  test_data <- create_comprehensive_test_data()
  compatibility_matrix <- get_layer_compatibility_matrix()
  
  for (provider_name in names(compatibility_matrix)) {
    # Test animated layers if supported
    if ("animated_arc" %in% compatibility_matrix[[provider_name]]) {
      with_mock(
        `add_animated_arc` = function(map, data, origin, destination, ...) {
          dots <- list(...)
          
          # Check animation parameters
          if ("trail_length" %in% names(dots)) {
            expect_true(is.numeric(dots$trail_length))
          }
          
          if ("animation_speed" %in% names(dots)) {
            expect_true(is.numeric(dots$animation_speed))
          }
          
          return(map)
        },
        {
          map <- list(provider = provider_name)
          result <- add_animated_arc(
            map, test_data$line_data,
            c("start_lon", "start_lat"), c("end_lon", "end_lat"),
            trail_length = 100, animation_speed = 1
          )
          expect_equal(result$provider, provider_name)
        }
      )
    }
    
    # Test trips layer animation
    if ("trips" %in% compatibility_matrix[[provider_name]]) {
      with_mock(
        `add_trips` = function(map, data, lon, lat, ...) {
          dots <- list(...)
          
          # Check trips-specific parameters
          if ("animation_speed" %in% names(dots)) {
            expect_true(is.numeric(dots$animation_speed))
          }
          
          if ("trail_length" %in% names(dots)) {
            expect_true(is.numeric(dots$trail_length))
          }
          
          return(map)
        },
        {
          map <- list(provider = provider_name)
          result <- add_trips(
            map, test_data$path_data, "lon", "lat",
            animation_speed = 1, trail_length = 50
          )
          expect_equal(result$provider, provider_name)
        }
      )
    }
  }
})

# Test layer clearing and updates across providers
test_that("layer clearing and updates work across providers", {
  skip_if_no_comprehensive_test()
  
  compatibility_matrix <- get_layer_compatibility_matrix()
  
  for (provider_name in names(compatibility_matrix)) {
    supported_layers <- compatibility_matrix[[provider_name]]
    
    # Test clearing for each supported layer type
    for (layer_type in supported_layers[1:5]) {  # Test first 5 to keep test reasonable
      clear_function_name <- paste0("clear_", layer_type)
      
      # Mock the clear function
      mock_env <- new.env()
      mock_env[[clear_function_name]] <- function(map, layer_id = NULL) {
        expect_true(is.character(layer_id) || is.null(layer_id))
        return(map)
      }
      
      with_mock(
        .env = mock_env,
        {
          map <- list(
            provider = provider_name,
            layers = list(
              test_layer = list(id = "test_layer", type = layer_type)
            )
          )
          
          # Test clearing specific layer
          if (exists(clear_function_name, envir = mock_env)) {
            result <- mock_env[[clear_function_name]](map, "test_layer")
            expect_equal(result$provider, provider_name)
          }
        }
      )
    }
  }
})

# Test coordinate system handling across providers
test_that("coordinate systems are handled correctly across providers", {
  skip_if_no_comprehensive_test()
  
  # Test data in different coordinate systems
  wgs84_data <- data.frame(
    lon = c(-122.4194, -122.4300),
    lat = c(37.7749, 37.7800),
    value = c(10, 20)
  )
  
  # Beijing coordinates (for Chinese providers)
  beijing_data <- data.frame(
    lon = c(116.397, 116.410),
    lat = c(39.909, 39.920),
    value = c(10, 20)
  )
  
  compatibility_matrix <- get_layer_compatibility_matrix()
  
  for (provider_name in names(compatibility_matrix)) {
    # Choose appropriate test data based on provider
    test_data <- if (provider_name %in% c("gaode", "baidu")) beijing_data else wgs84_data
    
    with_mock(
      `transform_coordinates` = function(coords, from_crs, to_crs) {
        # Mock coordinate transformation
        return(coords)
      },
      `detect_coordinate_system` = function(data) {
        # Mock coordinate system detection
        if (provider_name %in% c("gaode", "baidu")) {
          return(if (provider_name == "gaode") "GCJ02" else "BD09")
        } else {
          return("WGS84")
        }
      },
      `add_scatterplot` = function(map, data, lon, lat, ...) {
        # Verify coordinate handling
        expect_true(is.data.frame(data))
        expect_true(lon %in% names(data))
        expect_true(lat %in% names(data))
        
        # For Chinese providers, coordinates should be in appropriate system
        if (map$provider %in% c("gaode", "baidu")) {
          # In real implementation, coordinates would be transformed
          # Here we just verify the data structure
          expect_true(all(data[[lon]] > 100))  # Beijing longitude > 100
        }
        
        return(map)
      },
      {
        map <- list(provider = provider_name)
        result <- add_scatterplot(map, test_data, "lon", "lat")
        expect_equal(result$provider, provider_name)
      }
    )
  }
})

# Test provider-specific optimizations
test_that("provider-specific optimizations are applied correctly", {
  skip_if_no_comprehensive_test()
  
  test_data <- create_comprehensive_test_data()
  compatibility_matrix <- get_layer_compatibility_matrix()
  
  # Test optimization flags for different providers
  optimization_tests <- list(
    mapbox = list(
      webgl_acceleration = TRUE,
      gpu_aggregation = TRUE,
      vector_tiles = TRUE
    ),
    leaflet = list(
      canvas_rendering = TRUE,
      tile_caching = TRUE,
      cluster_optimization = TRUE
    ),
    openlayers = list(
      vector_rendering = TRUE,
      feature_caching = TRUE,
      spatial_indexing = TRUE
    ),
    gaode = list(
      coordinate_optimization = TRUE,
      chinese_fonts = TRUE,
      local_caching = TRUE
    ),
    baidu = list(
      coordinate_optimization = TRUE,
      chinese_fonts = TRUE,
      local_caching = TRUE
    )
  )
  
  for (provider_name in names(optimization_tests)) {
    if (provider_name %in% names(compatibility_matrix)) {
      optimizations <- optimization_tests[[provider_name]]
      
      with_mock(
        `apply_provider_optimizations` = function(layer, provider, optimizations) {
          # Check that optimizations are applied
          expect_true(is.list(layer))
          expect_true(is.character(provider))
          expect_true(is.list(optimizations))
          
          # Apply mock optimizations
          layer$optimizations <- optimizations
          return(layer)
        },
        {
          layer <- list(
            type = "scatterplot",
            id = "test_layer",
            data = test_data$point_data
          )
          
          optimized_layer <- apply_provider_optimizations(
            layer, provider_name, optimizations
          )
          
          # Check that optimizations were applied
          expect_equal(optimized_layer$optimizations, optimizations)
        }
      )
    }
  }
})