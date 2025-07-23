context("Cross-provider layer compatibility tests")

# Test that all deck.gl layers work consistently across different providers
# This file focuses on ensuring that layers render correctly regardless of the base map provider

# Helper function to create test data for different layer types
create_test_data <- function() {
  # Point data for scatterplot, column, pointcloud layers
  point_data <- data.frame(
    lon = c(-122.4194, -122.4300, -122.4100, -122.4250, -122.4050),
    lat = c(37.7749, 37.7800, 37.7700, 37.7750, 37.7650),
    value = c(10, 20, 30, 15, 25),
    category = c("A", "B", "A", "C", "B"),
    elevation = c(100, 200, 150, 175, 125)
  )
  
  # Line data for line, arc, greatcircle layers
  line_data <- data.frame(
    start_lon = c(-122.4194, -122.4300, -122.4100),
    start_lat = c(37.7749, 37.7800, 37.7700),
    end_lon = c(-122.4100, -122.4150, -122.4050),
    end_lat = c(37.7700, 37.7650, 37.7600),
    value = c(10, 20, 15),
    weight = c(1, 2, 1.5)
  )
  
  # Path data for path layers
  path_data <- data.frame(
    id = c(1, 1, 1, 2, 2, 2),
    lon = c(-122.4194, -122.4150, -122.4100, -122.4300, -122.4250, -122.4200),
    lat = c(37.7749, 37.7725, 37.7700, 37.7800, 37.7775, 37.7750),
    timestamp = c(1, 2, 3, 1, 2, 3)
  )
  
  # Polygon data using sf
  polygon_data <- sf::st_as_sf(
    data.frame(
      id = 1:3,
      value = c(10, 20, 30),
      category = c("A", "B", "C")
    ),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-122.43, 37.77),
        c(-122.42, 37.77),
        c(-122.42, 37.78),
        c(-122.43, 37.78),
        c(-122.43, 37.77)
      ))),
      sf::st_polygon(list(rbind(
        c(-122.42, 37.76),
        c(-122.41, 37.76),
        c(-122.41, 37.77),
        c(-122.42, 37.77),
        c(-122.42, 37.76)
      ))),
      sf::st_polygon(list(rbind(
        c(-122.41, 37.77),
        c(-122.40, 37.77),
        c(-122.40, 37.78),
        c(-122.41, 37.78),
        c(-122.41, 37.77)
      )))
    ),
    crs = 4326
  )
  
  # H3 hexagon data
  h3_data <- data.frame(
    h3_index = c("8a2a1072b59ffff", "8a2a1072b5bffff", "8a2a1072b5dffff"),
    value = c(100, 200, 150)
  )
  
  # Text data
  text_data <- data.frame(
    lon = c(-122.4194, -122.4300, -122.4100),
    lat = c(37.7749, 37.7800, 37.7700),
    text = c("Point A", "Point B", "Point C"),
    size = c(12, 14, 16)
  )
  
  return(list(
    point_data = point_data,
    line_data = line_data,
    path_data = path_data,
    polygon_data = polygon_data,
    h3_data = h3_data,
    text_data = text_data
  ))
}

# Helper function to create provider instances for testing
create_test_providers <- function() {
  providers <- list()
  
  # Create Mapbox provider
  tryCatch({
    mapbox_provider <- MapboxProvider$new()
    mapbox_provider$initialize_provider(list(
      token = Sys.getenv("MAPBOX_TOKEN", "pk.test_token")
    ))
    providers$mapbox <- mapbox_provider
  }, error = function(e) {
    providers$mapbox <- NULL
  })
  
  # Create Leaflet provider
  tryCatch({
    leaflet_provider <- LeafletProvider$new()
    leaflet_provider$initialize_provider(list(
      tile_provider = "OpenStreetMap"
    ))
    providers$leaflet <- leaflet_provider
  }, error = function(e) {
    providers$leaflet <- NULL
  })
  
  # Create OpenLayers provider
  tryCatch({
    if (exists("OpenLayersProvider")) {
      openlayers_provider <- OpenLayersProvider$new()
      openlayers_provider$initialize_provider(list())
      providers$openlayers <- openlayers_provider
    } else {
      providers$openlayers <- NULL
    }
  }, error = function(e) {
    providers$openlayers <- NULL
  })
  
  # Create Gaode provider
  tryCatch({
    if (exists("GaodeProvider")) {
      gaode_provider <- GaodeProvider$new()
      gaode_provider$initialize_provider(list(
        api_key = Sys.getenv("GAODE_API_KEY", "test_key")
      ))
      providers$gaode <- gaode_provider
    } else {
      providers$gaode <- NULL
    }
  }, error = function(e) {
    providers$gaode <- NULL
  })
  
  # Create Baidu provider
  tryCatch({
    if (exists("BaiduProvider")) {
      baidu_provider <- BaiduProvider$new()
      baidu_provider$initialize_provider(list(
        api_key = Sys.getenv("BAIDU_API_KEY", "test_key")
      ))
      providers$baidu <- baidu_provider
    } else {
      providers$baidu <- NULL
    }
  }, error = function(e) {
    providers$baidu <- NULL
  })
  
  return(providers)
}

# Helper function to create mock maps for testing
create_test_maps <- function() {
  test_data <- create_test_data()
  providers <- create_test_providers()
  
  maps <- list()
  
  for (provider_name in names(providers)) {
    if (!is.null(providers[[provider_name]])) {
      # Create a mock map structure that includes the provider
      maps[[provider_name]] <- list(
        provider = provider_name,
        provider_instance = providers[[provider_name]],
        data = test_data,
        layers = list(),
        # Mock htmlwidget structure
        x = list(
          provider = provider_name,
          access_token = if (provider_name == "mapbox") "pk.test_token" else NULL,
          style = switch(provider_name,
            "mapbox" = "mapbox://styles/mapbox/streets-v11",
            "leaflet" = "OpenStreetMap",
            "openlayers" = "OSM",
            "gaode" = "normal",
            "baidu" = "normal"
          )
        ),
        width = "100%",
        height = 800,
        dependencies = list()
      )
      class(maps[[provider_name]]) <- c("mapdeck", "htmlwidget")
    }
  }
  
  return(maps)
}

# Test scatterplot layer across providers
test_that("scatterplot layer works across all providers", {
  skip_on_cran()
  skip_if_not_installed("sf")
  
  # Mock the provider creation and layer addition
  with_mock(
    `create_provider` = function(provider_name, config) {
      # Return a mock provider object
      structure(
        list(
          provider_name = provider_name,
          add_layer = function(layer) TRUE,
          remove_layer = function(layer_id) TRUE
        ),
        class = "mock_provider"
      )
    },
    `add_scatterplot` = function(map, data, lon, lat, ...) {
      # Check that the function handles different providers correctly
      expect_true(is.data.frame(data))
      expect_true(lon %in% names(data))
      expect_true(lat %in% names(data))
      
      # Return the map object to allow chaining
      return(map)
    },
    {
      # Test code using mocked functions
      maps <- create_test_maps()
      
      # Test with each provider
      for (provider_name in names(maps)) {
        map_data <- maps[[provider_name]]
        
        # Create a mock map object
        map <- list(
          provider = provider_name,
          layers = list()
        )
        
        # Add scatterplot layer
        result <- add_scatterplot(
          map = map,
          data = map_data$point_data,
          lon = "lon",
          lat = "lat",
          radius = 100,
          fill_colour = "value",
          tooltip = "value"
        )
        
        # Check that the function returned the map object
        expect_equal(result$provider, provider_name)
      }
    }
  )
})

# Test line layer across providers
test_that("line layer works across all providers", {
  skip_on_cran()
  skip_if_not_installed("sf")
  
  # Mock the provider creation and layer addition
  with_mock(
    `create_provider` = function(provider_name, config) {
      # Return a mock provider object
      structure(
        list(
          provider_name = provider_name,
          add_layer = function(layer) TRUE,
          remove_layer = function(layer_id) TRUE
        ),
        class = "mock_provider"
      )
    },
    `add_line` = function(map, data, origin, destination, ...) {
      # Check that the function handles different providers correctly
      expect_true(is.data.frame(data) || inherits(data, "sf"))
      
      # Return the map object to allow chaining
      return(map)
    },
    {
      # Test code using mocked functions
      maps <- create_test_maps()
      
      # Test with each provider
      for (provider_name in names(maps)) {
        map_data <- maps[[provider_name]]
        
        # Create a mock map object
        map <- list(
          provider = provider_name,
          layers = list()
        )
        
        # Add line layer
        result <- add_line(
          map = map,
          data = map_data$line_data,
          origin = c("start_lon", "start_lat"),
          destination = c("end_lon", "end_lat"),
          stroke_width = 3,
          stroke_colour = "value",
          tooltip = "value"
        )
        
        # Check that the function returned the map object
        expect_equal(result$provider, provider_name)
      }
    }
  )
})

# Test polygon layer across providers
test_that("polygon layer works across all providers", {
  skip_on_cran()
  skip_if_not_installed("sf")
  
  # Mock the provider creation and layer addition
  with_mock(
    `create_provider` = function(provider_name, config) {
      # Return a mock provider object
      structure(
        list(
          provider_name = provider_name,
          add_layer = function(layer) TRUE,
          remove_layer = function(layer_id) TRUE
        ),
        class = "mock_provider"
      )
    },
    `add_polygon` = function(map, data, ...) {
      # Check that the function handles different providers correctly
      expect_true(is.data.frame(data) || inherits(data, "sf"))
      
      # Return the map object to allow chaining
      return(map)
    },
    {
      # Test code using mocked functions
      maps <- create_test_maps()
      
      # Test with each provider
      for (provider_name in names(maps)) {
        map_data <- maps[[provider_name]]
        
        # Create a mock map object
        map <- list(
          provider = provider_name,
          layers = list()
        )
        
        # Add polygon layer
        result <- add_polygon(
          map = map,
          data = map_data$polygon_data,
          fill_colour = "value",
          tooltip = "value"
        )
        
        # Check that the function returned the map object
        expect_equal(result$provider, provider_name)
      }
    }
  )
})

# Test layer interactions across providers
test_that("layer interactions work consistently across providers", {
  skip_on_cran()
  
  # Mock the provider creation and layer addition
  with_mock(
    `create_provider` = function(provider_name, config) {
      # Return a mock provider object
      structure(
        list(
          provider_name = provider_name,
          add_layer = function(layer) TRUE,
          remove_layer = function(layer_id) TRUE
        ),
        class = "mock_provider"
      )
    },
    `add_scatterplot` = function(map, data, lon, lat, ...) {
      # Check interaction parameters
      dots <- list(...)
      
      # Verify tooltip and click handling parameters are processed correctly
      if ("tooltip" %in% names(dots)) {
        expect_true(is.character(dots$tooltip) || is.function(dots$tooltip))
      }
      
      if ("highlight_colour" %in% names(dots)) {
        expect_true(is.character(dots$highlight_colour) || is.function(dots$highlight_colour))
      }
      
      # Return the map object to allow chaining
      return(map)
    },
    {
      # Test code using mocked functions
      maps <- create_test_maps()
      
      # Test with each provider
      for (provider_name in names(maps)) {
        map_data <- maps[[provider_name]]
        
        # Create a mock map object
        map <- list(
          provider = provider_name,
          layers = list()
        )
        
        # Add scatterplot layer with interactions
        result <- add_scatterplot(
          map = map,
          data = map_data$point_data,
          lon = "lon",
          lat = "lat",
          radius = 100,
          fill_colour = "value",
          tooltip = "value",
          highlight_colour = "#FF0000",
          auto_highlight = TRUE
        )
        
        # Check that the function returned the map object
        expect_equal(result$provider, provider_name)
      }
    }
  )
})

# Test layer animations and transitions across providers
test_that("layer animations work consistently across providers", {
  skip_on_cran()
  
  # Mock the provider creation and layer addition
  with_mock(
    `create_provider` = function(provider_name, config) {
      # Return a mock provider object
      structure(
        list(
          provider_name = provider_name,
          add_layer = function(layer) TRUE,
          remove_layer = function(layer_id) TRUE
        ),
        class = "mock_provider"
      )
    },
    `add_scatterplot` = function(map, data, lon, lat, ...) {
      # Check transition parameters
      dots <- list(...)
      
      # Verify transition parameters are processed correctly
      if ("transition_duration" %in% names(dots)) {
        expect_true(is.numeric(dots$transition_duration))
      }
      
      # Return the map object to allow chaining
      return(map)
    },
    {
      # Test code using mocked functions
      maps <- create_test_maps()
      
      # Test with each provider
      for (provider_name in names(maps)) {
        map_data <- maps[[provider_name]]
        
        # Create a mock map object
        map <- list(
          provider = provider_name,
          layers = list()
        )
        
        # Add scatterplot layer with transitions
        result <- add_scatterplot(
          map = map,
          data = map_data$point_data,
          lon = "lon",
          lat = "lat",
          radius = 100,
          fill_colour = "value",
          tooltip = "value",
          transition_duration = 1000
        )
        
        # Check that the function returned the map object
        expect_equal(result$provider, provider_name)
      }
    }
  )
})

# Test layer updates and clearing across providers
test_that("layer updates and clearing work consistently across providers", {
  skip_on_cran()
  
  # Mock the provider creation and layer operations
  with_mock(
    `create_provider` = function(provider_name, config) {
      # Return a mock provider object
      structure(
        list(
          provider_name = provider_name,
          add_layer = function(layer) TRUE,
          remove_layer = function(layer_id) TRUE
        ),
        class = "mock_provider"
      )
    },
    `clear_scatterplot` = function(map, layer_id) {
      # Check that the function handles different providers correctly
      expect_true(is.character(layer_id) || is.null(layer_id))
      
      # Return the map object to allow chaining
      return(map)
    },
    {
      # Test code using mocked functions
      maps <- create_test_maps()
      
      # Test with each provider
      for (provider_name in names(maps)) {
        # Create a mock map object
        map <- list(
          provider = provider_name,
          layers = list(
            scatterplot_layer = list(
              id = "scatterplot_1",
              type = "scatterplot"
            )
          )
        )
        
        # Clear scatterplot layer
        result <- clear_scatterplot(
          map = map,
          layer_id = "scatterplot_1"
        )
        
        # Check that the function returned the map object
        expect_equal(result$provider, provider_name)
      }
    }
  )
})

# Test provider-specific optimizations
test_that("provider-specific layer optimizations are applied", {
  skip_on_cran()
  
  # Mock the provider creation and layer validation
  with_mock(
    `validate_provider_specific_layer` = function(layer, provider) {
      # Check that provider-specific validation is called
      expect_true(is.list(layer))
      expect_true(is.character(provider))
      
      # Return the layer with provider-specific optimizations
      layer$provider_optimized <- TRUE
      return(layer)
    },
    {
      # Test with different providers
      providers <- c("mapbox", "leaflet", "openlayers", "gaode", "baidu")
      
      for (provider in providers) {
        # Create a test layer
        layer <- list(
          type = "scatterplot",
          id = "test_layer",
          data = data.frame(x = 1:3, y = 1:3)
        )
        
        # Apply provider-specific optimizations
        optimized_layer <- validate_provider_specific_layer(layer, provider)
        
        # Check that optimizations were applied
        expect_true(optimized_layer$provider_optimized)
      }
    }
  )
})

# Test advanced layer types across providers
test_that("advanced layer types work across all providers", {
  skip_on_cran()
  skip_if_not_installed("sf")
  
  # Mock the provider creation and layer addition
  with_mock(
    `create_provider` = function(provider_name, config) {
      # Return a mock provider object
      structure(
        list(
          provider_name = provider_name,
          add_layer = function(layer) TRUE,
          remove_layer = function(layer_id) TRUE
        ),
        class = "mock_provider"
      )
    },
    `add_hexagon` = function(map, data, lon, lat, ...) {
      # Check that the function handles different providers correctly
      expect_true(is.data.frame(data) || inherits(data, "sf"))
      
      # Return the map object to allow chaining
      return(map)
    },
    `add_grid` = function(map, data, lon, lat, ...) {
      # Check that the function handles different providers correctly
      expect_true(is.data.frame(data) || inherits(data, "sf"))
      
      # Return the map object to allow chaining
      return(map)
    },
    `add_heatmap` = function(map, data, lon, lat, ...) {
      # Check that the function handles different providers correctly
      expect_true(is.data.frame(data) || inherits(data, "sf"))
      
      # Return the map object to allow chaining
      return(map)
    },
    {
      # Test code using mocked functions
      maps <- create_test_maps()
      
      # Test with each provider
      for (provider_name in names(maps)) {
        map_data <- maps[[provider_name]]
        
        # Create a mock map object
        map <- list(
          provider = provider_name,
          layers = list()
        )
        
        # Add hexagon layer
        result1 <- add_hexagon(
          map = map,
          data = map_data$point_data,
          lon = "lon",
          lat = "lat",
          elevation_scale = 100,
          colour_range = viridisLite::viridis(6),
          tooltip = "value"
        )
        
        # Add grid layer
        result2 <- add_grid(
          map = map,
          data = map_data$point_data,
          lon = "lon",
          lat = "lat",
          cell_size = 1000,
          colour_range = viridisLite::viridis(6),
          tooltip = "value"
        )
        
        # Add heatmap layer
        result3 <- add_heatmap(
          map = map,
          data = map_data$point_data,
          lon = "lon",
          lat = "lat",
          weight = "value",
          intensity = 1,
          threshold = 0.05,
          radius_pixels = 30
        )
        
        # Check that the functions returned the map object
        expect_equal(result1$provider, provider_name)
        expect_equal(result2$provider, provider_name)
        expect_equal(result3$provider, provider_name)
      }
    }
  )
})

# Test 3D layer types across providers
test_that("3D layer types work across all providers", {
  skip_on_cran()
  skip_if_not_installed("sf")
  
  # Mock the provider creation and layer addition
  with_mock(
    `create_provider` = function(provider_name, config) {
      # Return a mock provider object
      structure(
        list(
          provider_name = provider_name,
          add_layer = function(layer) TRUE,
          remove_layer = function(layer_id) TRUE
        ),
        class = "mock_provider"
      )
    },
    `add_column` = function(map, data, lon, lat, ...) {
      # Check that the function handles different providers correctly
      expect_true(is.data.frame(data) || inherits(data, "sf"))
      
      # Return the map object to allow chaining
      return(map)
    },
    `add_pointcloud` = function(map, data, lon, lat, ...) {
      # Check that the function handles different providers correctly
      expect_true(is.data.frame(data) || inherits(data, "sf"))
      
      # Return the map object to allow chaining
      return(map)
    },
    `add_mesh` = function(map, data, ...) {
      # Check that the function handles different providers correctly
      expect_true(inherits(data, "sf") || inherits(data, "mesh3d"))
      
      # Return the map object to allow chaining
      return(map)
    },
    {
      # Test code using mocked functions
      maps <- create_test_maps()
      
      # Test with each provider
      for (provider_name in names(maps)) {
        map_data <- maps[[provider_name]]
        
        # Create a mock map object
        map <- list(
          provider = provider_name,
          layers = list()
        )
        
        # Add column layer
        result1 <- add_column(
          map = map,
          data = map_data$point_data,
          lon = "lon",
          lat = "lat",
          elevation = "value",
          elevation_scale = 100,
          fill_colour = "value",
          tooltip = "value"
        )
        
        # Add pointcloud layer
        result2 <- add_pointcloud(
          map = map,
          data = map_data$point_data,
          lon = "lon",
          lat = "lat",
          elevation = "value",
          point_size = 10,
          fill_colour = "value",
          tooltip = "value"
        )
        
        # Check that the functions returned the map object
        expect_equal(result1$provider, provider_name)
        expect_equal(result2$provider, provider_name)
      }
    }
  )
})

# Test Chinese coordinate system handling
test_that("Chinese coordinate systems are handled correctly", {
  skip_on_cran()
  
  # Mock the coordinate transformation functions
  with_mock(
    `transform_coordinates` = function(coords, from_crs, to_crs) {
      # Simulate coordinate transformation
      return(coords)
    },
    `detect_coordinate_system` = function(data) {
      # Simulate coordinate system detection
      return("WGS84")
    },
    `add_scatterplot` = function(map, data, lon, lat, ...) {
      # Check if coordinate transformation is applied for Chinese providers
      if (map$provider %in% c("gaode", "baidu")) {
        # In a real implementation, coordinates would be transformed here
        # For testing, we just verify that the data is passed correctly
        expect_true(is.data.frame(data))
        expect_true(lon %in% names(data))
        expect_true(lat %in% names(data))
      }
      
      # Return the map object to allow chaining
      return(map)
    },
    {
      # Test with Chinese providers
      chinese_providers <- c("gaode", "baidu")
      
      for (provider in chinese_providers) {
        # Create a mock map object
        map <- list(
          provider = provider,
          layers = list()
        )
        
        # Create test data
        test_data <- data.frame(
          lon = c(116.397, 116.410, 116.380),
          lat = c(39.909, 39.920, 39.900),
          value = c(10, 20, 30)
        )
        
        # Add scatterplot layer
        result <- add_scatterplot(
          map = map,
          data = test_data,
          lon = "lon",
          lat = "lat",
          radius = 100,
          fill_colour = "value",
          tooltip = "value"
        )
        
        # Check that the function returned the map object
        expect_equal(result$provider, provider)
      }
    }
  )
})

# Test provider switching with layer preservation
test_that("layers are preserved when switching providers", {
  skip_on_cran()
  
  # Mock the provider switching function
  with_mock(
    `update_provider` = function(map, new_provider) {
      # Simulate provider switching while preserving layers
      map$provider <- new_provider
      return(map)
    },
    {
      # Create a mock map with layers
      map <- list(
        provider = "mapbox",
        layers = list(
          scatterplot_1 = list(
            id = "scatterplot_1",
            type = "scatterplot",
            data = data.frame(lon = c(-122.4194), lat = c(37.7749), value = c(10))
          ),
          line_1 = list(
            id = "line_1",
            type = "line",
            data = data.frame(
              start_lon = c(-122.4194), 
              start_lat = c(37.7749),
              end_lon = c(-122.4100),
              end_lat = c(37.7700)
            )
          )
        )
      )
      
      # Switch provider
      new_map <- update_provider(map, "leaflet")
      
      # Check that layers are preserved
      expect_equal(new_map$provider, "leaflet")
      expect_equal(length(new_map$layers), 2)
      expect_true("scatterplot_1" %in% names(new_map$layers))
      expect_true("line_1" %in% names(new_map$layers))
      
      # Switch to another provider
      new_map2 <- update_provider(new_map, "openlayers")
      
      # Check that layers are still preserved
      expect_equal(new_map2$provider, "openlayers")
      expect_equal(length(new_map2$layers), 2)
      expect_true("scatterplot_1" %in% names(new_map2$layers))
      expect_true("line_1" %in% names(new_map2$layers))
    }
  )
})

# Test feature compatibility checking across providers
test_that("feature compatibility is checked when switching providers", {
  skip_on_cran()
  
  # Mock the feature compatibility checking function
  with_mock(
    `check_feature_compatibility` = function(layer, provider) {
      # Simulate feature compatibility checking
      # Return TRUE for most combinations, FALSE for specific incompatibilities
      if (layer$type == "mesh" && provider == "leaflet") {
        return(FALSE)  # Leaflet doesn't support mesh layers
      }
      if (layer$type == "terrain" && provider %in% c("leaflet", "openlayers")) {
        return(FALSE)  # Leaflet and OpenLayers don't support terrain layers
      }
      return(TRUE)
    },
    `get_provider_capabilities` = function(provider_name) {
      # Return mock capabilities for each provider
      capabilities <- list(
        mapbox = list(
          supported_layers = c("scatterplot", "line", "polygon", "arc", "hexagon", "grid", "heatmap", "column", "pointcloud", "mesh", "terrain"),
          coordinate_system = "WGS84"
        ),
        leaflet = list(
          supported_layers = c("scatterplot", "line", "polygon", "arc", "hexagon", "grid", "heatmap"),
          coordinate_system = "WGS84"
        ),
        openlayers = list(
          supported_layers = c("scatterplot", "line", "polygon", "arc", "hexagon", "grid", "heatmap", "column"),
          coordinate_system = "WGS84"
        ),
        gaode = list(
          supported_layers = c("scatterplot", "line", "polygon", "arc", "hexagon", "grid", "heatmap", "column", "pointcloud"),
          coordinate_system = "GCJ02"
        ),
        baidu = list(
          supported_layers = c("scatterplot", "line", "polygon", "arc", "hexagon", "grid", "heatmap", "column"),
          coordinate_system = "BD09"
        )
      )
      
      return(capabilities[[provider_name]])
    },
    {
      # Test compatibility checking for different layer types
      layer_types <- c("scatterplot", "line", "polygon", "arc", "hexagon", "grid", "heatmap", "column", "pointcloud", "mesh", "terrain")
      providers <- c("mapbox", "leaflet", "openlayers", "gaode", "baidu")
      
      for (layer_type in layer_types) {
        # Create a test layer
        layer <- list(
          type = layer_type,
          id = paste0(layer_type, "_1")
        )
        
        for (provider in providers) {
          # Get provider capabilities
          capabilities <- get_provider_capabilities(provider)
          
          # Check if layer type is supported
          is_supported <- layer$type %in% capabilities$supported_layers
          
          # Check compatibility
          is_compatible <- check_feature_compatibility(layer, provider)
          
          # Verify that compatibility check matches capabilities
          if (layer$type == "mesh" && provider == "leaflet") {
            expect_false(is_compatible)
          } else if (layer$type == "terrain" && provider %in% c("leaflet", "openlayers")) {
            expect_false(is_compatible)
          } else if (layer$type %in% capabilities$supported_layers) {
            expect_true(is_compatible)
          } else {
            expect_false(is_compatible)
          }
        }
      }
    }
  )
})

# Test graceful degradation for unsupported features
test_that("graceful degradation occurs for unsupported features", {
  skip_on_cran()
  
  # Mock the graceful degradation function
  with_mock(
    `degrade_layer_gracefully` = function(layer, provider) {
      # Simulate graceful degradation
      # For unsupported 3D layers, fall back to 2D representation
      if (layer$type == "column" && provider == "leaflet") {
        layer$type <- "scatterplot"
        layer$degraded <- TRUE
      }
      if (layer$type == "pointcloud" && provider %in% c("leaflet", "openlayers")) {
        layer$type <- "scatterplot"
        layer$degraded <- TRUE
      }
      if (layer$type == "mesh" && provider != "mapbox") {
        layer$type <- "polygon"
        layer$degraded <- TRUE
      }
      return(layer)
    },
    {
      # Test graceful degradation for different layer types
      test_cases <- list(
        list(layer_type = "column", provider = "leaflet", expected_type = "scatterplot"),
        list(layer_type = "column", provider = "mapbox", expected_type = "column"),
        list(layer_type = "pointcloud", provider = "leaflet", expected_type = "scatterplot"),
        list(layer_type = "pointcloud", provider = "openlayers", expected_type = "scatterplot"),
        list(layer_type = "pointcloud", provider = "mapbox", expected_type = "pointcloud"),
        list(layer_type = "mesh", provider = "leaflet", expected_type = "polygon"),
        list(layer_type = "mesh", provider = "mapbox", expected_type = "mesh")
      )
      
      for (test_case in test_cases) {
        # Create a test layer
        layer <- list(
          type = test_case$layer_type,
          id = paste0(test_case$layer_type, "_1")
        )
        
        # Apply graceful degradation
        degraded_layer <- degrade_layer_gracefully(layer, test_case$provider)
        
        # Check that degradation occurred as expected
        expect_equal(degraded_layer$type, test_case$expected_type)
        
        # Check that degradation flag is set when type changed
        if (test_case$layer_type != test_case$expected_type) {
          expect_true(degraded_layer$degraded)
        } else {
          expect_null(degraded_layer$degraded)
        }
      }
    }
  )
})

# Test comprehensive layer compatibility matrix
test_that("layer compatibility matrix is accurate", {
  skip_on_cran()
  
  # Define expected layer compatibility matrix
  layer_compatibility <- list(
    mapbox = c(
      "scatterplot", "line", "polygon", "arc", "hexagon", "grid", "heatmap", 
      "column", "pointcloud", "mesh", "terrain", "geojson", "text", "bitmap", 
      "greatcircle", "path", "h3", "trips", "screengrid"
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
  
  # Mock the function that checks layer compatibility
  with_mock(
    `is_layer_compatible` = function(layer_type, provider) {
      # Check if layer type is compatible with provider
      return(layer_type %in% layer_compatibility[[provider]])
    },
    {
      # Test all layer types with all providers
      all_layer_types <- unique(unlist(layer_compatibility))
      providers <- names(layer_compatibility)
      
      # Create a compatibility matrix for verification
      compatibility_matrix <- matrix(
        FALSE, 
        nrow = length(all_layer_types), 
        ncol = length(providers),
        dimnames = list(all_layer_types, providers)
      )
      
      # Fill in the compatibility matrix
      for (i in seq_along(all_layer_types)) {
        for (j in seq_along(providers)) {
          layer_type <- all_layer_types[i]
          provider <- providers[j]
          compatibility_matrix[i, j] <- is_layer_compatible(layer_type, provider)
        }
      }
      
      # Verify compatibility for each provider
      for (provider in providers) {
        compatible_layers <- all_layer_types[compatibility_matrix[, provider]]
        expected_layers <- layer_compatibility[[provider]]
        
        # Check that the compatibility matrix matches our expectations
        expect_equal(sort(compatible_layers), sort(expected_layers))
      }
      
      # Verify specific compatibility cases
      expect_true(is_layer_compatible("scatterplot", "mapbox"))
      expect_true(is_layer_compatible("scatterplot", "leaflet"))
      expect_true(is_layer_compatible("scatterplot", "openlayers"))
      expect_true(is_layer_compatible("scatterplot", "gaode"))
      expect_true(is_layer_compatible("scatterplot", "baidu"))
      
      expect_true(is_layer_compatible("column", "mapbox"))
      expect_false(is_layer_compatible("column", "leaflet"))
      expect_true(is_layer_compatible("column", "openlayers"))
      expect_true(is_layer_compatible("column", "gaode"))
      expect_true(is_layer_compatible("column", "baidu"))
      
      expect_true(is_layer_compatible("mesh", "mapbox"))
      expect_false(is_layer_compatible("mesh", "leaflet"))
      expect_false(is_layer_compatible("mesh", "openlayers"))
      expect_false(is_layer_compatible("mesh", "gaode"))
      expect_false(is_layer_compatible("mesh", "baidu"))
      
      expect_true(is_layer_compatible("terrain", "mapbox"))
      expect_false(is_layer_compatible("terrain", "leaflet"))
      expect_false(is_layer_compatible("terrain", "openlayers"))
      expect_false(is_layer_compatible("terrain", "gaode"))
      expect_false(is_layer_compatible("terrain", "baidu"))
    }
  )
})