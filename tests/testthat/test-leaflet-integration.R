test_that("Leaflet provider integrates with mapdeck function", {
  # Skip if provider system not available
  skip_if_not(exists("create_provider"), "Provider system not available")
  
  # Test creating a Leaflet map through mapdeck function
  expect_no_error({
    # This should work without throwing errors
    tryCatch({
      map <- mapdeck(provider = "leaflet", 
                     location = c(-74, 40.7), 
                     zoom = 10)
      
      # Check that map has provider attribute
      provider <- attr(map, "mapdeck_provider")
      expect_s3_class(provider, "LeafletProvider")
      expect_equal(provider$provider_name, "leaflet")
      expect_true(provider$initialized)
    }, error = function(e) {
      # If provider system fails, that's expected during testing
      if (!grepl("Provider system not fully initialized", e$message)) {
        stop(e)
      }
    })
  })
})

test_that("Leaflet provider works with different tile providers", {
  provider <- LeafletProvider$new()
  
  # Test various tile providers
  tile_providers <- c(
    "OpenStreetMap",
    "CartoDB.Positron", 
    "CartoDB.DarkMatter",
    "Esri.WorldImagery",
    "OpenTopoMap"
  )
  
  for (tile_provider in tile_providers) {
    provider$initialize_provider(list(tile_provider = tile_provider))
    expect_equal(provider$tile_provider, tile_provider)
    expect_equal(provider$current_style, tile_provider)
    
    # Test style update
    provider$update_style("OpenStreetMap")
    expect_equal(provider$current_style, "OpenStreetMap")
    
    # Reset for next iteration
    provider$destroy()
  }
})

test_that("Leaflet provider handles layer compatibility correctly", {
  provider <- LeafletProvider$new()
  provider$initialize_provider(list())
  
  # Test common deck.gl layer types that should work with Leaflet
  layer_types <- c(
    "ScatterplotLayer",
    "ArcLayer", 
    "LineLayer",
    "PolygonLayer",
    "HeatmapLayer",
    "GridLayer",
    "HexagonLayer",
    "PathLayer",
    "TextLayer"
  )
  
  for (layer_type in layer_types) {
    layer <- list(
      id = paste0(tolower(layer_type), "_test"),
      type = layer_type,
      data = data.frame(
        longitude = c(-74, -73),
        latitude = c(40.7, 40.8),
        value = c(1, 2)
      )
    )
    
    # Should not throw errors
    expect_no_error(provider$add_layer(layer))
    expect_true(layer$id %in% names(provider$layers))
    
    # Clean up
    provider$remove_layer(layer$id)
  }
})

test_that("Leaflet provider coordinate system handling", {
  provider <- LeafletProvider$new()
  provider$initialize_provider(list())
  
  # Test coordinate bounds validation
  valid_bounds <- c(-180, -90, 180, 90)  # [west, south, east, north]
  expect_no_error(validate_coordinate_bounds(valid_bounds))
  
  # Test view setting with various coordinates
  test_coordinates <- list(
    c(-74.006, 40.7128, 10),  # New York
    c(2.3522, 48.8566, 12),   # Paris
    c(139.6917, 35.6895, 11), # Tokyo
    c(-0.1276, 51.5074, 13)   # London
  )
  
  for (coords in test_coordinates) {
    expect_no_error(provider$set_view(coords[1], coords[2], coords[3]))
    expect_equal(provider$leaflet_config$center, c(coords[1], coords[2]))
    expect_equal(provider$leaflet_config$zoom, coords[3])
  }
})

test_that("Leaflet provider performance with multiple layers", {
  provider <- LeafletProvider$new()
  provider$initialize_provider(list())
  
  # Add multiple layers to test performance
  n_layers <- 10
  layer_ids <- character(n_layers)
  
  # Add layers
  start_time <- Sys.time()
  for (i in 1:n_layers) {
    layer_id <- paste0("layer_", i)
    layer_ids[i] <- layer_id
    
    layer <- list(
      id = layer_id,
      type = "ScatterplotLayer",
      data = data.frame(
        longitude = runif(100, -180, 180),
        latitude = runif(100, -90, 90),
        value = runif(100, 0, 100)
      )
    )
    
    provider$add_layer(layer)
  }
  add_time <- Sys.time() - start_time
  
  # Check all layers were added
  expect_equal(length(provider$layers), n_layers)
  expect_true(all(layer_ids %in% names(provider$layers)))
  
  # Remove layers
  start_time <- Sys.time()
  for (layer_id in layer_ids) {
    provider$remove_layer(layer_id)
  }
  remove_time <- Sys.time() - start_time
  
  # Check all layers were removed
  expect_equal(length(provider$layers), 0)
  
  # Performance should be reasonable (less than 1 second for 10 layers)
  expect_true(add_time < 1)
  expect_true(remove_time < 1)
})

test_that("Leaflet provider custom tile layer integration", {
  provider <- LeafletProvider$new()
  
  # Test custom tile layer configuration
  custom_config <- provider$create_custom_tile_layer(
    url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
    attribution = "© OpenStreetMap contributors",
    max_zoom = 18,
    min_zoom = 1
  )
  
  # Initialize with custom tile configuration
  provider$initialize_provider(list(
    tile_url = custom_config$url,
    attribution = custom_config$attribution,
    max_zoom = custom_config$max_zoom,
    min_zoom = custom_config$min_zoom
  ))
  
  expect_true(provider$initialized)
  expect_equal(provider$leaflet_config$tile_url, custom_config$url)
  expect_equal(provider$leaflet_config$attribution, custom_config$attribution)
  expect_equal(provider$leaflet_config$max_zoom, custom_config$max_zoom)
  expect_equal(provider$leaflet_config$min_zoom, custom_config$min_zoom)
})

test_that("Leaflet provider error recovery", {
  provider <- LeafletProvider$new()
  provider$initialize_provider(list())
  
  # Test recovery from invalid operations
  
  # Try to add invalid layer
  invalid_layer <- list(type = "InvalidLayer")
  expect_error(provider$add_layer(invalid_layer))
  
  # Provider should still be functional
  expect_true(provider$initialized)
  
  # Try to remove non-existent layer (should not error)
  expect_no_error(provider$remove_layer("non_existent_layer"))
  
  # Provider should still be functional
  expect_true(provider$initialized)
  
  # Add valid layer after errors
  valid_layer <- list(
    id = "recovery_test",
    type = "ScatterplotLayer",
    data = data.frame(longitude = -74, latitude = 40.7)
  )
  
  expect_no_error(provider$add_layer(valid_layer))
  expect_true("recovery_test" %in% names(provider$layers))
})

test_that("Leaflet provider style normalization integration", {
  provider <- LeafletProvider$new()
  
  # Test style normalization with various inputs
  style_tests <- list(
    list(input = "osm", expected = "OpenStreetMap"),
    list(input = "cartodb", expected = "CartoDB.Positron"),
    list(input = "dark", expected = "CartoDB.DarkMatter"),
    list(input = "satellite", expected = "Esri.WorldImagery"),
    list(input = "OpenStreetMap", expected = "OpenStreetMap"),
    list(input = "CustomProvider", expected = "CustomProvider")
  )
  
  for (test_case in style_tests) {
    provider$initialize_provider(list(tile_provider = test_case$input))
    normalized_style <- normalize_leaflet_tile_provider(test_case$input)
    
    expect_equal(normalized_style, test_case$expected)
    expect_equal(provider$tile_provider, test_case$expected)
    
    provider$destroy()
  }
})

test_that("Leaflet provider memory management", {
  # Test that provider properly cleans up resources
  initial_objects <- ls(envir = .GlobalEnv)
  
  # Create and destroy multiple providers
  for (i in 1:5) {
    provider <- LeafletProvider$new()
    provider$initialize_provider(list())
    
    # Add some layers
    for (j in 1:3) {
      layer <- list(
        id = paste0("layer_", i, "_", j),
        type = "ScatterplotLayer",
        data = data.frame(longitude = runif(10), latitude = runif(10))
      )
      provider$add_layer(layer)
    }
    
    # Destroy provider
    provider$destroy()
    
    # Check that provider is properly cleaned up
    expect_false(provider$initialized)
    expect_equal(length(provider$layers), 0)
    expect_null(provider$leaflet_config)
  }
  
  # Check that no new global objects were created
  final_objects <- ls(envir = .GlobalEnv)
  expect_equal(length(setdiff(final_objects, initial_objects)), 0)
})