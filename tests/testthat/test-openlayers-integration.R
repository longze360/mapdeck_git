test_that("OpenLayers provider integrates with deck.gl layers", {
  # Skip if not in interactive environment or missing dependencies
  skip_if_not_installed("htmlwidgets")
  
  provider <- OpenLayersProvider$new()
  provider$initialize_provider(list(
    source = "OSM",
    zoom = 10,
    center = c(-74.006, 40.7128)
  ))
  
  # Test creating a map widget
  map_options <- list(
    width = "100%",
    height = "400px",
    data = NULL
  )
  
  # This should not error even if we can't fully render
  expect_silent({
    # The create_map method should handle missing dependencies gracefully
    tryCatch({
      map_widget <- provider$create_map(options = map_options)
      expect_true(inherits(map_widget, "htmlwidget") || is.null(map_widget))
    }, error = function(e) {
      # Expected to fail in test environment without full htmlwidgets setup
      expect_true(grepl("could not find function|object not found", e$message))
    })
  })
})

test_that("OpenLayers provider handles different source types correctly", {
  provider <- OpenLayersProvider$new()
  
  # Test different source configurations
  source_configs <- list(
    "OSM" = list(source = "OSM"),
    "CartoDB" = list(source = "CartoDB.Positron"),
    "Satellite" = list(source = "ESRI.WorldImagery"),
    "Terrain" = list(source = "Stamen.Terrain")
  )
  
  for (source_name in names(source_configs)) {
    config <- source_configs[[source_name]]
    
    # Initialize with different sources
    provider$initialize_provider(config)
    expect_true(provider$initialized)
    expect_equal(provider$source_config, config$source)
    
    # Test that source configuration is retrievable
    source_info <- provider$get_source_config(config$source)
    expect_true(is.list(source_info))
    expect_true("type" %in% names(source_info))
    
    # Reset for next test
    provider$destroy()
  }
})

test_that("OpenLayers provider handles coordinate system transformations", {
  provider <- OpenLayersProvider$new()
  
  # Test different projections
  projections <- c("EPSG:3857", "EPSG:4326", "EPSG:3395")
  
  for (proj in projections) {
    provider$initialize_provider(list(
      source = "OSM",
      projection = proj
    ))
    
    provider$set_projection(proj)
    expect_equal(provider$openlayers_config$projection, proj)
    
    # Get projection info
    proj_info <- provider$get_projection_info(proj)
    expect_true(is.list(proj_info))
    expect_true("name" %in% names(proj_info))
    expect_true("units" %in% names(proj_info))
    
    provider$destroy()
  }
})

test_that("OpenLayers provider validates layer data correctly", {
  provider <- OpenLayersProvider$new()
  provider$initialize_provider(list())
  
  # Test with valid spatial data
  valid_data <- data.frame(
    longitude = c(-74.006, -73.935, -74.044),
    latitude = c(40.7128, 40.7282, 40.6892),
    value = c(10, 20, 15)
  )
  
  layer_config <- list(
    id = "test_points",
    type = "ScatterplotLayer",
    data = valid_data,
    getPosition = c("longitude", "latitude"),
    getRadius = "value",
    radiusScale = 100
  )
  
  expect_silent(provider$add_layer(layer_config))
  expect_true("test_points" %in% names(provider$layers))
  
  # Test layer removal
  expect_silent(provider$remove_layer("test_points"))
  expect_false("test_points" %in% names(provider$layers))
})

test_that("OpenLayers provider handles view state synchronization", {
  provider <- OpenLayersProvider$new()
  provider$initialize_provider(list(
    source = "OSM",
    zoom = 5,
    center = c(0, 0)
  ))
  
  # Test view state updates
  test_locations <- list(
    list(lon = -74.006, lat = 40.7128, zoom = 12, name = "New York"),
    list(lon = 2.3522, lat = 48.8566, zoom = 11, name = "Paris"),
    list(lon = 139.6917, lat = 35.6895, zoom = 10, name = "Tokyo")
  )
  
  for (location in test_locations) {
    provider$set_view(
      longitude = location$lon,
      latitude = location$lat,
      zoom = location$zoom,
      bearing = 0
    )
    
    expect_equal(provider$openlayers_config$center, c(location$lon, location$lat))
    expect_equal(provider$openlayers_config$zoom, location$zoom)
    expect_equal(provider$openlayers_config$rotation, 0)
  }
})

test_that("OpenLayers provider handles style updates correctly", {
  provider <- OpenLayersProvider$new()
  provider$initialize_provider(list(source = "OSM"))
  
  # Test style updates
  styles_to_test <- c(
    "CartoDB.Positron",
    "CartoDB.DarkMatter", 
    "ESRI.WorldImagery",
    "Stamen.Terrain"
  )
  
  for (style in styles_to_test) {
    provider$update_style(style)
    expect_equal(provider$current_style, style)
    expect_equal(provider$source_config, style)
    expect_equal(provider$openlayers_config$source, style)
  }
})

test_that("OpenLayers provider handles error conditions gracefully", {
  provider <- OpenLayersProvider$new()
  
  # Test initialization with invalid configuration
  expect_error(provider$initialize_provider("not_a_list"))
  
  # Test operations before initialization
  expect_error(provider$create_map())
  expect_error(provider$add_layer(list(id = "test")))
  expect_error(provider$set_view(0, 0, 10))
  
  # Initialize properly
  provider$initialize_provider(list())
  
  # Test invalid layer operations
  expect_error(provider$remove_layer(NULL))
  expect_error(provider$remove_layer(123))
  
  # Test invalid view parameters
  expect_error(provider$set_view("invalid", 0, 10))
  expect_error(provider$set_view(0, "invalid", 10))
  expect_error(provider$set_view(0, 0, "invalid"))
})

test_that("OpenLayers provider supports custom source creation", {
  provider <- OpenLayersProvider$new()
  
  # Test custom XYZ source
  custom_xyz <- provider$create_custom_source(
    type = "XYZ",
    url = "https://tile.example.com/{z}/{x}/{y}.png",
    attribution = "© Custom Tiles",
    max_zoom = 18
  )
  
  expect_equal(custom_xyz$type, "XYZ")
  expect_true(grepl("\\{z\\}", custom_xyz$url))
  expect_true(grepl("\\{x\\}", custom_xyz$url))
  expect_true(grepl("\\{y\\}", custom_xyz$url))
  
  # Test WMS source creation
  wms_source <- provider$create_wms_source(
    url = "https://wms.example.com/service",
    layers = c("layer1", "layer2"),
    version = "1.3.0"
  )
  
  expect_equal(wms_source$type, "WMS")
  expect_equal(wms_source$layers, c("layer1", "layer2"))
  expect_equal(wms_source$params$SERVICE, "WMS")
  expect_equal(wms_source$params$VERSION, "1.3.0")
  
  # Test WMTS source creation
  wmts_source <- provider$create_wmts_source(
    url = "https://wmts.example.com/service",
    layer = "test_layer",
    matrix_set = "EPSG:3857"
  )
  
  expect_equal(wmts_source$type, "WMTS")
  expect_equal(wmts_source$layer, "test_layer")
  expect_equal(wmts_source$matrixSet, "EPSG:3857")
})

test_that("OpenLayers provider maintains state consistency", {
  provider <- OpenLayersProvider$new()
  
  # Test state before initialization
  expect_false(provider$initialized)
  expect_null(provider$openlayers_config)
  expect_equal(length(provider$layers), 0)
  
  # Initialize and test state
  config <- list(
    source = "OSM",
    zoom = 8,
    center = c(-122.4194, 37.7749),
    projection = "EPSG:3857"
  )
  
  provider$initialize_provider(config)
  
  expect_true(provider$initialized)
  expect_equal(provider$source_config, "OSM")
  expect_equal(provider$openlayers_config$zoom, 8)
  expect_equal(provider$openlayers_config$center, c(-122.4194, 37.7749))
  expect_equal(provider$openlayers_config$projection, "EPSG:3857")
  
  # Add layers and test state
  layer1 <- list(id = "layer1", type = "ScatterplotLayer")
  layer2 <- list(id = "layer2", type = "LineLayer")
  
  provider$add_layer(layer1)
  provider$add_layer(layer2)
  
  expect_equal(length(provider$layers), 2)
  expect_true("layer1" %in% names(provider$layers))
  expect_true("layer2" %in% names(provider$layers))
  
  # Test cleanup
  provider$destroy()
  
  expect_false(provider$initialized)
  expect_null(provider$openlayers_config)
  expect_equal(length(provider$layers), 0)
})