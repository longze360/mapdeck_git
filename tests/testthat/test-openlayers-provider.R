test_that("OpenLayersProvider can be created and initialized", {
  # Create provider instance
  provider <- OpenLayersProvider$new()
  
  # Check initial state
  expect_equal(provider$provider_name, "openlayers")
  expect_false(provider$initialized)
  expect_equal(length(provider$layers), 0)
  
  # Initialize provider
  config <- list(
    source = "OSM",
    zoom = 5,
    center = c(-74.006, 40.7128)
  )
  
  provider$initialize_provider(config)
  
  # Check initialized state
  expect_true(provider$initialized)
  expect_equal(provider$source_config, "OSM")
  expect_equal(provider$current_style, "OSM")
  expect_equal(provider$openlayers_config$zoom, 5)
  expect_equal(provider$openlayers_config$center, c(-74.006, 40.7128))
})

test_that("OpenLayersProvider implements required interface methods", {
  provider <- OpenLayersProvider$new()
  
  # Check that all required methods exist
  required_methods <- c(
    "initialize_provider", "create_map", "update_style", "add_layer",
    "remove_layer", "set_view", "get_available_styles",
    "validate_config", "destroy"
  )
  
  for (method in required_methods) {
    expect_true(method %in% names(provider))
  }
})

test_that("OpenLayersProvider validates configuration correctly", {
  provider <- OpenLayersProvider$new()
  
  # Valid configuration
  valid_config <- list(
    source = "OSM",
    zoom = 10,
    center = c(0, 0)
  )
  
  expect_true(provider$validate_config(valid_config))
  
  # Invalid configuration (not a list)
  expect_false(provider$validate_config("invalid"))
  
  # Configuration with unknown source but no custom URL
  unknown_config <- list(
    source = "UnknownSource"
  )
  
  # Should still validate but with warning
  expect_warning(provider$validate_config(unknown_config))
})

test_that("OpenLayersProvider handles view state correctly", {
  provider <- OpenLayersProvider$new()
  provider$initialize_provider(list())
  
  # Set view state
  provider$set_view(
    longitude = -74.006,
    latitude = 40.7128,
    zoom = 12,
    pitch = 0,  # Should be ignored with warning
    bearing = 45
  )
  
  # Check internal state
  expect_equal(provider$openlayers_config$center, c(-74.006, 40.7128))
  expect_equal(provider$openlayers_config$zoom, 12)
  expect_equal(provider$openlayers_config$rotation, 45 * pi / 180)
  
  # Test coordinate validation
  expect_error(provider$set_view(longitude = 200, latitude = 0, zoom = 10))
  expect_error(provider$set_view(longitude = 0, latitude = 100, zoom = 10))
  expect_error(provider$set_view(longitude = 0, latitude = 0, zoom = -1))
  expect_error(provider$set_view(longitude = 0, latitude = 0, zoom = 30))
})

test_that("OpenLayersProvider returns available styles correctly", {
  provider <- OpenLayersProvider$new()
  
  # Get all styles
  all_styles <- provider$get_available_styles()
  expect_true(is.character(all_styles))
  expect_true(length(all_styles) > 0)
  expect_true("OSM" %in% all_styles)
  expect_true("CartoDB.Positron" %in% all_styles)
  
  # Get styles by category
  basic_styles <- provider$get_available_styles("basic")
  expect_true(is.character(basic_styles))
  expect_true("OSM" %in% basic_styles)
  
  satellite_styles <- provider$get_available_styles("satellite")
  expect_true(is.character(satellite_styles))
  expect_true("ESRI.WorldImagery" %in% satellite_styles)
  
  # Test unknown category
  expect_warning(unknown_styles <- provider$get_available_styles("unknown"))
  expect_equal(length(unknown_styles), 0)
})

test_that("OpenLayersProvider handles source configuration", {
  provider <- OpenLayersProvider$new()
  
  # Test known source configuration
  osm_config <- provider$get_source_config("OSM")
  expect_true(is.list(osm_config))
  expect_equal(osm_config$type, "OSM")
  expect_false(osm_config$requires_api_key)
  
  cartodb_config <- provider$get_source_config("CartoDB.Positron")
  expect_true(is.list(cartodb_config))
  expect_equal(cartodb_config$type, "XYZ")
  expect_true(grepl("cartocdn.com", cartodb_config$url))
  
  # Test unknown source
  unknown_config <- provider$get_source_config("UnknownSource")
  expect_true(is.list(unknown_config))
  expect_equal(unknown_config$type, "XYZ")
  expect_null(unknown_config$url)
})

test_that("OpenLayersProvider can create custom sources", {
  provider <- OpenLayersProvider$new()
  
  # Create valid custom XYZ source
  custom_source <- provider$create_custom_source(
    type = "XYZ",
    url = "https://example.com/{z}/{x}/{y}.png",
    attribution = "© Example",
    max_zoom = 18,
    min_zoom = 0
  )
  
  expect_true(is.list(custom_source))
  expect_equal(custom_source$type, "XYZ")
  expect_equal(custom_source$url, "https://example.com/{z}/{x}/{y}.png")
  expect_equal(custom_source$attribution, "© Example")
  expect_equal(custom_source$max_zoom, 18)
  expect_equal(custom_source$min_zoom, 0)
  expect_false(custom_source$requires_api_key)
  
  # Test validation errors
  expect_error(provider$create_custom_source("", "url"))  # Empty type
  expect_error(provider$create_custom_source("XYZ", ""))  # Empty URL
  expect_error(provider$create_custom_source("XYZ", "ftp://example.com"))  # Invalid protocol
  expect_error(provider$create_custom_source("XYZ", "https://example.com"))  # Missing placeholders
  expect_error(provider$create_custom_source("XYZ", "https://example.com/{z}/{x}/{y}.png", max_zoom = 0))  # Invalid zoom
})

test_that("OpenLayersProvider handles layer management", {
  provider <- OpenLayersProvider$new()
  provider$initialize_provider(list())
  
  # Add layer
  layer_config <- list(
    id = "test_layer",
    type = "ScatterplotLayer",
    data = data.frame(lon = c(-74, -73), lat = c(40, 41))
  )
  
  provider$add_layer(layer_config)
  expect_true("test_layer" %in% names(provider$layers))
  
  # Remove layer
  provider$remove_layer("test_layer")
  expect_false("test_layer" %in% names(provider$layers))
  
  # Test invalid layer ID
  expect_error(provider$remove_layer(123))
  expect_error(provider$remove_layer(c("a", "b")))
})

test_that("OpenLayersProvider can update styles", {
  provider <- OpenLayersProvider$new()
  provider$initialize_provider(list(source = "OSM"))
  
  # Update to different source
  provider$update_style("CartoDB.Positron")
  expect_equal(provider$current_style, "CartoDB.Positron")
  expect_equal(provider$source_config, "CartoDB.Positron")
  expect_equal(provider$openlayers_config$source, "CartoDB.Positron")
})

test_that("OpenLayersProvider can be destroyed cleanly", {
  provider <- OpenLayersProvider$new()
  provider$initialize_provider(list())
  
  # Add some layers
  provider$add_layer(list(id = "layer1", type = "ScatterplotLayer"))
  provider$add_layer(list(id = "layer2", type = "LineLayer"))
  
  # Destroy provider
  provider$destroy()
  
  # Check cleanup
  expect_equal(length(provider$layers), 0)
  expect_null(provider$map_instance)
  expect_null(provider$openlayers_config)
  expect_null(provider$source_config)
  expect_null(provider$current_style)
  expect_false(provider$initialized)
})

test_that("normalize_openlayers_source works correctly", {
  # Test known aliases
  expect_equal(normalize_openlayers_source("osm"), "OSM")
  expect_equal(normalize_openlayers_source("cartodb"), "CartoDB.Positron")
  expect_equal(normalize_openlayers_source("dark"), "CartoDB.DarkMatter")
  expect_equal(normalize_openlayers_source("satellite"), "ESRI.WorldImagery")
  expect_equal(normalize_openlayers_source("terrain"), "Stamen.Terrain")
  expect_equal(normalize_openlayers_source("vector"), "OSM.Vector")
  
  # Test case insensitivity
  expect_equal(normalize_openlayers_source("OSM"), "OSM")
  expect_equal(normalize_openlayers_source("CartoDB"), "CartoDB.Positron")
  
  # Test unknown source (should return as-is)
  expect_equal(normalize_openlayers_source("CustomSource"), "CustomSource")
  
  # Test NULL input
  expect_equal(normalize_openlayers_source(NULL), "OSM")
  
  # Test invalid input
  expect_error(normalize_openlayers_source(123))
  expect_error(normalize_openlayers_source(c("a", "b")))
})

test_that("OpenLayersProvider integrates with provider factory", {
  # Test that OpenLayers provider can be created through factory
  factory <- get_provider_factory()
  
  # Check that openlayers is available
  available_providers <- factory$get_available_providers()
  expect_true("openlayers" %in% available_providers)
  
  # Create provider through factory
  provider <- factory$create_provider("openlayers", list(source = "OSM"))
  expect_true(inherits(provider, "OpenLayersProvider"))
  expect_true(provider$initialized)
  expect_equal(provider$source_config, "OSM")
})