test_that("GaodeProvider implements IMapProvider interface", {
  skip_if_not_installed("R6")
  
  # Test that GaodeProvider class exists and inherits from IMapProvider
  expect_true(exists("GaodeProvider"))
  expect_true(inherits(GaodeProvider, "R6ClassGenerator"))
  
  # Create instance
  provider <- GaodeProvider$new()
  expect_true(inherits(provider, "GaodeProvider"))
  expect_true(inherits(provider, "IMapProvider"))
  
  # Test provider identification
  expect_equal(provider$provider_name, "gaode")
  expect_false(provider$initialized)
})

test_that("GaodeProvider initialization works correctly", {
  skip_if_not_installed("R6")
  
  provider <- GaodeProvider$new()
  
  # Test initialization with API key
  config <- list(
    api_key = "test_gaode_api_key_12345",
    style = "amap://styles/normal",
    zoom = 12,
    location = c(116.397, 39.909)
  )
  
  expect_silent(provider$initialize_provider(config))
  expect_true(provider$initialized)
  expect_equal(provider$api_key, "test_gaode_api_key_12345")
  expect_equal(provider$current_style, "amap://styles/normal")
})

test_that("GaodeProvider initialization fails without API key", {
  skip_if_not_installed("R6")
  
  provider <- GaodeProvider$new()
  
  # Test initialization without API key should fail
  config <- list(
    style = "amap://styles/normal",
    zoom = 12
  )
  
  expect_error(
    provider$initialize_provider(config),
    "Gaode Maps API key is required"
  )
})

test_that("GaodeProvider coordinate transformation works", {
  skip_if_not_installed("R6")
  
  provider <- GaodeProvider$new()
  provider$initialize_provider(list(api_key = "test_key"))
  
  # Test WGS84 to GCJ02 transformation
  wgs84_coords <- c(116.3974, 39.9093) # Beijing
  gcj02_coords <- provider$transform_coordinates(wgs84_coords, "WGS84", "GCJ02")
  
  expect_type(gcj02_coords, "double")
  expect_length(gcj02_coords, 2)
  
  # GCJ02 coordinates should be slightly different from WGS84
  expect_false(identical(wgs84_coords, gcj02_coords))
  
  # Test coordinate system detection
  test_data <- data.frame(
    longitude = c(116.3974, 116.4074),
    latitude = c(39.9093, 39.9193)
  )
  
  detected_crs <- provider$detect_coordinate_system(test_data)
  expect_type(detected_crs, "character")
  expect_true(detected_crs %in% c("WGS84", "GCJ02", "BD09"))
})

test_that("GaodeProvider data preparation works", {
  skip_if_not_installed("R6")
  
  provider <- GaodeProvider$new()
  provider$initialize_provider(list(api_key = "test_key"))
  
  # Test data preparation with coordinate transformation
  test_data <- data.frame(
    longitude = c(116.3974, 116.4074),
    latitude = c(39.9093, 39.9193),
    value = c(10, 20)
  )
  
  # Suppress transformation messages for testing
  suppressMessages({
    prepared_data <- provider$prepare_data(test_data, auto_transform = TRUE)
  })
  
  expect_true(is.data.frame(prepared_data))
  expect_equal(ncol(prepared_data), 3)
  expect_equal(nrow(prepared_data), 2)
  expect_true(all(c("longitude", "latitude", "value") %in% names(prepared_data)))
})

test_that("GaodeProvider style management works", {
  skip_if_not_installed("R6")
  
  provider <- GaodeProvider$new()
  provider$initialize_provider(list(api_key = "test_key"))
  
  # Test getting available styles
  styles <- provider$get_available_styles()
  expect_type(styles, "character")
  expect_true(length(styles) > 0)
  expect_true("amap://styles/normal" %in% styles)
  
  # Test style categories
  basic_styles <- provider$get_available_styles("basic")
  expect_type(basic_styles, "character")
  expect_true("amap://styles/normal" %in% basic_styles)
  
  satellite_styles <- provider$get_available_styles("satellite")
  expect_type(satellite_styles, "character")
  expect_true("amap://styles/satellite" %in% satellite_styles)
  
  # Test unknown category
  expect_warning(
    unknown_styles <- provider$get_available_styles("unknown"),
    "Unknown style category"
  )
  expect_length(unknown_styles, 0)
})

test_that("GaodeProvider view management works", {
  skip_if_not_installed("R6")
  
  provider <- GaodeProvider$new()
  provider$initialize_provider(list(api_key = "test_key"))
  
  # Test setting view
  expect_silent(provider$set_view(116.397, 39.909, 12, 0, 0))
  
  # Test view validation
  expect_error(provider$set_view(-200, 39.909, 12), "Longitude must be")
  expect_error(provider$set_view(116.397, 100, 12), "Latitude must be")
  expect_error(provider$set_view(116.397, 39.909, -1), "Zoom must be")
  expect_error(provider$set_view(116.397, 39.909, 12, -1), "Pitch must be")
  expect_error(provider$set_view(116.397, 39.909, 12, 0, 400), "Bearing must be")
})

test_that("GaodeProvider layer management works", {
  skip_if_not_installed("R6")
  
  provider <- GaodeProvider$new()
  provider$initialize_provider(list(api_key = "test_key"))
  
  # Test adding layer
  layer_config <- list(
    id = "test_layer",
    type = "scatterplot",
    data = data.frame(
      longitude = c(116.3974, 116.4074),
      latitude = c(39.9093, 39.9193)
    )
  )
  
  expect_silent(provider$add_layer(layer_config))
  expect_true("test_layer" %in% names(provider$layers))
  
  # Test removing layer
  expect_silent(provider$remove_layer("test_layer"))
  expect_false("test_layer" %in% names(provider$layers))
  
  # Test removing non-existent layer
  expect_silent(provider$remove_layer("non_existent"))
})

test_that("GaodeProvider configuration validation works", {
  skip_if_not_installed("R6")
  
  provider <- GaodeProvider$new()
  
  # Test valid configuration
  valid_config <- list(api_key = "test_key_12345")
  expect_true(provider$validate_config(valid_config))
  
  # Test invalid configuration (not a list)
  expect_false(provider$validate_config("not_a_list"))
  
  # Test configuration with short API key
  expect_warning(
    provider$validate_config(list(api_key = "short")),
    "API key appears to be too short"
  )
})

test_that("GaodeProvider destruction works", {
  skip_if_not_installed("R6")
  
  provider <- GaodeProvider$new()
  provider$initialize_provider(list(api_key = "test_key"))
  
  # Add some data
  provider$layers[["test"]] <- list(id = "test")
  provider$map_instance <- "test_instance"
  
  # Test destruction
  expect_silent(provider$destroy())
  expect_length(provider$layers, 0)
  expect_null(provider$map_instance)
  expect_null(provider$gaode_config)
  expect_null(provider$api_key)
  expect_false(provider$initialized)
})

test_that("GaodeProvider coordinate transformation accuracy", {
  skip_if_not_installed("R6")
  
  provider <- GaodeProvider$new()
  provider$initialize_provider(list(api_key = "test_key"))
  
  # Test transformation accuracy with known coordinates
  # Beijing Tiananmen Square coordinates
  wgs84_beijing <- c(116.3974, 39.9093)
  
  # Transform WGS84 -> GCJ02 -> WGS84
  gcj02_beijing <- provider$transform_coordinates(wgs84_beijing, "WGS84", "GCJ02")
  back_to_wgs84 <- provider$transform_coordinates(gcj02_beijing, "GCJ02", "WGS84")
  
  # Check that round-trip transformation is accurate within tolerance
  lon_diff <- abs(wgs84_beijing[1] - back_to_wgs84[1])
  lat_diff <- abs(wgs84_beijing[2] - back_to_wgs84[2])
  
  # Should be accurate within 1e-6 degrees (approximately 0.1 meters)
  expect_lt(lon_diff, 1e-6)
  expect_lt(lat_diff, 1e-6)
})

test_that("GaodeProvider handles edge cases", {
  skip_if_not_installed("R6")
  
  provider <- GaodeProvider$new()
  provider$initialize_provider(list(api_key = "test_key"))
  
  # Test with NULL data
  expect_null(provider$prepare_data(NULL))
  expect_null(provider$transform_coordinates(NULL))
  expect_equal(provider$detect_coordinate_system(NULL), "WGS84")
  
  # Test with empty data frame
  empty_df <- data.frame()
  expect_equal(provider$detect_coordinate_system(empty_df), "WGS84")
  
  # Test with coordinates outside China
  outside_china <- c(-74.006, 40.7128) # New York
  transformed <- provider$transform_coordinates(outside_china, "WGS84", "GCJ02")
  # Coordinates outside China should remain unchanged
  expect_equal(transformed, outside_china)
})

test_that("GaodeProvider integration with provider factory", {
  skip_if_not_installed("R6")
  
  # Test that Gaode provider is registered in factory
  factory <- get_provider_factory()
  available_providers <- factory$get_available_providers()
  expect_true("gaode" %in% available_providers)
  
  # Test creating Gaode provider through factory
  config <- list(api_key = "test_gaode_key")
  provider <- factory$create_provider("gaode", config)
  
  expect_true(inherits(provider, "GaodeProvider"))
  expect_true(provider$initialized)
  expect_equal(provider$api_key, "test_gaode_key")
  
  # Test provider capabilities
  capabilities <- factory$get_provider_capabilities("gaode")
  expect_true(is.list(capabilities))
  expect_equal(capabilities$name, "gaode")
  expect_equal(capabilities$coordinate_system, "GCJ02")
  expect_true(capabilities$authentication_required)
})