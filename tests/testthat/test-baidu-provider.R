test_that("BaiduProvider implements IMapProvider interface", {
  skip_if_not_installed("R6")
  
  # Test that BaiduProvider class exists and inherits from IMapProvider
  expect_true(exists("BaiduProvider"))
  expect_true(inherits(BaiduProvider, "R6ClassGenerator"))
  
  # Create instance
  provider <- BaiduProvider$new()
  expect_true(inherits(provider, "BaiduProvider"))
  expect_true(inherits(provider, "IMapProvider"))
  
  # Test provider identification
  expect_equal(provider$provider_name, "baidu")
  expect_false(provider$initialized)
})

test_that("BaiduProvider initialization works correctly", {
  skip_if_not_installed("R6")
  
  provider <- BaiduProvider$new()
  
  # Test initialization with API key
  config <- list(
    api_key = "test_baidu_api_key_12345",
    style = "normal",
    zoom = 11,
    location = c(116.404, 39.915)
  )
  
  expect_silent(provider$initialize_provider(config))
  expect_true(provider$initialized)
  expect_equal(provider$api_key, "test_baidu_api_key_12345")
  expect_equal(provider$current_style, "normal")
})

test_that("BaiduProvider initialization fails without API key", {
  skip_if_not_installed("R6")
  
  provider <- BaiduProvider$new()
  
  # Test initialization without API key should fail
  config <- list(
    style = "normal",
    zoom = 11
  )
  
  expect_error(
    provider$initialize_provider(config),
    "Baidu Maps API key is required"
  )
})

test_that("BaiduProvider coordinate transformation works", {
  skip_if_not_installed("R6")
  
  provider <- BaiduProvider$new()
  provider$initialize_provider(list(api_key = "test_key"))
  
  # Test WGS84 to BD09 transformation
  wgs84_coords <- c(116.3974, 39.9093) # Beijing
  bd09_coords <- provider$transform_coordinates(wgs84_coords, "WGS84", "BD09")
  
  expect_type(bd09_coords, "double")
  expect_length(bd09_coords, 2)
  
  # BD09 coordinates should be different from WGS84
  expect_false(identical(wgs84_coords, bd09_coords))
  
  # Test coordinate system detection
  test_data <- data.frame(
    longitude = c(116.3974, 116.4074),
    latitude = c(39.9093, 39.9193)
  )
  
  detected_crs <- provider$detect_coordinate_system(test_data)
  expect_type(detected_crs, "character")
  expect_true(detected_crs %in% c("WGS84", "GCJ02", "BD09"))
})

test_that("BaiduProvider data preparation works", {
  skip_if_not_installed("R6")
  
  provider <- BaiduProvider$new()
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

test_that("BaiduProvider style management works", {
  skip_if_not_installed("R6")
  
  provider <- BaiduProvider$new()
  provider$initialize_provider(list(api_key = "test_key"))
  
  # Test getting available styles
  styles <- provider$get_available_styles()
  expect_type(styles, "character")
  expect_true(length(styles) > 0)
  expect_true("normal" %in% styles)
  
  # Test style categories
  basic_styles <- provider$get_available_styles("basic")
  expect_type(basic_styles, "character")
  expect_true("normal" %in% basic_styles)
  
  satellite_styles <- provider$get_available_styles("satellite")
  expect_type(satellite_styles, "character")
  expect_true("satellite" %in% satellite_styles)
  
  # Test unknown category
  expect_warning(
    unknown_styles <- provider$get_available_styles("unknown"),
    "Unknown style category"
  )
  expect_length(unknown_styles, 0)
})

test_that("BaiduProvider view management works", {
  skip_if_not_installed("R6")
  
  provider <- BaiduProvider$new()
  provider$initialize_provider(list(api_key = "test_key"))
  
  # Test setting view
  expect_silent(provider$set_view(116.404, 39.915, 11, 0, 0))
  
  # Test view validation
  expect_error(provider$set_view(-200, 39.915, 11), "Longitude must be")
  expect_error(provider$set_view(116.404, 100, 11), "Latitude must be")
  expect_error(provider$set_view(116.404, 39.915, -1), "Zoom must be")
  expect_error(provider$set_view(116.404, 39.915, 11, -1), "Pitch must be")
  expect_error(provider$set_view(116.404, 39.915, 11, 0, 400), "Bearing must be")
})

test_that("BaiduProvider layer management works", {
  skip_if_not_installed("R6")
  
  provider <- BaiduProvider$new()
  provider$initialize_provider(list(api_key = "test_key"))
  
  # Test adding layer
  layer_config <- list(
    id = "test_layer",
    type = "scatterplot",
    data = data.frame(
      longitude = c(116.404, 116.414),
      latitude = c(39.915, 39.925)
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

test_that("BaiduProvider configuration validation works", {
  skip_if_not_installed("R6")
  
  provider <- BaiduProvider$new()
  
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

test_that("BaiduProvider destruction works", {
  skip_if_not_installed("R6")
  
  provider <- BaiduProvider$new()
  provider$initialize_provider(list(api_key = "test_key"))
  
  # Add some data
  provider$layers[["test"]] <- list(id = "test")
  provider$map_instance <- "test_instance"
  
  # Test destruction
  expect_silent(provider$destroy())
  expect_length(provider$layers, 0)
  expect_null(provider$map_instance)
  expect_null(provider$baidu_config)
  expect_null(provider$api_key)
  expect_false(provider$initialized)
})

test_that("BaiduProvider coordinate transformation accuracy", {
  skip_if_not_installed("R6")
  
  provider <- BaiduProvider$new()
  provider$initialize_provider(list(api_key = "test_key"))
  
  # Test transformation accuracy with known coordinates
  # Beijing Tiananmen Square coordinates
  wgs84_beijing <- c(116.3974, 39.9093)
  
  # Transform WGS84 -> BD09 -> WGS84
  bd09_beijing <- provider$transform_coordinates(wgs84_beijing, "WGS84", "BD09")
  back_to_wgs84 <- provider$transform_coordinates(bd09_beijing, "BD09", "WGS84")
  
  # Check that round-trip transformation is accurate within tolerance
  lon_diff <- abs(wgs84_beijing[1] - back_to_wgs84[1])
  lat_diff <- abs(wgs84_beijing[2] - back_to_wgs84[2])
  
  # Should be accurate within 1e-6 degrees (approximately 0.1 meters)
  expect_lt(lon_diff, 1e-6)
  expect_lt(lat_diff, 1e-6)
})

test_that("BaiduProvider handles edge cases", {
  skip_if_not_installed("R6")
  
  provider <- BaiduProvider$new()
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
  transformed <- provider$transform_coordinates(outside_china, "WGS84", "BD09")
  # For coordinates outside China, BD09 transformation should still work
  # but the result should be close to the original
  expect_type(transformed, "double")
  expect_length(transformed, 2)
})

test_that("BaiduProvider integration with provider factory", {
  skip_if_not_installed("R6")
  
  # Test that Baidu provider is registered in factory
  factory <- get_provider_factory()
  available_providers <- factory$get_available_providers()
  expect_true("baidu" %in% available_providers)
  
  # Test creating Baidu provider through factory
  config <- list(api_key = "test_baidu_key")
  provider <- factory$create_provider("baidu", config)
  
  expect_true(inherits(provider, "BaiduProvider"))
  expect_true(provider$initialized)
  expect_equal(provider$api_key, "test_baidu_key")
  
  # Test provider capabilities
  capabilities <- factory$get_provider_capabilities("baidu")
  expect_true(is.list(capabilities))
  expect_equal(capabilities$name, "baidu")
  expect_equal(capabilities$coordinate_system, "BD09")
  expect_true(capabilities$authentication_required)
})

test_that("BaiduProvider BD09 coordinate system specifics", {
  skip_if_not_installed("R6")
  
  provider <- BaiduProvider$new()
  provider$initialize_provider(list(api_key = "test_key"))
  
  # Test BD09 specific transformations
  wgs84_coords <- c(116.3974, 39.9093)
  
  # WGS84 -> BD09 should be different from WGS84 -> GCJ02
  bd09_coords <- provider$transform_coordinates(wgs84_coords, "WGS84", "BD09")
  
  # Create a temporary Gaode provider for comparison
  gaode_provider <- GaodeProvider$new()
  gaode_provider$initialize_provider(list(api_key = "test_key"))
  gcj02_coords <- gaode_provider$transform_coordinates(wgs84_coords, "WGS84", "GCJ02")
  
  # BD09 and GCJ02 should be different
  expect_false(identical(bd09_coords, gcj02_coords))
  
  # BD09 coordinates should be more offset than GCJ02 from WGS84
  bd09_offset <- sqrt((bd09_coords[1] - wgs84_coords[1])^2 + (bd09_coords[2] - wgs84_coords[2])^2)
  gcj02_offset <- sqrt((gcj02_coords[1] - wgs84_coords[1])^2 + (gcj02_coords[2] - wgs84_coords[2])^2)
  
  expect_gt(bd09_offset, gcj02_offset)
})

test_that("BaiduProvider supports Baidu-specific features", {
  skip_if_not_installed("R6")
  
  provider <- BaiduProvider$new()
  
  # Test Baidu-specific configuration options
  config <- list(
    api_key = "test_key",
    enable_3d = TRUE,
    enable_scroll_wheel_zoom = FALSE,
    enable_drag = TRUE,
    enable_click = FALSE
  )
  
  provider$initialize_provider(config)
  
  expect_true(provider$baidu_config$enable_3d)
  expect_false(provider$baidu_config$enable_scroll_wheel_zoom)
  expect_true(provider$baidu_config$enable_drag)
  expect_false(provider$baidu_config$enable_click)
  
  # Test Baidu-specific styles
  styles <- provider$get_available_styles()
  baidu_specific_styles <- c("redalert", "googlelite", "grassgreen", "midnight", 
                            "pink", "darkgreen", "bluish", "hardedge")
  
  for (style in baidu_specific_styles) {
    expect_true(style %in% styles, info = paste("Style", style, "should be available"))
  }
})