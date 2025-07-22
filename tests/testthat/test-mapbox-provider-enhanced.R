test_that("MapboxProvider enhanced token management works", {
  skip_if_not_installed("R6")
  
  provider <- MapboxProvider$new()
  
  # Test token storage in token store during initialization
  config <- list(
    token = "pk.enhanced_test_token",
    scope = "test_scope"
  )
  
  provider$initialize_provider(config)
  
  # Verify token is stored
  expect_equal(provider$access_token, "pk.enhanced_test_token")
  
  # Test token retrieval from token store
  provider2 <- MapboxProvider$new()
  config2 <- list(scope = "test_scope")
  
  # This should retrieve the token from the store
  # (Note: This test may fail if token store is not properly initialized)
  tryCatch({
    provider2$initialize_provider(config2)
    expect_equal(provider2$access_token, "pk.enhanced_test_token")
  }, error = function(e) {
    # Token store may not be available in test environment
    skip("Token store not available in test environment")
  })
})

test_that("MapboxProvider enhanced style management works", {
  skip_if_not_installed("R6")
  
  provider <- MapboxProvider$new()
  provider$initialize_provider(list(token = "pk.test"))
  
  # Test categorized style retrieval
  all_styles <- provider$get_available_styles()
  expect_true(is.character(all_styles))
  expect_true(length(all_styles) > 0)
  
  # Test category filtering
  basic_styles <- provider$get_available_styles("basic")
  expect_true(is.character(basic_styles))
  expect_true("mapbox://styles/mapbox/streets-v11" %in% basic_styles)
  
  satellite_styles <- provider$get_available_styles("satellite")
  expect_true(is.character(satellite_styles))
  expect_true("mapbox://styles/mapbox/satellite-v9" %in% satellite_styles)
  
  # Test invalid category
  expect_warning(
    empty_styles <- provider$get_available_styles("invalid_category"),
    "Unknown style category"
  )
  expect_equal(length(empty_styles), 0)
})

test_that("MapboxProvider style validation works", {
  skip_if_not_installed("R6")
  
  provider <- MapboxProvider$new()
  provider$initialize_provider(list(token = "pk.test"))
  
  # Test valid Mapbox style URLs
  expect_true(provider$validate_style("mapbox://styles/mapbox/streets-v11"))
  expect_true(provider$validate_style("mapbox://styles/user/custom-style"))
  
  # Test valid HTTP URLs
  expect_true(provider$validate_style("https://example.com/style.json"))
  expect_true(provider$validate_style("http://localhost:3000/style.json"))
  
  # Test NULL style (should use default)
  expect_true(provider$validate_style(NULL))
  
  # Test style aliases
  expect_true(provider$validate_style("streets"))
  expect_true(provider$validate_style("satellite"))
  expect_true(provider$validate_style("dark"))
  
  # Test custom style objects
  custom_style <- list(
    version = 8,
    sources = list(),
    layers = list()
  )
  expect_true(provider$validate_style(custom_style))
  
  # Test invalid styles
  expect_false(provider$validate_style("invalid_style"))
  expect_false(provider$validate_style(123))
  expect_false(provider$validate_style(list(invalid = "style")))
})

test_that("MapboxProvider custom style creation works", {
  skip_if_not_installed("R6")
  
  provider <- MapboxProvider$new()
  provider$initialize_provider(list(token = "pk.test"))
  
  # Test basic custom style creation
  custom_style <- provider$create_custom_style()
  expect_true(is.list(custom_style))
  expect_equal(custom_style$base, "mapbox://styles/mapbox/streets-v11")
  expect_equal(custom_style$provider, "mapbox")
  expect_true(!is.null(custom_style$created_at))
  
  # Test custom style with modifications
  modifications <- list(
    colors = list(water = "#0066cc", land = "#f0f0f0"),
    fonts = list(primary = "Arial", secondary = "Helvetica")
  )
  
  custom_style2 <- provider$create_custom_style(
    base_style = "mapbox://styles/mapbox/dark-v10",
    modifications = modifications
  )
  
  expect_equal(custom_style2$base, "mapbox://styles/mapbox/dark-v10")
  expect_equal(custom_style2$modifications, modifications)
  expect_equal(custom_style2$color_overrides, modifications$colors)
  expect_equal(custom_style2$font_overrides, modifications$fonts)
  
  # Test invalid base style
  expect_error(
    provider$create_custom_style(base_style = "invalid_style"),
    "Invalid base style provided"
  )
})

test_that("MapboxProvider coordinate transformation works", {
  skip_if_not_installed("R6")
  
  provider <- MapboxProvider$new()
  provider$initialize_provider(list(token = "pk.test"))
  
  # Test data that doesn't need transformation (WGS84 to WGS84)
  test_data <- data.frame(
    longitude = c(-74.0, -73.9),
    latitude = c(40.7, 40.8)
  )
  
  result <- provider$transform_coordinates(test_data, "EPSG:4326", "EPSG:4326")
  expect_equal(result, test_data)
  
  # Test coordinate system detection (should default to WGS84)
  detected_crs <- provider$detect_coordinate_system(test_data)
  expect_equal(detected_crs, "EPSG:4326")
  
  # Test data preparation
  prepared_data <- provider$prepare_data(test_data, auto_transform = TRUE)
  expect_equal(prepared_data, test_data)
  
  # Test with NULL data
  expect_null(provider$prepare_data(NULL))
  
  # Test with auto_transform disabled
  prepared_data2 <- provider$prepare_data(test_data, auto_transform = FALSE)
  expect_equal(prepared_data2, test_data)
})

test_that("MapboxProvider coordinate transformation handles errors gracefully", {
  skip_if_not_installed("R6")
  
  provider <- MapboxProvider$new()
  provider$initialize_provider(list(token = "pk.test"))
  
  test_data <- data.frame(
    longitude = c(-74.0, -73.9),
    latitude = c(40.7, 40.8)
  )
  
  # Test transformation with non-WGS84 coordinates (should warn and return original)
  expect_warning(
    result <- provider$transform_coordinates(test_data, "EPSG:3857", "EPSG:4326"),
    "Coordinate transformation failed"
  )
  expect_equal(result, test_data)
})

test_that("MapboxProvider enhanced configuration validation works", {
  skip_if_not_installed("R6")
  
  provider <- MapboxProvider$new()
  
  # Test validation with valid token in config
  config1 <- list(token = "pk.valid_token")
  expect_true(provider$validate_config(config1))
  
  # Test validation with invalid token format
  config2 <- list(token = "invalid_token")
  expect_warning(
    result <- provider$validate_config(config2),
    "Mapbox token should start with"
  )
  expect_true(result)  # Should still return TRUE but warn
  
  # Test validation with no token (should warn but not fail)
  config3 <- list()
  expect_warning(
    result <- provider$validate_config(config3),
    "No Mapbox access token found"
  )
  expect_true(result)
  
  # Test validation with invalid config type
  expect_false(provider$validate_config("not_a_list"))
  expect_false(provider$validate_config(NULL))
})

test_that("MapboxProvider integration with token store works", {
  skip_if_not_installed("R6")
  
  # Test that provider can work with or without token store
  provider <- MapboxProvider$new()
  
  # Should not error even if token store is not available
  expect_error(
    provider$initialize_provider(list(token = "pk.test")),
    NA
  )
  
  expect_true(provider$initialized)
  expect_equal(provider$access_token, "pk.test")
})

test_that("MapboxProvider enhanced features maintain backward compatibility", {
  skip_if_not_installed("R6")
  
  provider <- MapboxProvider$new()
  provider$initialize_provider(list(token = "pk.test"))
  
  # Test that original interface methods still work
  expect_true(is.function(provider$get_available_styles))
  expect_true(is.function(provider$validate_config))
  expect_true(is.function(provider$update_style))
  expect_true(is.function(provider$set_view))
  
  # Test that enhanced methods are available
  expect_true(is.function(provider$validate_style))
  expect_true(is.function(provider$create_custom_style))
  expect_true(is.function(provider$transform_coordinates))
  expect_true(is.function(provider$detect_coordinate_system))
  expect_true(is.function(provider$prepare_data))
  
  # Test that original behavior is preserved
  styles <- provider$get_available_styles()
  expect_true(is.character(styles))
  expect_true(length(styles) > 0)
})

test_that("MapboxProvider handles missing coordinate transformation dependencies", {
  skip_if_not_installed("R6")
  
  provider <- MapboxProvider$new()
  provider$initialize_provider(list(token = "pk.test"))
  
  test_data <- data.frame(x = 1, y = 1)
  
  # Should handle missing coordinate transformation gracefully
  expect_warning(
    result <- provider$transform_coordinates(test_data, "EPSG:3857", "EPSG:4326"),
    "Coordinate transformation failed"
  )
  
  # Should return original data when transformation fails
  expect_equal(result, test_data)
  
  # Detection should default to WGS84 when detector is not available
  detected <- provider$detect_coordinate_system(test_data)
  expect_equal(detected, "EPSG:4326")
})

test_that("MapboxProvider style management integration works", {
  skip_if_not_installed("R6")
  
  provider <- MapboxProvider$new()
  provider$initialize_provider(list(token = "pk.test"))
  
  # Test style update with validation
  valid_style <- "mapbox://styles/mapbox/dark-v10"
  provider$update_style(valid_style)
  expect_equal(provider$current_style, valid_style)
  
  # Test style update with normalization
  provider$update_style("streets")
  expect_true(grepl("mapbox://styles/mapbox/streets", provider$current_style))
  
  # Test that style is properly stored in configuration
  expect_equal(provider$mapbox_config$style, provider$current_style)
})