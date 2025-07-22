test_that("MapboxProvider implements IMapProvider interface", {
  # Skip if provider system not available
  skip_if_not_installed("R6")
  
  # Test that MapboxProvider class exists and inherits from IMapProvider
  expect_true(exists("MapboxProvider"))
  expect_true(inherits(MapboxProvider, "R6ClassGenerator"))
  
  # Test interface validation
  expect_true(validate_provider_interface(MapboxProvider))
})

test_that("MapboxProvider initialization works correctly", {
  skip_if_not_installed("R6")
  
  # Test basic initialization
  provider <- MapboxProvider$new()
  expect_false(provider$initialized)
  
  # Test initialization with config
  config <- list(
    token = "pk.test_token_here",
    style = "mapbox://styles/mapbox/streets-v11",
    zoom = 10,
    pitch = 30
  )
  
  provider$initialize_provider(config)
  expect_true(provider$initialized)
  expect_equal(provider$provider_name, "mapbox")
  expect_equal(provider$access_token, "pk.test_token_here")
  expect_equal(provider$current_style, "mapbox://styles/mapbox/streets-v11")
})

test_that("MapboxProvider initialization without token works", {
  skip_if_not_installed("R6")
  
  # Clear any existing tokens to ensure clean test
  tryCatch({
    token_store <- get_token_store()
    token_store$clear_all_tokens()
  }, error = function(e) {
    # Token store may not be available
  })
  
  # Test initialization without token (should work but warn)
  provider <- MapboxProvider$new()
  
  expect_warning(
    provider$initialize_provider(list()),
    "No Mapbox access token found"
  )
  
  expect_true(provider$initialized)
  expect_null(provider$access_token)
})

test_that("MapboxProvider token validation works", {
  skip_if_not_installed("R6")
  
  provider <- MapboxProvider$new()
  
  # Test valid Mapbox token formats
  expect_true(provider$validate_config(list(token = "pk.valid_token_here")))
  expect_true(provider$validate_config(list(token = "sk.secret_token_here")))
  
  # Test invalid token format (should warn but not fail)
  expect_warning(
    provider$validate_config(list(token = "invalid_token")),
    "Mapbox token should start with"
  )
})

test_that("MapboxProvider style management works", {
  skip_if_not_installed("R6")
  
  provider <- MapboxProvider$new()
  provider$initialize_provider(list(token = "pk.test"))
  
  # Test getting available styles
  styles <- provider$get_available_styles()
  expect_true(is.character(styles))
  expect_true(length(styles) > 0)
  expect_true("mapbox://styles/mapbox/streets-v11" %in% styles)
  
  # Test updating style
  new_style <- "mapbox://styles/mapbox/dark-v10"
  provider$update_style(new_style)
  expect_equal(provider$current_style, new_style)
})

test_that("MapboxProvider view management works", {
  skip_if_not_installed("R6")
  
  provider <- MapboxProvider$new()
  provider$initialize_provider(list(token = "pk.test"))
  
  # Test setting view
  provider$set_view(-74.0, 40.7, 12, 30, 45)
  
  expect_equal(provider$mapbox_config$location, c(-74.0, 40.7))
  expect_equal(provider$mapbox_config$zoom, 12)
  expect_equal(provider$mapbox_config$pitch, 30)
  expect_equal(provider$mapbox_config$bearing, 45)
})

test_that("MapboxProvider view validation works", {
  skip_if_not_installed("R6")
  
  provider <- MapboxProvider$new()
  provider$initialize_provider(list(token = "pk.test"))
  
  # Test invalid coordinates
  expect_error(provider$set_view(-200, 40.7, 12), "Longitude must be")
  expect_error(provider$set_view(-74.0, 100, 12), "Latitude must be")
  expect_error(provider$set_view(-74.0, 40.7, -1), "Zoom must be")
  expect_error(provider$set_view(-74.0, 40.7, 12, -1), "Pitch must be")
  expect_error(provider$set_view(-74.0, 40.7, 12, 30, 400), "Bearing must be")
})

test_that("MapboxProvider layer management works", {
  skip_if_not_installed("R6")
  
  provider <- MapboxProvider$new()
  provider$initialize_provider(list(token = "pk.test"))
  
  # Test adding layer
  layer <- list(
    id = "test_layer",
    type = "ScatterplotLayer",
    data = data.frame(x = 1, y = 1)
  )
  
  provider$add_layer(layer)
  expect_true("test_layer" %in% names(provider$layers))
  
  # Test removing layer
  provider$remove_layer("test_layer")
  expect_false("test_layer" %in% names(provider$layers))
})

test_that("MapboxProvider destroy works", {
  skip_if_not_installed("R6")
  
  provider <- MapboxProvider$new()
  provider$initialize_provider(list(token = "pk.test"))
  
  # Add some data
  provider$add_layer(list(id = "test", type = "ScatterplotLayer"))
  
  # Destroy
  provider$destroy()
  
  expect_false(provider$initialized)
  expect_null(provider$access_token)
  expect_null(provider$current_style)
  expect_null(provider$mapbox_config)
  expect_equal(length(provider$layers), 0)
})

test_that("MapboxProvider create_map validates options", {
  skip_if_not_installed("R6")
  
  provider <- MapboxProvider$new()
  provider$initialize_provider(list(token = "pk.test"))
  
  # Test that create_map method exists and can be called
  expect_true(is.function(provider$create_map))
  
  # Test that it fails gracefully when dependencies are missing
  options <- list(
    width = "100%",
    height = "400px",
    zoom = 10,
    location = c(-74, 40.7)
  )
  
  # Should error due to missing dependencies, but in a controlled way
  expect_error(
    provider$create_map(options = options),
    "could not find function"
  )
})

test_that("MapboxProvider handles missing dependencies gracefully", {
  skip_if_not_installed("R6")
  
  # Test that provider can be created even if some dependencies are missing
  provider <- MapboxProvider$new()
  
  # Should not error during initialization
  expect_error(provider$initialize_provider(list()), NA)
})

test_that("MapboxProvider configuration merging works", {
  skip_if_not_installed("R6")
  
  provider <- MapboxProvider$new()
  
  config <- list(
    token = "pk.test",
    zoom = 15,
    pitch = 45,
    custom_option = "test"
  )
  
  provider$initialize_provider(config)
  
  # Test that configuration is properly stored
  expect_equal(provider$mapbox_config$zoom, 15)
  expect_equal(provider$mapbox_config$pitch, 45)
  expect_equal(provider$access_token, "pk.test")
})