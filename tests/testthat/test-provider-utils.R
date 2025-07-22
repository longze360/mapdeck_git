test_that("validate_provider_name works correctly", {
  # Test valid provider names
  expect_equal(validate_provider_name("mapbox"), "mapbox")
  expect_equal(validate_provider_name("leaflet"), "leaflet")
  
  # Test NULL handling
  expect_null(validate_provider_name(NULL, allow_null = TRUE))
  expect_error(validate_provider_name(NULL), "Provider name cannot be NULL")
  
  # Test invalid inputs
  expect_error(validate_provider_name(123), "Provider name must be a single character string")
  expect_error(validate_provider_name(c("a", "b")), "Provider name must be a single character string")
  expect_error(validate_provider_name(""), "Provider name cannot be empty")
  
  # Test unavailable provider
  expect_error(
    validate_provider_name("nonexistent"),
    "Provider 'nonexistent' is not available"
  )
})

test_that("validate_map_options works correctly", {
  # Test valid options
  options <- list(longitude = -74, latitude = 40.7, zoom = 10)
  validated <- validate_map_options(options, "mapbox")
  
  expect_equal(validated$longitude, -74)
  expect_equal(validated$latitude, 40.7)
  expect_equal(validated$zoom, 10)
  expect_equal(validated$pitch, 0)  # Default value
  expect_equal(validated$bearing, 0)  # Default value
  
  # Test with all options
  full_options <- list(
    longitude = -74,
    latitude = 40.7,
    zoom = 10,
    pitch = 30,
    bearing = 45,
    width = "800px",
    height = "600px"
  )
  validated <- validate_map_options(full_options, "mapbox")
  expect_equal(validated$pitch, 30)
  expect_equal(validated$bearing, 45)
  expect_equal(validated$width, "800px")
  expect_equal(validated$height, "600px")
  
  # Test invalid inputs
  expect_error(validate_map_options("not_a_list", "mapbox"), "Options must be a list")
  
  # Test invalid numeric values
  expect_error(
    validate_map_options(list(longitude = "not_numeric"), "mapbox"),
    "Option 'longitude' must be a single numeric value"
  )
  
  expect_error(
    validate_map_options(list(longitude = c(1, 2)), "mapbox"),
    "Option 'longitude' must be a single numeric value"
  )
  
  # Test range validation
  expect_error(
    validate_map_options(list(longitude = 200), "mapbox"),
    "Longitude must be between -180 and 180"
  )
  
  expect_error(
    validate_map_options(list(latitude = 100), "mapbox"),
    "Latitude must be between -90 and 90"
  )
  
  expect_error(
    validate_map_options(list(zoom = -1), "mapbox"),
    "Zoom must be between 0 and 24"
  )
  
  expect_error(
    validate_map_options(list(pitch = 70), "mapbox"),
    "Pitch must be between 0 and 60 degrees"
  )
  
  expect_error(
    validate_map_options(list(bearing = 400), "mapbox"),
    "Bearing must be between 0 and 360 degrees"
  )
})

test_that("validate_layer_config works correctly", {
  # Test valid layer
  layer <- list(type = "ScatterplotLayer", data = data.frame(x = 1, y = 1))
  validated <- validate_layer_config(layer, "mapbox")
  
  expect_equal(validated$type, "ScatterplotLayer")
  expect_true(!is.null(validated$id))  # Should generate ID
  expect_true(is.character(validated$id))
  
  # Test layer with existing ID
  layer_with_id <- list(type = "ScatterplotLayer", id = "my_layer")
  validated <- validate_layer_config(layer_with_id, "mapbox")
  expect_equal(validated$id, "my_layer")
  
  # Test invalid inputs
  expect_error(validate_layer_config("not_a_list", "mapbox"), "Layer must be a list")
  expect_error(validate_layer_config(list(), "mapbox"), "Layer must have a 'type' field")
  expect_error(
    validate_layer_config(list(type = 123), "mapbox"),
    "Layer type must be a single character string"
  )
  expect_error(
    validate_layer_config(list(type = "test", id = 123), "mapbox"),
    "Layer ID must be a single character string"
  )
})

test_that("create_provider_error works correctly", {
  error <- create_provider_error("Test message", "mapbox", "TEST_CODE")
  
  expect_true(inherits(error, "MapdeckProviderError"))
  expect_true(inherits(error, "error"))
  expect_true(inherits(error, "condition"))
  expect_equal(error$message, "Test message")
  expect_equal(error$provider, "mapbox")
  expect_equal(error$code, "TEST_CODE")
  
  # Test without code
  error2 <- create_provider_error("Test message", "mapbox")
  expect_null(error2$code)
  
  # Test invalid inputs
  expect_error(
    create_provider_error(123, "mapbox"),
    "Error message must be a single character string"
  )
  
  expect_error(
    create_provider_error("message", 123),
    "Provider must be a single character string"
  )
})

test_that("handle_provider_error works correctly", {
  # Test with provider error
  provider_error <- create_provider_error("Test error", "mapbox", "TEST_CODE")
  
  expect_error(
    handle_provider_error(provider_error, "testing"),
    "Provider error during testing:\nProvider: mapbox\nError: Test error\nCode: TEST_CODE"
  )
  
  # Test with provider error without code
  provider_error_no_code <- create_provider_error("Test error", "mapbox")
  
  expect_error(
    handle_provider_error(provider_error_no_code, "testing"),
    "Provider error during testing:\nProvider: mapbox\nError: Test error"
  )
  
  # Test with regular error
  regular_error <- simpleError("Regular error")
  expect_error(handle_provider_error(regular_error), "Regular error")
})

test_that("normalize_style_name works correctly", {
  # Test NULL style (should return provider default)
  style <- normalize_style_name(NULL, "mapbox")
  expect_equal(style, "mapbox://styles/mapbox/streets-v11")
  
  # Test style aliases
  expect_equal(
    normalize_style_name("streets", "mapbox"),
    "mapbox://styles/mapbox/streets-v11"
  )
  expect_equal(
    normalize_style_name("streets", "leaflet"),
    "OpenStreetMap"
  )
  expect_equal(
    normalize_style_name("satellite", "mapbox"),
    "mapbox://styles/mapbox/satellite-v9"
  )
  expect_equal(
    normalize_style_name("dark", "gaode"),
    "amap://styles/dark"
  )
  
  # Test unknown alias (should return as-is)
  expect_equal(
    normalize_style_name("custom_style", "mapbox"),
    "custom_style"
  )
  
  # Test complex style (should return as-is)
  complex_style <- list(version = 8, sources = list())
  expect_identical(
    normalize_style_name(complex_style, "mapbox"),
    complex_style
  )
})

test_that("check_feature_support works correctly", {
  # Test supported features
  expect_true(check_feature_support("mapbox", "3d_terrain"))
  expect_false(check_feature_support("leaflet", "3d_terrain"))
  expect_true(check_feature_support("gaode", "coordinate_transform"))
  
  # Test unsupported features
  expect_false(check_feature_support("mapbox", "nonexistent_feature"))
  
  # Test invalid provider
  expect_false(check_feature_support("nonexistent", "3d_terrain"))
  
  # Test invalid inputs
  expect_error(
    check_feature_support(123, "feature"),
    "Provider name must be a single character string"
  )
  
  expect_error(
    check_feature_support("mapbox", 123),
    "Feature must be a single character string"
  )
})

test_that("generate_unique_id works correctly", {
  # Test default prefix
  id1 <- generate_unique_id()
  expect_true(is.character(id1))
  expect_true(grepl("^id_", id1))
  
  # Test custom prefix
  id2 <- generate_unique_id("layer")
  expect_true(grepl("^layer_", id2))
  
  # Test uniqueness
  id3 <- generate_unique_id("test")
  id4 <- generate_unique_id("test")
  expect_false(id3 == id4)
  
  # Test invalid input
  expect_error(
    generate_unique_id(123),
    "Prefix must be a single character string"
  )
})

test_that("validate_coordinate_bounds works correctly", {
  # Test valid bounds
  bounds <- c(-180, -90, 180, 90)
  validated <- validate_coordinate_bounds(bounds)
  expect_equal(validated, bounds)
  
  # Test typical bounds
  nyc_bounds <- c(-74.1, 40.6, -73.9, 40.8)
  validated <- validate_coordinate_bounds(nyc_bounds)
  expect_equal(validated, nyc_bounds)
  
  # Test invalid inputs
  expect_error(
    validate_coordinate_bounds("not_numeric"),
    "Bounds must be a numeric vector of length 4"
  )
  
  expect_error(
    validate_coordinate_bounds(c(1, 2, 3)),
    "Bounds must be a numeric vector of length 4"
  )
  
  # Test invalid longitude bounds
  expect_error(
    validate_coordinate_bounds(c(-200, -90, 180, 90)),
    "West longitude must be between -180 and 180"
  )
  
  expect_error(
    validate_coordinate_bounds(c(-180, -90, 200, 90)),
    "East longitude must be between -180 and 180"
  )
  
  # Test invalid latitude bounds
  expect_error(
    validate_coordinate_bounds(c(-180, -100, 180, 90)),
    "South latitude must be between -90 and 90"
  )
  
  expect_error(
    validate_coordinate_bounds(c(-180, -90, 180, 100)),
    "North latitude must be between -90 and 90"
  )
  
  # Test logical relationship validation
  expect_error(
    validate_coordinate_bounds(c(180, -90, -180, 90)),
    "West longitude must be less than east longitude"
  )
  
  expect_error(
    validate_coordinate_bounds(c(-180, 90, 180, -90)),
    "South latitude must be less than north latitude"
  )
})