test_that("validate_provider_name works correctly", {
  # Mock get_available_providers to return test providers
  mockery::stub(validate_provider_name, "get_available_providers", 
                c("mapbox", "leaflet", "test"))
  
  # Test valid provider name
  expect_equal(validate_provider_name("mapbox"), "mapbox")
  
  # Test NULL with allow_null = TRUE
  expect_null(validate_provider_name(NULL, allow_null = TRUE))
  
  # Test NULL with allow_null = FALSE
  expect_error(validate_provider_name(NULL), "Provider name cannot be NULL")
  
  # Test invalid types
  expect_error(validate_provider_name(123), "must be a single character string")
  expect_error(validate_provider_name(c("a", "b")), "must be a single character string")
  
  # Test empty string
  expect_error(validate_provider_name(""), "Provider name cannot be empty")
  
  # Test unavailable provider
  expect_error(validate_provider_name("nonexistent"), "Provider .* is not available")
})

test_that("validate_map_options validates coordinates correctly", {
  # Test valid options
  valid_options <- list(
    longitude = -74.0,
    latitude = 40.7,
    zoom = 10,
    pitch = 30,
    bearing = 45
  )
  expect_equal(validate_map_options(valid_options), valid_options)
  
  # Test invalid longitude
  expect_error(
    validate_map_options(list(longitude = 200)),
    "longitude must be between -180 and 180"
  )
  
  expect_error(
    validate_map_options(list(longitude = "invalid")),
    "longitude must be a single numeric value"
  )
  
  # Test invalid latitude
  expect_error(
    validate_map_options(list(latitude = 100)),
    "latitude must be between -90 and 90"
  )
  
  # Test invalid zoom
  expect_error(
    validate_map_options(list(zoom = -1)),
    "zoom must be between 0 and 24"
  )
  
  expect_error(
    validate_map_options(list(zoom = 30)),
    "zoom must be between 0 and 24"
  )
  
  # Test invalid pitch
  expect_error(
    validate_map_options(list(pitch = 70)),
    "pitch must be between 0 and 60"
  )
  
  # Test invalid bearing
  expect_error(
    validate_map_options(list(bearing = 200)),
    "bearing must be between -180 and 180"
  )
  
  # Test non-list input
  expect_error(validate_map_options("not_list"), "Map options must be a list")
})

test_that("validate_layer_config works correctly", {
  # Test valid layer config
  valid_layer <- list(
    type = "ScatterplotLayer",
    id = "test-layer",
    data = data.frame(x = 1, y = 2)
  )
  expect_equal(validate_layer_config(valid_layer), valid_layer)
  
  # Test missing required fields
  expect_error(
    validate_layer_config(list(type = "ScatterplotLayer")),
    "Layer configuration missing required fields: id"
  )
  
  expect_error(
    validate_layer_config(list(id = "test")),
    "Layer configuration missing required fields: type"
  )
  
  # Test invalid layer type
  expect_error(
    validate_layer_config(list(type = 123, id = "test")),
    "Layer type must be a single character string"
  )
  
  # Test invalid layer ID
  expect_error(
    validate_layer_config(list(type = "test", id = 123)),
    "Layer id must be a single character string"
  )
  
  # Test non-list input
  expect_error(validate_layer_config("not_list"), "Layer configuration must be a list")
})

test_that("create_error_handler creates proper error handler", {
  handler <- create_error_handler("test_provider", "test_operation")
  
  # Test error handling
  test_error <- simpleError("test error message")
  handled_error <- handler(test_error)
  
  expect_true(inherits(handled_error, "MapdeckProviderError"))
  expect_equal(handled_error$provider, "test_provider")
  expect_equal(handled_error$operation, "test_operation")
  expect_true(grepl("test error message", handled_error$message))
})

test_that("normalize_style works correctly", {
  # Test NULL style
  expect_null(normalize_style(NULL))
  
  # Test character style
  expect_equal(normalize_style("mapbox://styles/mapbox/streets-v11"), 
               "mapbox://styles/mapbox/streets-v11")
  
  # Test invalid character style (multiple values)
  expect_error(normalize_style(c("style1", "style2")), 
               "Style name must be a single character string")
  
  # Test list style
  style_list <- list(version = 8, sources = list(), layers = list())
  expect_equal(normalize_style(style_list), style_list)
  
  # Test list style without version (should warn)
  expect_warning(
    normalize_style(list(sources = list())),
    "Style specification missing 'version' field"
  )
  
  # Test invalid style type
  expect_error(normalize_style(123), "Style must be a character string or list")
})

test_that("check_provider_feature works correctly", {
  # Mock get_provider_capabilities
  mockery::stub(check_provider_feature, "get_provider_capabilities", 
                c("feature1", "feature2", "feature3"))
  
  expect_true(check_provider_feature("test_provider", "feature1"))
  expect_false(check_provider_feature("test_provider", "nonexistent"))
  
  # Test with NULL capabilities
  mockery::stub(check_provider_feature, "get_provider_capabilities", NULL)
  expect_warning(result <- check_provider_feature("test_provider", "feature1"))
  expect_false(result)
})

test_that("generate_layer_id creates unique IDs", {
  id1 <- generate_layer_id()
  id2 <- generate_layer_id()
  
  expect_true(is.character(id1))
  expect_true(is.character(id2))
  expect_true(nchar(id1) > 0)
  expect_true(nchar(id2) > 0)
  expect_false(id1 == id2)  # Should be unique
  
  # Test with custom prefix
  custom_id <- generate_layer_id("custom")
  expect_true(grepl("^custom_", custom_id))
})

test_that("sanitize_container_id works correctly", {
  # Test valid ID
  expect_equal(sanitize_container_id("valid_id"), "valid_id")
  
  # Test ID with invalid characters
  expect_equal(sanitize_container_id("invalid-id!@#"), "invalid-id___")
  
  # Test ID starting with number
  expect_equal(sanitize_container_id("123invalid"), "map_123invalid")
  
  # Test empty string
  expect_error(sanitize_container_id(""), "Container ID cannot be empty")
  
  # Test invalid types
  expect_error(sanitize_container_id(123), "must be a single character string")
  expect_error(sanitize_container_id(c("a", "b")), "must be a single character string")
})

test_that("standardize_coordinates works correctly", {
  # Test with data frame
  df <- data.frame(
    longitude = c(-74.0, -73.9),
    latitude = c(40.7, 40.8),
    value = c(1, 2)
  )
  
  result <- standardize_coordinates(df)
  expect_equal(result, df)
  
  # Test with custom column names
  df_custom <- data.frame(
    lon = c(-74.0, -73.9),
    lat = c(40.7, 40.8),
    value = c(1, 2)
  )
  
  result_custom <- standardize_coordinates(df_custom, "lon", "lat")
  expect_equal(result_custom, df_custom)
  
  # Test with matrix input
  mat <- matrix(c(-74.0, 40.7, -73.9, 40.8), ncol = 2)
  colnames(mat) <- c("longitude", "latitude")
  
  result_mat <- standardize_coordinates(mat)
  expect_true(is.data.frame(result_mat))
  
  # Test missing columns
  expect_error(
    standardize_coordinates(df, "missing_col", "latitude"),
    "Longitude column 'missing_col' not found"
  )
  
  # Test invalid coordinate values
  df_invalid <- data.frame(
    longitude = c(-200, 200),  # Invalid longitude
    latitude = c(40.7, 40.8)
  )
  
  expect_warning(
    standardize_coordinates(df_invalid),
    "longitude values are outside valid range"
  )
  
  # Test NA values
  df_na <- data.frame(
    longitude = c(-74.0, NA),
    latitude = c(40.7, 40.8)
  )
  
  expect_warning(
    standardize_coordinates(df_na),
    "Coordinate data contains NA values"
  )
  
  # Test non-numeric coordinates
  df_char <- data.frame(
    longitude = c("a", "b"),
    latitude = c(40.7, 40.8)
  )
  
  expect_error(
    standardize_coordinates(df_char),
    "Coordinate columns must contain numeric values"
  )
})