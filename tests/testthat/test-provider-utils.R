test_that("validate_provider_name works correctly", {
  # Clear registry for clean test
  registry <- get_provider_registry()
  registry$clear_registry()
  
  # Test with NULL
  expect_false(validate_provider_name(NULL))
  expect_true(validate_provider_name(NULL, allow_null = TRUE))
  
  # Test with invalid types
  expect_false(validate_provider_name(123))
  expect_false(validate_provider_name(c("a", "b")))
  
  # Test with unregistered provider
  expect_false(validate_provider_name("unregistered"))
  
  # Register a mock provider and test
  MockProvider <- R6::R6Class(
    "MockProvider",
    inherit = IMapProvider,
    public = list(
      initialize = function(config = list()) invisible(self),
      validate_config = function(config) list(valid = TRUE, errors = character(0))
    )
  )
  registry$register_provider("test_provider", MockProvider)
  
  expect_true(validate_provider_name("test_provider"))
})

test_that("normalize_location works correctly", {
  # Test with NULL
  expect_equal(normalize_location(NULL), c(0, 0))
  
  # Test with numeric vector
  expect_equal(normalize_location(c(1, 2)), c(1, 2))
  expect_equal(normalize_location(c(1, 2, 3)), c(1, 2))
  
  # Test with named list
  expect_equal(normalize_location(list(lon = 1, lat = 2)), c(1, 2))
  expect_equal(normalize_location(list(lng = 1, lat = 2)), c(1, 2))
  
  # Test with unnamed list
  expect_equal(normalize_location(list(1, 2)), c(1, 2))
  
  # Test with data.frame
  df <- data.frame(longitude = 1, latitude = 2)
  expect_equal(normalize_location(df), c(1, 2))
  
  df2 <- data.frame(lon = 1, lat = 2)
  expect_equal(normalize_location(df2), c(1, 2))
  
  # Test error cases
  expect_error(normalize_location("invalid"), "Location must be a numeric vector")
  expect_error(normalize_location(list()), "Invalid location format")
})

test_that("validate_zoom works correctly", {
  # Test with NULL
  expect_equal(validate_zoom(NULL), 0)
  
  # Test with valid zoom
  expect_equal(validate_zoom(10), 10)
  
  # Test with zoom below minimum
  expect_warning(result <- validate_zoom(-1))
  expect_equal(result, 0)
  
  # Test with zoom above maximum
  expect_warning(result <- validate_zoom(25))
  expect_equal(result, 20)
  
  # Test with custom limits
  expect_equal(validate_zoom(5, min_zoom = 2, max_zoom = 15), 5)
  
  # Test error cases
  expect_error(validate_zoom("invalid"), "Zoom must be a single numeric value")
  expect_error(validate_zoom(c(1, 2)), "Zoom must be a single numeric value")
})

test_that("validate_bearing works correctly", {
  # Test with NULL
  expect_equal(validate_bearing(NULL), 0)
  
  # Test with valid bearing
  expect_equal(validate_bearing(90), 90)
  
  # Test with bearing normalization
  expect_equal(validate_bearing(450), 90)
  expect_equal(validate_bearing(-90), 270)
  
  # Test error cases
  expect_error(validate_bearing("invalid"), "Bearing must be a single numeric value")
  expect_error(validate_bearing(c(1, 2)), "Bearing must be a single numeric value")
})

test_that("validate_pitch works correctly", {
  # Test with NULL
  expect_equal(validate_pitch(NULL), 0)
  
  # Test with valid pitch
  expect_equal(validate_pitch(30), 30)
  
  # Test with pitch below minimum
  expect_warning(result <- validate_pitch(-10))
  expect_equal(result, 0)
  
  # Test with pitch above maximum
  expect_warning(result <- validate_pitch(70))
  expect_equal(result, 60)
  
  # Test with custom limits
  expect_equal(validate_pitch(45, min_pitch = 10, max_pitch = 50), 45)
  
  # Test error cases
  expect_error(validate_pitch("invalid"), "Pitch must be a single numeric value")
  expect_error(validate_pitch(c(1, 2)), "Pitch must be a single numeric value")
})

test_that("generate_layer_id works correctly", {
  # Test basic functionality
  id1 <- generate_layer_id("test")
  id2 <- generate_layer_id("test")
  
  expect_true(is.character(id1))
  expect_true(nchar(id1) > 0)
  expect_true(grepl("layer_test_", id1))
  expect_false(id1 == id2)  # Should be unique
  
  # Test with custom prefix
  id3 <- generate_layer_id("test", "custom")
  expect_true(grepl("custom_test_", id3))
})

test_that("merge_layer_options works correctly", {
  defaults <- list(
    option1 = "default1",
    option2 = 10,
    nested = list(sub1 = "default_sub1", sub2 = 20)
  )
  
  user_options <- list(
    option1 = "user1",
    option3 = "new_option",
    nested = list(sub1 = "user_sub1", sub3 = "new_sub")
  )
  
  result <- merge_layer_options(defaults, user_options)
  
  expect_equal(result$option1, "user1")
  expect_equal(result$option2, 10)
  expect_equal(result$option3, "new_option")
  expect_equal(result$nested$sub1, "user_sub1")
  expect_equal(result$nested$sub2, 20)
  expect_equal(result$nested$sub3, "new_sub")
  
  # Test with NULL user options
  result2 <- merge_layer_options(defaults, NULL)
  expect_equal(result2, defaults)
  
  # Test error cases
  expect_error(merge_layer_options("invalid", list()), "must be lists")
  expect_error(merge_layer_options(list(), "invalid"), "must be lists")
})

test_that("validate_layer_data works correctly", {
  # Test with NULL data
  expect_true(validate_layer_data(NULL))
  expect_false(validate_layer_data(NULL, required_columns = c("col1")))
  
  # Test with data.frame
  df <- data.frame(col1 = 1:3, col2 = letters[1:3])
  expect_true(validate_layer_data(df))
  expect_true(validate_layer_data(df, required_columns = c("col1")))
  expect_warning(result <- validate_layer_data(df, required_columns = c("missing")))
  expect_false(result)
  
  # Test with list
  lst <- list(col1 = 1:3, col2 = letters[1:3])
  expect_true(validate_layer_data(lst))
  expect_true(validate_layer_data(lst, required_columns = c("col1")))
  
  # Test with invalid data
  expect_false(validate_layer_data("invalid"))
})

test_that("convert_style_to_provider works correctly", {
  # Test with NULL
  expect_null(convert_style_to_provider(NULL, "mapbox"))
  
  # Test Mapbox conversions
  expect_equal(convert_style_to_provider("streets", "mapbox"), 
               "mapbox://styles/mapbox/streets-v11")
  expect_equal(convert_style_to_provider("mapbox://custom", "mapbox"), 
               "mapbox://custom")
  
  # Test Leaflet conversions
  expect_equal(convert_style_to_provider("streets", "leaflet"), 
               "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png")
  
  # Test Gaode conversions
  expect_equal(convert_style_to_provider("streets", "gaode"), "normal")
  
  # Test Baidu conversions
  expect_equal(convert_style_to_provider("streets", "baidu"), "normal")
  
  # Test unknown provider
  expect_equal(convert_style_to_provider("streets", "unknown"), "streets")
})

test_that("check_provider_feature_support works correctly", {
  # Clear registry for clean test
  registry <- get_provider_registry()
  registry$clear_registry()
  
  # Test with unregistered provider
  expect_false(check_provider_feature_support("unregistered", "feature1"))
  
  # Register a mock provider with capabilities
  MockProvider <- R6::R6Class(
    "MockProvider",
    inherit = IMapProvider,
    public = list(
      initialize = function(config = list()) invisible(self),
      validate_config = function(config) list(valid = TRUE, errors = character(0))
    )
  )
  registry$register_provider("test_provider", MockProvider, c("feature1", "feature2"))
  
  expect_true(check_provider_feature_support("test_provider", "feature1"))
  expect_true(check_provider_feature_support("test_provider", "feature2"))
  expect_false(check_provider_feature_support("test_provider", "feature3"))
})