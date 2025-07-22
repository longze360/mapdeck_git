test_that("ProviderConfig class works correctly", {
  # Test basic configuration creation
  config <- create_provider_config(
    name = "test",
    type = "test_type",
    defaults = list(option1 = "default1", option2 = 10),
    required_options = c("required1"),
    validation_rules = list(
      option2 = function(x) is.numeric(x) && x > 0
    )
  )
  
  expect_true(R6::is.R6(config))
  expect_equal(config$get_name(), "test")
  expect_equal(config$get_type(), "test_type")
  expect_equal(config$get_option("option1"), "default1")
  expect_equal(config$get_option("option2"), 10)
})

test_that("ProviderConfig validation works correctly", {
  config <- create_provider_config(
    name = "test",
    type = "test_type",
    defaults = list(option1 = "default1"),
    required_options = c("required1"),
    validation_rules = list(
      option1 = function(x) is.character(x) && nchar(x) > 0
    )
  )
  
  # Test validation with missing required option
  result <- config$validate()
  expect_false(result$valid)
  expect_true(grepl("Missing required options: required1", result$errors))
  
  # Test validation with valid required option
  config$set_option("required1", "value1")
  result <- config$validate()
  expect_true(result$valid)
  expect_equal(length(result$errors), 0)
})

test_that("ProviderConfig option setting and validation works", {
  config <- create_provider_config(
    name = "test",
    type = "test_type",
    validation_rules = list(
      numeric_option = function(x) is.numeric(x) && x > 0
    )
  )
  
  # Test setting valid option
  config$set_option("numeric_option", 5)
  expect_equal(config$get_option("numeric_option"), 5)
  
  # Test setting invalid option
  expect_error(config$set_option("numeric_option", -1), "Invalid value for option")
})

test_that("ProviderConfig merging works correctly", {
  config <- create_provider_config(
    name = "test",
    type = "test_type",
    defaults = list(option1 = "default1", option2 = 10)
  )
  
  # Test merging with list
  config$merge_config(list(option1 = "new_value", option3 = "added"))
  expect_equal(config$get_option("option1"), "new_value")
  expect_equal(config$get_option("option2"), 10)
  expect_equal(config$get_option("option3"), "added")
  
  # Test merging with another ProviderConfig
  other_config <- create_provider_config(
    name = "other",
    type = "other_type",
    defaults = list(option4 = "other_value")
  )
  config$merge_config(other_config)
  expect_equal(config$get_option("option4"), "other_value")
})

test_that("Standard provider configurations are available", {
  # Test that standard configs exist
  expect_true("mapbox" %in% names(STANDARD_PROVIDER_CONFIGS))
  expect_true("leaflet" %in% names(STANDARD_PROVIDER_CONFIGS))
  expect_true("openlayers" %in% names(STANDARD_PROVIDER_CONFIGS))
  expect_true("gaode" %in% names(STANDARD_PROVIDER_CONFIGS))
  expect_true("baidu" %in% names(STANDARD_PROVIDER_CONFIGS))
  
  # Test getting standard config
  mapbox_config <- get_standard_provider_config("mapbox")
  expect_true(R6::is.R6(mapbox_config))
  expect_equal(mapbox_config$get_name(), "mapbox")
  expect_equal(mapbox_config$get_type(), PROVIDER_TYPES$MAPBOX)
  
  # Test error for unknown provider type
  expect_error(get_standard_provider_config("unknown"), "Unknown provider type")
})

test_that("ProviderConfig to_list works correctly", {
  config <- create_provider_config(
    name = "test",
    type = "test_type",
    defaults = list(option1 = "value1", option2 = 10)
  )
  
  config_list <- config$to_list()
  expect_true(is.list(config_list))
  expect_equal(config_list$option1, "value1")
  expect_equal(config_list$option2, 10)
})