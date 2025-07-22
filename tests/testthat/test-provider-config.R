test_that("ProviderConfig initializes correctly", {
  config <- ProviderConfig$new(
    name = "test_provider",
    type = "test_type",
    auth_required = TRUE,
    features = c("feature1", "feature2"),
    coord_sys = "EPSG:4326",
    style = "test_style"
  )
  
  expect_equal(config$name, "test_provider")
  expect_equal(config$type, "test_type")
  expect_true(config$authentication_required)
  expect_equal(config$supported_features, c("feature1", "feature2"))
  expect_equal(config$coordinate_system, "EPSG:4326")
  expect_equal(config$default_style, "test_style")
})

test_that("ProviderConfig validates input parameters", {
  # Test invalid name
  expect_error(
    ProviderConfig$new(name = NULL, type = "test"),
    "Provider name must be a single character string"
  )
  
  expect_error(
    ProviderConfig$new(name = c("a", "b"), type = "test"),
    "Provider name must be a single character string"
  )
  
  # Test invalid type
  expect_error(
    ProviderConfig$new(name = "test", type = NULL),
    "Provider type must be a single character string"
  )
  
  # Test invalid authentication_required
  expect_error(
    ProviderConfig$new(name = "test", type = "test", auth_required = "yes"),
    "authentication_required must be logical"
  )
  
  # Test invalid features
  expect_error(
    ProviderConfig$new(name = "test", type = "test", features = 123),
    "supported_features must be a character vector"
  )
})

test_that("ProviderConfig supports_feature method works", {
  config <- ProviderConfig$new(
    name = "test",
    type = "test",
    features = c("feature1", "feature2", "feature3")
  )
  
  expect_true(config$supports_feature("feature1"))
  expect_true(config$supports_feature("feature2"))
  expect_false(config$supports_feature("nonexistent"))
})

test_that("ProviderConfig to_list method works", {
  config <- ProviderConfig$new(
    name = "test",
    type = "test",
    auth_required = TRUE,
    features = c("feature1", "feature2")
  )
  
  config_list <- config$to_list()
  
  expect_true(is.list(config_list))
  expect_equal(config_list$name, "test")
  expect_equal(config_list$type, "test")
  expect_true(config_list$authentication_required)
  expect_equal(config_list$supported_features, c("feature1", "feature2"))
})

test_that("ProviderConfig update method works", {
  config <- ProviderConfig$new(name = "test", type = "test")
  
  config$update(list(
    default_style = "new_style",
    supported_features = c("new_feature")
  ))
  
  expect_equal(config$default_style, "new_style")
  expect_equal(config$supported_features, c("new_feature"))
})

test_that("create_default_provider_configs creates all expected providers", {
  configs <- create_default_provider_configs()
  
  expected_providers <- c("mapbox", "leaflet", "openlayers", "gaode", "baidu")
  expect_true(all(expected_providers %in% names(configs)))
  
  # Test each config is a ProviderConfig instance
  for (provider in expected_providers) {
    expect_true(inherits(configs[[provider]], "ProviderConfig"))
  }
})

test_that("mapbox config has correct settings", {
  configs <- create_default_provider_configs()
  mapbox_config <- configs$mapbox
  
  expect_equal(mapbox_config$name, "mapbox")
  expect_equal(mapbox_config$type, "mapbox-gl")
  expect_true(mapbox_config$authentication_required)
  expect_equal(mapbox_config$coordinate_system, "EPSG:4326")
  expect_true("vector_tiles" %in% mapbox_config$supported_features)
})

test_that("chinese provider configs have correct coordinate systems", {
  configs <- create_default_provider_configs()
  
  expect_equal(configs$gaode$coordinate_system, "GCJ02")
  expect_equal(configs$baidu$coordinate_system, "BD09")
})

test_that("get_provider_config works correctly", {
  # Test existing provider
  mapbox_config <- get_provider_config("mapbox")
  expect_true(inherits(mapbox_config, "ProviderConfig"))
  expect_equal(mapbox_config$name, "mapbox")
  
  # Test non-existing provider
  expect_null(get_provider_config("nonexistent"))
})

test_that("list_available_providers returns correct providers", {
  providers <- list_available_providers()
  expected_providers <- c("mapbox", "leaflet", "openlayers", "gaode", "baidu")
  
  expect_true(is.character(providers))
  expect_true(all(expected_providers %in% providers))
})

test_that("validate_provider_config works correctly", {
  # Test valid config
  valid_config <- ProviderConfig$new(name = "test", type = "test")
  expect_true(validate_provider_config(valid_config))
  
  # Test invalid config (not ProviderConfig instance)
  expect_false(validate_provider_config(list()))
  expect_false(validate_provider_config("not_config"))
  
  # Test config with missing fields
  incomplete_config <- list(name = "test")
  class(incomplete_config) <- "ProviderConfig"
  expect_false(validate_provider_config(incomplete_config))
})