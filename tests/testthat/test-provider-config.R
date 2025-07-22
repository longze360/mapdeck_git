test_that("ProviderConfig initialization works correctly", {
  config <- ProviderConfig$new(
    name = "test",
    type = "webgl",
    authentication_required = TRUE,
    supported_features = list(feature1 = TRUE, feature2 = FALSE),
    coordinate_system = "EPSG:4326",
    default_style = "test_style"
  )
  
  expect_equal(config$name, "test")
  expect_equal(config$type, "webgl")
  expect_true(config$authentication_required)
  expect_equal(config$supported_features, list(feature1 = TRUE, feature2 = FALSE))
  expect_equal(config$coordinate_system, "EPSG:4326")
  expect_equal(config$default_style, "test_style")
})

test_that("ProviderConfig validation works correctly", {
  # Test missing required fields
  expect_error(
    ProviderConfig$new(),
    "argument \"name\" is missing"
  )
  
  expect_error(
    ProviderConfig$new(
      name = NULL,
      type = "webgl",
      authentication_required = TRUE,
      supported_features = list(),
      coordinate_system = "EPSG:4326",
      default_style = "test"
    ),
    "Required configuration field 'name' is missing"
  )
  
  # Test invalid field types
  expect_error(
    ProviderConfig$new(
      name = c("test1", "test2"),
      type = "webgl",
      authentication_required = TRUE,
      supported_features = list(),
      coordinate_system = "EPSG:4326",
      default_style = "test"
    ),
    "Configuration field 'name' must be a single character string"
  )
  
  expect_error(
    ProviderConfig$new(
      name = "test",
      type = 123,
      authentication_required = TRUE,
      supported_features = list(),
      coordinate_system = "EPSG:4326",
      default_style = "test"
    ),
    "Configuration field 'type' must be a single character string"
  )
  
  expect_error(
    ProviderConfig$new(
      name = "test",
      type = "webgl",
      authentication_required = "yes",
      supported_features = list(),
      coordinate_system = "EPSG:4326",
      default_style = "test"
    ),
    "Configuration field 'authentication_required' must be a single logical value"
  )
  
  expect_error(
    ProviderConfig$new(
      name = "test",
      type = "webgl",
      authentication_required = TRUE,
      supported_features = "not_a_list",
      coordinate_system = "EPSG:4326",
      default_style = "test"
    ),
    "Configuration field 'supported_features' must be a list"
  )
})

test_that("ProviderConfig supports_feature method works", {
  config <- ProviderConfig$new(
    name = "test",
    type = "webgl",
    authentication_required = TRUE,
    supported_features = list(feature1 = TRUE, feature2 = FALSE, feature3 = NULL),
    coordinate_system = "EPSG:4326",
    default_style = "test_style"
  )
  
  expect_true(config$supports_feature("feature1"))
  expect_false(config$supports_feature("feature2"))
  expect_false(config$supports_feature("feature3"))
  expect_false(config$supports_feature("nonexistent"))
  
  # Test invalid input
  expect_error(config$supports_feature(123), "Feature must be a single character string")
  expect_error(config$supports_feature(c("a", "b")), "Feature must be a single character string")
})

test_that("ProviderConfig get_summary method works", {
  config <- ProviderConfig$new(
    name = "test",
    type = "webgl",
    authentication_required = TRUE,
    supported_features = list(feature1 = TRUE, feature2 = FALSE, feature3 = TRUE),
    coordinate_system = "EPSG:4326",
    default_style = "test_style"
  )
  
  summary <- config$get_summary()
  
  expect_equal(summary$name, "test")
  expect_equal(summary$type, "webgl")
  expect_true(summary$authentication_required)
  expect_equal(summary$coordinate_system, "EPSG:4326")
  expect_equal(summary$default_style, "test_style")
  expect_equal(summary$supported_features, c("feature1", "feature3"))
})

test_that("ProviderConfig update method works", {
  config <- ProviderConfig$new(
    name = "test",
    type = "webgl",
    authentication_required = TRUE,
    supported_features = list(feature1 = TRUE),
    coordinate_system = "EPSG:4326",
    default_style = "test_style"
  )
  
  # Test valid update
  result <- config$update(default_style = "new_style", type = "canvas")
  expect_equal(config$default_style, "new_style")
  expect_equal(config$type, "canvas")
  expect_identical(result, config)  # Should return self for chaining
  
  # Test warning for unknown field
  expect_warning(
    config$update(unknown_field = "value"),
    "Unknown configuration field: unknown_field"
  )
  
  # Test that validation is called after update
  expect_error(
    config$update(name = NULL),
    "Required configuration field 'name' is missing"
  )
})

test_that("create_default_provider_configs works correctly", {
  configs <- create_default_provider_configs()
  
  expect_true(is.list(configs))
  expect_true(length(configs) > 0)
  
  # Check that all expected providers are present
  expected_providers <- c("mapbox", "leaflet", "openlayers", "gaode", "baidu")
  expect_true(all(expected_providers %in% names(configs)))
  
  # Check that all configs are ProviderConfig objects
  for (config in configs) {
    expect_true(inherits(config, "ProviderConfig"))
  }
  
  # Test specific provider configurations
  mapbox_config <- configs$mapbox
  expect_equal(mapbox_config$name, "mapbox")
  expect_equal(mapbox_config$type, "webgl")
  expect_true(mapbox_config$authentication_required)
  expect_true(mapbox_config$supports_feature("3d_terrain"))
  expect_equal(mapbox_config$coordinate_system, "EPSG:4326")
  
  leaflet_config <- configs$leaflet
  expect_equal(leaflet_config$name, "leaflet")
  expect_equal(leaflet_config$type, "dom")
  expect_false(leaflet_config$authentication_required)
  expect_false(leaflet_config$supports_feature("3d_terrain"))
  
  gaode_config <- configs$gaode
  expect_equal(gaode_config$name, "gaode")
  expect_equal(gaode_config$coordinate_system, "GCJ02")
  expect_true(gaode_config$supports_feature("coordinate_transform"))
  
  baidu_config <- configs$baidu
  expect_equal(baidu_config$name, "baidu")
  expect_equal(baidu_config$coordinate_system, "BD09")
  expect_true(baidu_config$supports_feature("coordinate_transform"))
})

test_that("get_provider_config works correctly", {
  # Test valid provider
  config <- get_provider_config("mapbox")
  expect_true(inherits(config, "ProviderConfig"))
  expect_equal(config$name, "mapbox")
  
  # Test invalid provider
  config <- get_provider_config("nonexistent")
  expect_null(config)
  
  # Test invalid input
  expect_error(get_provider_config(123), "Provider name must be a single character string")
  expect_error(get_provider_config(c("a", "b")), "Provider name must be a single character string")
})

test_that("list_available_providers works correctly", {
  providers <- list_available_providers()
  
  expect_true(is.character(providers))
  expect_true(length(providers) > 0)
  
  expected_providers <- c("mapbox", "leaflet", "openlayers", "gaode", "baidu")
  expect_true(all(expected_providers %in% providers))
})