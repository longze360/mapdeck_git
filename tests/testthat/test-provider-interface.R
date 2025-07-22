test_that("IMapProvider interface is properly defined", {
  # Test that the interface class exists
  expect_true(exists("IMapProvider"))
  expect_true(R6::is.R6Class(IMapProvider))
  
  # Test that interface methods throw errors when not implemented
  provider <- IMapProvider$new()
  
  expect_error(provider$initialize(), "must be implemented by provider subclass")
  expect_error(provider$create_map(), "must be implemented by provider subclass")
  expect_error(provider$update_style(), "must be implemented by provider subclass")
  expect_error(provider$add_layer(), "must be implemented by provider subclass")
  expect_error(provider$remove_layer(), "must be implemented by provider subclass")
  expect_error(provider$set_view(), "must be implemented by provider subclass")
  expect_error(provider$validate_config(), "must be implemented by provider subclass")
  expect_error(provider$destroy(), "must be implemented by provider subclass")
})

test_that("Provider capabilities constants are defined", {
  expect_true(exists("PROVIDER_CAPABILITIES"))
  expect_true(is.list(PROVIDER_CAPABILITIES))
  expect_true("AUTHENTICATION_REQUIRED" %in% names(PROVIDER_CAPABILITIES))
  expect_true("CUSTOM_STYLES" %in% names(PROVIDER_CAPABILITIES))
})

test_that("Provider types constants are defined", {
  expect_true(exists("PROVIDER_TYPES"))
  expect_true(is.list(PROVIDER_TYPES))
  expect_equal(PROVIDER_TYPES$MAPBOX, "mapbox")
  expect_equal(PROVIDER_TYPES$LEAFLET, "leaflet")
  expect_equal(PROVIDER_TYPES$OPENLAYERS, "openlayers")
  expect_equal(PROVIDER_TYPES$GAODE, "gaode")
  expect_equal(PROVIDER_TYPES$BAIDU, "baidu")
})