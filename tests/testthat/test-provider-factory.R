test_that("ProviderRegistry initialization works correctly", {
  registry <- ProviderRegistry$new()
  
  expect_true(is.list(registry$providers))
  expect_true(is.list(registry$factories))
  expect_true(length(registry$providers) > 0)  # Should have default providers
  
  # Check that default providers are registered
  expected_providers <- c("mapbox", "leaflet", "openlayers", "gaode", "baidu")
  expect_true(all(expected_providers %in% names(registry$providers)))
})

test_that("ProviderRegistry register_provider works correctly", {
  registry <- ProviderRegistry$new()
  
  # Create a test provider class
  TestProvider <- R6::R6Class("TestProvider",
    inherit = IMapProvider,
    public = list(
      initialize = function(config = list()) { invisible(self) },
      create_map = function(container, options = list()) { list() },
      update_style = function(style) { invisible(self) },
      add_layer = function(layer) { invisible(self) },
      remove_layer = function(layer_id) { invisible(self) },
      set_view = function(longitude, latitude, zoom, pitch = 0, bearing = 0) { invisible(self) },
      get_available_styles = function() { character(0) },
      validate_config = function(config) { TRUE },
      destroy = function() { invisible(NULL) }
    )
  )
  
  test_config <- ProviderConfig$new(
    name = "test",
    type = "test",
    authentication_required = FALSE,
    supported_features = list(),
    coordinate_system = "EPSG:4326",
    default_style = "test"
  )
  
  # Test successful registration
  result <- registry$register_provider("test", TestProvider, test_config)
  expect_identical(result, registry)  # Should return self for chaining
  expect_true(registry$is_registered("test"))
  expect_identical(registry$get_provider_config("test"), test_config)
  expect_identical(registry$get_provider_factory("test"), TestProvider)
  
  # Test invalid inputs
  expect_error(
    registry$register_provider(123, TestProvider, test_config),
    "Provider name must be a single character string"
  )
  
  expect_error(
    registry$register_provider("test2", "not_a_class", test_config),
    "Provider class must be an R6 class generator"
  )
  
  expect_error(
    registry$register_provider("test3", TestProvider, "not_a_config"),
    "Config must be a ProviderConfig object"
  )
})

test_that("ProviderRegistry unregister_provider works correctly", {
  registry <- ProviderRegistry$new()
  
  # Test unregistering existing provider
  expect_true(registry$is_registered("mapbox"))
  result <- registry$unregister_provider("mapbox")
  expect_identical(result, registry)  # Should return self for chaining
  expect_false(registry$is_registered("mapbox"))
  
  # Test unregistering non-existent provider
  expect_warning(
    registry$unregister_provider("nonexistent"),
    "Provider 'nonexistent' not found in registry"
  )
  
  # Test invalid input
  expect_error(
    registry$unregister_provider(123),
    "Provider name must be a single character string"
  )
})

test_that("ProviderRegistry query methods work correctly", {
  registry <- ProviderRegistry$new()
  
  # Test is_registered
  expect_true(registry$is_registered("mapbox"))
  expect_false(registry$is_registered("nonexistent"))
  expect_false(registry$is_registered(123))
  
  # Test get_provider_config
  config <- registry$get_provider_config("mapbox")
  expect_true(inherits(config, "ProviderConfig"))
  expect_equal(config$name, "mapbox")
  expect_null(registry$get_provider_config("nonexistent"))
  expect_null(registry$get_provider_config(123))
  
  # Test get_provider_factory
  # Note: Default providers have NULL factories initially
  factory <- registry$get_provider_factory("mapbox")
  expect_null(factory)  # Default providers don't have factories yet
  expect_null(registry$get_provider_factory("nonexistent"))
  expect_null(registry$get_provider_factory(123))
  
  # Test list_providers
  providers <- registry$list_providers()
  expect_true(is.character(providers))
  expect_true("mapbox" %in% providers)
  
  # Test get_all_capabilities
  capabilities <- registry$get_all_capabilities()
  expect_true(is.list(capabilities))
  expect_true("mapbox" %in% names(capabilities))
  expect_true(is.list(capabilities$mapbox))
})

test_that("ProviderFactory initialization works correctly", {
  # Test with default registry
  factory <- ProviderFactory$new()
  expect_true(inherits(factory$registry, "ProviderRegistry"))
  
  # Test with custom registry
  custom_registry <- ProviderRegistry$new()
  factory2 <- ProviderFactory$new(custom_registry)
  expect_identical(factory2$registry, custom_registry)
  
  # Test with invalid registry
  expect_error(
    ProviderFactory$new("not_a_registry"),
    "Registry must be a ProviderRegistry instance"
  )
})

test_that("ProviderFactory create_provider validation works", {
  factory <- ProviderFactory$new()
  
  # Test invalid provider name
  expect_error(
    factory$create_provider(123),
    "Provider name must be a single character string"
  )
  
  expect_error(
    factory$create_provider("nonexistent"),
    "Provider 'nonexistent' is not registered"
  )
  
  # Test invalid config
  expect_error(
    factory$create_provider("mapbox", "not_a_list"),
    "Config must be a list"
  )
  
  # Test provider without factory (default providers)
  expect_error(
    factory$create_provider("mapbox", list()),
    "Provider class for 'mapbox' is not available yet"
  )
})

test_that("ProviderFactory merge_configs works correctly", {
  factory <- ProviderFactory$new()
  
  default_config <- ProviderConfig$new(
    name = "test",
    type = "webgl",
    authentication_required = TRUE,
    supported_features = list(feature1 = TRUE),
    coordinate_system = "EPSG:4326",
    default_style = "default"
  )
  
  user_config <- list(
    default_style = "custom",
    token = "user_token",
    new_option = "value"
  )
  
  merged <- factory$merge_configs(default_config, user_config)
  
  expect_equal(merged$name, "test")
  expect_equal(merged$type, "webgl")
  expect_equal(merged$default_style, "custom")  # User override
  expect_equal(merged$token, "user_token")  # User addition
  expect_equal(merged$new_option, "value")  # User addition
})

test_that("ProviderFactory utility methods work correctly", {
  factory <- ProviderFactory$new()
  
  # Test get_available_providers
  providers <- factory$get_available_providers()
  expect_true(is.character(providers))
  expect_true("mapbox" %in% providers)
  
  # Test get_provider_capabilities
  capabilities <- factory$get_provider_capabilities("mapbox")
  expect_true(is.list(capabilities))
  expect_equal(capabilities$name, "mapbox")
  
  expect_error(
    factory$get_provider_capabilities("nonexistent"),
    "Provider 'nonexistent' not found"
  )
  
  expect_error(
    factory$get_provider_capabilities(123),
    "Provider name must be a single character string"
  )
  
  # Test validate_provider_config
  valid_config <- list(token = "test_token")
  expect_true(factory$validate_provider_config("mapbox", valid_config))
  
  invalid_config <- list()  # Missing token for auth-required provider
  expect_false(factory$validate_provider_config("mapbox", invalid_config))
  
  leaflet_config <- list()  # No auth required for leaflet
  expect_true(factory$validate_provider_config("leaflet", leaflet_config))
  
  expect_error(
    factory$validate_provider_config("nonexistent", list()),
    "Provider 'nonexistent' not found"
  )
})

test_that("Global provider factory functions work correctly", {
  # Test get_provider_factory
  factory1 <- get_provider_factory()
  expect_true(inherits(factory1, "ProviderFactory"))
  
  # Should return same instance on subsequent calls
  factory2 <- get_provider_factory()
  expect_identical(factory1, factory2)
  
  # Test get_provider_capabilities
  capabilities <- get_provider_capabilities("mapbox")
  expect_true(is.list(capabilities))
  expect_equal(capabilities$name, "mapbox")
  
  # Test register_provider (should not error)
  TestProvider <- R6::R6Class("TestProvider",
    inherit = IMapProvider,
    public = list(
      initialize = function(config = list()) { invisible(self) },
      create_map = function(container, options = list()) { list() },
      update_style = function(style) { invisible(self) },
      add_layer = function(layer) { invisible(self) },
      remove_layer = function(layer_id) { invisible(self) },
      set_view = function(longitude, latitude, zoom, pitch = 0, bearing = 0) { invisible(self) },
      get_available_styles = function() { character(0) },
      validate_config = function(config) { TRUE },
      destroy = function() { invisible(NULL) }
    )
  )
  
  test_config <- ProviderConfig$new(
    name = "test_global",
    type = "test",
    authentication_required = FALSE,
    supported_features = list(),
    coordinate_system = "EPSG:4326",
    default_style = "test"
  )
  
  expect_silent(register_provider("test_global", TestProvider, test_config))
  
  # Test create_provider with registered provider
  provider <- create_provider("test_global", list())
  expect_true(inherits(provider, "TestProvider"))
})