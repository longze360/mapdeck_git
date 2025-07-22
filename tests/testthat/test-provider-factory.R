test_that("ProviderRegistry initializes correctly", {
  registry <- ProviderRegistry$new()
  
  expect_true(is.list(registry$providers))
  expect_true(is.list(registry$configs))
  expect_equal(length(registry$providers), 0)
  expect_true(length(registry$configs) > 0)  # Default configs loaded
})

test_that("ProviderRegistry register_provider works", {
  registry <- ProviderRegistry$new()
  
  # Create a test provider class
  TestProvider <- R6::R6Class("TestProvider",
    inherit = IMapProvider,
    public = list(
      initialize = function(config = list()) {},
      create_map = function(container, options = list()) {},
      update_style = function(style) {},
      add_layer = function(layer) {},
      remove_layer = function(layer_id) {},
      set_view = function(longitude, latitude, zoom, pitch = 0, bearing = 0) {},
      get_available_styles = function() { return(c("style1", "style2")) },
      validate_config = function(config) { return(TRUE) },
      destroy = function() {}
    )
  )
  
  # Register the provider
  test_config <- ProviderConfig$new(name = "test", type = "test")
  registry$register_provider("test", TestProvider, test_config)
  
  expect_true(registry$is_registered("test"))
  expect_true("test" %in% registry$get_registered_providers())
  expect_equal(registry$get_provider_class("test"), TestProvider)
})

test_that("ProviderRegistry validates provider interface", {
  registry <- ProviderRegistry$new()
  
  # Create invalid provider (missing methods)
  InvalidProvider <- R6::R6Class("InvalidProvider",
    public = list(
      initialize = function() {}
      # Missing other required methods
    )
  )
  
  expect_error(
    registry$register_provider("invalid", InvalidProvider),
    "Provider class must implement IMapProvider interface"
  )
})

test_that("ProviderRegistry unregister_provider works", {
  registry <- ProviderRegistry$new()
  
  # Create and register test provider
  TestProvider <- R6::R6Class("TestProvider",
    inherit = IMapProvider,
    public = list(
      initialize = function(config = list()) {},
      create_map = function(container, options = list()) {},
      update_style = function(style) {},
      add_layer = function(layer) {},
      remove_layer = function(layer_id) {},
      set_view = function(longitude, latitude, zoom, pitch = 0, bearing = 0) {},
      get_available_styles = function() { return(c()) },
      validate_config = function(config) { return(TRUE) },
      destroy = function() {}
    )
  )
  
  test_config <- ProviderConfig$new(name = "test", type = "test")
  registry$register_provider("test", TestProvider, test_config)
  
  expect_true(registry$is_registered("test"))
  
  # Unregister
  registry$unregister_provider("test")
  expect_false(registry$is_registered("test"))
})

test_that("ProviderFactory initializes correctly", {
  factory <- ProviderFactory$new()
  
  expect_true(inherits(factory$registry, "ProviderRegistry"))
})

test_that("ProviderFactory create_provider works", {
  factory <- ProviderFactory$new()
  
  # Create test provider
  TestProvider <- R6::R6Class("TestProvider",
    inherit = IMapProvider,
    public = list(
      config = NULL,
      initialize = function(config = list()) {
        self$config <- config
      },
      create_map = function(container, options = list()) {},
      update_style = function(style) {},
      add_layer = function(layer) {},
      remove_layer = function(layer_id) {},
      set_view = function(longitude, latitude, zoom, pitch = 0, bearing = 0) {},
      get_available_styles = function() { return(c()) },
      validate_config = function(config) { return(TRUE) },
      destroy = function() {}
    )
  )
  
  # Register provider
  test_config <- ProviderConfig$new(name = "test", type = "test")
  factory$registry$register_provider("test", TestProvider, test_config)
  
  # Create instance
  instance <- factory$create_provider("test", list(custom_option = "value"))
  
  expect_true(inherits(instance, "TestProvider"))
  expect_equal(instance$config$custom_option, "value")
})

test_that("ProviderFactory handles unregistered providers", {
  factory <- ProviderFactory$new()
  
  expect_error(
    factory$create_provider("nonexistent"),
    "Provider not registered: nonexistent"
  )
})

test_that("ProviderFactory get_provider_info works", {
  factory <- ProviderFactory$new()
  
  # Create and register test provider
  TestProvider <- R6::R6Class("TestProvider",
    inherit = IMapProvider,
    public = list(
      initialize = function(config = list()) {},
      create_map = function(container, options = list()) {},
      update_style = function(style) {},
      add_layer = function(layer) {},
      remove_layer = function(layer_id) {},
      set_view = function(longitude, latitude, zoom, pitch = 0, bearing = 0) {},
      get_available_styles = function() { return(c()) },
      validate_config = function(config) { return(TRUE) },
      destroy = function() {}
    )
  )
  
  test_config <- ProviderConfig$new(
    name = "test", 
    type = "test_type",
    auth_required = TRUE,
    features = c("feature1", "feature2")
  )
  factory$registry$register_provider("test", TestProvider, test_config)
  
  info <- factory$get_provider_info("test")
  
  expect_equal(info$name, "test")
  expect_equal(info$type, "test_type")
  expect_true(info$authentication_required)
  expect_equal(info$capabilities, c("feature1", "feature2"))
  
  # Test non-existent provider
  expect_null(factory$get_provider_info("nonexistent"))
})

test_that("global provider factory functions work", {
  # Test get_provider_factory
  factory1 <- get_provider_factory()
  factory2 <- get_provider_factory()
  expect_identical(factory1, factory2)  # Should be same instance
  
  # Test get_available_providers (should work even without registered providers)
  providers <- get_available_providers()
  expect_true(is.character(providers))
})

test_that("register_provider convenience function works", {
  # Create test provider
  TestProvider <- R6::R6Class("TestProvider",
    inherit = IMapProvider,
    public = list(
      initialize = function(config = list()) {},
      create_map = function(container, options = list()) {},
      update_style = function(style) {},
      add_layer = function(layer) {},
      remove_layer = function(layer_id) {},
      set_view = function(longitude, latitude, zoom, pitch = 0, bearing = 0) {},
      get_available_styles = function() { return(c()) },
      validate_config = function(config) { return(TRUE) },
      destroy = function() {}
    )
  )
  
  # Register using convenience function
  test_config <- ProviderConfig$new(name = "convenience_test", type = "test")
  register_provider("convenience_test", TestProvider, test_config)
  
  # Verify registration
  factory <- get_provider_factory()
  expect_true(factory$registry$is_registered("convenience_test"))
})

test_that("create_provider convenience function works", {
  # Assuming we have a registered provider from previous test
  TestProvider <- R6::R6Class("TestProvider2",
    inherit = IMapProvider,
    public = list(
      initialize = function(config = list()) {},
      create_map = function(container, options = list()) {},
      update_style = function(style) {},
      add_layer = function(layer) {},
      remove_layer = function(layer_id) {},
      set_view = function(longitude, latitude, zoom, pitch = 0, bearing = 0) {},
      get_available_styles = function() { return(c()) },
      validate_config = function(config) { return(TRUE) },
      destroy = function() {}
    )
  )
  
  test_config <- ProviderConfig$new(name = "convenience_test2", type = "test")
  register_provider("convenience_test2", TestProvider, test_config)
  
  # Create using convenience function
  instance <- create_provider("convenience_test2")
  expect_true(inherits(instance, "TestProvider2"))
})