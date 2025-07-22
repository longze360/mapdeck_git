test_that("Provider registry works correctly", {
  # Get registry instance
  registry <- get_provider_registry()
  expect_true(R6::is.R6(registry))
  
  # Clear registry for clean test
  registry$clear_registry()
  
  # Test empty registry
  expect_equal(length(registry$list_providers()), 0)
  expect_false(registry$is_registered("test_provider"))
  
  # Create a mock provider class
  MockProvider <- R6::R6Class(
    "MockProvider",
    inherit = IMapProvider,
    public = list(
      initialize = function(config = list()) {
        self$provider_name <- "mock"
        self$config <- config
        invisible(self)
      },
      validate_config = function(config) {
        list(valid = TRUE, errors = character(0))
      }
    )
  )
  
  # Register mock provider
  registry$register_provider("mock", MockProvider, list("test_capability"))
  
  # Test registration
  expect_true(registry$is_registered("mock"))
  expect_equal(registry$list_providers(), "mock")
  expect_equal(registry$get_capabilities("mock"), list("test_capability"))
  
  # Test getting provider class
  provider_class <- registry$get_provider_class("mock")
  expect_true(R6::is.R6Class(provider_class))
  
  # Test error for unregistered provider
  expect_error(registry$get_provider_class("nonexistent"), "not registered")
  expect_error(registry$get_capabilities("nonexistent"), "not registered")
})

test_that("Provider factory creates providers correctly", {
  # Get factory instance
  factory <- get_provider_factory()
  expect_true(R6::is.R6(factory))
  
  # Clear registry and register mock provider
  registry <- get_provider_registry()
  registry$clear_registry()
  
  MockProvider <- R6::R6Class(
    "MockProvider",
    inherit = IMapProvider,
    public = list(
      initialize = function(config = list()) {
        self$provider_name <- "mock"
        self$config <- config
        invisible(self)
      },
      validate_config = function(config) {
        list(valid = TRUE, errors = character(0))
      }
    )
  )
  
  registry$register_provider("mock", MockProvider)
  
  # Test creating provider
  provider <- factory$create_provider("mock", list(test_option = "value"))
  expect_true(R6::is.R6(provider))
  expect_equal(provider$provider_name, "mock")
  expect_equal(provider$config$test_option, "value")
  
  # Test error for unregistered provider
  expect_error(factory$create_provider("nonexistent"), "not registered")
})

test_that("Exported functions work correctly", {
  # Clear registry
  registry <- get_provider_registry()
  registry$clear_registry()
  
  # Test list_providers with empty registry
  expect_equal(list_providers(), character(0))
  
  # Create and register mock provider
  MockProvider <- R6::R6Class(
    "MockProvider",
    inherit = IMapProvider,
    public = list(
      initialize = function(config = list()) {
        self$provider_name <- "mock"
        self$config <- config
        invisible(self)
      },
      validate_config = function(config) {
        list(valid = TRUE, errors = character(0))
      }
    )
  )
  
  # Test register_provider function
  register_provider("mock", MockProvider, list("test_capability"))
  expect_equal(list_providers(), "mock")
  expect_equal(get_provider_capabilities("mock"), list("test_capability"))
  
  # Test create_provider function
  provider <- create_provider("mock", list(test = "value"))
  expect_true(R6::is.R6(provider))
  expect_equal(provider$provider_name, "mock")
})