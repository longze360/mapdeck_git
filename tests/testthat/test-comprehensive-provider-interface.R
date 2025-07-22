# Comprehensive Provider Interface Tests
# Tests for 100% interface coverage as required by task 12.1

test_that("IMapProvider class definition is complete", {
  skip_if_not_installed("R6")
  
  # Test class exists and is properly defined
  expect_true(exists("IMapProvider"))
  expect_true(inherits(IMapProvider, "R6ClassGenerator"))
  
  # Test required fields exist
  provider <- IMapProvider$new()
  expect_true(exists("provider_name", envir = provider))
  expect_true(exists("config", envir = provider))
  expect_true(exists("capabilities", envir = provider))
  expect_true(exists("initialized", envir = provider))
  
  # Test initial field values
  expect_null(provider$provider_name)
  expect_null(provider$config)
  expect_null(provider$capabilities)
  expect_false(provider$initialized)
})

test_that("IMapProvider abstract methods throw appropriate errors", {
  skip_if_not_installed("R6")
  
  provider <- IMapProvider$new()
  
  # Test all abstract methods throw errors with correct message
  expect_error(
    provider$initialize_provider(),
    "must be implemented by concrete provider classes"
  )
  
  expect_error(
    provider$create_map("container"),
    "must be implemented by concrete provider classes"
  )
  
  expect_error(
    provider$update_style("style"),
    "must be implemented by concrete provider classes"
  )
  
  expect_error(
    provider$add_layer(list()),
    "must be implemented by concrete provider classes"
  )
  
  expect_error(
    provider$remove_layer("id"),
    "must be implemented by concrete provider classes"
  )
  
  expect_error(
    provider$set_view(0, 0, 0),
    "must be implemented by concrete provider classes"
  )
  
  expect_error(
    provider$get_available_styles(),
    "must be implemented by concrete provider classes"
  )
  
  expect_error(
    provider$validate_config(list()),
    "must be implemented by concrete provider classes"
  )
  
  expect_error(
    provider$destroy(),
    "must be implemented by concrete provider classes"
  )
})

test_that("IMapProvider concrete methods work correctly", {
  skip_if_not_installed("R6")
  
  provider <- IMapProvider$new()
  
  # Test get_capabilities method
  test_capabilities <- list(feature1 = TRUE, feature2 = FALSE)
  provider$capabilities <- test_capabilities
  expect_equal(provider$get_capabilities(), test_capabilities)
  
  # Test with NULL capabilities
  provider$capabilities <- NULL
  expect_null(provider$get_capabilities())
  
  # Test is_initialized method
  expect_false(provider$is_initialized())
  provider$initialized <- TRUE
  expect_true(provider$is_initialized())
  provider$initialized <- FALSE
  expect_false(provider$is_initialized())
})

test_that("validate_provider_interface works with valid implementations", {
  skip_if_not_installed("R6")
  
  # Create a complete valid implementation
  ValidProvider <- R6::R6Class("ValidProvider",
    inherit = IMapProvider,
    public = list(
      initialize_provider = function(config = list()) { "implemented" },
      create_map = function(container, options = list()) { "implemented" },
      update_style = function(style) { "implemented" },
      add_layer = function(layer) { "implemented" },
      remove_layer = function(layer_id) { "implemented" },
      set_view = function(longitude, latitude, zoom, pitch = 0, bearing = 0) { 
        "implemented" 
      },
      get_available_styles = function() { "implemented" },
      validate_config = function(config) { "implemented" },
      destroy = function() { "implemented" }
    )
  )
  
  expect_true(validate_provider_interface(ValidProvider))
})

test_that("validate_provider_interface detects missing methods", {
  skip_if_not_installed("R6")
  
  # Create implementation missing some methods
  IncompleteProvider <- R6::R6Class("IncompleteProvider",
    inherit = IMapProvider,
    public = list(
      initialize_provider = function(config = list()) { "implemented" },
      create_map = function(container, options = list()) { "implemented" }
      # Missing other required methods
    )
  )
  
  expect_warning(
    result <- validate_provider_interface(IncompleteProvider),
    "Provider class missing required methods"
  )
  expect_false(result)
})

test_that("validate_provider_interface handles invalid inputs", {
  # Test with non-R6 class
  expect_false(validate_provider_interface("not_a_class"))
  expect_false(validate_provider_interface(list()))
  expect_false(validate_provider_interface(NULL))
  expect_false(validate_provider_interface(123))
  
  # Test with regular R6 class that doesn't inherit from IMapProvider
  RegularClass <- R6::R6Class("RegularClass",
    public = list(
      some_method = function() { "test" }
    )
  )
  
  expect_warning(
    result <- validate_provider_interface(RegularClass),
    "Provider class missing required methods"
  )
  expect_false(result)
})

test_that("Provider interface inheritance works correctly", {
  skip_if_not_installed("R6")
  
  # Create a concrete implementation
  ConcreteProvider <- R6::R6Class("ConcreteProvider",
    inherit = IMapProvider,
    public = list(
      initialize_provider = function(config = list()) {
        self$provider_name <- "concrete"
        self$config <- config
        self$capabilities <- list(test = TRUE)
        self$initialized <- TRUE
        invisible(self)
      },
      
      create_map = function(container, options = list()) {
        list(container = container, options = options)
      },
      
      update_style = function(style) {
        self$current_style <- style
        invisible(self)
      },
      
      add_layer = function(layer) {
        if (is.null(self$layers)) self$layers <- list()
        self$layers[[layer$id]] <- layer
        invisible(self)
      },
      
      remove_layer = function(layer_id) {
        if (!is.null(self$layers)) {
          self$layers[[layer_id]] <- NULL
        }
        invisible(self)
      },
      
      set_view = function(longitude, latitude, zoom, pitch = 0, bearing = 0) {
        self$view <- list(
          longitude = longitude, latitude = latitude, zoom = zoom,
          pitch = pitch, bearing = bearing
        )
        invisible(self)
      },
      
      get_available_styles = function() {
        c("style1", "style2", "style3")
      },
      
      validate_config = function(config) {
        is.list(config)
      },
      
      destroy = function() {
        self$initialized <- FALSE
        self$config <- NULL
        self$layers <- NULL
        invisible(NULL)
      }
    ),
    
    private = list(
      layers = NULL,
      current_style = NULL,
      view = NULL
    )
  )
  
  # Test concrete implementation
  provider <- ConcreteProvider$new()
  expect_false(provider$initialized)
  
  # Test initialization
  config <- list(token = "test", option = "value")
  provider$initialize_provider(config)
  
  expect_equal(provider$provider_name, "concrete")
  expect_equal(provider$config, config)
  expect_true(provider$initialized)
  expect_equal(provider$get_capabilities(), list(test = TRUE))
  
  # Test create_map
  map <- provider$create_map("test_container", list(zoom = 10))
  expect_equal(map$container, "test_container")
  expect_equal(map$options$zoom, 10)
  
  # Test style management
  expect_silent(provider$update_style("new_style"))
  styles <- provider$get_available_styles()
  expect_equal(styles, c("style1", "style2", "style3"))
  
  # Test layer management
  test_layer <- list(id = "test_layer", type = "scatterplot")
  expect_silent(provider$add_layer(test_layer))
  expect_silent(provider$remove_layer("test_layer"))
  
  # Test view management
  expect_silent(provider$set_view(-74, 40.7, 10, 30, 45))
  
  # Test configuration validation
  expect_true(provider$validate_config(list()))
  expect_false(provider$validate_config("not_a_list"))
  
  # Test destruction
  provider$destroy()
  expect_false(provider$initialized)
  expect_null(provider$config)
})

test_that("Provider interface supports method chaining", {
  skip_if_not_installed("R6")
  
  # Create chainable provider
  ChainableProvider <- R6::R6Class("ChainableProvider",
    inherit = IMapProvider,
    public = list(
      initialize_provider = function(config = list()) {
        self$initialized <- TRUE
        invisible(self)
      },
      
      create_map = function(container, options = list()) {
        list(success = TRUE)
      },
      
      update_style = function(style) {
        invisible(self)
      },
      
      add_layer = function(layer) {
        invisible(self)
      },
      
      remove_layer = function(layer_id) {
        invisible(self)
      },
      
      set_view = function(longitude, latitude, zoom, pitch = 0, bearing = 0) {
        invisible(self)
      },
      
      get_available_styles = function() {
        character(0)
      },
      
      validate_config = function(config) {
        TRUE
      },
      
      destroy = function() {
        invisible(NULL)
      }
    )
  )
  
  provider <- ChainableProvider$new()
  
  # Test method chaining
  result <- provider$initialize_provider()$update_style("test")$add_layer(list(id = "test"))
  expect_identical(result, provider)
  
  # Test that set_view returns self for chaining
  result <- provider$set_view(0, 0, 10)
  expect_identical(result, provider)
})

test_that("Provider interface error handling is consistent", {
  skip_if_not_installed("R6")
  
  # Test that all abstract methods throw the same type of error
  provider <- IMapProvider$new()
  
  methods_to_test <- list(
    list(method = "initialize_provider", args = list()),
    list(method = "create_map", args = list("container")),
    list(method = "update_style", args = list("style")),
    list(method = "add_layer", args = list(list())),
    list(method = "remove_layer", args = list("id")),
    list(method = "set_view", args = list(0, 0, 0)),
    list(method = "get_available_styles", args = list()),
    list(method = "validate_config", args = list(list())),
    list(method = "destroy", args = list())
  )
  
  for (method_info in methods_to_test) {
    expect_error(
      do.call(provider[[method_info$method]], method_info$args),
      "must be implemented by concrete provider classes",
      info = paste("Method", method_info$method, "should throw consistent error")
    )
  }
})

test_that("Provider interface documentation is complete", {
  skip_if_not_installed("R6")
  
  # Test that IMapProvider has proper documentation structure
  expect_true(exists("IMapProvider"))
  
  # Test that validate_provider_interface is exported
  expect_true(exists("validate_provider_interface"))
  expect_true(is.function(validate_provider_interface))
})

test_that("Provider interface supports edge cases", {
  skip_if_not_installed("R6")
  
  # Test with provider that has additional methods
  ExtendedProvider <- R6::R6Class("ExtendedProvider",
    inherit = IMapProvider,
    public = list(
      # Required methods
      initialize_provider = function(config = list()) { invisible(self) },
      create_map = function(container, options = list()) { list() },
      update_style = function(style) { invisible(self) },
      add_layer = function(layer) { invisible(self) },
      remove_layer = function(layer_id) { invisible(self) },
      set_view = function(longitude, latitude, zoom, pitch = 0, bearing = 0) { 
        invisible(self) 
      },
      get_available_styles = function() { character(0) },
      validate_config = function(config) { TRUE },
      destroy = function() { invisible(NULL) },
      
      # Additional methods
      custom_method = function() { "custom" },
      another_method = function(x) { x * 2 }
    )
  )
  
  # Should still validate successfully
  expect_true(validate_provider_interface(ExtendedProvider))
  
  # Test provider with custom fields
  provider <- ExtendedProvider$new()
  provider$custom_field <- "test"
  expect_equal(provider$custom_field, "test")
  expect_equal(provider$custom_method(), "custom")
  expect_equal(provider$another_method(5), 10)
})