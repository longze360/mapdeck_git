test_that("IMapProvider interface is properly defined", {
  # Test that IMapProvider class exists
  expect_true(exists("IMapProvider"))
  expect_true(inherits(IMapProvider, "R6ClassGenerator"))
  
  # Test required fields
  provider <- IMapProvider$new()
  expect_true(is.null(provider$provider_name))
  expect_true(is.null(provider$config))
  expect_true(is.null(provider$capabilities))
  expect_false(provider$initialized)
})

test_that("IMapProvider abstract methods throw errors", {
  provider <- IMapProvider$new()
  
  # Test that all abstract methods throw errors
  expect_error(provider$initialize(), "must be implemented by concrete provider classes")
  expect_error(provider$create_map("container"), "must be implemented by concrete provider classes")
  expect_error(provider$update_style("style"), "must be implemented by concrete provider classes")
  expect_error(provider$add_layer(list()), "must be implemented by concrete provider classes")
  expect_error(provider$remove_layer("id"), "must be implemented by concrete provider classes")
  expect_error(provider$set_view(0, 0, 0), "must be implemented by concrete provider classes")
  expect_error(provider$get_available_styles(), "must be implemented by concrete provider classes")
  expect_error(provider$validate_config(list()), "must be implemented by concrete provider classes")
  expect_error(provider$destroy(), "must be implemented by concrete provider classes")
})

test_that("IMapProvider concrete methods work correctly", {
  provider <- IMapProvider$new()
  
  # Test get_capabilities method
  provider$capabilities <- list(feature1 = TRUE, feature2 = FALSE)
  expect_equal(provider$get_capabilities(), list(feature1 = TRUE, feature2 = FALSE))
  
  # Test is_initialized method
  expect_false(provider$is_initialized())
  provider$initialized <- TRUE
  expect_true(provider$is_initialized())
})

test_that("validate_provider_interface works correctly", {
  # Test with valid R6 class that implements interface
  TestProvider <- R6::R6Class("TestProvider",
    inherit = IMapProvider,
    public = list(
      initialize = function(config = list()) { "implemented" },
      create_map = function(container, options = list()) { "implemented" },
      update_style = function(style) { "implemented" },
      add_layer = function(layer) { "implemented" },
      remove_layer = function(layer_id) { "implemented" },
      set_view = function(longitude, latitude, zoom, pitch = 0, bearing = 0) { "implemented" },
      get_available_styles = function() { "implemented" },
      validate_config = function(config) { "implemented" },
      destroy = function() { "implemented" }
    )
  )
  
  expect_true(validate_provider_interface(TestProvider))
  
  # Test with incomplete implementation
  IncompleteProvider <- R6::R6Class("IncompleteProvider",
    inherit = IMapProvider,
    public = list(
      initialize = function(config = list()) { "implemented" }
      # Missing other required methods
    )
  )
  
  expect_warning(
    result <- validate_provider_interface(IncompleteProvider),
    "Provider class missing required methods"
  )
  expect_false(result)
  
  # Test with non-R6 class
  expect_false(validate_provider_interface("not_a_class"))
  expect_false(validate_provider_interface(list()))
})

test_that("Provider interface inheritance works", {
  # Create a concrete implementation
  ConcreteProvider <- R6::R6Class("ConcreteProvider",
    inherit = IMapProvider,
    public = list(
      initialize = function(config = list()) {
        self$provider_name <- "test"
        self$config <- config
        self$capabilities <- list(test = TRUE)
        self$initialized <- TRUE
        invisible(self)
      },
      create_map = function(container, options = list()) {
        list(container = container, options = options)
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
        c("style1", "style2")
      },
      validate_config = function(config) {
        TRUE
      },
      destroy = function() {
        self$initialized <- FALSE
        invisible(NULL)
      }
    )
  )
  
  # Test concrete implementation
  provider <- ConcreteProvider$new()
  provider$initialize(list(token = "test"))
  
  expect_equal(provider$provider_name, "test")
  expect_equal(provider$config, list(token = "test"))
  expect_true(provider$initialized)
  expect_equal(provider$get_capabilities(), list(test = TRUE))
  
  # Test methods work without errors
  expect_silent(provider$update_style("new_style"))
  expect_silent(provider$add_layer(list(id = "test")))
  expect_silent(provider$remove_layer("test"))
  expect_silent(provider$set_view(-74, 40.7, 10))
  
  expect_equal(provider$get_available_styles(), c("style1", "style2"))
  expect_true(provider$validate_config(list()))
  
  map <- provider$create_map("container", list(zoom = 10))
  expect_equal(map$container, "container")
  expect_equal(map$options$zoom, 10)
  
  provider$destroy()
  expect_false(provider$initialized)
})