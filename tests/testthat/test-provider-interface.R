test_that("IMapProvider interface is properly defined", {
  # Test that IMapProvider class exists and is an R6 class
  expect_true(R6::is.R6Class(IMapProvider))
  
  # Test required public fields
  expected_fields <- c("provider_name", "provider_type", "config", "capabilities")
  actual_fields <- names(IMapProvider$public_fields)
  expect_true(all(expected_fields %in% actual_fields))
  
  # Test required public methods
  expected_methods <- c(
    "initialize", "create_map", "update_style", "add_layer", 
    "remove_layer", "set_view", "get_available_styles", 
    "validate_config", "get_capabilities", "destroy"
  )
  actual_methods <- names(IMapProvider$public_methods)
  expect_true(all(expected_methods %in% actual_methods))
})

test_that("IMapProvider methods throw appropriate errors", {
  # Create instance (should work)
  provider <- IMapProvider$new()
  
  # Test that abstract methods throw errors
  expect_error(provider$initialize(), "must be implemented by provider subclass")
  expect_error(provider$create_map("container"), "must be implemented by provider subclass")
  expect_error(provider$update_style("style"), "must be implemented by provider subclass")
  expect_error(provider$add_layer(list()), "must be implemented by provider subclass")
  expect_error(provider$remove_layer("id"), "must be implemented by provider subclass")
  expect_error(provider$set_view(0, 0, 10), "must be implemented by provider subclass")
  expect_error(provider$get_available_styles(), "must be implemented by provider subclass")
  expect_error(provider$validate_config(list()), "must be implemented by provider subclass")
  expect_error(provider$destroy(), "must be implemented by provider subclass")
})

test_that("get_capabilities method works correctly", {
  provider <- IMapProvider$new()
  
  # Test with NULL capabilities
  expect_null(provider$get_capabilities())
  
  # Test with set capabilities
  provider$capabilities <- c("feature1", "feature2")
  expect_equal(provider$get_capabilities(), c("feature1", "feature2"))
})

test_that("validate_provider_interface works correctly", {
  # Test with non-R6 class
  expect_false(validate_provider_interface("not_a_class"))
  expect_false(validate_provider_interface(list()))
  
  # Test with R6 class missing methods
  IncompleteProvider <- R6::R6Class("IncompleteProvider",
    public = list(
      initialize = function() {},
      create_map = function(container, options) {}
      # Missing other required methods
    )
  )
  
  expect_warning(result <- validate_provider_interface(IncompleteProvider))
  expect_false(result)
  
  # Test with complete provider implementation
  CompleteProvider <- R6::R6Class("CompleteProvider",
    public = list(
      initialize = function(config = list()) {},
      create_map = function(container, options = list()) {},
      update_style = function(style) {},
      add_layer = function(layer) {},
      remove_layer = function(layer_id) {},
      set_view = function(longitude, latitude, zoom, pitch = 0, bearing = 0) {},
      get_available_styles = function() {},
      validate_config = function(config) {},
      get_capabilities = function() {},
      destroy = function() {}
    )
  )
  
  expect_true(validate_provider_interface(CompleteProvider))
})

test_that("provider interface validation handles edge cases", {
  # Test with class that has extra methods (should still pass)
  ExtendedProvider <- R6::R6Class("ExtendedProvider",
    public = list(
      initialize = function(config = list()) {},
      create_map = function(container, options = list()) {},
      update_style = function(style) {},
      add_layer = function(layer) {},
      remove_layer = function(layer_id) {},
      set_view = function(longitude, latitude, zoom, pitch = 0, bearing = 0) {},
      get_available_styles = function() {},
      validate_config = function(config) {},
      get_capabilities = function() {},
      destroy = function() {},
      extra_method = function() {}  # Extra method
    )
  )
  
  expect_true(validate_provider_interface(ExtendedProvider))
})