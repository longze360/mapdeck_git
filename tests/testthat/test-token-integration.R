context("Token Management Integration")

# Test integration between old and new token management systems
test_that("set_token backward compatibility works correctly", {
  # Clear any existing tokens
  clear_tokens()
  
  # Test backward compatible usage (single parameter)
  set_token("pk.backward_compatible_token")
  
  # Should be accessible through both old and new methods
  expect_equal(get_access_token(), "pk.backward_compatible_token")
  expect_equal(get_access_token("mapbox"), "pk.backward_compatible_token")
  
  # Should appear in token listing
  tokens <- mapdeck_tokens()
  expect_true(inherits(tokens, "mapdeck_api"))
  expect_true("mapbox" %in% names(tokens$mapdeck))
})

test_that("set_token multi-provider functionality works correctly", {
  # Clear any existing tokens
  clear_tokens()
  
  # Set tokens for multiple providers
  set_token("pk.mapbox_token_123456789", "mapbox")
  set_token("gaode_api_key_123456789", "gaode")
  set_token("baidu_api_key_123456789", "baidu")
  
  # Test retrieval
  expect_equal(get_access_token("mapbox"), "pk.mapbox_token_123456789")
  expect_equal(get_access_token("gaode"), "gaode_api_key_123456789")
  expect_equal(get_access_token("baidu"), "baidu_api_key_123456789")
  
  # Test token listing
  tokens <- mapdeck_tokens()
  expect_true("mapbox" %in% names(tokens$mapdeck))
  expect_true("gaode" %in% names(tokens$mapdeck))
  expect_true("baidu" %in% names(tokens$mapdeck))
})

test_that("set_token with scopes works correctly", {
  # Clear any existing tokens
  clear_tokens()
  
  # Set tokens with different scopes
  set_token("pk.dev_token", "mapbox", "development")
  set_token("pk.prod_token", "mapbox", "production")
  
  # Test retrieval by scope
  expect_equal(get_access_token("mapbox", "development"), "pk.dev_token")
  expect_equal(get_access_token("mapbox", "production"), "pk.prod_token")
  expect_null(get_access_token("mapbox", "default"))
  
  # Test token listing
  tokens <- mapdeck_tokens()
  expect_true("mapbox" %in% names(tokens$mapdeck))
})

test_that("get_access_token backward compatibility works correctly", {
  # Clear any existing tokens
  clear_tokens()
  
  # Set token using old method (directly in options)
  options <- list(mapdeck = list(mapbox = "pk.legacy_token"))
  class(options) <- "mapdeck_api"
  options(mapdeck = options)
  
  # Should be retrievable through new method
  expect_equal(get_access_token(), "pk.legacy_token")
  expect_equal(get_access_token("mapbox"), "pk.legacy_token")
})

test_that("get_access_token prioritizes new system over legacy", {
  # Clear any existing tokens
  clear_tokens()
  
  # Set token in legacy system
  options <- list(mapdeck = list(mapbox = "pk.legacy_token"))
  class(options) <- "mapdeck_api"
  options(mapdeck = options)
  
  # Set token in new system
  token_store <- get_token_store()
  token_store$set_token("mapbox", "pk.new_token")
  
  # New system should take precedence
  expect_equal(get_access_token("mapbox"), "pk.new_token")
})

test_that("mapdeck_tokens displays all tokens correctly", {
  # Clear any existing tokens
  clear_tokens()
  
  # Set up tokens in both systems
  set_token("pk.mapbox_token", "mapbox")
  set_token("gaode_key", "gaode")
  
  # Test basic display
  tokens <- mapdeck_tokens()
  expect_true(inherits(tokens, "mapdeck_api"))
  expect_true("mapbox" %in% names(tokens$mapdeck))
  expect_true("gaode" %in% names(tokens$mapdeck))
  
  # Test with show_values
  tokens_with_values <- mapdeck_tokens(show_values = TRUE)
  expect_true(inherits(tokens_with_values, "mapdeck_api"))
  
  # Values should be masked or hidden
  mapbox_value <- tokens_with_values$mapdeck$mapbox$default
  expect_true(mapbox_value == "[SET]" || grepl("\\.\\.\\.", mapbox_value))
})

test_that("mapdeck_tokens handles empty state correctly", {
  # Clear all tokens
  clear_tokens()
  
  # Should return invisible with message
  expect_output(result <- mapdeck_tokens(), "no tokens found")
  expect_null(result)
})

test_that("clear_tokens works for all providers", {
  # Set up tokens for multiple providers
  set_token("pk.mapbox_token", "mapbox")
  set_token("gaode_key", "gaode")
  set_token("baidu_key", "baidu")
  
  # Verify tokens exist
  expect_equal(get_access_token("mapbox"), "pk.mapbox_token")
  expect_equal(get_access_token("gaode"), "gaode_key")
  expect_equal(get_access_token("baidu"), "baidu_key")
  
  # Clear all tokens
  clear_tokens()
  
  # Verify all tokens are cleared
  expect_null(get_access_token("mapbox"))
  expect_null(get_access_token("gaode"))
  expect_null(get_access_token("baidu"))
})

test_that("clear_tokens works for specific provider", {
  # Set up tokens for multiple providers
  set_token("pk.mapbox_token", "mapbox")
  set_token("gaode_key", "gaode")
  
  # Clear only Mapbox token
  clear_tokens("mapbox")
  
  # Verify only Mapbox token is cleared
  expect_null(get_access_token("mapbox"))
  expect_equal(get_access_token("gaode"), "gaode_key")
})

test_that("clear_tokens handles legacy system correctly", {
  # Set token in legacy system
  options <- list(mapdeck = list(mapbox = "pk.legacy_token"))
  class(options) <- "mapdeck_api"
  options(mapdeck = options)
  
  # Verify token exists
  expect_equal(get_access_token("mapbox"), "pk.legacy_token")
  
  # Clear tokens
  clear_tokens()
  
  # Verify legacy token is cleared
  legacy_options <- getOption("mapdeck")
  expect_true(is.na(legacy_options$mapdeck$mapbox))
})

test_that("token management input validation works correctly", {
  # Test set_token validation
  expect_error(set_token(""), "Token must be a single non-empty character string")
  expect_error(set_token("   "), "Token must be a single non-empty character string")
  expect_error(set_token(NULL), "Token must be a single non-empty character string")
  expect_error(set_token(c("a", "b")), "Token must be a single non-empty character string")
  expect_error(set_token("token", provider = NULL), "Provider must be a single character string")
  expect_error(set_token("token", scope = NULL), "Scope must be a single character string")
  
  # Test get_access_token validation
  expect_error(get_access_token(provider = NULL), "Provider must be a single character string")
  expect_error(get_access_token(scope = NULL), "Scope must be a single character string")
  
  # Test clear_tokens validation
  expect_error(clear_tokens(provider = c("a", "b")), "Provider must be a single character string or NULL")
})

test_that("token management handles environment variables correctly", {
  # Skip if running in CI to avoid environment conflicts
  testthat::skip_on_ci()
  
  # Clear existing tokens
  clear_tokens()
  
  # Set environment variable
  old_env <- Sys.getenv("MAPBOX_TOKEN", unset = NA)
  Sys.setenv("MAPBOX_TOKEN" = "pk.env_token_123")
  
  # Create new token store to trigger environment loading
  # Clear the global store first
  .token_store <<- NULL
  
  # Get token should find environment variable
  token <- get_access_token("mapbox")
  expect_equal(token, "pk.env_token_123")
  
  # Restore environment
  if (is.na(old_env)) {
    Sys.unsetenv("MAPBOX_TOKEN")
  } else {
    Sys.setenv("MAPBOX_TOKEN" = old_env)
  }
})

test_that("print.mapdeck_api handles different token formats correctly", {
  # Clear existing tokens
  clear_tokens()
  
  # Set up tokens with different scopes
  set_token("pk.default_token", "mapbox", "default")
  set_token("pk.prod_token", "mapbox", "production")
  set_token("gaode_key", "gaode")
  
  # Test printing
  tokens <- mapdeck_tokens()
  expect_output(print(tokens), "Mapdeck tokens")
  expect_output(print(tokens), "mapbox:")
  expect_output(print(tokens), "mapbox \\(production\\):")
  expect_output(print(tokens), "gaode:")
})

test_that("token system integration with legacy code works", {
  # This test ensures that existing mapdeck code continues to work
  
  # Clear tokens
  clear_tokens()
  
  # Set token using old method
  set_token("pk.integration_test_token")
  
  # Simulate how existing mapdeck code would access tokens
  token <- get_access_token()
  expect_equal(token, "pk.integration_test_token")
  
  # Verify it's also accessible through new methods
  expect_equal(get_access_token("mapbox"), "pk.integration_test_token")
  expect_equal(get_access_token("mapbox", "default"), "pk.integration_test_token")
})

# Clean up after tests
teardown({
  clear_tokens()
  .token_store <<- NULL
})