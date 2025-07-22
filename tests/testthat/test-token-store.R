context("TokenStore")

# Test TokenStore class functionality
test_that("TokenStore initialization works correctly", {
  # Test basic initialization
  store <- TokenStore$new()
  expect_true(inherits(store, "TokenStore"))
  expect_true(inherits(store, "R6"))
  expect_false(store$encrypted)
  expect_true(is.list(store$tokens))
  expect_true(is.list(store$env_vars))
  
  # Test encrypted initialization
  encrypted_store <- TokenStore$new(encrypted = TRUE)
  expect_true(encrypted_store$encrypted)
})

test_that("TokenStore set_token works correctly", {
  store <- TokenStore$new()
  
  # Test basic token setting
  store$set_token("mapbox", "pk.test_token_123")
  expect_equal(store$get_token("mapbox"), "pk.test_token_123")
  
  # Test token setting with scope
  store$set_token("mapbox", "pk.scoped_token", "production")
  expect_equal(store$get_token("mapbox", "production"), "pk.scoped_token")
  expect_equal(store$get_token("mapbox", "default"), "pk.test_token_123")
  
  # Test multiple providers
  store$set_token("gaode", "gaode_api_key_123")
  expect_equal(store$get_token("gaode"), "gaode_api_key_123")
  expect_equal(store$get_token("mapbox"), "pk.test_token_123")
})

test_that("TokenStore input validation works correctly", {
  store <- TokenStore$new()
  
  # Test invalid provider parameter
  expect_error(store$set_token(NULL, "token"), "Provider must be a single character string")
  expect_error(store$set_token(c("a", "b"), "token"), "Provider must be a single character string")
  expect_error(store$set_token(123, "token"), "Provider must be a single character string")
  
  # Test invalid token parameter
  expect_error(store$set_token("mapbox", NULL), "Token must be a single non-empty character string")
  expect_error(store$set_token("mapbox", c("a", "b")), "Token must be a single non-empty character string")
  expect_error(store$set_token("mapbox", 123), "Token must be a single non-empty character string")
  
  # Test invalid scope parameter
  expect_error(store$set_token("mapbox", "token", NULL), "Scope must be a single character string")
  expect_error(store$set_token("mapbox", "token", c("a", "b")), "Scope must be a single character string")
  expect_error(store$set_token("mapbox", "token", 123), "Scope must be a single character string")
})

test_that("TokenStore get_token works correctly", {
  store <- TokenStore$new()
  
  # Test getting non-existent token
  expect_null(store$get_token("mapbox"))
  expect_null(store$get_token("mapbox", "production"))
  
  # Test getting existing token
  store$set_token("mapbox", "pk.test_token")
  expect_equal(store$get_token("mapbox"), "pk.test_token")
  
  # Test getting token with different scopes
  store$set_token("mapbox", "pk.prod_token", "production")
  expect_equal(store$get_token("mapbox", "production"), "pk.prod_token")
  expect_equal(store$get_token("mapbox", "default"), "pk.test_token")
  expect_null(store$get_token("mapbox", "staging"))
})

test_that("TokenStore remove_token works correctly", {
  store <- TokenStore$new()
  
  # Set up test tokens
  store$set_token("mapbox", "pk.default_token")
  store$set_token("mapbox", "pk.prod_token", "production")
  store$set_token("gaode", "gaode_key")
  
  # Test removing specific scope
  store$remove_token("mapbox", "production")
  expect_null(store$get_token("mapbox", "production"))
  expect_equal(store$get_token("mapbox", "default"), "pk.default_token")
  expect_equal(store$get_token("gaode"), "gaode_key")
  
  # Test removing default scope
  store$remove_token("mapbox", "default")
  expect_null(store$get_token("mapbox"))
  expect_equal(store$get_token("gaode"), "gaode_key")
  
  # Test removing non-existent token (should not error)
  expect_silent(store$remove_token("nonexistent", "default"))
})

test_that("TokenStore list_tokens works correctly", {
  store <- TokenStore$new()
  
  # Test empty store
  tokens <- store$list_tokens()
  expect_true(is.list(tokens))
  expect_equal(length(tokens), 0)
  
  # Test with tokens
  store$set_token("mapbox", "pk.very_long_token_that_should_be_masked_properly")
  store$set_token("mapbox", "pk.prod_token", "production")
  store$set_token("gaode", "short")
  
  # Test without showing values
  tokens <- store$list_tokens(show_values = FALSE)
  expect_equal(tokens$mapbox$default, "[SET]")
  expect_equal(tokens$mapbox$production, "[SET]")
  expect_equal(tokens$gaode$default, "[SET]")
  
  # Test with showing values (masked)
  tokens <- store$list_tokens(show_values = TRUE)
  expect_true(grepl("^pk\\.very_.*erly$", tokens$mapbox$default))
  expect_equal(tokens$gaode$default, "[HIDDEN]")  # Short tokens are hidden
})

test_that("TokenStore clear_all_tokens works correctly", {
  store <- TokenStore$new()
  
  # Set up test tokens
  store$set_token("mapbox", "pk.token1")
  store$set_token("gaode", "gaode_key")
  
  # Verify tokens exist
  expect_equal(store$get_token("mapbox"), "pk.token1")
  expect_equal(store$get_token("gaode"), "gaode_key")
  
  # Clear all tokens
  store$clear_all_tokens()
  
  # Verify tokens are cleared
  expect_null(store$get_token("mapbox"))
  expect_null(store$get_token("gaode"))
  expect_equal(length(store$list_tokens()), 0)
})

test_that("TokenStore environment variable loading works correctly", {
  # Skip if running in CI to avoid environment conflicts
  testthat::skip_on_ci()
  
  store <- TokenStore$new()
  
  # Clear any existing tokens
  store$clear_all_tokens()
  
  # Test Mapbox environment variables
  old_env <- Sys.getenv("MAPBOX_TOKEN", unset = NA)
  Sys.setenv("MAPBOX_TOKEN" = "pk.env_token")
  
  # Create new store to trigger environment loading
  new_store <- TokenStore$new()
  expect_equal(new_store$get_token("mapbox"), "pk.env_token")
  
  # Restore environment
  if (is.na(old_env)) {
    Sys.unsetenv("MAPBOX_TOKEN")
  } else {
    Sys.setenv("MAPBOX_TOKEN" = old_env)
  }
})

test_that("TokenStore token validation works correctly", {
  store <- TokenStore$new()
  
  # Test Mapbox token validation
  expect_true(store$validate_token("mapbox", "pk.valid_mapbox_token_123"))
  expect_true(store$validate_token("mapbox", "sk.valid_secret_key_456"))
  expect_false(store$validate_token("mapbox", "invalid_token"))
  expect_false(store$validate_token("mapbox", "pk.short"))
  
  # Test Gaode token validation
  expect_true(store$validate_token("gaode", "valid_gaode_api_key_123456"))
  expect_false(store$validate_token("gaode", "short"))
  
  # Test Baidu token validation
  expect_true(store$validate_token("baidu", "valid_baidu_api_key_123456"))
  expect_false(store$validate_token("baidu", "short"))
  
  # Test providers that don't require authentication
  expect_true(store$validate_token("leaflet", "any_value"))
  expect_true(store$validate_token("openlayers", "any_value"))
  
  # Test validation with stored token
  store$set_token("mapbox", "pk.stored_token_123456789")
  expect_true(store$validate_token("mapbox"))
  
  store$set_token("mapbox", "invalid")
  expect_false(store$validate_token("mapbox"))
})

test_that("TokenStore encryption works correctly", {
  # Test encrypted storage
  encrypted_store <- TokenStore$new(encrypted = TRUE)
  
  # Set a token
  encrypted_store$set_token("mapbox", "pk.secret_token_123")
  
  # Verify token can be retrieved correctly
  expect_equal(encrypted_store$get_token("mapbox"), "pk.secret_token_123")
  
  # Verify token is actually encrypted in storage
  raw_token <- encrypted_store$tokens$mapbox$default
  expect_false(raw_token == "pk.secret_token_123")
  expect_true(nchar(raw_token) > nchar("pk.secret_token_123"))
})

test_that("TokenStore requires_authentication works correctly", {
  store <- TokenStore$new()
  
  # Test providers that require authentication
  expect_true(store$requires_authentication("mapbox"))
  expect_true(store$requires_authentication("gaode"))
  expect_true(store$requires_authentication("baidu"))
  
  # Test providers that don't require authentication
  expect_false(store$requires_authentication("leaflet"))
  expect_false(store$requires_authentication("openlayers"))
})

test_that("get_token_store global function works correctly", {
  # Test getting global store
  store1 <- get_token_store()
  expect_true(inherits(store1, "TokenStore"))
  
  # Test that subsequent calls return the same instance
  store2 <- get_token_store()
  expect_identical(store1, store2)
  
  # Test setting a token through global store
  store1$set_token("mapbox", "pk.global_token")
  expect_equal(store2$get_token("mapbox"), "pk.global_token")
})

test_that("TokenStore handles edge cases correctly", {
  store <- TokenStore$new()
  
  # Test empty string token
  expect_error(store$set_token("mapbox", ""), "Token must be a single non-empty character string")
  
  # Test empty string provider
  expect_error(store$set_token("", "token"), "Provider name cannot be empty")
  
  # Test whitespace-only strings
  expect_error(store$set_token("mapbox", "   "), "Token must be a single non-empty character string")
  
  # Test very long tokens
  long_token <- paste0("pk.", paste(rep("a", 1000), collapse = ""))
  expect_silent(store$set_token("mapbox", long_token))
  expect_equal(store$get_token("mapbox"), long_token)
})

test_that("TokenStore method chaining works correctly", {
  store <- TokenStore$new()
  
  # Test method chaining
  result <- store$set_token("mapbox", "pk.token1")$set_token("gaode", "gaode_key")
  expect_identical(result, store)
  expect_equal(store$get_token("mapbox"), "pk.token1")
  expect_equal(store$get_token("gaode"), "gaode_key")
  
  # Test chaining with remove
  result <- store$remove_token("mapbox")$clear_all_tokens()
  expect_identical(result, store)
  expect_null(store$get_token("gaode"))
})