# Comprehensive Token Management Tests
# Tests for 100% token management coverage with security validation

test_that("TokenStore initialization handles all scenarios", {
  skip_if_not_installed("R6")
  
  # Test basic initialization
  store <- TokenStore$new()
  expect_s3_class(store, "TokenStore")
  expect_s3_class(store, "R6")
  expect_false(store$encrypted)
  expect_true(is.list(store$tokens))
  expect_true(is.list(store$env_vars))
  expect_equal(length(store$tokens), 0)
  
  # Test encrypted initialization
  encrypted_store <- TokenStore$new(encrypted = TRUE)
  expect_true(encrypted_store$encrypted)
  expect_true(is.list(encrypted_store$tokens))
  
  # Test initialization with custom encryption key
  if (exists("openssl")) {
    custom_store <- TokenStore$new(encrypted = TRUE, encryption_key = "custom_key")
    expect_true(custom_store$encrypted)
  }
})

test_that("TokenStore set_token validates all input types", {
  skip_if_not_installed("R6")
  
  store <- TokenStore$new()
  
  # Test valid token setting
  expect_silent(store$set_token("mapbox", "pk.test_token_123"))
  expect_equal(store$get_token("mapbox"), "pk.test_token_123")
  
  # Test with different scopes
  expect_silent(store$set_token("mapbox", "pk.prod_token", "production"))
  expect_silent(store$set_token("mapbox", "pk.dev_token", "development"))
  expect_equal(store$get_token("mapbox", "production"), "pk.prod_token")
  expect_equal(store$get_token("mapbox", "development"), "pk.dev_token")
  expect_equal(store$get_token("mapbox", "default"), "pk.test_token_123")
  
  # Test provider parameter validation
  expect_error(store$set_token(NULL, "token"), 
               "Provider must be a single character string")
  expect_error(store$set_token(NA, "token"), 
               "Provider must be a single character string")
  expect_error(store$set_token(c("a", "b"), "token"), 
               "Provider must be a single character string")
  expect_error(store$set_token(123, "token"), 
               "Provider must be a single character string")
  expect_error(store$set_token(list(), "token"), 
               "Provider must be a single character string")
  
  # Test token parameter validation
  expect_error(store$set_token("mapbox", NULL), 
               "Token must be a single non-empty character string")
  expect_error(store$set_token("mapbox", NA), 
               "Token must be a single non-empty character string")
  expect_error(store$set_token("mapbox", c("a", "b")), 
               "Token must be a single non-empty character string")
  expect_error(store$set_token("mapbox", 123), 
               "Token must be a single non-empty character string")
  expect_error(store$set_token("mapbox", ""), 
               "Token must be a single non-empty character string")
  expect_error(store$set_token("mapbox", "   "), 
               "Token must be a single non-empty character string")
  
  # Test scope parameter validation
  expect_error(store$set_token("mapbox", "token", NULL), 
               "Scope must be a single character string")
  expect_error(store$set_token("mapbox", "token", c("a", "b")), 
               "Scope must be a single character string")
  expect_error(store$set_token("mapbox", "token", 123), 
               "Scope must be a single character string")
  expect_error(store$set_token("mapbox", "token", ""), 
               "Provider name cannot be empty")
})

test_that("TokenStore get_token handles all edge cases", {
  skip_if_not_installed("R6")
  
  store <- TokenStore$new()
  
  # Test getting non-existent tokens
  expect_null(store$get_token("nonexistent"))
  expect_null(store$get_token("mapbox", "nonexistent_scope"))
  
  # Test with various provider names
  store$set_token("mapbox", "pk.mapbox_token")
  store$set_token("gaode", "gaode_api_key")
  store$set_token("baidu", "baidu_api_key")
  store$set_token("leaflet", "not_needed_but_stored")
  
  expect_equal(store$get_token("mapbox"), "pk.mapbox_token")
  expect_equal(store$get_token("gaode"), "gaode_api_key")
  expect_equal(store$get_token("baidu"), "baidu_api_key")
  expect_equal(store$get_token("leaflet"), "not_needed_but_stored")
  
  # Test parameter validation for get_token
  expect_null(store$get_token(NULL))
  expect_null(store$get_token(123))
  expect_null(store$get_token(c("a", "b")))
})

test_that("TokenStore remove_token works comprehensively", {
  skip_if_not_installed("R6")
  
  store <- TokenStore$new()
  
  # Set up test tokens
  store$set_token("mapbox", "pk.default")
  store$set_token("mapbox", "pk.prod", "production")
  store$set_token("mapbox", "pk.dev", "development")
  store$set_token("gaode", "gaode_key")
  
  # Test removing specific scope
  store$remove_token("mapbox", "production")
  expect_null(store$get_token("mapbox", "production"))
  expect_equal(store$get_token("mapbox", "default"), "pk.default")
  expect_equal(store$get_token("mapbox", "development"), "pk.dev")
  expect_equal(store$get_token("gaode"), "gaode_key")
  
  # Test removing default scope
  store$remove_token("mapbox", "default")
  expect_null(store$get_token("mapbox", "default"))
  expect_equal(store$get_token("mapbox", "development"), "pk.dev")
  
  # Test removing entire provider
  store$remove_token("gaode")
  expect_null(store$get_token("gaode"))
  
  # Test removing non-existent tokens (should not error)
  expect_silent(store$remove_token("nonexistent"))
  expect_silent(store$remove_token("mapbox", "nonexistent_scope"))
  
  # Test method chaining
  result <- store$remove_token("mapbox", "development")
  expect_identical(result, store)
})

test_that("TokenStore list_tokens provides comprehensive information", {
  skip_if_not_installed("R6")
  
  store <- TokenStore$new()
  
  # Test empty store
  tokens <- store$list_tokens()
  expect_true(is.list(tokens))
  expect_equal(length(tokens), 0)
  
  # Set up various tokens
  store$set_token("mapbox", "pk.very_long_token_that_should_be_masked_properly_123456789")
  store$set_token("mapbox", "pk.prod_token_also_long_enough", "production")
  store$set_token("gaode", "short")
  store$set_token("baidu", "baidu_api_key_medium_length")
  
  # Test without showing values
  tokens <- store$list_tokens(show_values = FALSE)
  expect_equal(tokens$mapbox$default, "[SET]")
  expect_equal(tokens$mapbox$production, "[SET]")
  expect_equal(tokens$gaode$default, "[SET]")
  expect_equal(tokens$baidu$default, "[SET]")
  
  # Test with showing values (masked)
  tokens <- store$list_tokens(show_values = TRUE)
  
  # Long tokens should be masked showing first/last characters
  expect_true(grepl("^pk\\.very_.*erly_123456789$", tokens$mapbox$default))
  expect_true(grepl("^pk\\.prod_.*enough$", tokens$mapbox$production))
  expect_true(grepl("^baidu_.*length$", tokens$baidu$default))
  
  # Short tokens should be hidden
  expect_equal(tokens$gaode$default, "[HIDDEN]")
  
  # Test parameter validation
  expect_error(store$list_tokens(show_values = "invalid"), 
               "show_values must be logical")
  expect_error(store$list_tokens(show_values = c(TRUE, FALSE)), 
               "show_values must be a single logical value")
})

test_that("TokenStore clear_all_tokens works completely", {
  skip_if_not_installed("R6")
  
  store <- TokenStore$new()
  
  # Set up multiple tokens
  store$set_token("mapbox", "pk.token1")
  store$set_token("mapbox", "pk.token2", "prod")
  store$set_token("gaode", "gaode_key")
  store$set_token("baidu", "baidu_key")
  
  # Verify tokens exist
  expect_equal(length(store$list_tokens()), 3)
  
  # Clear all tokens
  result <- store$clear_all_tokens()
  expect_identical(result, store)  # Test method chaining
  
  # Verify all tokens are cleared
  expect_equal(length(store$list_tokens()), 0)
  expect_null(store$get_token("mapbox"))
  expect_null(store$get_token("mapbox", "prod"))
  expect_null(store$get_token("gaode"))
  expect_null(store$get_token("baidu"))
})

test_that("TokenStore environment variable loading works correctly", {
  skip_if_not_installed("R6")
  skip_on_ci()  # Skip on CI to avoid environment conflicts
  
  # Save original environment
  original_mapbox <- Sys.getenv("MAPBOX_TOKEN", unset = NA)
  original_mapbox_key <- Sys.getenv("MAPBOX_KEY", unset = NA)
  original_gaode <- Sys.getenv("GAODE_API_KEY", unset = NA)
  original_baidu <- Sys.getenv("BAIDU_API_KEY", unset = NA)
  
  # Set test environment variables
  Sys.setenv("MAPBOX_TOKEN" = "pk.env_mapbox_token")
  Sys.setenv("MAPBOX_KEY" = "pk.env_mapbox_key")  # Alternative name
  Sys.setenv("GAODE_API_KEY" = "env_gaode_key")
  Sys.setenv("BAIDU_API_KEY" = "env_baidu_key")
  
  # Create new store to trigger environment loading
  store <- TokenStore$new()
  
  # Test that environment variables are loaded
  expect_equal(store$get_token("mapbox"), "pk.env_mapbox_token")
  expect_equal(store$get_token("gaode"), "env_gaode_key")
  expect_equal(store$get_token("baidu"), "env_baidu_key")
  
  # Test precedence (MAPBOX_TOKEN should take precedence over MAPBOX_KEY)
  Sys.unsetenv("MAPBOX_TOKEN")
  store2 <- TokenStore$new()
  expect_equal(store2$get_token("mapbox"), "pk.env_mapbox_key")
  
  # Restore original environment
  if (is.na(original_mapbox)) {
    Sys.unsetenv("MAPBOX_TOKEN")
  } else {
    Sys.setenv("MAPBOX_TOKEN" = original_mapbox)
  }
  
  if (is.na(original_mapbox_key)) {
    Sys.unsetenv("MAPBOX_KEY")
  } else {
    Sys.setenv("MAPBOX_KEY" = original_mapbox_key)
  }
  
  if (is.na(original_gaode)) {
    Sys.unsetenv("GAODE_API_KEY")
  } else {
    Sys.setenv("GAODE_API_KEY" = original_gaode)
  }
  
  if (is.na(original_baidu)) {
    Sys.unsetenv("BAIDU_API_KEY")
  } else {
    Sys.setenv("BAIDU_API_KEY" = original_baidu)
  }
})

test_that("TokenStore token validation is comprehensive", {
  skip_if_not_installed("R6")
  
  store <- TokenStore$new()
  
  # Test Mapbox token validation
  expect_true(store$validate_token("mapbox", "pk.valid_mapbox_token_123456789"))
  expect_true(store$validate_token("mapbox", "sk.valid_secret_key_123456789"))
  expect_false(store$validate_token("mapbox", "invalid_token"))
  expect_false(store$validate_token("mapbox", "pk.short"))
  expect_false(store$validate_token("mapbox", "sk.short"))
  expect_false(store$validate_token("mapbox", "wrong_prefix_token_123456789"))
  
  # Test Gaode token validation
  expect_true(store$validate_token("gaode", "valid_gaode_api_key_123456789"))
  expect_false(store$validate_token("gaode", "short"))
  expect_false(store$validate_token("gaode", ""))
  
  # Test Baidu token validation
  expect_true(store$validate_token("baidu", "valid_baidu_api_key_123456789"))
  expect_false(store$validate_token("baidu", "short"))
  expect_false(store$validate_token("baidu", ""))
  
  # Test providers that don't require authentication
  expect_true(store$validate_token("leaflet", "any_value"))
  expect_true(store$validate_token("leaflet", ""))
  expect_true(store$validate_token("openlayers", "any_value"))
  expect_true(store$validate_token("openlayers", ""))
  
  # Test validation with stored tokens
  store$set_token("mapbox", "pk.stored_valid_token_123456789")
  expect_true(store$validate_token("mapbox"))
  
  store$set_token("mapbox", "invalid")
  expect_false(store$validate_token("mapbox"))
  
  # Test with non-existent provider
  expect_false(store$validate_token("nonexistent", "token"))
  
  # Test parameter validation
  expect_false(store$validate_token(NULL, "token"))
  expect_false(store$validate_token(123, "token"))
  expect_false(store$validate_token("mapbox", NULL))
  expect_false(store$validate_token("mapbox", 123))
})

test_that("TokenStore encryption works securely", {
  skip_if_not_installed("R6")
  
  # Test encrypted storage
  encrypted_store <- TokenStore$new(encrypted = TRUE)
  
  # Set tokens
  encrypted_store$set_token("mapbox", "pk.secret_token_123456789")
  encrypted_store$set_token("gaode", "secret_gaode_key_987654321")
  
  # Verify tokens can be retrieved correctly
  expect_equal(encrypted_store$get_token("mapbox"), "pk.secret_token_123456789")
  expect_equal(encrypted_store$get_token("gaode"), "secret_gaode_key_987654321")
  
  # Verify tokens are actually encrypted in storage
  raw_mapbox <- encrypted_store$tokens$mapbox$default
  raw_gaode <- encrypted_store$tokens$gaode$default
  
  expect_false(raw_mapbox == "pk.secret_token_123456789")
  expect_false(raw_gaode == "secret_gaode_key_987654321")
  expect_true(nchar(raw_mapbox) > nchar("pk.secret_token_123456789"))
  expect_true(nchar(raw_gaode) > nchar("secret_gaode_key_987654321"))
  
  # Test that different instances with same key can decrypt
  if (exists("openssl")) {
    key <- "test_encryption_key"
    store1 <- TokenStore$new(encrypted = TRUE, encryption_key = key)
    store2 <- TokenStore$new(encrypted = TRUE, encryption_key = key)
    
    store1$set_token("test", "test_token_123")
    # Manually copy encrypted token to store2
    store2$tokens <- store1$tokens
    
    expect_equal(store2$get_token("test"), "test_token_123")
  }
})

test_that("TokenStore requires_authentication works correctly", {
  skip_if_not_installed("R6")
  
  store <- TokenStore$new()
  
  # Test providers that require authentication
  expect_true(store$requires_authentication("mapbox"))
  expect_true(store$requires_authentication("gaode"))
  expect_true(store$requires_authentication("baidu"))
  
  # Test providers that don't require authentication
  expect_false(store$requires_authentication("leaflet"))
  expect_false(store$requires_authentication("openlayers"))
  
  # Test unknown provider (should default to requiring auth)
  expect_true(store$requires_authentication("unknown_provider"))
  
  # Test parameter validation
  expect_true(store$requires_authentication(NULL))  # Safe default
  expect_true(store$requires_authentication(123))   # Safe default
})

test_that("Global token store functions work correctly", {
  skip_if_not_installed("R6")
  
  # Test getting global store
  store1 <- get_token_store()
  expect_s3_class(store1, "TokenStore")
  
  # Test that subsequent calls return the same instance
  store2 <- get_token_store()
  expect_identical(store1, store2)
  
  # Test that changes persist across calls
  store1$set_token("test_global", "test_token")
  expect_equal(store2$get_token("test_global"), "test_token")
  
  # Clean up
  store1$remove_token("test_global")
})

test_that("TokenStore handles edge cases and stress conditions", {
  skip_if_not_installed("R6")
  
  store <- TokenStore$new()
  
  # Test with very long tokens
  long_token <- paste0("pk.", paste(rep("a", 1000), collapse = ""))
  expect_silent(store$set_token("mapbox", long_token))
  expect_equal(store$get_token("mapbox"), long_token)
  
  # Test with special characters in tokens
  special_token <- "pk.token-with_special.chars123!@#"
  expect_silent(store$set_token("mapbox", special_token))
  expect_equal(store$get_token("mapbox"), special_token)
  
  # Test with many providers and scopes
  providers <- c("mapbox", "gaode", "baidu", "custom1", "custom2")
  scopes <- c("default", "production", "development", "testing")
  
  for (provider in providers) {
    for (scope in scopes) {
      token <- paste0(provider, "_", scope, "_token")
      store$set_token(provider, token, scope)
    }
  }
  
  # Verify all tokens are stored correctly
  for (provider in providers) {
    for (scope in scopes) {
      expected_token <- paste0(provider, "_", scope, "_token")
      expect_equal(store$get_token(provider, scope), expected_token)
    }
  }
  
  # Test listing many tokens
  all_tokens <- store$list_tokens()
  expect_equal(length(all_tokens), length(providers))
  
  for (provider in providers) {
    expect_equal(length(all_tokens[[provider]]), length(scopes))
  }
})

test_that("TokenStore security features work correctly", {
  skip_if_not_installed("R6")
  
  store <- TokenStore$new()
  
  # Test token masking in list_tokens
  store$set_token("mapbox", "pk.this_is_a_very_long_token_that_should_be_masked")
  store$set_token("gaode", "short")
  
  masked_tokens <- store$list_tokens(show_values = TRUE)
  
  # Long tokens should be masked
  expect_true(grepl("^pk\\.this_.*masked$", masked_tokens$mapbox$default))
  expect_false(grepl("very_long_token", masked_tokens$mapbox$default))
  
  # Short tokens should be completely hidden
  expect_equal(masked_tokens$gaode$default, "[HIDDEN]")
  
  # Test that raw token storage is not directly accessible
  expect_false(exists("tokens", envir = globalenv()))
  
  # Test that encryption key is not exposed
  encrypted_store <- TokenStore$new(encrypted = TRUE)
  expect_false(exists("encryption_key", envir = encrypted_store))
})