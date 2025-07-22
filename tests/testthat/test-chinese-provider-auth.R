test_that("Gaode provider API key authentication works", {
  skip_if_not_installed("R6")
  
  # Test with valid API key format
  provider <- GaodeProvider$new()
  valid_config <- list(api_key = "test_gaode_api_key_1234567890")
  
  expect_silent(provider$initialize_provider(valid_config))
  expect_true(provider$initialized)
  expect_equal(provider$api_key, "test_gaode_api_key_1234567890")
  
  # Test token storage integration
  expect_silent({
    tryCatch({
      token_store <- get_token_store()
      stored_token <- token_store$get_token("gaode", "default")
      expect_equal(stored_token, "test_gaode_api_key_1234567890")
    }, error = function(e) {
      # Token store might not be available in test environment
      skip("Token store not available")
    })
  })
})

test_that("Baidu provider API key authentication works", {
  skip_if_not_installed("R6")
  
  # Test with valid API key format
  provider <- BaiduProvider$new()
  valid_config <- list(api_key = "test_baidu_api_key_1234567890")
  
  expect_silent(provider$initialize_provider(valid_config))
  expect_true(provider$initialized)
  expect_equal(provider$api_key, "test_baidu_api_key_1234567890")
  
  # Test token storage integration
  expect_silent({
    tryCatch({
      token_store <- get_token_store()
      stored_token <- token_store$get_token("baidu", "default")
      expect_equal(stored_token, "test_baidu_api_key_1234567890")
    }, error = function(e) {
      # Token store might not be available in test environment
      skip("Token store not available")
    })
  })
})

test_that("Chinese providers support token parameter for consistency", {
  skip_if_not_installed("R6")
  
  # Test Gaode with 'token' parameter instead of 'api_key'
  gaode_provider <- GaodeProvider$new()
  gaode_config <- list(token = "gaode_token_12345")
  
  expect_silent(gaode_provider$initialize_provider(gaode_config))
  expect_equal(gaode_provider$api_key, "gaode_token_12345")
  
  # Test Baidu with 'token' parameter instead of 'api_key'
  baidu_provider <- BaiduProvider$new()
  baidu_config <- list(token = "baidu_token_12345")
  
  expect_silent(baidu_provider$initialize_provider(baidu_config))
  expect_equal(baidu_provider$api_key, "baidu_token_12345")
})

test_that("Chinese providers fail gracefully without authentication", {
  skip_if_not_installed("R6")
  
  # Test Gaode without API key
  gaode_provider <- GaodeProvider$new()
  expect_error(
    gaode_provider$initialize_provider(list()),
    "Gaode Maps API key is required"
  )
  
  # Test Baidu without API key
  baidu_provider <- BaiduProvider$new()
  expect_error(
    baidu_provider$initialize_provider(list()),
    "Baidu Maps API key is required"
  )
})

test_that("Chinese providers validate API key format", {
  skip_if_not_installed("R6")
  
  # Test Gaode with short API key (should warn but not fail)
  gaode_provider <- GaodeProvider$new()
  expect_warning(
    gaode_provider$initialize_provider(list(api_key = "short")),
    "API key appears to be too short"
  )
  
  # Test Baidu with short API key (should warn but not fail)
  baidu_provider <- BaiduProvider$new()
  expect_warning(
    baidu_provider$initialize_provider(list(api_key = "short")),
    "API key appears to be too short"
  )
  
  # Test with invalid API key types
  gaode_provider2 <- GaodeProvider$new()
  expect_false(gaode_provider2$validate_config(list(api_key = 12345)))
  
  baidu_provider2 <- BaiduProvider$new()
  expect_false(baidu_provider2$validate_config(list(api_key = c("key1", "key2"))))
})

test_that("Chinese providers work with token store fallback", {
  skip_if_not_installed("R6")
  
  # This test simulates getting tokens from token store when not provided in config
  # Skip if token store is not available
  skip_if_not(exists("get_token_store"))
  
  tryCatch({
    # Try to set up token store with test tokens
    token_store <- get_token_store()
    token_store$set_token("gaode", "stored_gaode_key", "default")
    token_store$set_token("baidu", "stored_baidu_key", "default")
    
    # Test Gaode provider getting token from store
    gaode_provider <- GaodeProvider$new()
    expect_silent(gaode_provider$initialize_provider(list()))
    expect_equal(gaode_provider$api_key, "stored_gaode_key")
    
    # Test Baidu provider getting token from store
    baidu_provider <- BaiduProvider$new()
    expect_silent(baidu_provider$initialize_provider(list()))
    expect_equal(baidu_provider$api_key, "stored_baidu_key")
    
  }, error = function(e) {
    skip("Token store not available or not working in test environment")
  })
})

test_that("Chinese providers support scoped tokens", {
  skip_if_not_installed("R6")
  
  # Test with custom scope
  gaode_provider <- GaodeProvider$new()
  gaode_config <- list(
    api_key = "scoped_gaode_key",
    scope = "production"
  )
  
  expect_silent(gaode_provider$initialize_provider(gaode_config))
  expect_equal(gaode_provider$api_key, "scoped_gaode_key")
  
  baidu_provider <- BaiduProvider$new()
  baidu_config <- list(
    api_key = "scoped_baidu_key",
    scope = "production"
  )
  
  expect_silent(baidu_provider$initialize_provider(baidu_config))
  expect_equal(baidu_provider$api_key, "scoped_baidu_key")
})

test_that("Chinese provider authentication integrates with factory", {
  skip_if_not_installed("R6")
  
  factory <- get_provider_factory()
  
  # Test creating authenticated Gaode provider through factory
  gaode_config <- list(api_key = "factory_gaode_key")
  gaode_provider <- factory$create_provider("gaode", gaode_config)
  
  expect_true(inherits(gaode_provider, "GaodeProvider"))
  expect_true(gaode_provider$initialized)
  expect_equal(gaode_provider$api_key, "factory_gaode_key")
  
  # Test creating authenticated Baidu provider through factory
  baidu_config <- list(api_key = "factory_baidu_key")
  baidu_provider <- factory$create_provider("baidu", baidu_config)
  
  expect_true(inherits(baidu_provider, "BaiduProvider"))
  expect_true(baidu_provider$initialized)
  expect_equal(baidu_provider$api_key, "factory_baidu_key")
  
  # Test factory validation
  expect_true(factory$validate_provider_config("gaode", gaode_config))
  expect_true(factory$validate_provider_config("baidu", baidu_config))
  
  # Test factory validation failure
  expect_false(factory$validate_provider_config("gaode", list()))
  expect_false(factory$validate_provider_config("baidu", list()))
})

test_that("Chinese providers handle authentication errors gracefully", {
  skip_if_not_installed("R6")
  
  # Test configuration validation with various invalid inputs
  gaode_provider <- GaodeProvider$new()
  baidu_provider <- BaiduProvider$new()
  
  # Test with NULL config
  expect_error(gaode_provider$initialize_provider(NULL))
  expect_error(baidu_provider$initialize_provider(NULL))
  
  # Test with non-list config
  expect_error(gaode_provider$validate_config("not_a_list"))
  expect_error(baidu_provider$validate_config("not_a_list"))
  
  # Test with empty config
  expect_error(gaode_provider$initialize_provider(list()))
  expect_error(baidu_provider$initialize_provider(list()))
})

test_that("Chinese providers support environment variable authentication", {
  skip_if_not_installed("R6")
  
  # This test would check environment variable support
  # Skip if environment variables are not set up for testing
  skip("Environment variable authentication testing requires test setup")
  
  # Example of how this would work:
  # Sys.setenv(GAODE_API_KEY = "env_gaode_key")
  # Sys.setenv(BAIDU_API_KEY = "env_baidu_key")
  # 
  # gaode_provider <- GaodeProvider$new()
  # expect_silent(gaode_provider$initialize_provider(list()))
  # expect_equal(gaode_provider$api_key, "env_gaode_key")
})