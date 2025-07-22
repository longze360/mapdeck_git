# Comprehensive All Providers Tests
# Tests for 100% provider interface coverage across all implementations

test_that("All providers implement IMapProvider interface correctly", {
  skip_if_not_installed("R6")
  
  # List of all provider classes that should exist
  provider_classes <- list(
    "MapboxProvider",
    "LeafletProvider", 
    "OpenLayersProvider",
    "GaodeProvider",
    "BaiduProvider"
  )
  
  for (provider_name in provider_classes) {
    # Test that provider class exists
    expect_true(exists(provider_name), 
                info = paste("Provider class", provider_name, "should exist"))
    
    if (exists(provider_name)) {
      provider_class <- get(provider_name)
      
      # Test interface compliance
      expect_true(validate_provider_interface(provider_class),
                  info = paste(provider_name, "should implement IMapProvider interface"))
      
      # Test that provider can be instantiated
      provider <- provider_class$new()
      expect_true(inherits(provider, "IMapProvider"),
                  info = paste(provider_name, "should inherit from IMapProvider"))
      expect_false(provider$initialized,
                   info = paste(provider_name, "should not be initialized by default"))
    }
  }
})

test_that("MapboxProvider comprehensive interface coverage", {
  skip_if_not_installed("R6")
  skip_if_not(exists("MapboxProvider"), "MapboxProvider not available")
  
  provider <- MapboxProvider$new()
  
  # Test provider identification
  expect_equal(provider$provider_name, "mapbox")
  expect_false(provider$initialized)
  
  # Test initialization with valid token
  config <- list(
    token = "pk.test_mapbox_token_123456789",
    style = "mapbox://styles/mapbox/streets-v11",
    zoom = 10,
    location = c(-74.0, 40.7)
  )
  
  expect_silent(provider$initialize_provider(config))
  expect_true(provider$initialized)
  expect_equal(provider$access_token, "pk.test_mapbox_token_123456789")
  
  # Test all interface methods
  test_provider_interface_compliance(MapboxProvider, config)
  
  # Test Mapbox-specific features
  styles <- provider$get_available_styles()
  expect_true(is.character(styles))
  expect_true(length(styles) > 0)
  expect_true("mapbox://styles/mapbox/streets-v11" %in% styles)
  
  # Test view management
  expect_silent(provider$set_view(-74.0, 40.7, 12, 30, 45))
  
  # Test layer management
  test_layer <- list(id = "test_layer", type = "ScatterplotLayer", data = data.frame(x = 1, y = 1))
  expect_silent(provider$add_layer(test_layer))
  expect_true("test_layer" %in% names(provider$layers))
  expect_silent(provider$remove_layer("test_layer"))
  expect_false("test_layer" %in% names(provider$layers))
  
  # Test configuration validation
  expect_true(provider$validate_config(list(token = "pk.valid_token_123456789")))
  expect_warning(provider$validate_config(list(token = "invalid_token")))
  
  # Test destruction
  provider$destroy()
  expect_false(provider$initialized)
})

test_that("LeafletProvider comprehensive interface coverage", {
  skip_if_not_installed("R6")
  skip_if_not(exists("LeafletProvider"), "LeafletProvider not available")
  
  provider <- LeafletProvider$new()
  
  # Test provider identification
  expect_equal(provider$provider_name, "leaflet")
  expect_false(provider$initialized)
  
  # Test initialization (no token required)
  config <- list(
    tile_provider = "OpenStreetMap",
    zoom = 10,
    center = c(-74.0, 40.7)
  )
  
  expect_silent(provider$initialize_provider(config))
  expect_true(provider$initialized)
  
  # Test all interface methods
  test_provider_interface_compliance(LeafletProvider, config)
  
  # Test Leaflet-specific features
  styles <- provider$get_available_styles()
  expect_true(is.character(styles))
  expect_true(length(styles) > 0)
  expect_true("OpenStreetMap" %in% styles)
  expect_true("CartoDB.Positron" %in% styles)
  
  # Test style categories
  basic_styles <- provider$get_available_styles("basic")
  expect_true("OpenStreetMap" %in% basic_styles)
  
  # Test view management (should warn about unsupported features)
  expect_warning(provider$set_view(-74.0, 40.7, 10, pitch = 30), 
                 "does not support pitch")
  expect_warning(provider$set_view(-74.0, 40.7, 10, bearing = 45), 
                 "does not support bearing")
  
  # Test tile provider configuration
  tile_config <- provider$get_tile_provider_config("OpenStreetMap")
  expect_true(is.list(tile_config))
  expect_true("url" %in% names(tile_config))
  expect_true("attribution" %in% names(tile_config))
  
  # Test custom tile layer creation
  custom_layer <- provider$create_custom_tile_layer(
    url = "https://example.com/{z}/{x}/{y}.png",
    attribution = "Custom tiles"
  )
  expect_equal(custom_layer$url, "https://example.com/{z}/{x}/{y}.png")
  
  # Test destruction
  provider$destroy()
  expect_false(provider$initialized)
})

test_that("OpenLayersProvider comprehensive interface coverage", {
  skip_if_not_installed("R6")
  skip_if_not(exists("OpenLayersProvider"), "OpenLayersProvider not available")
  
  provider <- OpenLayersProvider$new()
  
  # Test provider identification
  expect_equal(provider$provider_name, "openlayers")
  expect_false(provider$initialized)
  
  # Test initialization
  config <- list(
    source_type = "OSM",
    zoom = 10,
    center = c(-74.0, 40.7)
  )
  
  expect_silent(provider$initialize_provider(config))
  expect_true(provider$initialized)
  
  # Test all interface methods
  test_provider_interface_compliance(OpenLayersProvider, config)
  
  # Test OpenLayers-specific features
  styles <- provider$get_available_styles()
  expect_true(is.character(styles))
  expect_true(length(styles) > 0)
  
  # Test source configuration
  if (exists("get_openlayers_source_config", envir = provider)) {
    source_config <- provider$get_openlayers_source_config("OSM")
    expect_true(is.list(source_config))
  }
  
  # Test destruction
  provider$destroy()
  expect_false(provider$initialized)
})

test_that("GaodeProvider comprehensive interface coverage", {
  skip_if_not_installed("R6")
  skip_if_not(exists("GaodeProvider"), "GaodeProvider not available")
  
  provider <- GaodeProvider$new()
  
  # Test provider identification
  expect_equal(provider$provider_name, "gaode")
  expect_false(provider$initialized)
  
  # Test initialization with API key
  config <- list(
    api_key = "test_gaode_api_key_123456789",
    style = "amap://styles/normal",
    zoom = 12,
    location = c(116.397, 39.909)
  )
  
  expect_silent(provider$initialize_provider(config))
  expect_true(provider$initialized)
  expect_equal(provider$api_key, "test_gaode_api_key_123456789")
  
  # Test initialization failure without API key
  provider2 <- GaodeProvider$new()
  expect_error(provider2$initialize_provider(list()),
               "Gaode Maps API key is required")
  
  # Test all interface methods
  test_provider_interface_compliance(GaodeProvider, config)
  
  # Test Gaode-specific features
  styles <- provider$get_available_styles()
  expect_true(is.character(styles))
  expect_true(length(styles) > 0)
  expect_true("amap://styles/normal" %in% styles)
  
  # Test coordinate transformation
  wgs84_coords <- c(116.3974, 39.9093)
  gcj02_coords <- provider$transform_coordinates(wgs84_coords, "WGS84", "GCJ02")
  expect_true(is.numeric(gcj02_coords))
  expect_equal(length(gcj02_coords), 2)
  expect_false(identical(wgs84_coords, gcj02_coords))
  
  # Test coordinate system detection
  test_data <- data.frame(longitude = c(116.3974), latitude = c(39.9093))
  detected_crs <- provider$detect_coordinate_system(test_data)
  expect_true(detected_crs %in% c("WGS84", "GCJ02", "BD09"))
  
  # Test data preparation
  suppressMessages({
    prepared_data <- provider$prepare_data(test_data, auto_transform = TRUE)
  })
  expect_true(is.data.frame(prepared_data))
  
  # Test destruction
  provider$destroy()
  expect_false(provider$initialized)
})

test_that("BaiduProvider comprehensive interface coverage", {
  skip_if_not_installed("R6")
  skip_if_not(exists("BaiduProvider"), "BaiduProvider not available")
  
  provider <- BaiduProvider$new()
  
  # Test provider identification
  expect_equal(provider$provider_name, "baidu")
  expect_false(provider$initialized)
  
  # Test initialization with API key
  config <- list(
    api_key = "test_baidu_api_key_123456789",
    style = "normal",
    zoom = 11,
    location = c(116.404, 39.915)
  )
  
  expect_silent(provider$initialize_provider(config))
  expect_true(provider$initialized)
  expect_equal(provider$api_key, "test_baidu_api_key_123456789")
  
  # Test initialization failure without API key
  provider2 <- BaiduProvider$new()
  expect_error(provider2$initialize_provider(list()),
               "Baidu Maps API key is required")
  
  # Test all interface methods
  test_provider_interface_compliance(BaiduProvider, config)
  
  # Test Baidu-specific features
  styles <- provider$get_available_styles()
  expect_true(is.character(styles))
  expect_true(length(styles) > 0)
  expect_true("normal" %in% styles)
  
  # Test Baidu-specific styles
  baidu_styles <- c("redalert", "googlelite", "grassgreen", "midnight")
  for (style in baidu_styles) {
    expect_true(style %in% styles, 
                info = paste("Baidu style", style, "should be available"))
  }
  
  # Test coordinate transformation (BD09)
  wgs84_coords <- c(116.3974, 39.9093)
  bd09_coords <- provider$transform_coordinates(wgs84_coords, "WGS84", "BD09")
  expect_true(is.numeric(bd09_coords))
  expect_equal(length(bd09_coords), 2)
  expect_false(identical(wgs84_coords, bd09_coords))
  
  # Test BD09 specific configuration
  bd09_config <- list(
    api_key = "test_key",
    enable_3d = TRUE,
    enable_scroll_wheel_zoom = FALSE
  )
  
  provider3 <- BaiduProvider$new()
  provider3$initialize_provider(bd09_config)
  expect_true(provider3$baidu_config$enable_3d)
  expect_false(provider3$baidu_config$enable_scroll_wheel_zoom)
  
  # Test destruction
  provider$destroy()
  expect_false(provider$initialized)
})

test_that("All providers handle error conditions consistently", {
  skip_if_not_installed("R6")
  
  provider_configs <- list(
    list(class = "MapboxProvider", config = list(token = "pk.test_token_123456789")),
    list(class = "LeafletProvider", config = list(tile_provider = "OpenStreetMap")),
    list(class = "GaodeProvider", config = list(api_key = "test_gaode_key_123456789")),
    list(class = "BaiduProvider", config = list(api_key = "test_baidu_key_123456789"))
  )
  
  for (provider_info in provider_configs) {
    if (exists(provider_info$class)) {
      provider_class <- get(provider_info$class)
      provider <- provider_class$new()
      
      # Test operations before initialization
      expect_error(provider$create_map(), 
                   "Provider must be initialized",
                   info = paste(provider_info$class, "should require initialization"))
      
      expect_error(provider$update_style("test"), 
                   "Provider must be initialized",
                   info = paste(provider_info$class, "should require initialization"))
      
      expect_error(provider$add_layer(list(id = "test")), 
                   "Provider must be initialized",
                   info = paste(provider_info$class, "should require initialization"))
      
      # Initialize provider
      provider$initialize_provider(provider_info$config)
      
      # Test invalid view parameters
      expect_error(provider$set_view(-200, 40.7, 10), 
                   "Longitude must be",
                   info = paste(provider_info$class, "should validate longitude"))
      
      expect_error(provider$set_view(-74, 100, 10), 
                   "Latitude must be",
                   info = paste(provider_info$class, "should validate latitude"))
      
      expect_error(provider$set_view(-74, 40.7, -1), 
                   "Zoom must be",
                   info = paste(provider_info$class, "should validate zoom"))
      
      # Test invalid layer operations
      expect_error(provider$add_layer(list()), 
                   "Layer must have an id",
                   info = paste(provider_info$class, "should require layer ID"))
    }
  }
})

test_that("All providers support method chaining", {
  skip_if_not_installed("R6")
  
  provider_configs <- list(
    list(class = "MapboxProvider", config = list(token = "pk.test_token_123456789")),
    list(class = "LeafletProvider", config = list(tile_provider = "OpenStreetMap")),
    list(class = "GaodeProvider", config = list(api_key = "test_gaode_key_123456789")),
    list(class = "BaiduProvider", config = list(api_key = "test_baidu_key_123456789"))
  )
  
  for (provider_info in provider_configs) {
    if (exists(provider_info$class)) {
      provider_class <- get(provider_info$class)
      provider <- provider_class$new()
      
      # Test method chaining
      result <- provider$initialize_provider(provider_info$config)$
        update_style("test_style")$
        add_layer(list(id = "test_layer", type = "ScatterplotLayer"))$
        set_view(-74, 40.7, 10)
      
      expect_identical(result, provider,
                       info = paste(provider_info$class, "should support method chaining"))
      
      # Verify operations were applied
      expect_true(provider$initialized)
      expect_true("test_layer" %in% names(provider$layers))
    }
  }
})

test_that("All providers handle configuration validation", {
  skip_if_not_installed("R6")
  
  provider_configs <- list(
    list(class = "MapboxProvider", 
         valid = list(token = "pk.valid_token_123456789"),
         invalid = list(token = "invalid")),
    list(class = "LeafletProvider", 
         valid = list(tile_provider = "OpenStreetMap"),
         invalid = list(tile_provider = c("provider1", "provider2"))),
    list(class = "GaodeProvider", 
         valid = list(api_key = "valid_gaode_key_123456789"),
         invalid = list(api_key = "short")),
    list(class = "BaiduProvider", 
         valid = list(api_key = "valid_baidu_key_123456789"),
         invalid = list(api_key = "short"))
  )
  
  for (provider_info in provider_configs) {
    if (exists(provider_info$class)) {
      provider_class <- get(provider_info$class)
      provider <- provider_class$new()
      
      # Test valid configuration
      expect_true(provider$validate_config(provider_info$valid),
                  info = paste(provider_info$class, "should accept valid config"))
      
      # Test invalid configuration (may warn but should not error)
      if (provider_info$class %in% c("MapboxProvider", "GaodeProvider", "BaiduProvider")) {
        expect_warning(provider$validate_config(provider_info$invalid),
                       info = paste(provider_info$class, "should warn about invalid config"))
      } else {
        expect_false(provider$validate_config(provider_info$invalid),
                     info = paste(provider_info$class, "should reject invalid config"))
      }
      
      # Test non-list configuration
      expect_false(provider$validate_config("not_a_list"),
                   info = paste(provider_info$class, "should reject non-list config"))
    }
  }
})

test_that("All providers integrate with provider factory", {
  skip_if_not_installed("R6")
  
  # Test that all providers are registered in factory
  factory <- get_provider_factory()
  available_providers <- factory$get_available_providers()
  
  expected_providers <- c("mapbox", "leaflet", "openlayers", "gaode", "baidu")
  for (provider_name in expected_providers) {
    expect_true(provider_name %in% available_providers,
                info = paste("Provider", provider_name, "should be registered in factory"))
    
    # Test getting capabilities
    capabilities <- factory$get_provider_capabilities(provider_name)
    expect_true(is.list(capabilities),
                info = paste("Provider", provider_name, "should have capabilities"))
    expect_equal(capabilities$name, provider_name)
  }
})

test_that("All providers handle destruction properly", {
  skip_if_not_installed("R6")
  
  provider_configs <- list(
    list(class = "MapboxProvider", config = list(token = "pk.test_token_123456789")),
    list(class = "LeafletProvider", config = list(tile_provider = "OpenStreetMap")),
    list(class = "GaodeProvider", config = list(api_key = "test_gaode_key_123456789")),
    list(class = "BaiduProvider", config = list(api_key = "test_baidu_key_123456789"))
  )
  
  for (provider_info in provider_configs) {
    if (exists(provider_info$class)) {
      provider_class <- get(provider_info$class)
      provider <- provider_class$new()
      
      # Initialize and add some data
      provider$initialize_provider(provider_info$config)
      provider$add_layer(list(id = "test_layer", type = "ScatterplotLayer"))
      
      # Verify initialization
      expect_true(provider$initialized)
      expect_true("test_layer" %in% names(provider$layers))
      
      # Test destruction
      provider$destroy()
      
      # Verify cleanup
      expect_false(provider$initialized,
                   info = paste(provider_info$class, "should be uninitialized after destroy"))
      expect_equal(length(provider$layers), 0,
                   info = paste(provider_info$class, "should clear layers after destroy"))
    }
  }
})

test_that("Provider-specific coordinate transformations work correctly", {
  skip_if_not_installed("R6")
  
  # Test Chinese providers coordinate transformations
  chinese_providers <- list(
    list(class = "GaodeProvider", config = list(api_key = "test_key"), crs = "GCJ02"),
    list(class = "BaiduProvider", config = list(api_key = "test_key"), crs = "BD09")
  )
  
  test_coords <- c(116.3974, 39.9093)  # Beijing
  
  for (provider_info in chinese_providers) {
    if (exists(provider_info$class)) {
      provider_class <- get(provider_info$class)
      provider <- provider_class$new()
      provider$initialize_provider(provider_info$config)
      
      # Test coordinate transformation
      transformed <- provider$transform_coordinates(test_coords, "WGS84", provider_info$crs)
      expect_true(is.numeric(transformed))
      expect_equal(length(transformed), 2)
      expect_false(identical(test_coords, transformed))
      
      # Test round-trip accuracy
      back_transformed <- provider$transform_coordinates(transformed, provider_info$crs, "WGS84")
      expect_true(all(abs(test_coords - back_transformed) < 1e-6),
                  info = paste(provider_info$class, "coordinate round-trip should be accurate"))
    }
  }
})