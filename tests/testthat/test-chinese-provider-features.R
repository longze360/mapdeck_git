test_that("Gaode provider supports Chinese map styles", {
  skip_if_not_installed("R6")
  
  provider <- GaodeProvider$new()
  provider$initialize_provider(list(api_key = "test_key"))
  
  # Test getting all available styles
  all_styles <- provider$get_available_styles()
  expect_type(all_styles, "character")
  expect_true(length(all_styles) > 0)
  
  # Test specific Gaode styles
  expected_styles <- c(
    "amap://styles/normal",
    "amap://styles/light", 
    "amap://styles/dark",
    "amap://styles/satellite",
    "amap://styles/hybrid"
  )
  
  for (style in expected_styles) {
    expect_true(style %in% all_styles, info = paste("Style", style, "should be available"))
  }
  
  # Test style categories
  basic_styles <- provider$get_available_styles("basic")
  expect_true("amap://styles/normal" %in% basic_styles)
  expect_true("amap://styles/light" %in% basic_styles)
  
  satellite_styles <- provider$get_available_styles("satellite")
  expect_true("amap://styles/satellite" %in% satellite_styles)
  expect_true("amap://styles/hybrid" %in% satellite_styles)
  
  specialty_styles <- provider$get_available_styles("specialty")
  expect_true(length(specialty_styles) > 0)
})

test_that("Baidu provider supports Chinese map styles", {
  skip_if_not_installed("R6")
  
  provider <- BaiduProvider$new()
  provider$initialize_provider(list(api_key = "test_key"))
  
  # Test getting all available styles
  all_styles <- provider$get_available_styles()
  expect_type(all_styles, "character")
  expect_true(length(all_styles) > 0)
  
  # Test specific Baidu styles
  expected_styles <- c(
    "normal", "light", "dark", "satellite", "hybrid",
    "redalert", "googlelite", "grassgreen", "midnight",
    "pink", "darkgreen", "bluish"
  )
  
  for (style in expected_styles) {
    expect_true(style %in% all_styles, info = paste("Style", style, "should be available"))
  }
  
  # Test style categories
  basic_styles <- provider$get_available_styles("basic")
  expect_true("normal" %in% basic_styles)
  expect_true("light" %in% basic_styles)
  
  satellite_styles <- provider$get_available_styles("satellite")
  expect_true("satellite" %in% satellite_styles)
  expect_true("hybrid" %in% satellite_styles)
  
  specialty_styles <- provider$get_available_styles("specialty")
  expect_true("hardedge" %in% specialty_styles)
  expect_true("grayscale" %in% specialty_styles)
})

test_that("Chinese providers support style updates", {
  skip_if_not_installed("R6")
  
  # Test Gaode style updates
  gaode_provider <- GaodeProvider$new()
  gaode_provider$initialize_provider(list(api_key = "test_key"))
  
  original_style <- gaode_provider$current_style
  expect_silent(gaode_provider$update_style("amap://styles/dark"))
  expect_equal(gaode_provider$current_style, "amap://styles/dark")
  expect_equal(gaode_provider$gaode_config$style, "amap://styles/dark")
  
  # Test Baidu style updates
  baidu_provider <- BaiduProvider$new()
  baidu_provider$initialize_provider(list(api_key = "test_key"))
  
  original_style <- baidu_provider$current_style
  expect_silent(baidu_provider$update_style("dark"))
  expect_equal(baidu_provider$current_style, "dark")
  expect_equal(baidu_provider$baidu_config$style, "dark")
})

test_that("Gaode provider supports Chinese-specific features", {
  skip_if_not_installed("R6")
  
  provider <- GaodeProvider$new()
  
  # Test Chinese-specific configuration
  config <- list(
    api_key = "test_key",
    lang = "zh_cn",
    features = list("bg", "road", "building", "point"),
    location = c(116.397, 39.909) # Beijing
  )
  
  provider$initialize_provider(config)
  
  expect_equal(provider$gaode_config$lang, "zh_cn")
  expect_equal(provider$gaode_config$features, list("bg", "road", "building", "point"))
  
  # Test that location is properly handled (should be transformed to GCJ02)
  expect_type(provider$gaode_config$location, "double")
  expect_length(provider$gaode_config$location, 2)
})

test_that("Baidu provider supports Chinese-specific features", {
  skip_if_not_installed("R6")
  
  provider <- BaiduProvider$new()
  
  # Test Chinese-specific configuration
  config <- list(
    api_key = "test_key",
    enable_3d = TRUE,
    enable_scroll_wheel_zoom = FALSE,
    enable_drag = TRUE,
    enable_click = FALSE,
    location = c(116.404, 39.915) # Beijing
  )
  
  provider$initialize_provider(config)
  
  expect_true(provider$baidu_config$enable_3d)
  expect_false(provider$baidu_config$enable_scroll_wheel_zoom)
  expect_true(provider$baidu_config$enable_drag)
  expect_false(provider$baidu_config$enable_click)
  
  # Test that location is properly handled (should be transformed to BD09)
  expect_type(provider$baidu_config$location, "double")
  expect_length(provider$baidu_config$location, 2)
})

test_that("Chinese providers handle Chinese coordinate bounds correctly", {
  skip_if_not_installed("R6")
  
  gaode_provider <- GaodeProvider$new()
  gaode_provider$initialize_provider(list(api_key = "test_key"))
  
  baidu_provider <- BaiduProvider$new()
  baidu_provider$initialize_provider(list(api_key = "test_key"))
  
  # Test coordinates within China
  china_coords <- data.frame(
    longitude = c(116.3974, 121.4737, 113.2644), # Beijing, Shanghai, Guangzhou
    latitude = c(39.9093, 31.2304, 23.1291)
  )
  
  gaode_detection <- gaode_provider$detect_coordinate_system(china_coords)
  baidu_detection <- baidu_provider$detect_coordinate_system(china_coords)
  
  # Should detect as Chinese coordinate system
  expect_true(gaode_detection %in% c("GCJ02", "BD09"))
  expect_true(baidu_detection %in% c("GCJ02", "BD09"))
  
  # Test coordinates outside China
  outside_coords <- data.frame(
    longitude = c(-74.006, 2.3522), # New York, Paris
    latitude = c(40.7128, 48.8566)
  )
  
  gaode_detection_outside <- gaode_provider$detect_coordinate_system(outside_coords)
  baidu_detection_outside <- baidu_provider$detect_coordinate_system(outside_coords)
  
  # Should detect as WGS84
  expect_equal(gaode_detection_outside, "WGS84")
  expect_equal(baidu_detection_outside, "WGS84")
})

test_that("Chinese providers support proper zoom levels", {
  skip_if_not_installed("R6")
  
  # Test Gaode zoom levels
  gaode_provider <- GaodeProvider$new()
  gaode_config <- list(
    api_key = "test_key",
    zoom = 12,
    max_zoom = 18,
    min_zoom = 3
  )
  gaode_provider$initialize_provider(gaode_config)
  
  expect_equal(gaode_provider$gaode_config$zoom, 12)
  expect_equal(gaode_provider$gaode_config$max_zoom, 18)
  expect_equal(gaode_provider$gaode_config$min_zoom, 3)
  
  # Test Baidu zoom levels
  baidu_provider <- BaiduProvider$new()
  baidu_config <- list(
    api_key = "test_key",
    zoom = 11,
    max_zoom = 19,
    min_zoom = 3
  )
  baidu_provider$initialize_provider(baidu_config)
  
  expect_equal(baidu_provider$baidu_config$zoom, 11)
  expect_equal(baidu_provider$baidu_config$max_zoom, 19)
  expect_equal(baidu_provider$baidu_config$min_zoom, 3)
})

test_that("Chinese providers support 3D features", {
  skip_if_not_installed("R6")
  
  # Test Gaode 3D support
  gaode_provider <- GaodeProvider$new()
  gaode_config <- list(
    api_key = "test_key",
    pitch = 45,
    max_pitch = 60,
    min_pitch = 0
  )
  gaode_provider$initialize_provider(gaode_config)
  
  expect_equal(gaode_provider$gaode_config$pitch, 45)
  expect_equal(gaode_provider$gaode_config$max_pitch, 60)
  expect_equal(gaode_provider$gaode_config$min_pitch, 0)
  
  # Test Baidu 3D support
  baidu_provider <- BaiduProvider$new()
  baidu_config <- list(
    api_key = "test_key",
    pitch = 30,
    enable_3d = TRUE,
    max_pitch = 60,
    min_pitch = 0
  )
  baidu_provider$initialize_provider(baidu_config)
  
  expect_equal(baidu_provider$baidu_config$pitch, 30)
  expect_true(baidu_provider$baidu_config$enable_3d)
  expect_equal(baidu_provider$baidu_config$max_pitch, 60)
  expect_equal(baidu_provider$baidu_config$min_pitch, 0)
})

test_that("Chinese providers handle bearing/rotation correctly", {
  skip_if_not_installed("R6")
  
  gaode_provider <- GaodeProvider$new()
  gaode_provider$initialize_provider(list(api_key = "test_key"))
  
  baidu_provider <- BaiduProvider$new()
  baidu_provider$initialize_provider(list(api_key = "test_key"))
  
  # Test setting bearing/rotation
  expect_silent(gaode_provider$set_view(116.397, 39.909, 12, 0, 45))
  expect_equal(gaode_provider$gaode_config$bearing, 45)
  
  expect_silent(baidu_provider$set_view(116.404, 39.915, 11, 0, 90))
  expect_equal(baidu_provider$baidu_config$bearing, 90)
  
  # Test bearing validation
  expect_error(gaode_provider$set_view(116.397, 39.909, 12, 0, 400))
  expect_error(baidu_provider$set_view(116.404, 39.915, 11, 0, -10))
})

test_that("Chinese providers support layer data transformation", {
  skip_if_not_installed("R6")
  
  gaode_provider <- GaodeProvider$new()
  gaode_provider$initialize_provider(list(api_key = "test_key"))
  
  baidu_provider <- BaiduProvider$new()
  baidu_provider$initialize_provider(list(api_key = "test_key"))
  
  # Test layer with WGS84 data
  layer_data <- data.frame(
    longitude = c(116.3974, 121.4737),
    latitude = c(39.9093, 31.2304),
    value = c(10, 20)
  )
  
  layer_config <- list(
    id = "test_layer",
    type = "scatterplot",
    data = layer_data
  )
  
  # Add layer to both providers
  expect_silent(gaode_provider$add_layer(layer_config))
  expect_silent(baidu_provider$add_layer(layer_config))
  
  # Verify layers were added
  expect_true("test_layer" %in% names(gaode_provider$layers))
  expect_true("test_layer" %in% names(baidu_provider$layers))
  
  # Verify data was transformed (coordinates should be different from original)
  gaode_layer_data <- gaode_provider$layers[["test_layer"]]$data
  baidu_layer_data <- baidu_provider$layers[["test_layer"]]$data
  
  expect_false(identical(layer_data$longitude, gaode_layer_data$longitude))
  expect_false(identical(layer_data$longitude, baidu_layer_data$longitude))
  expect_false(identical(gaode_layer_data$longitude, baidu_layer_data$longitude))
})

test_that("Chinese providers maintain feature compatibility", {
  skip_if_not_installed("R6")
  
  # Test that both providers support the same core features
  gaode_provider <- GaodeProvider$new()
  gaode_provider$initialize_provider(list(api_key = "test_key"))
  
  baidu_provider <- BaiduProvider$new()
  baidu_provider$initialize_provider(list(api_key = "test_key"))
  
  # Both should support coordinate transformation
  expect_true(is.function(gaode_provider$transform_coordinates))
  expect_true(is.function(baidu_provider$transform_coordinates))
  
  # Both should support data preparation
  expect_true(is.function(gaode_provider$prepare_data))
  expect_true(is.function(baidu_provider$prepare_data))
  
  # Both should support layer management
  expect_true(is.function(gaode_provider$add_layer))
  expect_true(is.function(gaode_provider$remove_layer))
  expect_true(is.function(baidu_provider$add_layer))
  expect_true(is.function(baidu_provider$remove_layer))
  
  # Both should support view management
  expect_true(is.function(gaode_provider$set_view))
  expect_true(is.function(baidu_provider$set_view))
  
  # Both should support style management
  expect_true(is.function(gaode_provider$get_available_styles))
  expect_true(is.function(gaode_provider$update_style))
  expect_true(is.function(baidu_provider$get_available_styles))
  expect_true(is.function(baidu_provider$update_style))
})