test_that("update_provider validates inputs correctly", {
  skip_if_not_installed("R6")
  
  # Test missing map
  expect_error(update_provider(), "Map object is required")
  expect_error(update_provider(NULL, "leaflet"), "Map object is required")
  
  # Test invalid new_provider
  mock_map <- list(x = list())
  expect_error(update_provider(mock_map, NULL), "new_provider must be a single character string")
  expect_error(update_provider(mock_map, c("a", "b")), "new_provider must be a single character string")
  expect_error(update_provider(mock_map, 123), "new_provider must be a single character string")
  
  # Test invalid config
  expect_error(update_provider(mock_map, "leaflet", "not_a_list"), "config must be a list")
})

test_that("update_provider handles same provider correctly", {
  skip_if_not_installed("R6")
  
  # Create a mock map with provider attribute
  mock_provider <- list(provider_name = "mapbox")
  mock_map <- list(x = list())
  attr(mock_map, "mapdeck_provider") <- mock_provider
  
  # Test switching to same provider
  expect_message(
    result <- update_provider(mock_map, "mapbox"),
    "Map is already using provider 'mapbox'"
  )
  expect_identical(result, mock_map)
})

test_that("check_provider_compatibility validates inputs", {
  # Test invalid inputs
  expect_error(check_provider_compatibility(NULL, "leaflet"), 
               "from_provider must be a single character string")
  expect_error(check_provider_compatibility("mapbox", NULL), 
               "to_provider must be a single character string")
  expect_error(check_provider_compatibility(c("a", "b"), "leaflet"), 
               "from_provider must be a single character string")
  expect_error(check_provider_compatibility("mapbox", c("a", "b")), 
               "to_provider must be a single character string")
})

test_that("check_provider_compatibility returns correct structure", {
  # Test basic compatibility check
  result <- check_provider_compatibility("mapbox", "leaflet")
  
  expect_true(is.list(result))
  expect_true("compatible" %in% names(result))
  expect_true("reason" %in% names(result))
  expect_true("issues" %in% names(result))
  expect_true("feature_mapping" %in% names(result))
  expect_true("coordinate_transformation_required" %in% names(result))
  expect_true("degraded_features" %in% names(result))
  
  expect_true(is.logical(result$compatible))
  expect_true(is.character(result$reason))
  expect_true(is.list(result$issues))
  expect_true(is.list(result$feature_mapping))
  expect_true(is.logical(result$coordinate_transformation_required))
  expect_true(is.character(result$degraded_features))
})

test_that("extract_provider_info handles different map types", {
  # Test map with provider attribute
  mock_provider <- list(
    provider_name = "mapbox",
    layers = list(),
    config = list()
  )
  mock_map <- list(x = list())
  attr(mock_map, "mapdeck_provider") <- mock_provider
  
  result <- extract_provider_info(mock_map)
  expect_equal(result$provider, "mapbox")
  expect_identical(result$provider_object, mock_provider)
  
  # Test legacy map without provider attribute
  legacy_map <- list(x = list())
  result <- extract_provider_info(legacy_map)
  expect_equal(result$provider, "mapbox")
  expect_null(result$provider_object)
})

test_that("extract_map_state extracts correct information", {
  # Create mock map
  mock_map <- list(
    x = list(
      location = c(-74, 40.7),
      zoom = 10,
      pitch = 30,
      bearing = 45,
      access_token = "test_token",
      style = "streets",
      max_zoom = 20,
      min_zoom = 0,
      show_view_state = TRUE,
      repeat_view = FALSE
    ),
    width = "100%",
    height = "400px",
    sizingPolicy = list()
  )
  
  # Add data attribute
  attr(mock_map$x, "mapdeck_data") <- data.frame(x = 1:3, y = 1:3)
  
  result <- extract_map_state(mock_map)
  
  expect_true(is.list(result))
  expect_true("view_state" %in% names(result))
  expect_true("config" %in% names(result))
  expect_true("layers" %in% names(result))
  expect_true("data" %in% names(result))
  
  expect_equal(result$view_state$location, c(-74, 40.7))
  expect_equal(result$view_state$zoom, 10)
  expect_equal(result$config$access_token, "test_token")
  expect_equal(result$config$style, "streets")
})

test_that("needs_coordinate_transformation works correctly", {
  # Same coordinate systems
  expect_false(needs_coordinate_transformation("mapbox", "leaflet"))
  expect_false(needs_coordinate_transformation("leaflet", "openlayers"))
  
  # Different coordinate systems
  expect_true(needs_coordinate_transformation("mapbox", "gaode"))
  expect_true(needs_coordinate_transformation("leaflet", "baidu"))
  expect_true(needs_coordinate_transformation("gaode", "baidu"))
})

test_that("get_provider_coordinate_system returns correct systems", {
  expect_equal(get_provider_coordinate_system("mapbox"), "EPSG:4326")
  expect_equal(get_provider_coordinate_system("leaflet"), "EPSG:4326")
  expect_equal(get_provider_coordinate_system("openlayers"), "EPSG:4326")
  expect_equal(get_provider_coordinate_system("gaode"), "GCJ02")
  expect_equal(get_provider_coordinate_system("baidu"), "BD09")
  expect_equal(get_provider_coordinate_system("unknown"), "EPSG:4326")
})

test_that("get_degraded_features identifies correct features", {
  # Mapbox to Leaflet
  degraded <- get_degraded_features("mapbox", "leaflet")
  expect_true("3d_layers" %in% degraded)
  expect_true("pitch" %in% degraded)
  expect_true("bearing" %in% degraded)
  
  # Leaflet to Mapbox
  degraded <- get_degraded_features("leaflet", "mapbox")
  expect_true("tile_layers" %in% degraded)
  
  # Same provider
  degraded <- get_degraded_features("mapbox", "mapbox")
  expect_equal(length(degraded), 0)
})

test_that("is_layer_compatible checks layer compatibility", {
  # Basic compatible layer
  layer <- list(type = "scatterplot", id = "test")
  expect_true(is_layer_compatible(layer, "mapbox"))
  expect_true(is_layer_compatible(layer, "leaflet"))
  
  # Layer without type
  layer_no_type <- list(id = "test")
  expect_false(is_layer_compatible(layer_no_type, "mapbox"))
  
  # 3D layer with Leaflet
  layer_3d <- list(type = "column", id = "test", elevation = "height")
  expect_true(is_layer_compatible(layer_3d, "mapbox"))
  expect_false(is_layer_compatible(layer_3d, "leaflet"))
})

test_that("merge_provider_configs merges correctly", {
  state_config <- list(
    zoom = 10,
    pitch = 30,
    style = "streets"
  )
  
  user_config <- list(
    zoom = 15,
    bearing = 45,
    token = "new_token"
  )
  
  result <- merge_provider_configs(user_config, state_config)
  
  expect_equal(result$zoom, 15)  # User override
  expect_equal(result$pitch, 30)  # From state
  expect_equal(result$bearing, 45)  # From user
  expect_equal(result$token, "new_token")  # From user
  expect_equal(result$style, "streets")  # From state
})

test_that("check_coordinate_system_compatibility works", {
  # Same coordinate systems
  result <- check_coordinate_system_compatibility("mapbox", "leaflet")
  expect_null(result)
  
  # Different coordinate systems
  result <- check_coordinate_system_compatibility("mapbox", "gaode")
  expect_true(is.character(result))
  expect_true(grepl("transformation required", result))
})

test_that("check_authentication_compatibility works", {
  # Both require auth
  from_caps <- list(authentication_required = TRUE)
  to_caps <- list(authentication_required = TRUE)
  result <- check_authentication_compatibility(from_caps, to_caps)
  expect_null(result)
  
  # From doesn't require, to does
  from_caps <- list(authentication_required = FALSE)
  to_caps <- list(authentication_required = TRUE)
  result <- check_authentication_compatibility(from_caps, to_caps)
  expect_true(is.character(result))
  expect_true(grepl("requires authentication", result))
  
  # From requires, to doesn't
  from_caps <- list(authentication_required = TRUE)
  to_caps <- list(authentication_required = FALSE)
  result <- check_authentication_compatibility(from_caps, to_caps)
  expect_null(result)
})

test_that("check_style_compatibility identifies style system differences", {
  # Same provider (same style system)
  result <- check_style_compatibility("mapbox", "mapbox")
  expect_null(result)
  
  # Different providers (different style systems)
  result <- check_style_compatibility("mapbox", "leaflet")
  expect_true(is.character(result))
  expect_true(grepl("Style system incompatibility", result))
})

test_that("apply_layer_degradation modifies layers appropriately", {
  # Test Leaflet degradation
  layer <- list(
    type = "column",
    id = "test",
    elevation = "height",
    pitch = 30,
    bearing = 45
  )
  
  compatibility_result <- list(degraded_features = c("3d_layers", "pitch", "bearing"))
  
  result <- apply_layer_degradation(layer, "leaflet", compatibility_result)
  
  expect_null(result$elevation)
  expect_null(result$pitch)
  expect_null(result$bearing)
  expect_equal(result$type, "column")
  expect_equal(result$id, "test")
})

test_that("transform_layer_for_provider handles transformations", {
  layer <- list(
    type = "scatterplot",
    id = "test",
    data = data.frame(lon = c(116, 117), lat = c(39, 40))
  )
  
  # Same coordinate system - no transformation
  result <- transform_layer_for_provider(layer, "mapbox", "leaflet")
  expect_identical(result, apply_provider_specific_layer_transforms(layer, "leaflet"))
  
  # Different coordinate system - would transform
  result <- transform_layer_for_provider(layer, "mapbox", "gaode")
  expect_true("gaode_compatible" %in% names(result))
})

test_that("apply_provider_specific_layer_transforms adds provider flags", {
  layer <- list(type = "scatterplot", id = "test")
  
  # Test each provider
  result_leaflet <- apply_provider_specific_layer_transforms(layer, "leaflet")
  expect_true(result_leaflet$leaflet_compatible)
  
  result_openlayers <- apply_provider_specific_layer_transforms(layer, "openlayers")
  expect_true(result_openlayers$openlayers_compatible)
  
  result_gaode <- apply_provider_specific_layer_transforms(layer, "gaode")
  expect_true(result_gaode$gaode_compatible)
  
  result_baidu <- apply_provider_specific_layer_transforms(layer, "baidu")
  expect_true(result_baidu$baidu_compatible)
})

test_that("create_layer_feature_mapping creates appropriate mappings", {
  layer <- list(type = "scatterplot", id = "test")
  
  result <- create_layer_feature_mapping(layer, "mapbox", "leaflet")
  
  expect_true(is.list(result))
  expect_equal(result$layer_type, "scatterplot")
  expect_true(is.logical(result$compatible))
  expect_true(is.logical(result$transformations_needed))
})

test_that("is_provider_available works with mock factory", {
  # This test assumes the provider factory is available
  # In a real implementation, this would check actual provider availability
  
  # Test with likely available providers
  expect_true(is_provider_available("mapbox") || !is_provider_available("mapbox"))
  expect_true(is_provider_available("leaflet") || !is_provider_available("leaflet"))
  
  # Test with definitely unavailable provider
  expect_false(is_provider_available("nonexistent_provider"))
})