test_that("validate_provider_name works correctly", {
  
  # Test valid provider names
  expect_equal(validate_provider_name("mapbox"), "mapbox")
  expect_equal(validate_provider_name("leaflet"), "leaflet")
  
  # Test NULL handling
  expect_null(validate_provider_name(NULL, allow_null = TRUE))
  expect_error(validate_provider_name(NULL), "Provider name cannot be NULL")
  
  # Test invalid inputs
  expect_error(validate_provider_name(123), "Provider name must be a single character string")
  expect_error(validate_provider_name(c("a", "b")), "Provider name must be a single character string")
  expect_error(validate_provider_name(""), "Provider name cannot be empty")
  
  # Test unavailable provider
  expect_error(
    validate_provider_name("nonexistent"),
    "Provider 'nonexistent' is not available"
  )
})

test_that("validate_map_options validates basic options correctly", {
  
  # Test valid options
  options <- list(
    width = "100%",
    height = 400,
    padding = 10,
    location = c(-74, 40.7),
    zoom = 10,
    pitch = 30,
    bearing = 45,
    max_zoom = 20,
    min_zoom = 0,
    max_pitch = 60,
    min_pitch = 0,
    show_view_state = TRUE,
    repeat_view = FALSE,
    libraries = c("h3")
  )
  
  result <- validate_map_options(options, "mapbox")
  expect_true(is.list(result))
  expect_equal(result$width, "100%")
  expect_equal(result$height, 400)
  expect_equal(result$zoom, 10)
  
  # Test invalid options
  expect_error(validate_map_options("not_a_list", "mapbox"), "Options must be a list")
  expect_error(validate_map_options(list(), "not_a_string"), "Provider must be a single character string")
  
  # Test invalid width/height
  expect_error(validate_map_options(list(width = TRUE), "mapbox"), "Width must be numeric or character")
  expect_error(validate_map_options(list(height = TRUE), "mapbox"), "Height must be numeric or character")
  
  # Test invalid padding
  expect_error(validate_map_options(list(padding = "invalid"), "mapbox"), "Padding must be a single numeric value")
  expect_error(validate_map_options(list(padding = c(1, 2)), "mapbox"), "Padding must be a single numeric value")
  
  # Test invalid location
  expect_error(validate_map_options(list(location = "invalid"), "mapbox"), "Location must be a numeric vector of length 2")
  expect_error(validate_map_options(list(location = c(1, 2, 3)), "mapbox"), "Location must be a numeric vector of length 2")
  
  # Test invalid zoom
  expect_error(validate_map_options(list(zoom = "invalid"), "mapbox"), "Zoom must be a single numeric value")
  expect_error(validate_map_options(list(zoom = c(1, 2)), "mapbox"), "Zoom must be a single numeric value")
  
  # Test invalid libraries
  expect_error(validate_map_options(list(libraries = 123), "mapbox"), "Libraries must be a character vector")
  
  # Test invalid boolean options
  expect_error(validate_map_options(list(show_view_state = "true"), "mapbox"), "show_view_state must be a single logical value")
  expect_error(validate_map_options(list(repeat_view = c(TRUE, FALSE)), "mapbox"), "repeat_view must be a single logical value")
})

test_that("validate_layer_config validates layer configuration", {
  
  # Test valid layer
  layer <- list(
    type = "scatterplot",
    id = "test_layer",
    data = data.frame(x = 1:3, y = 1:3)
  )
  
  result <- validate_layer_config(layer, "mapbox")
  expect_true(is.list(result))
  expect_equal(result$type, "scatterplot")
  expect_equal(result$id, "test_layer")
  
  # Test invalid inputs
  expect_error(validate_layer_config("not_a_list", "mapbox"), "Layer must be a list")
  expect_error(validate_layer_config(list(), "not_a_string"), "Provider must be a single character string")
  
  # Test missing type
  expect_error(validate_layer_config(list(id = "test"), "mapbox"), "Layer must have a 'type' field")
  
  # Test invalid type
  expect_error(validate_layer_config(list(type = 123), "mapbox"), "Layer type must be a single character string")
  expect_error(validate_layer_config(list(type = c("a", "b")), "mapbox"), "Layer type must be a single character string")
  
  # Test auto-generated ID
  layer_no_id <- list(type = "scatterplot")
  result <- validate_layer_config(layer_no_id, "mapbox")
  expect_true(!is.null(result$id))
  expect_true(is.character(result$id))
  
  # Test invalid ID
  expect_error(validate_layer_config(list(type = "scatterplot", id = 123), "mapbox"), 
               "Layer ID must be a single character string")
  
  # Test invalid data
  expect_error(validate_layer_config(list(type = "scatterplot", data = "invalid"), "mapbox"), 
               "Layer data must be a data frame or list")
})

test_that("normalize_style_name works for different providers", {
  
  # Test NULL style
  expect_equal(normalize_style_name(NULL, "mapbox"), get_default_style("mapbox"))
  
  # Test invalid provider
  expect_error(normalize_style_name("streets", 123), "Provider must be a single character string")
  expect_error(normalize_style_name("streets", c("a", "b")), "Provider must be a single character string")
  
  # Test character styles
  expect_true(is.character(normalize_style_name("streets", "mapbox")))
  expect_true(is.character(normalize_style_name("osm", "leaflet")))
  
  # Test list styles
  custom_style <- list(version = 8, sources = list(), layers = list())
  result <- normalize_style_name(custom_style, "mapbox")
  expect_true(is.list(result))
})

test_that("get_default_style returns correct defaults", {
  expect_equal(get_default_style("mapbox"), "mapbox://styles/mapbox/streets-v11")
  expect_equal(get_default_style("leaflet"), "OpenStreetMap")
  expect_equal(get_default_style("openlayers"), "osm")
  expect_equal(get_default_style("gaode"), "normal")
  expect_equal(get_default_style("baidu"), "normal")
  expect_equal(get_default_style("unknown"), "default")
})

test_that("normalize_mapbox_style handles aliases correctly", {
  expect_equal(normalize_mapbox_style("streets"), "mapbox://styles/mapbox/streets-v11")
  expect_equal(normalize_mapbox_style("dark"), "mapbox://styles/mapbox/dark-v10")
  expect_equal(normalize_mapbox_style("satellite"), "mapbox://styles/mapbox/satellite-v9")
  
  # Test case insensitivity
  expect_equal(normalize_mapbox_style("STREETS"), "mapbox://styles/mapbox/streets-v11")
  expect_equal(normalize_mapbox_style("Dark"), "mapbox://styles/mapbox/dark-v10")
  
  # Test unknown alias
  expect_equal(normalize_mapbox_style("unknown_style"), "unknown_style")
  
  # Test full URL (should return as-is)
  full_url <- "mapbox://styles/custom/style"
  expect_equal(normalize_mapbox_style(full_url), full_url)
})

test_that("normalize_leaflet_style uses existing function", {
  # This should use the normalize_leaflet_tile_provider function
  expect_equal(normalize_leaflet_style("osm"), "OpenStreetMap")
  expect_equal(normalize_leaflet_style("cartodb"), "CartoDB.Positron")
  
  # Test error handling
  expect_equal(normalize_leaflet_style("unknown"), "unknown")
})

test_that("normalize_openlayers_style handles aliases", {
  expect_equal(normalize_openlayers_style("osm"), "osm")
  expect_equal(normalize_openlayers_style("openstreetmap"), "osm")
  expect_equal(normalize_openlayers_style("bing"), "bing")
  
  # Test case insensitivity
  expect_equal(normalize_openlayers_style("OSM"), "osm")
  
  # Test unknown alias
  expect_equal(normalize_openlayers_style("unknown"), "unknown")
})

test_that("normalize_gaode_style handles aliases", {
  expect_equal(normalize_gaode_style("normal"), "normal")
  expect_equal(normalize_gaode_style("satellite"), "satellite")
  expect_equal(normalize_gaode_style("dark"), "dark")
  
  # Test case insensitivity
  expect_equal(normalize_gaode_style("NORMAL"), "normal")
  
  # Test unknown alias
  expect_equal(normalize_gaode_style("unknown"), "unknown")
})

test_that("normalize_baidu_style handles aliases", {
  expect_equal(normalize_baidu_style("normal"), "normal")
  expect_equal(normalize_baidu_style("satellite"), "satellite")
  expect_equal(normalize_baidu_style("hybrid"), "hybrid")
  
  # Test case insensitivity
  expect_equal(normalize_baidu_style("NORMAL"), "normal")
  
  # Test unknown alias
  expect_equal(normalize_baidu_style("unknown"), "unknown")
})

test_that("validate_mapbox_options enforces Mapbox constraints", {
  # Test valid options
  options <- list(pitch = 30, bearing = 180)
  result <- validate_mapbox_options(options)
  expect_equal(result$pitch, 30)
  expect_equal(result$bearing, 180)
  
  # Test invalid pitch
  expect_error(validate_mapbox_options(list(pitch = -10)), "Mapbox pitch must be between 0 and 60 degrees")
  expect_error(validate_mapbox_options(list(pitch = 70)), "Mapbox pitch must be between 0 and 60 degrees")
  
  # Test invalid bearing
  expect_error(validate_mapbox_options(list(bearing = -10)), "Mapbox bearing must be between 0 and 360 degrees")
  expect_error(validate_mapbox_options(list(bearing = 370)), "Mapbox bearing must be between 0 and 360 degrees")
})

test_that("validate_leaflet_options handles Leaflet limitations", {
  # Test pitch warning and correction
  expect_warning(
    result <- validate_leaflet_options(list(pitch = 30)),
    "Leaflet does not support pitch - setting to 0"
  )
  expect_equal(result$pitch, 0)
  
  # Test bearing warning and correction
  expect_warning(
    result <- validate_leaflet_options(list(bearing = 45)),
    "Leaflet does not support bearing - setting to 0"
  )
  expect_equal(result$bearing, 0)
  
  # Test valid tile provider
  options <- list(tile_provider = "osm")
  result <- validate_leaflet_options(options)
  expect_equal(result$tile_provider, "OpenStreetMap")  # Should be normalized
  
  # Test invalid tile provider
  expect_error(validate_leaflet_options(list(tile_provider = 123)), 
               "tile_provider must be a single character string")
})

test_that("validate_gaode_options validates API key", {
  # Test valid API key
  options <- list(api_key = "test_key")
  result <- validate_gaode_options(options)
  expect_equal(result$api_key, "test_key")
  
  # Test invalid API key
  expect_error(validate_gaode_options(list(api_key = 123)), 
               "api_key must be a single character string")
  expect_error(validate_gaode_options(list(api_key = c("a", "b"))), 
               "api_key must be a single character string")
})

test_that("validate_baidu_options validates API key", {
  # Test valid API key
  options <- list(api_key = "test_key")
  result <- validate_baidu_options(options)
  expect_equal(result$api_key, "test_key")
  
  # Test invalid API key
  expect_error(validate_baidu_options(list(api_key = 123)), 
               "api_key must be a single character string")
  expect_error(validate_baidu_options(list(api_key = c("a", "b"))), 
               "api_key must be a single character string")
})

test_that("validate_leaflet_layer warns about 3D limitations", {
  # Test layer with elevation
  layer <- list(type = "column", elevation = "height")
  
  expect_warning(
    result <- validate_leaflet_layer(layer),
    "Leaflet has limited support for 3D elevation"
  )
  expect_equal(result$type, "column")
  expect_equal(result$elevation, "height")
})

test_that("provider-specific layer validation works", {
  layer <- list(type = "scatterplot", id = "test")
  
  # Test each provider
  expect_identical(validate_mapbox_layer(layer), layer)
  expect_identical(validate_openlayers_layer(layer), layer)
  expect_identical(validate_gaode_layer(layer), layer)
  expect_identical(validate_baidu_layer(layer), layer)
  
  # Leaflet should warn for 3D layers
  layer_3d <- list(type = "column", elevation = "height")
  expect_warning(validate_leaflet_layer(layer_3d), "limited support for 3D elevation")
})

test_that("normalize_custom_style validates custom styles", {
  # Test valid Mapbox custom style
  mapbox_style <- list(version = 8, sources = list(), layers = list())
  result <- normalize_custom_style(mapbox_style, "mapbox")
  expect_identical(result, mapbox_style)
  
  # Test invalid Mapbox custom style
  expect_warning(
    normalize_custom_style(list(version = 8), "mapbox"),
    "Custom Mapbox style missing required fields"
  )
  
  # Test Leaflet custom style
  leaflet_style <- list(url = "https://example.com/{z}/{x}/{y}.png")
  result <- normalize_custom_style(leaflet_style, "leaflet")
  expect_identical(result, leaflet_style)
  
  # Test invalid Leaflet custom style
  expect_warning(
    normalize_custom_style(list(attribution = "test"), "leaflet"),
    "Custom Leaflet style missing URL"
  )
  
  # Test non-list input
  expect_error(normalize_custom_style("not_a_list", "mapbox"), "Custom style must be a list")
})