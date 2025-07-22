test_that("LeafletProvider can be created", {
  provider <- LeafletProvider$new()
  
  expect_s3_class(provider, "LeafletProvider")
  expect_s3_class(provider, "IMapProvider")
  expect_equal(provider$provider_name, "leaflet")
  expect_false(provider$initialized)
})

test_that("LeafletProvider can be initialized", {
  provider <- LeafletProvider$new()
  
  # Test basic initialization
  provider$initialize_provider(list(tile_provider = "OpenStreetMap"))
  
  expect_true(provider$initialized)
  expect_equal(provider$tile_provider, "OpenStreetMap")
  expect_equal(provider$current_style, "OpenStreetMap")
})

test_that("LeafletProvider initialization with custom config", {
  provider <- LeafletProvider$new()
  
  config <- list(
    tile_provider = "CartoDB.Positron",
    max_zoom = 15,
    min_zoom = 2,
    zoom = 8,
    center = c(-74, 40.7),
    attribution = "Custom attribution"
  )
  
  provider$initialize_provider(config)
  
  expect_true(provider$initialized)
  expect_equal(provider$tile_provider, "CartoDB.Positron")
  expect_equal(provider$leaflet_config$max_zoom, 15)
  expect_equal(provider$leaflet_config$min_zoom, 2)
  expect_equal(provider$leaflet_config$zoom, 8)
  expect_equal(provider$leaflet_config$center, c(-74, 40.7))
  expect_equal(provider$leaflet_config$attribution, "Custom attribution")
})

test_that("LeafletProvider validates configuration", {
  provider <- LeafletProvider$new()
  
  # Valid configuration
  valid_config <- list(tile_provider = "OpenStreetMap")
  expect_true(provider$validate_config(valid_config))
  
  # Invalid tile provider type
  invalid_config1 <- list(tile_provider = c("provider1", "provider2"))
  expect_false(provider$validate_config(invalid_config1))
  
  # Invalid tile URL
  invalid_config2 <- list(tile_url = "not-a-url")
  expect_warning(provider$validate_config(invalid_config2))
})

test_that("LeafletProvider can update style", {
  provider <- LeafletProvider$new()
  provider$initialize_provider(list(tile_provider = "OpenStreetMap"))
  
  # Update to a different tile provider
  provider$update_style("CartoDB.DarkMatter")
  
  expect_equal(provider$current_style, "CartoDB.DarkMatter")
  expect_equal(provider$tile_provider, "CartoDB.DarkMatter")
  expect_equal(provider$leaflet_config$tile_provider, "CartoDB.DarkMatter")
})

test_that("LeafletProvider can set view", {
  provider <- LeafletProvider$new()
  provider$initialize_provider(list())
  
  # Set view
  provider$set_view(-74, 40.7, 10)
  
  expect_equal(provider$leaflet_config$center, c(-74, 40.7))
  expect_equal(provider$leaflet_config$zoom, 10)
})

test_that("LeafletProvider validates view parameters", {
  provider <- LeafletProvider$new()
  provider$initialize_provider(list())
  
  # Invalid longitude
  expect_error(provider$set_view(200, 40.7, 10), "Longitude must be")
  
  # Invalid latitude
  expect_error(provider$set_view(-74, 100, 10), "Latitude must be")
  
  # Invalid zoom
  expect_error(provider$set_view(-74, 40.7, -1), "Zoom must be")
})

test_that("LeafletProvider warns about unsupported features", {
  provider <- LeafletProvider$new()
  provider$initialize_provider(list())
  
  # Pitch and bearing are not supported by Leaflet
  expect_warning(provider$set_view(-74, 40.7, 10, pitch = 30), 
                 "does not support pitch")
  expect_warning(provider$set_view(-74, 40.7, 10, bearing = 45), 
                 "does not support bearing")
})

test_that("LeafletProvider can manage layers", {
  provider <- LeafletProvider$new()
  provider$initialize_provider(list())
  
  # Add layer
  layer <- list(
    id = "test_layer",
    type = "ScatterplotLayer",
    data = data.frame(x = 1, y = 1)
  )
  
  provider$add_layer(layer)
  expect_true("test_layer" %in% names(provider$layers))
  
  # Remove layer
  provider$remove_layer("test_layer")
  expect_false("test_layer" %in% names(provider$layers))
})

test_that("LeafletProvider returns available styles", {
  provider <- LeafletProvider$new()
  
  # Get all styles
  all_styles <- provider$get_available_styles()
  expect_true(length(all_styles) > 0)
  expect_true("OpenStreetMap" %in% all_styles)
  expect_true("CartoDB.Positron" %in% all_styles)
  expect_true("Esri.WorldImagery" %in% all_styles)
  
  # Get styles by category
  basic_styles <- provider$get_available_styles("basic")
  expect_true("OpenStreetMap" %in% basic_styles)
  
  satellite_styles <- provider$get_available_styles("satellite")
  expect_true("Esri.WorldImagery" %in% satellite_styles)
  
  # Invalid category
  expect_warning(provider$get_available_styles("invalid_category"))
})

test_that("LeafletProvider can be destroyed", {
  provider <- LeafletProvider$new()
  provider$initialize_provider(list())
  
  # Add some data
  provider$add_layer(list(id = "test", type = "ScatterplotLayer"))
  
  # Destroy
  provider$destroy()
  
  expect_false(provider$initialized)
  expect_equal(length(provider$layers), 0)
  expect_null(provider$leaflet_config)
  expect_null(provider$tile_provider)
  expect_null(provider$current_style)
})

test_that("LeafletProvider enhanced features work correctly", {
  provider <- LeafletProvider$new()
  provider$initialize_provider(list())
  
  # Test get_tile_provider_config
  config <- provider$get_tile_provider_config("OpenStreetMap")
  expect_true(is.list(config))
  expect_true("url" %in% names(config))
  expect_true("attribution" %in% names(config))
  expect_false(config$requires_api_key)
  
  # Test unknown provider config
  unknown_config <- provider$get_tile_provider_config("UnknownProvider")
  expect_true(is.list(unknown_config))
  expect_null(unknown_config$url)
  
  # Test create_custom_tile_layer
  custom_layer <- provider$create_custom_tile_layer(
    url = "https://example.com/{z}/{x}/{y}.png",
    attribution = "Custom tiles",
    max_zoom = 15,
    min_zoom = 2
  )
  
  expect_equal(custom_layer$url, "https://example.com/{z}/{x}/{y}.png")
  expect_equal(custom_layer$attribution, "Custom tiles")
  expect_equal(custom_layer$max_zoom, 15)
  expect_equal(custom_layer$min_zoom, 2)
  expect_false(custom_layer$requires_api_key)
  
  # Test set_tile_provider_options
  provider$set_tile_provider_options(list(max_zoom = 16, detect_retina = FALSE))
  expect_equal(provider$leaflet_config$max_zoom, 16)
  expect_false(provider$leaflet_config$detect_retina)
})

test_that("LeafletProvider custom tile layer validation works", {
  provider <- LeafletProvider$new()
  
  # Test invalid URL
  expect_error(
    provider$create_custom_tile_layer("not-a-url"),
    "URL must start with http"
  )
  
  # Test missing placeholders
  expect_error(
    provider$create_custom_tile_layer("https://example.com/tile.png"),
    "URL must contain \\{z\\}, \\{x\\}, and \\{y\\} placeholders"
  )
  
  # Test invalid zoom levels
  expect_error(
    provider$create_custom_tile_layer(
      "https://example.com/{z}/{x}/{y}.png",
      max_zoom = 25
    ),
    "max_zoom must be a single numeric value between 1 and 20"
  )
  
  expect_error(
    provider$create_custom_tile_layer(
      "https://example.com/{z}/{x}/{y}.png",
      min_zoom = 15,
      max_zoom = 10
    ),
    "min_zoom must be a single numeric value between 0 and max_zoom"
  )
})

test_that("LeafletProvider enhanced get_available_styles works", {
  provider <- LeafletProvider$new()
  
  # Test basic styles (no API key required)
  basic_styles <- provider$get_available_styles("basic")
  expect_true("OpenStreetMap" %in% basic_styles)
  expect_true(length(basic_styles) > 0)
  
  # Test with API key required providers
  terrain_styles_basic <- provider$get_available_styles("terrain", include_api_key_required = FALSE)
  terrain_styles_full <- provider$get_available_styles("terrain", include_api_key_required = TRUE)
  
  expect_true(length(terrain_styles_full) >= length(terrain_styles_basic))
  expect_true("Thunderforest.Landscape" %in% terrain_styles_full)
  
  # Test all styles
  all_styles <- provider$get_available_styles()
  expect_true(length(all_styles) > 10)
  expect_true("OpenStreetMap" %in% all_styles)
  expect_true("CartoDB.Positron" %in% all_styles)
})

test_that("LeafletProvider integration with deck.gl layers", {
  provider <- LeafletProvider$new()
  provider$initialize_provider(list())
  
  # Test adding multiple layers
  layer1 <- list(
    id = "scatterplot_layer",
    type = "ScatterplotLayer",
    data = data.frame(longitude = c(-74, -73), latitude = c(40.7, 40.8))
  )
  
  layer2 <- list(
    id = "arc_layer", 
    type = "ArcLayer",
    data = data.frame(
      source_lng = -74, source_lat = 40.7,
      target_lng = -73, target_lat = 40.8
    )
  )
  
  provider$add_layer(layer1)
  provider$add_layer(layer2)
  
  expect_equal(length(provider$layers), 2)
  expect_true("scatterplot_layer" %in% names(provider$layers))
  expect_true("arc_layer" %in% names(provider$layers))
  
  # Test removing specific layer
  provider$remove_layer("scatterplot_layer")
  expect_equal(length(provider$layers), 1)
  expect_false("scatterplot_layer" %in% names(provider$layers))
  expect_true("arc_layer" %in% names(provider$layers))
})

test_that("LeafletProvider error handling works correctly", {
  provider <- LeafletProvider$new()
  
  # Test operations before initialization
  expect_error(provider$create_map(), "Provider must be initialized")
  expect_error(provider$update_style("new_style"), "Provider must be initialized")
  expect_error(provider$add_layer(list()), "Provider must be initialized")
  expect_error(provider$remove_layer("test"), "Provider must be initialized")
  expect_error(provider$set_view(0, 0, 10), "Provider must be initialized")
  
  # Initialize for further tests
  provider$initialize_provider(list())
  
  # Test invalid layer ID
  expect_error(provider$remove_layer(c("id1", "id2")), "Layer ID must be a single character string")
  
  # Test invalid tile provider options
  expect_error(provider$set_tile_provider_options("not_a_list"), "Options must be a list")
  
  # Test warning for unknown options
  expect_warning(provider$set_tile_provider_options(list(unknown_option = "value")))
})

test_that("normalize_leaflet_tile_provider works correctly", {
  # Test aliases
  expect_equal(normalize_leaflet_tile_provider("osm"), "OpenStreetMap")
  expect_equal(normalize_leaflet_tile_provider("cartodb"), "CartoDB.Positron")
  expect_equal(normalize_leaflet_tile_provider("dark"), "CartoDB.DarkMatter")
  expect_equal(normalize_leaflet_tile_provider("satellite"), "Esri.WorldImagery")
  
  # Test case insensitive
  expect_equal(normalize_leaflet_tile_provider("OSM"), "OpenStreetMap")
  expect_equal(normalize_leaflet_tile_provider("CARTODB"), "CartoDB.Positron")
  
  # Test unknown provider (should return as-is)
  expect_equal(normalize_leaflet_tile_provider("CustomProvider"), "CustomProvider")
  
  # Test NULL (should return default)
  expect_equal(normalize_leaflet_tile_provider(NULL), "OpenStreetMap")
  
  # Test invalid input
  expect_error(normalize_leaflet_tile_provider(c("provider1", "provider2")))
})