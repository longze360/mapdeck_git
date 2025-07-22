test_that("StyleResolver can be created and resolves styles correctly", {
  resolver <- StyleResolver$new()
  
  # Test basic style resolution
  expect_equal(resolver$resolve_style("streets", "mapbox"), 
               "mapbox://styles/mapbox/streets-v11")
  expect_equal(resolver$resolve_style("dark", "leaflet"), 
               "CartoDB.DarkMatter")
  expect_equal(resolver$resolve_style("satellite", "openlayers"), 
               "ESRI.WorldImagery")
  
  # Test case insensitive resolution
  expect_equal(resolver$resolve_style("STREETS", "mapbox"), 
               "mapbox://styles/mapbox/streets-v11")
  expect_equal(resolver$resolve_style("Dark", "leaflet"), 
               "CartoDB.DarkMatter")
  
  # Test unknown style falls back to default
  expect_warning(result <- resolver$resolve_style("unknown_style", "mapbox"))
  expect_equal(result, resolver$get_default_style("mapbox"))
  
  # Test unknown provider falls back to default
  expect_warning(result <- resolver$resolve_style("streets", "unknown_provider"))
  expect_equal(result, resolver$get_default_style("unknown_provider"))
})

test_that("StyleResolver provides available styles and categories", {
  resolver <- StyleResolver$new()
  
  # Test getting all available styles
  all_styles <- resolver$get_available_styles()
  expect_true(is.character(all_styles))
  expect_true(length(all_styles) > 0)
  expect_true("streets" %in% all_styles)
  expect_true("dark" %in% all_styles)
  expect_true("satellite" %in% all_styles)
  
  # Test getting styles by category
  basic_styles <- resolver$get_available_styles("basic")
  expect_true(is.character(basic_styles))
  expect_true("streets" %in% basic_styles)
  expect_true("outdoors" %in% basic_styles)
  
  satellite_styles <- resolver$get_available_styles("satellite")
  expect_true("satellite" %in% satellite_styles)
  
  # Test unknown category
  expect_warning(unknown_styles <- resolver$get_available_styles("unknown"))
  expect_equal(length(unknown_styles), 0)
  
  # Test getting style categories
  categories <- resolver$get_style_categories()
  expect_true(is.character(categories))
  expect_true("basic" %in% categories)
  expect_true("satellite" %in% categories)
  expect_true("monochrome" %in% categories)
})

test_that("StyleResolver can add custom style mappings", {
  resolver <- StyleResolver$new()
  
  # Add custom mapping
  resolver$add_style_mapping("custom_style", "mapbox", "mapbox://styles/custom/style")
  
  # Test that custom mapping works
  expect_equal(resolver$resolve_style("custom_style", "mapbox"), 
               "mapbox://styles/custom/style")
  
  # Test that it appears in available styles
  all_styles <- resolver$get_available_styles()
  expect_true("custom_style" %in% all_styles)
  
  # Test validation errors
  expect_error(resolver$add_style_mapping(123, "mapbox", "style"))
  expect_error(resolver$add_style_mapping("style", 123, "style"))
  expect_error(resolver$add_style_mapping("style", "mapbox", 123))
})

test_that("StyleValidator validates styles correctly", {
  validator <- StyleValidator$new()
  
  # Test valid generic styles
  expect_true(validator$validate_style("streets", "mapbox"))
  expect_true(validator$validate_style("dark", "leaflet"))
  expect_true(validator$validate_style("satellite", "openlayers"))
  
  # Test NULL style (should be valid - uses default)
  expect_true(validator$validate_style(NULL, "mapbox"))
  
  # Test provider-specific URL patterns
  expect_true(validator$validate_style("mapbox://styles/mapbox/streets-v11", "mapbox"))
  expect_true(validator$validate_style("https://example.com/style.json", "mapbox"))
  expect_true(validator$validate_style("amap://styles/normal", "gaode"))
  
  # Test invalid styles
  expect_false(validator$validate_style("invalid_style_name", "mapbox"))
  expect_false(validator$validate_style(123, "mapbox"))
  
  # Test unknown provider
  expect_warning(result <- validator$validate_style("streets", "unknown_provider"))
  expect_false(result)
})

test_that("StyleValidator handles custom style objects", {
  validator <- StyleValidator$new()
  
  # Test valid Mapbox custom style
  mapbox_style <- list(
    version = 8,
    sources = list(),
    layers = list()
  )
  expect_true(validator$validate_style(mapbox_style, "mapbox"))
  
  # Test invalid custom style (missing required fields)
  invalid_style <- list(version = 8)
  expect_false(validator$validate_style(invalid_style, "mapbox"))
  
  # Test custom style for other providers (should be valid)
  generic_style <- list(custom = TRUE)
  expect_true(validator$validate_style(generic_style, "leaflet"))
})

test_that("StyleValidator provides style compatibility information", {
  validator <- StyleValidator$new()
  
  # Test compatibility for generic style
  compatibility <- validator$get_style_compatibility("streets")
  expect_true(is.list(compatibility))
  expect_true("mapbox" %in% names(compatibility))
  expect_true("leaflet" %in% names(compatibility))
  expect_true(compatibility$mapbox)
  expect_true(compatibility$leaflet)
  
  # Test compatibility for provider-specific style
  mapbox_compatibility <- validator$get_style_compatibility("mapbox://styles/mapbox/streets-v11")
  expect_true(mapbox_compatibility$mapbox)
  # Other providers might not support Mapbox URLs
  
  # Test validation error
  expect_error(validator$get_style_compatibility(123))
})

test_that("ThemeManager can be created and applies themes correctly", {
  theme_manager <- ThemeManager$new()
  
  # Test getting available themes
  themes <- theme_manager$get_available_themes()
  expect_true(is.character(themes))
  expect_true("light" %in% themes)
  expect_true("dark" %in% themes)
  expect_true("satellite" %in% themes)
  
  # Test applying themes
  light_theme <- theme_manager$apply_theme("light", "mapbox")
  expect_true(is.list(light_theme))
  expect_true("style" %in% names(light_theme))
  expect_true("colors" %in% names(light_theme))
  
  dark_theme <- theme_manager$apply_theme("dark", "leaflet")
  expect_true(is.list(dark_theme))
  expect_equal(dark_theme$style, "CartoDB.DarkMatter")
  
  # Test current theme tracking
  expect_equal(theme_manager$current_theme, "dark")
  
  # Test unknown theme
  expect_error(theme_manager$apply_theme("unknown_theme", "mapbox"))
})

test_that("ThemeManager provides theme information", {
  theme_manager <- ThemeManager$new()
  
  # Test getting theme info
  light_info <- theme_manager$get_theme_info("light")
  expect_true(is.list(light_info))
  expect_equal(light_info$name, "light")
  expect_true("description" %in% names(light_info))
  expect_true("style_base" %in% names(light_info))
  expect_true("supported_providers" %in% names(light_info))
  
  # Test unknown theme
  expect_error(theme_manager$get_theme_info("unknown_theme"))
})

test_that("ThemeManager can create custom themes", {
  theme_manager <- ThemeManager$new()
  
  # Create custom theme
  provider_configs <- list(
    "mapbox" = list(
      style = "mapbox://styles/custom/theme",
      colors = list(background = "#ff0000"),
      options = list()
    )
  )
  
  theme_manager$create_custom_theme(
    name = "custom_red",
    description = "Custom red theme",
    style_base = "custom",
    provider_configs = provider_configs
  )
  
  # Test that custom theme is available
  themes <- theme_manager$get_available_themes()
  expect_true("custom_red" %in% themes)
  
  # Test applying custom theme
  custom_theme <- theme_manager$apply_theme("custom_red", "mapbox")
  expect_equal(custom_theme$style, "mapbox://styles/custom/theme")
  expect_equal(custom_theme$colors$background, "#ff0000")
  
  # Test validation errors
  expect_error(theme_manager$create_custom_theme(123, "desc", "base", list()))
  expect_error(theme_manager$create_custom_theme("name", 123, "base", list()))
  expect_error(theme_manager$create_custom_theme("name", "desc", 123, list()))
  expect_error(theme_manager$create_custom_theme("name", "desc", "base", "not_list"))
})

test_that("Global style management functions work correctly", {
  # Test global resolver
  resolver <- get_style_resolver()
  expect_true(inherits(resolver, "StyleResolver"))
  
  # Test that multiple calls return same instance
  resolver2 <- get_style_resolver()
  expect_identical(resolver, resolver2)
  
  # Test global validator
  validator <- get_style_validator()
  expect_true(inherits(validator, "StyleValidator"))
  
  # Test global theme manager
  theme_manager <- get_theme_manager()
  expect_true(inherits(theme_manager, "ThemeManager"))
})

test_that("Updated mapdeck_style function works correctly", {
  # Test backward compatibility (default to mapbox)
  expect_equal(mapdeck_style("dark"), "mapbox://styles/mapbox/dark-v10")
  expect_equal(mapdeck_style("streets"), "mapbox://styles/mapbox/streets-v11")
  
  # Test with explicit provider
  expect_equal(mapdeck_style("dark", provider = "mapbox"), 
               "mapbox://styles/mapbox/dark-v10")
  expect_equal(mapdeck_style("dark", provider = "leaflet"), 
               "CartoDB.DarkMatter")
  expect_equal(mapdeck_style("satellite", provider = "openlayers"), 
               "ESRI.WorldImagery")
  
  # Test with NULL style (uses default)
  mapbox_default <- mapdeck_style(NULL, provider = "mapbox")
  expect_equal(mapbox_default, "mapbox://styles/mapbox/streets-v11")
  
  leaflet_default <- mapdeck_style(NULL, provider = "leaflet")
  expect_equal(leaflet_default, "OpenStreetMap")
  
  # Test validation
  expect_warning(mapdeck_style("invalid_style", provider = "mapbox", validate = TRUE))
  
  # Test without validation
  expect_silent(mapdeck_style("invalid_style", provider = "mapbox", validate = FALSE))
  
  # Test validation errors
  expect_error(mapdeck_style("dark", provider = 123))
  expect_error(mapdeck_style(123, provider = "mapbox"))
})

test_that("get_available_styles function works correctly", {
  # Test getting generic styles
  generic_styles <- get_available_styles()
  expect_true(is.character(generic_styles))
  expect_true(length(generic_styles) > 0)
  expect_true("streets" %in% generic_styles)
  expect_true("dark" %in% generic_styles)
  
  # Test getting styles by category
  satellite_styles <- get_available_styles(category = "satellite")
  expect_true(is.character(satellite_styles))
  expect_true("satellite" %in% satellite_styles)
  
  # Test getting provider-specific styles (may fall back to generic)
  mapbox_styles <- get_available_styles(provider = "mapbox")
  expect_true(is.character(mapbox_styles))
  expect_true(length(mapbox_styles) > 0)
  
  # Test validation error
  expect_error(get_available_styles(provider = 123))
})

test_that("apply_map_theme function works correctly", {
  # Test applying themes
  light_config <- apply_map_theme("light", "mapbox")
  expect_true(is.list(light_config))
  expect_true("style" %in% names(light_config))
  expect_true("colors" %in% names(light_config))
  
  dark_config <- apply_map_theme("dark", "leaflet")
  expect_true(is.list(dark_config))
  expect_equal(dark_config$style, "CartoDB.DarkMatter")
  
  # Test validation errors
  expect_error(apply_map_theme(123, "mapbox"))
  expect_error(apply_map_theme("light", 123))
  expect_error(apply_map_theme("unknown_theme", "mapbox"))
})

test_that("validate_map_style function works correctly", {
  # Test valid styles
  expect_true(validate_map_style("streets", "mapbox"))
  expect_true(validate_map_style("dark", "leaflet"))
  expect_true(validate_map_style(NULL, "mapbox"))
  
  # Test invalid styles
  expect_false(validate_map_style("invalid_style", "mapbox"))
  
  # Test validation error
  expect_error(validate_map_style("streets", 123))
})

test_that("Style management integrates with provider system", {
  # Test that style resolution works with actual provider configurations
  resolver <- get_style_resolver()
  
  # Test all supported providers
  providers <- c("mapbox", "leaflet", "openlayers", "gaode", "baidu")
  styles <- c("streets", "dark", "satellite")
  
  for (provider in providers) {
    for (style in styles) {
      resolved <- resolver$resolve_style(style, provider)
      expect_true(is.character(resolved))
      expect_true(nchar(resolved) > 0)
      
      # Validate the resolved style
      validator <- get_style_validator()
      # Note: Some styles might not be fully compatible, so we don't assert TRUE
      validation_result <- validator$validate_style(resolved, provider)
      expect_true(is.logical(validation_result))
    }
  }
})

test_that("Style management handles edge cases correctly", {
  resolver <- StyleResolver$new()
  validator <- StyleValidator$new()
  theme_manager <- ThemeManager$new()
  
  # Test empty strings
  expect_warning(result <- resolver$resolve_style("", "mapbox"))
  expect_equal(result, resolver$get_default_style("mapbox"))
  
  # Test very long style names
  long_style <- paste(rep("a", 1000), collapse = "")
  expect_warning(result <- resolver$resolve_style(long_style, "mapbox"))
  expect_equal(result, resolver$get_default_style("mapbox"))
  
  # Test special characters in style names
  special_style <- "style-with-special_chars.123"
  expect_warning(result <- resolver$resolve_style(special_style, "mapbox"))
  expect_equal(result, resolver$get_default_style("mapbox"))
  
  # Test validation with edge cases
  expect_false(validator$validate_style("", "mapbox"))
  expect_false(validator$validate_style(list(), "mapbox"))  # Empty list
  
  # Test theme manager with fallback
  satellite_theme <- theme_manager$apply_theme("satellite", "unknown_provider")
  expect_true(is.list(satellite_theme))
  expect_equal(satellite_theme$style, "satellite")  # Should use fallback
})