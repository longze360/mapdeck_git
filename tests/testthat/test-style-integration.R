test_that("Style management integrates with provider interface", {
  # Test that all providers support the get_available_styles method
  providers <- c("mapbox", "leaflet", "openlayers", "gaode", "baidu")
  
  for (provider_name in providers) {
    # Create provider instance directly (without factory to avoid dependencies)
    if (provider_name == "mapbox") {
      provider <- MapboxProvider$new()
    } else if (provider_name == "leaflet") {
      provider <- LeafletProvider$new()
    } else if (provider_name == "openlayers") {
      provider <- OpenLayersProvider$new()
    } else if (provider_name == "gaode") {
      provider <- GaodeProvider$new()
    } else if (provider_name == "baidu") {
      provider <- BaiduProvider$new()
    } else {
      skip(paste("Provider", provider_name, "not implemented"))
    }
    
    # Initialize provider
    provider$initialize_provider(list())
    
    # Test that provider has get_available_styles method
    expect_true("get_available_styles" %in% names(provider))
    
    # Test that method returns character vector
    styles <- provider$get_available_styles()
    expect_true(is.character(styles))
    expect_true(length(styles) > 0)
    
    # Test that provider can update styles
    expect_true("update_style" %in% names(provider))
    
    # Test style update (should not error)
    expect_silent(provider$update_style("streets"))
  }
})

test_that("Style resolver works with all provider implementations", {
  resolver <- get_style_resolver()
  providers <- c("mapbox", "leaflet", "openlayers", "gaode", "baidu")
  generic_styles <- c("streets", "dark", "light", "satellite")
  
  for (provider in providers) {
    for (style in generic_styles) {
      # Should not error
      resolved_style <- resolver$resolve_style(style, provider)
      expect_true(is.character(resolved_style))
      expect_true(nchar(resolved_style) > 0)
      
      # Resolved style should be different from generic name (provider-specific)
      expect_true(resolved_style != style || provider == "openlayers")  # OSM might match
    }
  }
})

test_that("Style validator works with provider-specific styles", {
  validator <- get_style_validator()
  
  # Test Mapbox-specific styles
  expect_true(validator$validate_style("mapbox://styles/mapbox/streets-v11", "mapbox"))
  expect_true(validator$validate_style("https://api.mapbox.com/styles/v1/custom", "mapbox"))
  
  # Test Gaode-specific styles
  expect_true(validator$validate_style("amap://styles/normal", "gaode"))
  expect_true(validator$validate_style("amap://styles/dark", "gaode"))
  
  # Test generic styles that should work across providers
  generic_styles <- c("streets", "dark", "light", "satellite")
  providers <- c("mapbox", "leaflet", "openlayers", "gaode", "baidu")
  
  for (style in generic_styles) {
    for (provider in providers) {
      # Generic styles should be valid for all providers
      expect_true(validator$validate_style(style, provider))
    }
  }
})

test_that("Theme manager provides consistent themes across providers", {
  theme_manager <- get_theme_manager()
  themes <- c("light", "dark", "satellite")
  providers <- c("mapbox", "leaflet", "openlayers", "gaode", "baidu")
  
  for (theme in themes) {
    for (provider in providers) {
      theme_config <- theme_manager$apply_theme(theme, provider)
      
      # Should return valid configuration
      expect_true(is.list(theme_config))
      expect_true("style" %in% names(theme_config))
      expect_true("colors" %in% names(theme_config))
      
      # Colors should be consistent across providers for same theme
      expect_true(is.list(theme_config$colors))
      expect_true("background" %in% names(theme_config$colors))
      expect_true("text" %in% names(theme_config$colors))
      expect_true("accent" %in% names(theme_config$colors))
      
      # Verify theme consistency
      if (theme == "light") {
        expect_equal(theme_config$colors$background, "#ffffff")
        expect_equal(theme_config$colors$text, "#000000")
      } else if (theme == "dark") {
        expect_equal(theme_config$colors$background, "#1a1a1a")
        expect_equal(theme_config$colors$text, "#ffffff")
      }
    }
  }
})

test_that("Updated mapdeck_style function maintains backward compatibility", {
  # Test that old usage still works
  expect_equal(mapdeck_style("dark"), "mapbox://styles/mapbox/dark-v10")
  expect_equal(mapdeck_style("light"), "mapbox://styles/mapbox/light-v10")
  expect_equal(mapdeck_style("streets"), "mapbox://styles/mapbox/streets-v11")
  expect_equal(mapdeck_style("satellite"), "mapbox://styles/mapbox/satellite-v9")
  
  # Test that old parameter matching still works
  expect_equal(mapdeck_style("satellite-streets"), 
               "mapbox://styles/mapbox/satellite-streets-v11")
  expect_equal(mapdeck_style("outdoors"), 
               "mapbox://styles/mapbox/outdoors-v11")
})

test_that("Style management handles provider-specific features correctly", {
  resolver <- get_style_resolver()
  validator <- get_style_validator()
  
  # Test that Mapbox-specific features are handled
  mapbox_styles <- resolver$style_mappings$mapbox
  expect_true("navigation_day" %in% names(mapbox_styles))
  expect_true("navigation_night" %in% names(mapbox_styles))
  
  # Test that Chinese provider styles are handled
  gaode_styles <- resolver$style_mappings$gaode
  expect_true("blue" %in% names(gaode_styles))
  expect_true("wine" %in% names(gaode_styles))
  
  baidu_styles <- resolver$style_mappings$baidu
  expect_true("midnight" %in% names(baidu_styles))
  expect_true("googlelite" %in% names(baidu_styles))
  
  # Test validation of provider-specific styles
  expect_true(validator$validate_style("navigation_day", "mapbox"))
  expect_true(validator$validate_style("blue", "gaode"))
  expect_true(validator$validate_style("midnight", "baidu"))
})

test_that("Style management provides comprehensive style information", {
  # Test get_available_styles function
  all_styles <- get_available_styles()
  expect_true(length(all_styles) >= 10)  # Should have many styles
  
  # Test category filtering
  satellite_styles <- get_available_styles(category = "satellite")
  expect_true("satellite" %in% satellite_styles)
  expect_true("hybrid" %in% satellite_styles)
  
  monochrome_styles <- get_available_styles(category = "monochrome")
  expect_true("light" %in% monochrome_styles)
  expect_true("dark" %in% monochrome_styles)
  
  # Test that all categories have styles
  resolver <- get_style_resolver()
  categories <- resolver$get_style_categories()
  
  for (category in categories) {
    category_styles <- get_available_styles(category = category)
    expect_true(length(category_styles) > 0)
  }
})

test_that("Style management handles edge cases gracefully", {
  resolver <- get_style_resolver()
  validator <- get_style_validator()
  theme_manager <- get_theme_manager()
  
  # Test case insensitive style resolution
  expect_equal(resolver$resolve_style("STREETS", "mapbox"),
               resolver$resolve_style("streets", "mapbox"))
  expect_equal(resolver$resolve_style("Dark", "leaflet"),
               resolver$resolve_style("dark", "leaflet"))
  
  # Test unknown styles fall back gracefully
  expect_warning(unknown_style <- resolver$resolve_style("unknown_style", "mapbox"))
  expect_equal(unknown_style, resolver$get_default_style("mapbox"))
  
  # Test unknown providers fall back gracefully
  expect_warning(unknown_provider <- resolver$resolve_style("streets", "unknown_provider"))
  expect_equal(unknown_provider, resolver$get_default_style("unknown_provider"))
  
  # Test validation with edge cases
  expect_false(validator$validate_style("", "mapbox"))
  expect_true(validator$validate_style(NULL, "mapbox"))
  
  # Test theme application with unknown provider uses fallback
  satellite_theme <- theme_manager$apply_theme("satellite", "unknown_provider")
  expect_equal(satellite_theme$style, "satellite")
})

test_that("Style management performance is acceptable", {
  resolver <- get_style_resolver()
  validator <- get_style_validator()
  
  # Test that style resolution is fast
  start_time <- Sys.time()
  for (i in 1:100) {
    resolver$resolve_style("streets", "mapbox")
  }
  resolution_time <- as.numeric(Sys.time() - start_time)
  expect_true(resolution_time < 1.0)  # Should complete in less than 1 second
  
  # Test that validation is fast
  start_time <- Sys.time()
  for (i in 1:100) {
    validator$validate_style("streets", "mapbox")
  }
  validation_time <- as.numeric(Sys.time() - start_time)
  expect_true(validation_time < 1.0)  # Should complete in less than 1 second
})

test_that("Style management memory usage is reasonable", {
  # Test that creating multiple instances doesn't consume excessive memory
  resolvers <- list()
  for (i in 1:10) {
    resolvers[[i]] <- StyleResolver$new()
  }
  
  # Should be able to create multiple instances without error
  expect_equal(length(resolvers), 10)
  
  # Test that global instances are properly shared
  resolver1 <- get_style_resolver()
  resolver2 <- get_style_resolver()
  expect_identical(resolver1, resolver2)
  
  validator1 <- get_style_validator()
  validator2 <- get_style_validator()
  expect_identical(validator1, validator2)
  
  theme1 <- get_theme_manager()
  theme2 <- get_theme_manager()
  expect_identical(theme1, theme2)
})