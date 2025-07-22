#!/usr/bin/env Rscript

# Load required packages
library(R6)

# Source the required files
source("R/style-management.R")
source("R/map_styles.R")

# Test style management system
cat("Testing Style Management System...\n")

# Test StyleResolver
cat("\n=== Testing StyleResolver ===\n")
resolver <- StyleResolver$new()

# Test basic style resolution
cat("Testing basic style resolution:\n")
mapbox_streets <- resolver$resolve_style("streets", "mapbox")
cat("  Mapbox streets:", mapbox_streets, "\n")

leaflet_dark <- resolver$resolve_style("dark", "leaflet")
cat("  Leaflet dark:", leaflet_dark, "\n")

openlayers_satellite <- resolver$resolve_style("satellite", "openlayers")
cat("  OpenLayers satellite:", openlayers_satellite, "\n")

# Test available styles
available_styles <- resolver$get_available_styles()
cat("  Available styles count:", length(available_styles), "\n")
cat("  First few styles:", paste(head(available_styles, 5), collapse = ", "), "\n")

# Test style categories
categories <- resolver$get_style_categories()
cat("  Style categories:", paste(categories, collapse = ", "), "\n")

basic_styles <- resolver$get_available_styles("basic")
cat("  Basic styles:", paste(basic_styles, collapse = ", "), "\n")

# Test StyleValidator
cat("\n=== Testing StyleValidator ===\n")
validator <- StyleValidator$new()

# Test style validation
cat("Testing style validation:\n")
is_valid_streets <- validator$validate_style("streets", "mapbox")
cat("  'streets' valid for mapbox:", is_valid_streets, "\n")

is_valid_url <- validator$validate_style("mapbox://styles/mapbox/streets-v11", "mapbox")
cat("  Mapbox URL valid for mapbox:", is_valid_url, "\n")

is_valid_invalid <- validator$validate_style("invalid_style", "mapbox")
cat("  'invalid_style' valid for mapbox:", is_valid_invalid, "\n")

# Test compatibility
compatibility <- validator$get_style_compatibility("streets")
cat("  'streets' compatibility across providers:\n")
for (provider in names(compatibility)) {
  cat("    ", provider, ":", compatibility[[provider]], "\n")
}

# Test ThemeManager
cat("\n=== Testing ThemeManager ===\n")
theme_manager <- ThemeManager$new()

# Test available themes
available_themes <- theme_manager$get_available_themes()
cat("Available themes:", paste(available_themes, collapse = ", "), "\n")

# Test applying themes
cat("Testing theme application:\n")
light_theme <- theme_manager$apply_theme("light", "mapbox")
cat("  Light theme for mapbox - style:", light_theme$style, "\n")
cat("  Light theme colors - background:", light_theme$colors$background, "\n")

dark_theme <- theme_manager$apply_theme("dark", "leaflet")
cat("  Dark theme for leaflet - style:", dark_theme$style, "\n")
cat("  Dark theme colors - text:", dark_theme$colors$text, "\n")

# Test theme info
light_info <- theme_manager$get_theme_info("light")
cat("  Light theme description:", light_info$description, "\n")

# Test updated mapdeck_style function
cat("\n=== Testing Updated mapdeck_style Function ===\n")

# Test backward compatibility
old_style <- mapdeck_style("dark")
cat("Backward compatible dark style:", old_style, "\n")

# Test with provider parameter
mapbox_dark <- mapdeck_style("dark", provider = "mapbox")
cat("Mapbox dark style:", mapbox_dark, "\n")

leaflet_dark <- mapdeck_style("dark", provider = "leaflet")
cat("Leaflet dark style:", leaflet_dark, "\n")

openlayers_streets <- mapdeck_style("streets", provider = "openlayers")
cat("OpenLayers streets style:", openlayers_streets, "\n")

# Test with NULL style (default)
mapbox_default <- mapdeck_style(NULL, provider = "mapbox")
cat("Mapbox default style:", mapbox_default, "\n")

# Test new utility functions
cat("\n=== Testing Utility Functions ===\n")

# Test get_available_styles
generic_styles <- get_available_styles()
cat("Generic styles count:", length(generic_styles), "\n")

satellite_styles <- get_available_styles(category = "satellite")
cat("Satellite styles:", paste(satellite_styles, collapse = ", "), "\n")

# Test apply_map_theme
theme_config <- apply_map_theme("dark", "mapbox")
cat("Applied dark theme style:", theme_config$style, "\n")

# Test validate_map_style
is_streets_valid <- validate_map_style("streets", "mapbox")
cat("'streets' valid for mapbox:", is_streets_valid, "\n")

# Test global instances
cat("\n=== Testing Global Instances ===\n")
global_resolver <- get_style_resolver()
global_validator <- get_style_validator()
global_theme_manager <- get_theme_manager()

cat("Global resolver class:", class(global_resolver)[1], "\n")
cat("Global validator class:", class(global_validator)[1], "\n")
cat("Global theme manager class:", class(global_theme_manager)[1], "\n")

# Test that multiple calls return same instance
resolver2 <- get_style_resolver()
cat("Same resolver instance:", identical(global_resolver, resolver2), "\n")

cat("\nStyle Management System test completed successfully!\n")