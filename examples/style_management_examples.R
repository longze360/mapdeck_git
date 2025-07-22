#' Style Management Examples
#'
#' This file demonstrates how to use the enhanced style management system
#' in mapdeck for consistent styling across different map providers.

# Load required libraries
library(mapdeck)

# =============================================================================
# Basic Style Usage
# =============================================================================

# Backward compatible usage (defaults to Mapbox)
dark_style <- mapdeck_style("dark")
print(paste("Dark style:", dark_style))

# Multi-provider usage
mapbox_dark <- mapdeck_style("dark", provider = "mapbox")
leaflet_dark <- mapdeck_style("dark", provider = "leaflet")
openlayers_dark <- mapdeck_style("dark", provider = "openlayers")

print(paste("Mapbox dark:", mapbox_dark))
print(paste("Leaflet dark:", leaflet_dark))
print(paste("OpenLayers dark:", openlayers_dark))

# =============================================================================
# Exploring Available Styles
# =============================================================================

# Get all available generic styles
all_styles <- get_available_styles()
print(paste("Available styles:", paste(all_styles, collapse = ", ")))

# Get styles by category
satellite_styles <- get_available_styles(category = "satellite")
print(paste("Satellite styles:", paste(satellite_styles, collapse = ", ")))

monochrome_styles <- get_available_styles(category = "monochrome")
print(paste("Monochrome styles:", paste(monochrome_styles, collapse = ", ")))

# Get provider-specific styles
mapbox_styles <- get_available_styles(provider = "mapbox")
print(paste("Mapbox styles count:", length(mapbox_styles)))

# =============================================================================
# Using the Style Resolver Directly
# =============================================================================

# Get the global style resolver
resolver <- get_style_resolver()

# Resolve styles for different providers
print("Style resolution examples:")
print(paste("  streets -> mapbox:", resolver$resolve_style("streets", "mapbox")))
print(paste("  streets -> leaflet:", resolver$resolve_style("streets", "leaflet")))
print(paste("  streets -> gaode:", resolver$resolve_style("streets", "gaode")))

# Get style categories
categories <- resolver$get_style_categories()
print(paste("Style categories:", paste(categories, collapse = ", ")))

# Add custom style mapping
resolver$add_style_mapping("custom_blue", "mapbox", "mapbox://styles/custom/blue")
custom_style <- resolver$resolve_style("custom_blue", "mapbox")
print(paste("Custom style:", custom_style))

# =============================================================================
# Style Validation
# =============================================================================

# Get the global style validator
validator <- get_style_validator()

# Validate different styles
print("Style validation examples:")
print(paste("  'streets' valid for mapbox:", validator$validate_style("streets", "mapbox")))
print(paste("  'invalid' valid for mapbox:", validator$validate_style("invalid", "mapbox")))

# Check style compatibility across providers
compatibility <- validator$get_style_compatibility("streets")
print("'streets' compatibility:")
for (provider in names(compatibility)) {
  print(paste("  ", provider, ":", compatibility[[provider]]))
}

# Validate provider-specific URLs
mapbox_url_valid <- validator$validate_style("mapbox://styles/mapbox/streets-v11", "mapbox")
print(paste("Mapbox URL valid:", mapbox_url_valid))

gaode_url_valid <- validator$validate_style("amap://styles/normal", "gaode")
print(paste("Gaode URL valid:", gaode_url_valid))

# =============================================================================
# Theme Management
# =============================================================================

# Get the global theme manager
theme_manager <- get_theme_manager()

# Get available themes
themes <- theme_manager$get_available_themes()
print(paste("Available themes:", paste(themes, collapse = ", ")))

# Apply themes to different providers
print("Theme application examples:")
light_mapbox <- apply_map_theme("light", "mapbox")
print(paste("  Light theme for Mapbox - style:", light_mapbox$style))
print(paste("  Light theme background color:", light_mapbox$colors$background))

dark_leaflet <- apply_map_theme("dark", "leaflet")
print(paste("  Dark theme for Leaflet - style:", dark_leaflet$style))
print(paste("  Dark theme text color:", dark_leaflet$colors$text))

# Get theme information
light_info <- theme_manager$get_theme_info("light")
print(paste("Light theme description:", light_info$description))
print(paste("Light theme supported providers:", paste(light_info$supported_providers, collapse = ", ")))

# Create custom theme
custom_theme_config <- list(
  "mapbox" = list(
    style = "mapbox://styles/mapbox/outdoors-v11",
    colors = list(
      background = "#f0f8e8",
      text = "#2d5016",
      accent = "#7cb342"
    ),
    options = list()
  ),
  "leaflet" = list(
    style = "OpenTopoMap",
    colors = list(
      background = "#f0f8e8",
      text = "#2d5016",
      accent = "#7cb342"
    ),
    options = list()
  )
)

theme_manager$create_custom_theme(
  name = "nature",
  description = "Nature-inspired green theme",
  style_base = "outdoors",
  provider_configs = custom_theme_config
)

# Apply custom theme
nature_theme <- apply_map_theme("nature", "mapbox")
print(paste("Custom nature theme style:", nature_theme$style))
print(paste("Custom nature theme accent:", nature_theme$colors$accent))

# =============================================================================
# Practical Usage Examples
# =============================================================================

# Example 1: Creating maps with consistent styling across providers
create_consistent_map <- function(provider, data) {
  # Use the same generic style name for all providers
  style <- mapdeck_style("dark", provider = provider)
  
  # Apply consistent theme
  theme_config <- apply_map_theme("dark", provider)
  
  print(paste("Creating", provider, "map with style:", style))
  print(paste("Theme background color:", theme_config$colors$background))
  
  # In a real application, you would create the map here:
  # map <- mapdeck(style = style, provider = provider) %>%
  #   add_scatterplot(data = data, ...)
  
  return(list(style = style, theme = theme_config))
}

# Simulate creating maps with different providers
providers <- c("mapbox", "leaflet", "openlayers")
for (provider in providers) {
  map_config <- create_consistent_map(provider, data.frame(x = 1, y = 1))
}

# Example 2: Style validation before map creation
validate_and_create_map <- function(style_name, provider) {
  # Validate style first
  if (!validate_map_style(style_name, provider)) {
    warning(paste("Style", style_name, "may not be compatible with", provider))
    # Fall back to default style
    style_name <- NULL
  }
  
  # Get the actual style
  style <- mapdeck_style(style_name, provider = provider, validate = FALSE)
  
  print(paste("Using style", style, "for provider", provider))
  return(style)
}

# Test with valid and invalid styles
validate_and_create_map("streets", "mapbox")
validate_and_create_map("invalid_style", "mapbox")

# Example 3: Dynamic style switching
switch_map_style <- function(current_provider, new_style) {
  # Resolve the new style for the current provider
  resolver <- get_style_resolver()
  resolved_style <- resolver$resolve_style(new_style, current_provider)
  
  # Validate the style
  validator <- get_style_validator()
  if (validator$validate_style(resolved_style, current_provider)) {
    print(paste("Switching to style:", resolved_style))
    # In a real application, you would update the map here
    return(resolved_style)
  } else {
    print("Style not compatible, keeping current style")
    return(NULL)
  }
}

# Test style switching
switch_map_style("mapbox", "satellite")
switch_map_style("leaflet", "terrain")

# =============================================================================
# Advanced Usage
# =============================================================================

# Example 4: Batch style operations
batch_style_operations <- function() {
  resolver <- get_style_resolver()
  validator <- get_style_validator()
  
  providers <- c("mapbox", "leaflet", "openlayers", "gaode", "baidu")
  styles <- c("streets", "dark", "satellite")
  
  results <- list()
  
  for (provider in providers) {
    provider_results <- list()
    
    for (style in styles) {
      resolved <- resolver$resolve_style(style, provider)
      valid <- validator$validate_style(resolved, provider)
      
      provider_results[[style]] <- list(
        resolved = resolved,
        valid = valid
      )
    }
    
    results[[provider]] <- provider_results
  }
  
  return(results)
}

# Run batch operations
batch_results <- batch_style_operations()
print("Batch style operations completed")

# Example 5: Style compatibility matrix
create_compatibility_matrix <- function() {
  validator <- get_style_validator()
  resolver <- get_style_resolver()
  
  styles <- resolver$get_available_styles()
  providers <- c("mapbox", "leaflet", "openlayers", "gaode", "baidu")
  
  matrix_data <- matrix(FALSE, nrow = length(styles), ncol = length(providers))
  rownames(matrix_data) <- styles
  colnames(matrix_data) <- providers
  
  for (i in seq_along(styles)) {
    for (j in seq_along(providers)) {
      matrix_data[i, j] <- validator$validate_style(styles[i], providers[j])
    }
  }
  
  return(matrix_data)
}

# Create and display compatibility matrix
compatibility_matrix <- create_compatibility_matrix()
print("Style compatibility matrix created")
print(paste("Matrix dimensions:", paste(dim(compatibility_matrix), collapse = "x")))

print("Style management examples completed successfully!")