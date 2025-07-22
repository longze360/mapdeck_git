#' Mapdeck Style
#'
#' Get provider-specific map styles using the enhanced style management system.
#' This function now supports multiple map providers and provides consistent
#' style names across different backends.
#'
#' @param style Character string specifying the style name. Can be a generic style
#'   name (e.g., "streets", "dark", "satellite") or a provider-specific style
#'   identifier. If NULL, uses the provider's default style.
#' @param provider Character string specifying the map provider. One of "mapbox",
#'   "leaflet", "openlayers", "gaode", or "baidu". If NULL, defaults to "mapbox"
#'   for backward compatibility.
#' @param validate Logical indicating whether to validate the style before returning.
#'   Default is TRUE.
#'
#' @return Character string containing the provider-specific style identifier.
#'
#' @examples
#' \donttest{
#'
#' ## Basic usage with Mapbox (backward compatible)
#' mapdeck_style("dark")
#' mapdeck_style("streets")
#'
#' ## Multi-provider usage
#' mapdeck_style("dark", provider = "mapbox")
#' mapdeck_style("dark", provider = "leaflet")
#' mapdeck_style("satellite", provider = "openlayers")
#'
#' ## Chinese providers
#' mapdeck_style("streets", provider = "gaode")
#' mapdeck_style("satellite", provider = "baidu")
#'
#' ## Get available styles
#' resolver <- get_style_resolver()
#' available_styles <- resolver$get_available_styles()
#' print(available_styles)
#'
#' }
#'
#' @export
mapdeck_style <- function(style = NULL, provider = NULL, validate = TRUE) {
  # Default provider for backward compatibility
  if (is.null(provider)) {
    provider <- "mapbox"
  }
  
  # Validate provider
  if (!is.character(provider) || length(provider) != 1) {
    stop("Provider must be a single character string")
  }
  
  # Get style resolver
  resolver <- get_style_resolver()
  
  # Handle NULL style - use provider default
  if (is.null(style)) {
    resolved_style <- resolver$get_default_style(provider)
  } else {
    # Validate style parameter
    if (!is.character(style) || length(style) != 1) {
      stop("Style must be a single character string or NULL")
    }
    
    # Resolve generic style to provider-specific style
    resolved_style <- resolver$resolve_style(style, provider)
  }
  
  # Validate style if requested
  if (validate) {
    validator <- get_style_validator()
    if (!validator$validate_style(resolved_style, provider)) {
      warning(sprintf("Style '%s' may not be fully compatible with provider '%s'", 
                     resolved_style, provider))
    }
  }
  
  return(resolved_style)
}

#' Get Available Map Styles
#'
#' Get list of available map styles for a specific provider or all providers.
#'
#' @param provider Character string specifying the map provider. If NULL,
#'   returns generic style names that work across providers.
#' @param category Character string to filter styles by category (optional).
#'   Categories include "basic", "monochrome", "satellite", "terrain", "artistic".
#'
#' @return Character vector of available style names.
#'
#' @examples
#' \donttest{
#'
#' ## Get all generic styles
#' get_available_styles()
#'
#' ## Get styles by category
#' get_available_styles(category = "satellite")
#' get_available_styles(category = "monochrome")
#'
#' ## Get provider-specific styles
#' get_available_styles(provider = "mapbox")
#' get_available_styles(provider = "leaflet")
#'
#' }
#'
#' @export
get_available_styles <- function(provider = NULL, category = NULL) {
  resolver <- get_style_resolver()
  
  if (is.null(provider)) {
    # Return generic style names
    return(resolver$get_available_styles(category = category))
  }
  
  # Validate provider
  if (!is.character(provider) || length(provider) != 1) {
    stop("Provider must be a single character string")
  }
  
  # Get provider-specific styles through the provider instance
  tryCatch({
    factory <- get_provider_factory()
    provider_instance <- factory$create_provider(provider, list())
    
    if (methods::hasMethod("get_available_styles", class(provider_instance))) {
      return(provider_instance$get_available_styles(category = category))
    } else {
      # Fallback to generic styles
      return(resolver$get_available_styles(category = category))
    }
  }, error = function(e) {
    # Fallback to generic styles if provider creation fails
    return(resolver$get_available_styles(category = category))
  })
}

#' Apply Map Theme
#'
#' Apply a consistent theme across different map providers.
#'
#' @param theme_name Character string specifying the theme name.
#'   Available themes: "light", "dark", "satellite".
#' @param provider Character string specifying the map provider.
#'
#' @return List containing theme configuration for the provider.
#'
#' @examples
#' \donttest{
#'
#' ## Apply themes to different providers
#' apply_map_theme("dark", "mapbox")
#' apply_map_theme("light", "leaflet")
#' apply_map_theme("satellite", "openlayers")
#'
#' ## Get available themes
#' theme_manager <- get_theme_manager()
#' available_themes <- theme_manager$get_available_themes()
#' print(available_themes)
#'
#' }
#'
#' @export
apply_map_theme <- function(theme_name, provider) {
  if (!is.character(theme_name) || length(theme_name) != 1) {
    stop("Theme name must be a single character string")
  }
  
  if (!is.character(provider) || length(provider) != 1) {
    stop("Provider must be a single character string")
  }
  
  theme_manager <- get_theme_manager()
  return(theme_manager$apply_theme(theme_name, provider))
}

#' Validate Map Style
#'
#' Validate that a style is compatible with a specific provider.
#'
#' @param style Character string or list containing style specification.
#' @param provider Character string specifying the map provider.
#'
#' @return Logical indicating if the style is valid for the provider.
#'
#' @examples
#' \donttest{
#'
#' ## Validate styles for different providers
#' validate_map_style("streets", "mapbox")
#' validate_map_style("OpenStreetMap", "leaflet")
#' validate_map_style("amap://styles/normal", "gaode")
#'
#' ## Check compatibility across providers
#' validator <- get_style_validator()
#' compatibility <- validator$get_style_compatibility("streets")
#' print(compatibility)
#'
#' }
#'
#' @export
validate_map_style <- function(style, provider) {
  if (!is.character(provider) || length(provider) != 1) {
    stop("Provider must be a single character string")
  }
  
  validator <- get_style_validator()
  return(validator$validate_style(style, provider))
}

