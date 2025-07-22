#' Provider Utilities
#'
#' This file contains shared utility functions used across different
#' map providers for common operations like validation, error handling,
#' and data processing.
#'
#' @name provider-utils
NULL

#' Validate Provider Name
#'
#' Validate that a provider name is valid and supported.
#'
#' @param provider_name Character string identifying the provider
#' @param allow_null Logical indicating if NULL values are allowed
#' @return Character string (validated provider name) or NULL
#'
#' @examples
#' \donttest{
#' # Validate provider name
#' provider <- validate_provider_name("mapbox")
#' }
#'
#' @export
validate_provider_name <- function(provider_name, allow_null = FALSE) {
  if (is.null(provider_name)) {
    if (allow_null) {
      return(NULL)
    } else {
      stop("Provider name cannot be NULL")
    }
  }
  
  if (!is.character(provider_name) || length(provider_name) != 1) {
    stop("Provider name must be a single character string")
  }
  
  if (nchar(provider_name) == 0) {
    stop("Provider name cannot be empty")
  }
  
  # Check if provider is available
  factory <- get_provider_factory()
  available_providers <- factory$get_available_providers()
  
  if (!provider_name %in% available_providers) {
    stop(sprintf(
      "Provider '%s' is not available. Available providers: %s",
      provider_name,
      paste(available_providers, collapse = ", ")
    ))
  }
  
  return(provider_name)
}

#' Validate Map Options
#'
#' Validate and normalize map initialization options.
#'
#' @param options List containing map options
#' @param provider_name Character string identifying the provider
#' @return List with validated and normalized options
#'
#' @examples
#' \donttest{
#' # Validate map options
#' options <- validate_map_options(
#'   list(longitude = -74, latitude = 40.7, zoom = 10),
#'   "mapbox"
#' )
#' }
#'
#' @export
validate_map_options <- function(options, provider_name) {
  if (!is.list(options)) {
    stop("Options must be a list")
  }
  
  # Default options
  defaults <- list(
    longitude = 0,
    latitude = 0,
    zoom = 0,
    pitch = 0,
    bearing = 0,
    width = "100%",
    height = "400px"
  )
  
  # Merge with defaults
  validated_options <- defaults
  for (key in names(options)) {
    validated_options[[key]] <- options[[key]]
  }
  
  # Validate numeric values
  numeric_fields <- c("longitude", "latitude", "zoom", "pitch", "bearing")
  for (field in numeric_fields) {
    if (!is.null(validated_options[[field]])) {
      if (!is.numeric(validated_options[[field]]) || 
          length(validated_options[[field]]) != 1) {
        stop(sprintf("Option '%s' must be a single numeric value", field))
      }
    }
  }
  
  # Validate ranges
  if (!is.null(validated_options$longitude)) {
    if (validated_options$longitude < -180 || validated_options$longitude > 180) {
      stop("Longitude must be between -180 and 180")
    }
  }
  
  if (!is.null(validated_options$latitude)) {
    if (validated_options$latitude < -90 || validated_options$latitude > 90) {
      stop("Latitude must be between -90 and 90")
    }
  }
  
  if (!is.null(validated_options$zoom)) {
    if (validated_options$zoom < 0 || validated_options$zoom > 24) {
      stop("Zoom must be between 0 and 24")
    }
  }
  
  if (!is.null(validated_options$pitch)) {
    if (validated_options$pitch < 0 || validated_options$pitch > 60) {
      stop("Pitch must be between 0 and 60 degrees")
    }
  }
  
  if (!is.null(validated_options$bearing)) {
    if (validated_options$bearing < 0 || validated_options$bearing >= 360) {
      stop("Bearing must be between 0 and 360 degrees")
    }
  }
  
  return(validated_options)
}

#' Validate Layer Configuration
#'
#' Validate deck.gl layer configuration for provider compatibility.
#'
#' @param layer List containing layer configuration
#' @param provider_name Character string identifying the provider
#' @return List with validated layer configuration
#'
#' @examples
#' \donttest{
#' # Validate layer configuration
#' layer <- validate_layer_config(
#'   list(type = "ScatterplotLayer", data = data.frame(x = 1, y = 1)),
#'   "mapbox"
#' )
#' }
#'
#' @export
validate_layer_config <- function(layer, provider_name) {
  if (!is.list(layer)) {
    stop("Layer must be a list")
  }
  
  # Check required fields
  if (is.null(layer$type)) {
    stop("Layer must have a 'type' field")
  }
  
  if (!is.character(layer$type) || length(layer$type) != 1) {
    stop("Layer type must be a single character string")
  }
  
  # Validate layer ID
  if (is.null(layer$id)) {
    # Generate unique ID if not provided
    layer$id <- paste0(layer$type, "_", as.integer(Sys.time() * 1000))
  }
  
  if (!is.character(layer$id) || length(layer$id) != 1) {
    stop("Layer ID must be a single character string")
  }
  
  # Check provider capabilities for layer type
  factory <- get_provider_factory()
  capabilities <- factory$get_provider_capabilities(provider_name)
  
  # For now, assume all providers support all layer types
  # This can be enhanced later with provider-specific layer support
  
  return(layer)
}

#' Create Provider Error
#'
#' Create a standardized provider-specific error.
#'
#' @param message Character string containing error message
#' @param provider Character string identifying the provider
#' @param code Character string containing error code (optional)
#' @param call Call object for error context (optional)
#' @return Error condition object
#'
#' @examples
#' \donttest{
#' # Create provider error
#' error <- create_provider_error("Invalid token", "mapbox", "AUTH_ERROR")
#' }
#'
#' @export
create_provider_error <- function(message, provider, code = NULL, call = NULL) {
  if (!is.character(message) || length(message) != 1) {
    stop("Error message must be a single character string")
  }
  
  if (!is.character(provider) || length(provider) != 1) {
    stop("Provider must be a single character string")
  }
  
  error_data <- list(
    message = message,
    provider = provider,
    code = code,
    call = call
  )
  
  structure(
    error_data,
    class = c("MapdeckProviderError", "error", "condition")
  )
}

#' Handle Provider Error
#'
#' Handle provider-specific errors with appropriate messaging.
#'
#' @param error Error object to handle
#' @param context Character string providing error context
#' @return NULL (function stops execution)
#'
#' @examples
#' \donttest{
#' # Handle provider error
#' tryCatch({
#'   # Some operation that might fail
#' }, error = function(e) {
#'   handle_provider_error(e, "map initialization")
#' })
#' }
#'
#' @export
handle_provider_error <- function(error, context = "operation") {
  if (inherits(error, "MapdeckProviderError")) {
    # Enhanced error message for provider errors
    enhanced_message <- sprintf(
      "Provider error during %s:\nProvider: %s\nError: %s",
      context,
      error$provider,
      error$message
    )
    
    if (!is.null(error$code)) {
      enhanced_message <- paste0(enhanced_message, "\nCode: ", error$code)
    }
    
    stop(enhanced_message, call. = FALSE)
  } else {
    # Re-throw non-provider errors
    stop(error)
  }
}

#' Normalize Style Name
#'
#' Normalize style names for provider compatibility.
#'
#' @param style Character string or list containing style specification
#' @param provider_name Character string identifying the provider
#' @return Normalized style specification
#'
#' @examples
#' \donttest{
#' # Normalize style name
#' style <- normalize_style_name("streets", "mapbox")
#' }
#'
#' @export
normalize_style_name <- function(style, provider_name) {
  if (is.null(style)) {
    # Use provider default
    factory <- get_provider_factory()
    config <- factory$registry$get_provider_config(provider_name)
    return(config$default_style)
  }
  
  if (is.character(style) && length(style) == 1) {
    # Handle common style aliases
    style_aliases <- list(
      "streets" = list(
        "mapbox" = "mapbox://styles/mapbox/streets-v11",
        "leaflet" = "OpenStreetMap",
        "openlayers" = "OSM",
        "gaode" = "amap://styles/normal",
        "baidu" = "normal"
      ),
      "satellite" = list(
        "mapbox" = "mapbox://styles/mapbox/satellite-v9",
        "leaflet" = "Esri.WorldImagery",
        "openlayers" = "satellite",
        "gaode" = "amap://styles/satellite",
        "baidu" = "satellite"
      ),
      "dark" = list(
        "mapbox" = "mapbox://styles/mapbox/dark-v10",
        "leaflet" = "CartoDB.DarkMatter",
        "openlayers" = "dark",
        "gaode" = "amap://styles/dark",
        "baidu" = "dark"
      )
    )
    
    if (style %in% names(style_aliases)) {
      provider_styles <- style_aliases[[style]]
      if (provider_name %in% names(provider_styles)) {
        return(provider_styles[[provider_name]])
      }
    }
    
    # Return as-is if no alias found
    return(style)
  }
  
  # Return complex styles as-is
  return(style)
}

#' Check Feature Support
#'
#' Check if a provider supports a specific feature.
#'
#' @param provider_name Character string identifying the provider
#' @param feature Character string identifying the feature
#' @return Logical indicating if feature is supported
#'
#' @examples
#' \donttest{
#' # Check if Mapbox supports 3D terrain
#' supported <- check_feature_support("mapbox", "3d_terrain")
#' }
#'
#' @export
check_feature_support <- function(provider_name, feature) {
  if (!is.character(provider_name) || length(provider_name) != 1) {
    stop("Provider name must be a single character string")
  }
  
  if (!is.character(feature) || length(feature) != 1) {
    stop("Feature must be a single character string")
  }
  
  factory <- get_provider_factory()
  config <- factory$registry$get_provider_config(provider_name)
  
  if (is.null(config)) {
    return(FALSE)
  }
  
  return(config$supports_feature(feature))
}

#' Generate Unique ID
#'
#' Generate a unique identifier for layers or other objects.
#'
#' @param prefix Character string prefix for the ID
#' @return Character string containing unique ID
#'
#' @examples
#' \donttest{
#' # Generate unique layer ID
#' id <- generate_unique_id("layer")
#' }
#'
#' @export
generate_unique_id <- function(prefix = "id") {
  if (!is.character(prefix) || length(prefix) != 1) {
    stop("Prefix must be a single character string")
  }
  
  # Use a simpler approach to avoid integer overflow
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  random_part <- sample(1000:9999, 1)
  
  return(paste0(prefix, "_", timestamp, "_", random_part))
}

#' Validate Coordinate Bounds
#'
#' Validate that coordinate bounds are within valid ranges.
#'
#' @param bounds Numeric vector of length 4 containing [west, south, east, north]
#' @return Numeric vector with validated bounds
#'
#' @examples
#' \donttest{
#' # Validate bounds
#' bounds <- validate_coordinate_bounds(c(-180, -90, 180, 90))
#' }
#'
#' @export
validate_coordinate_bounds <- function(bounds) {
  if (!is.numeric(bounds) || length(bounds) != 4) {
    stop("Bounds must be a numeric vector of length 4 [west, south, east, north]")
  }
  
  west <- bounds[1]
  south <- bounds[2]
  east <- bounds[3]
  north <- bounds[4]
  
  # Validate longitude bounds
  if (west < -180 || west > 180) {
    stop("West longitude must be between -180 and 180")
  }
  
  if (east < -180 || east > 180) {
    stop("East longitude must be between -180 and 180")
  }
  
  # Validate latitude bounds
  if (south < -90 || south > 90) {
    stop("South latitude must be between -90 and 90")
  }
  
  if (north < -90 || north > 90) {
    stop("North latitude must be between -90 and 90")
  }
  
  # Validate logical relationships
  if (west >= east) {
    stop("West longitude must be less than east longitude")
  }
  
  if (south >= north) {
    stop("South latitude must be less than north latitude")
  }
  
  return(bounds)
}