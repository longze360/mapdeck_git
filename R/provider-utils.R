#' Provider Utilities
#'
#' Shared utility functions for common provider operations.
#' These utilities provide consistent functionality across different providers.
#'
#' @keywords internal
NULL

#' Validate Provider Name
#'
#' Validate that a provider name is valid and registered.
#'
#' @param provider_name Character string identifying the provider
#' @param allow_null Logical indicating if NULL values are allowed
#' @return Logical indicating if provider name is valid
#' @keywords internal
validate_provider_name <- function(provider_name, allow_null = FALSE) {
  if (is.null(provider_name)) {
    return(allow_null)
  }
  
  if (!is.character(provider_name) || length(provider_name) != 1) {
    return(FALSE)
  }
  
  registry <- get_provider_registry()
  return(registry$is_registered(provider_name))
}

#' Normalize Location
#'
#' Normalize location coordinates to a consistent format.
#'
#' @param location Numeric vector, list, or data.frame containing coordinates
#' @return Numeric vector of length 2 (longitude, latitude)
#' @keywords internal
normalize_location <- function(location) {
  if (is.null(location)) {
    return(c(0, 0))
  }
  
  if (is.data.frame(location)) {
    if (nrow(location) == 0) {
      return(c(0, 0))
    }
    # Try common column names
    lon_cols <- c("lon", "lng", "longitude", "x")
    lat_cols <- c("lat", "latitude", "y")
    
    lon_col <- intersect(names(location), lon_cols)[1]
    lat_col <- intersect(names(location), lat_cols)[1]
    
    if (is.na(lon_col) || is.na(lat_col)) {
      stop("Could not identify longitude and latitude columns in location data.frame")
    }
    
    location <- c(location[[lon_col]][1], location[[lat_col]][1])
  }
  
  if (is.list(location)) {
    if ("lon" %in% names(location) && "lat" %in% names(location)) {
      location <- c(location$lon, location$lat)
    } else if ("lng" %in% names(location) && "lat" %in% names(location)) {
      location <- c(location$lng, location$lat)
    } else if (length(location) >= 2) {
      location <- c(location[[1]], location[[2]])
    } else {
      stop("Invalid location format")
    }
  }
  
  if (!is.numeric(location) || length(location) < 2) {
    stop("Location must be a numeric vector of at least length 2")
  }
  
  # Return as longitude, latitude
  return(as.numeric(location[1:2]))
}

#' Validate Zoom Level
#'
#' Validate and normalize zoom level values.
#'
#' @param zoom Numeric zoom level
#' @param min_zoom Minimum allowed zoom level
#' @param max_zoom Maximum allowed zoom level
#' @return Numeric zoom level within valid range
#' @keywords internal
validate_zoom <- function(zoom, min_zoom = 0, max_zoom = 20) {
  if (is.null(zoom)) {
    return(0)
  }
  
  if (!is.numeric(zoom) || length(zoom) != 1) {
    stop("Zoom must be a single numeric value")
  }
  
  if (zoom < min_zoom) {
    warning(sprintf("Zoom level %g is below minimum %g, using minimum", zoom, min_zoom))
    return(min_zoom)
  }
  
  if (zoom > max_zoom) {
    warning(sprintf("Zoom level %g is above maximum %g, using maximum", zoom, max_zoom))
    return(max_zoom)
  }
  
  return(zoom)
}

#' Validate Bearing
#'
#' Validate and normalize bearing values.
#'
#' @param bearing Numeric bearing in degrees
#' @return Numeric bearing normalized to 0-360 range
#' @keywords internal
validate_bearing <- function(bearing) {
  if (is.null(bearing)) {
    return(0)
  }
  
  if (!is.numeric(bearing) || length(bearing) != 1) {
    stop("Bearing must be a single numeric value")
  }
  
  # Normalize to 0-360 range
  bearing <- bearing %% 360
  if (bearing < 0) {
    bearing <- bearing + 360
  }
  
  return(bearing)
}

#' Validate Pitch
#'
#' Validate and normalize pitch values.
#'
#' @param pitch Numeric pitch in degrees
#' @param min_pitch Minimum allowed pitch
#' @param max_pitch Maximum allowed pitch
#' @return Numeric pitch within valid range
#' @keywords internal
validate_pitch <- function(pitch, min_pitch = 0, max_pitch = 60) {
  if (is.null(pitch)) {
    return(0)
  }
  
  if (!is.numeric(pitch) || length(pitch) != 1) {
    stop("Pitch must be a single numeric value")
  }
  
  if (pitch < min_pitch) {
    warning(sprintf("Pitch %g is below minimum %g, using minimum", pitch, min_pitch))
    return(min_pitch)
  }
  
  if (pitch > max_pitch) {
    warning(sprintf("Pitch %g is above maximum %g, using maximum", pitch, max_pitch))
    return(max_pitch)
  }
  
  return(pitch)
}

#' Generate Layer ID
#'
#' Generate a unique identifier for a map layer.
#'
#' @param layer_type Character string specifying the layer type
#' @param prefix Optional prefix for the layer ID
#' @return Character string layer ID
#' @keywords internal
generate_layer_id <- function(layer_type, prefix = "layer") {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  random_suffix <- sample(1000:9999, 1)
  return(sprintf("%s_%s_%s_%d", prefix, layer_type, timestamp, random_suffix))
}

#' Merge Layer Options
#'
#' Merge layer options with defaults, handling nested lists properly.
#'
#' @param defaults List of default options
#' @param user_options List of user-provided options
#' @return List of merged options
#' @keywords internal
merge_layer_options <- function(defaults, user_options) {
  if (is.null(user_options)) {
    return(defaults)
  }
  
  if (!is.list(defaults) || !is.list(user_options)) {
    stop("Both defaults and user_options must be lists")
  }
  
  # Start with defaults
  merged <- defaults
  
  # Override with user options
  for (key in names(user_options)) {
    if (key %in% names(merged) && is.list(merged[[key]]) && is.list(user_options[[key]])) {
      # Recursively merge nested lists
      merged[[key]] <- merge_layer_options(merged[[key]], user_options[[key]])
    } else {
      # Direct assignment for non-list values or new keys
      merged[[key]] <- user_options[[key]]
    }
  }
  
  return(merged)
}

#' Validate Layer Data
#'
#' Validate that layer data is in the correct format for visualization.
#'
#' @param data Data to validate (data.frame, sf object, or list)
#' @param required_columns Character vector of required column names
#' @return Logical indicating if data is valid
#' @keywords internal
validate_layer_data <- function(data, required_columns = character(0)) {
  if (is.null(data)) {
    return(length(required_columns) == 0)
  }
  
  # Handle different data types
  if (inherits(data, "sf")) {
    # sf objects are always valid for spatial data
    data_names <- names(data)
  } else if (is.data.frame(data)) {
    data_names <- names(data)
  } else if (is.list(data)) {
    data_names <- names(data)
  } else {
    return(FALSE)
  }
  
  # Check for required columns
  missing_columns <- setdiff(required_columns, data_names)
  if (length(missing_columns) > 0) {
    warning(sprintf(
      "Missing required columns: %s",
      paste(missing_columns, collapse = ", ")
    ))
    return(FALSE)
  }
  
  return(TRUE)
}

#' Convert Style to Provider Format
#'
#' Convert a generic style specification to provider-specific format.
#'
#' @param style Style specification (character or list)
#' @param provider_type Character string specifying the provider type
#' @return Provider-specific style specification
#' @keywords internal
convert_style_to_provider <- function(style, provider_type) {
  if (is.null(style)) {
    return(NULL)
  }
  
  # Handle different provider types
  switch(provider_type,
    "mapbox" = {
      if (is.character(style) && !grepl("^mapbox://", style)) {
        # Convert generic style names to Mapbox URLs
        style_map <- list(
          "streets" = "mapbox://styles/mapbox/streets-v11",
          "satellite" = "mapbox://styles/mapbox/satellite-v9",
          "dark" = "mapbox://styles/mapbox/dark-v10",
          "light" = "mapbox://styles/mapbox/light-v10",
          "outdoors" = "mapbox://styles/mapbox/outdoors-v11"
        )
        
        if (style %in% names(style_map)) {
          return(style_map[[style]])
        }
      }
      return(style)
    },
    
    "leaflet" = {
      if (is.character(style)) {
        # Convert generic style names to tile layer URLs
        style_map <- list(
          "streets" = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
          "satellite" = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
          "dark" = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png",
          "light" = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png"
        )
        
        if (style %in% names(style_map)) {
          return(style_map[[style]])
        }
      }
      return(style)
    },
    
    "gaode" = {
      if (is.character(style)) {
        # Convert generic style names to Gaode style IDs
        style_map <- list(
          "streets" = "normal",
          "satellite" = "satellite",
          "dark" = "dark",
          "light" = "light"
        )
        
        if (style %in% names(style_map)) {
          return(style_map[[style]])
        }
      }
      return(style)
    },
    
    "baidu" = {
      if (is.character(style)) {
        # Convert generic style names to Baidu style IDs
        style_map <- list(
          "streets" = "normal",
          "satellite" = "satellite",
          "dark" = "dark",
          "light" = "light"
        )
        
        if (style %in% names(style_map)) {
          return(style_map[[style]])
        }
      }
      return(style)
    },
    
    # Default: return style as-is
    return(style)
  )
}

#' Check Provider Feature Support
#'
#' Check if a provider supports a specific feature.
#'
#' @param provider_name Character string identifying the provider
#' @param feature Character string identifying the feature
#' @return Logical indicating if feature is supported
#' @keywords internal
check_provider_feature_support <- function(provider_name, feature) {
  if (!validate_provider_name(provider_name)) {
    return(FALSE)
  }
  
  registry <- get_provider_registry()
  capabilities <- registry$get_capabilities(provider_name)
  
  return(feature %in% capabilities)
}