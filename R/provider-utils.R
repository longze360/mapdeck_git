#' Provider Utilities
#'
#' This file contains shared utility functions used across different map
#' providers for common operations like validation, error handling, and
#' data processing.
#'
#' @name provider-utils
NULL

#' Validate Provider Name
#'
#' Validates that a provider name is valid and available.
#'
#' @param provider_name Character string provider name to validate
#' @param allow_null Logical indicating if NULL values are allowed
#'
#' @return Character string validated provider name
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
  
  available_providers <- get_available_providers()
  if (length(available_providers) > 0 && !provider_name %in% available_providers) {
    stop("Provider '", provider_name, "' is not available. ",
         "Available providers: ", paste(available_providers, collapse = ", "))
  }
  
  return(provider_name)
}

#' Validate Map Options
#'
#' Validates common map initialization options across providers.
#'
#' @param options List of map options to validate
#'
#' @return List of validated options
#'
#' @export
validate_map_options <- function(options) {
  if (!is.list(options)) {
    stop("Map options must be a list")
  }
  
  # Validate longitude
  if ("longitude" %in% names(options)) {
    lng <- options$longitude
    if (!is.numeric(lng) || length(lng) != 1 || is.na(lng)) {
      stop("longitude must be a single numeric value")
    }
    if (lng < -180 || lng > 180) {
      stop("longitude must be between -180 and 180")
    }
  }
  
  # Validate latitude
  if ("latitude" %in% names(options)) {
    lat <- options$latitude
    if (!is.numeric(lat) || length(lat) != 1 || is.na(lat)) {
      stop("latitude must be a single numeric value")
    }
    if (lat < -90 || lat > 90) {
      stop("latitude must be between -90 and 90")
    }
  }
  
  # Validate zoom
  if ("zoom" %in% names(options)) {
    zoom <- options$zoom
    if (!is.numeric(zoom) || length(zoom) != 1 || is.na(zoom)) {
      stop("zoom must be a single numeric value")
    }
    if (zoom < 0 || zoom > 24) {
      stop("zoom must be between 0 and 24")
    }
  }
  
  # Validate pitch
  if ("pitch" %in% names(options)) {
    pitch <- options$pitch
    if (!is.numeric(pitch) || length(pitch) != 1 || is.na(pitch)) {
      stop("pitch must be a single numeric value")
    }
    if (pitch < 0 || pitch > 60) {
      stop("pitch must be between 0 and 60")
    }
  }
  
  # Validate bearing
  if ("bearing" %in% names(options)) {
    bearing <- options$bearing
    if (!is.numeric(bearing) || length(bearing) != 1 || is.na(bearing)) {
      stop("bearing must be a single numeric value")
    }
    if (bearing < -180 || bearing > 180) {
      stop("bearing must be between -180 and 180")
    }
  }
  
  return(options)
}

#' Validate Layer Configuration
#'
#' Validates deck.gl layer configuration for provider compatibility.
#'
#' @param layer List containing layer configuration
#' @param provider_name Character string provider name
#'
#' @return List of validated layer configuration
#'
#' @export
validate_layer_config <- function(layer, provider_name = NULL) {
  if (!is.list(layer)) {
    stop("Layer configuration must be a list")
  }
  
  # Validate required fields
  required_fields <- c("type", "id")
  missing_fields <- setdiff(required_fields, names(layer))
  if (length(missing_fields) > 0) {
    stop("Layer configuration missing required fields: ", 
         paste(missing_fields, collapse = ", "))
  }
  
  # Validate layer type
  if (!is.character(layer$type) || length(layer$type) != 1) {
    stop("Layer type must be a single character string")
  }
  
  # Validate layer ID
  if (!is.character(layer$id) || length(layer$id) != 1) {
    stop("Layer id must be a single character string")
  }
  
  # Provider-specific validation
  if (!is.null(provider_name)) {
    capabilities <- get_provider_capabilities(provider_name)
    if (!is.null(capabilities)) {
      # Check if layer type is supported (basic check)
      # More detailed validation would be provider-specific
      if ("deck_layers" %in% capabilities || length(capabilities) > 0) {
        # Provider supports layers - detailed validation would go here
      }
    }
  }
  
  return(layer)
}

#' Create Error Handler
#'
#' Creates a standardized error handler for provider operations.
#'
#' @param provider_name Character string provider name
#' @param operation Character string operation name
#'
#' @return Function that handles errors consistently
#'
#' @export
create_error_handler <- function(provider_name, operation) {
  function(error) {
    error_msg <- paste0("Provider '", provider_name, "' failed during '", 
                       operation, "': ", error$message)
    
    # Create structured error
    structure(
      list(
        message = error_msg,
        provider = provider_name,
        operation = operation,
        original_error = error
      ),
      class = c("MapdeckProviderError", "error", "condition")
    )
  }
}

#' Normalize Style Name
#'
#' Normalizes style names for consistent handling across providers.
#'
#' @param style Character string or list style specification
#' @param provider_name Character string provider name
#'
#' @return Normalized style specification
#'
#' @export
normalize_style <- function(style, provider_name = NULL) {
  if (is.null(style)) {
    return(NULL)
  }
  
  # Handle character style names
  if (is.character(style)) {
    if (length(style) != 1) {
      stop("Style name must be a single character string")
    }
    return(style)
  }
  
  # Handle list-based style specifications
  if (is.list(style)) {
    # Basic validation for list-based styles
    if (!"version" %in% names(style)) {
      warning("Style specification missing 'version' field")
    }
    return(style)
  }
  
  stop("Style must be a character string or list")
}

#' Check Provider Feature Support
#'
#' Checks if a provider supports a specific feature.
#'
#' @param provider_name Character string provider name
#' @param feature Character string feature name
#'
#' @return Logical indicating if feature is supported
#'
#' @export
check_provider_feature <- function(provider_name, feature) {
  capabilities <- get_provider_capabilities(provider_name)
  
  if (is.null(capabilities)) {
    warning("No capabilities information available for provider: ", provider_name)
    return(FALSE)
  }
  
  return(feature %in% capabilities)
}

#' Generate Unique Layer ID
#'
#' Generates a unique identifier for map layers.
#'
#' @param prefix Character string prefix for the ID
#'
#' @return Character string unique layer ID
#'
#' @export
generate_layer_id <- function(prefix = "layer") {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  random_suffix <- sample(1000:9999, 1)
  return(paste0(prefix, "_", timestamp, "_", random_suffix))
}

#' Sanitize HTML Container ID
#'
#' Sanitizes HTML container IDs for use across different providers.
#'
#' @param container_id Character string container ID
#'
#' @return Character string sanitized container ID
#'
#' @export
sanitize_container_id <- function(container_id) {
  if (!is.character(container_id) || length(container_id) != 1) {
    stop("Container ID must be a single character string")
  }
  
  if (nchar(container_id) == 0) {
    stop("Container ID cannot be empty")
  }
  
  # Remove invalid characters and ensure it starts with a letter
  sanitized <- gsub("[^a-zA-Z0-9_-]", "_", container_id)
  
  # Ensure it starts with a letter
  if (!grepl("^[a-zA-Z]", sanitized)) {
    sanitized <- paste0("map_", sanitized)
  }
  
  return(sanitized)
}

#' Convert Coordinates to Standard Format
#'
#' Converts coordinate data to a standard format for provider consumption.
#'
#' @param data Data frame or matrix containing coordinate data
#' @param lon_col Character string or numeric index for longitude column
#' @param lat_col Character string or numeric index for latitude column
#'
#' @return Data frame with standardized coordinate columns
#'
#' @export
standardize_coordinates <- function(data, lon_col = "longitude", lat_col = "latitude") {
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("Data must be a data frame or matrix")
  }
  
  # Convert to data frame if matrix
  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }
  
  # Handle column references
  if (is.character(lon_col)) {
    if (!lon_col %in% names(data)) {
      stop("Longitude column '", lon_col, "' not found in data")
    }
  } else if (is.numeric(lon_col)) {
    if (lon_col < 1 || lon_col > ncol(data)) {
      stop("Longitude column index out of range")
    }
    lon_col <- names(data)[lon_col]
  }
  
  if (is.character(lat_col)) {
    if (!lat_col %in% names(data)) {
      stop("Latitude column '", lat_col, "' not found in data")
    }
  } else if (is.numeric(lat_col)) {
    if (lat_col < 1 || lat_col > ncol(data)) {
      stop("Latitude column index out of range")
    }
    lat_col <- names(data)[lat_col]
  }
  
  # Validate coordinate values
  lon_values <- data[[lon_col]]
  lat_values <- data[[lat_col]]
  
  if (!is.numeric(lon_values) || !is.numeric(lat_values)) {
    stop("Coordinate columns must contain numeric values")
  }
  
  if (any(is.na(lon_values)) || any(is.na(lat_values))) {
    warning("Coordinate data contains NA values")
  }
  
  if (any(lon_values < -180 | lon_values > 180, na.rm = TRUE)) {
    warning("Some longitude values are outside valid range (-180 to 180)")
  }
  
  if (any(lat_values < -90 | lat_values > 90, na.rm = TRUE)) {
    warning("Some latitude values are outside valid range (-90 to 90)")
  }
  
  return(data)
}