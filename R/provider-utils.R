#' Provider Utility Functions
#'
#' This file contains utility functions for provider management, validation,
#' and configuration handling.
#'
#' @name provider-utils
NULL

#' Validate Provider Name
#'
#' Validate that a provider name is valid and available.
#'
#' @param provider_name Character string identifying the provider
#' @param allow_null Logical indicating if NULL values are allowed
#' @return Validated provider name or NULL if allow_null is TRUE
#'
#' @examples
#' \donttest{
#' # Validate provider names
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
  tryCatch({
    factory <- get_provider_factory()
    available_providers <- factory$get_available_providers()
    
    if (!provider_name %in% available_providers) {
      stop(sprintf("Provider '%s' is not available. Available providers: %s",
                  provider_name, paste(available_providers, collapse = ", ")))
    }
  }, error = function(e) {
    if (grepl("not available", e$message)) {
      stop(e$message)
    }
    # If we can't check availability, assume it's valid
  })
  
  return(provider_name)
}

#' Validate Map Options
#'
#' Validate map options for a specific provider.
#'
#' @param options List containing map options
#' @param provider Character string identifying the provider
#' @return Validated and normalized options list
#'
#' @examples
#' \donttest{
#' # Validate map options
#' options <- validate_map_options(list(zoom = 10), "mapbox")
#' }
#'
#' @export
validate_map_options <- function(options, provider) {
  
  if (!is.list(options)) {
    stop("Options must be a list")
  }
  
  if (!is.character(provider) || length(provider) != 1) {
    stop("Provider must be a single character string")
  }
  
  # Validate common options
  validated_options <- list()
  
  # Validate data
  if (!is.null(options$data)) {
    validated_options$data <- options$data
  }
  
  # Validate dimensions
  if (!is.null(options$width)) {
    if (!is.numeric(options$width) && !is.character(options$width)) {
      stop("Width must be numeric or character")
    }
    validated_options$width <- options$width
  }
  
  if (!is.null(options$height)) {
    if (!is.numeric(options$height) && !is.character(options$height)) {
      stop("Height must be numeric or character")
    }
    validated_options$height <- options$height
  }
  
  # Validate padding
  if (!is.null(options$padding)) {
    if (!is.numeric(options$padding) || length(options$padding) != 1) {
      stop("Padding must be a single numeric value")
    }
    validated_options$padding <- options$padding
  }
  
  # Validate view state
  if (!is.null(options$location)) {
    if (!is.numeric(options$location) || length(options$location) != 2) {
      stop("Location must be a numeric vector of length 2")
    }
    validated_options$location <- options$location
  }
  
  if (!is.null(options$zoom)) {
    if (!is.numeric(options$zoom) || length(options$zoom) != 1) {
      stop("Zoom must be a single numeric value")
    }
    validated_options$zoom <- options$zoom
  }
  
  if (!is.null(options$pitch)) {
    if (!is.numeric(options$pitch) || length(options$pitch) != 1) {
      stop("Pitch must be a single numeric value")
    }
    validated_options$pitch <- options$pitch
  }
  
  if (!is.null(options$bearing)) {
    if (!is.numeric(options$bearing) || length(options$bearing) != 1) {
      stop("Bearing must be a single numeric value")
    }
    validated_options$bearing <- options$bearing
  }
  
  # Validate zoom limits
  if (!is.null(options$max_zoom)) {
    if (!is.numeric(options$max_zoom) || length(options$max_zoom) != 1) {
      stop("max_zoom must be a single numeric value")
    }
    validated_options$max_zoom <- options$max_zoom
  }
  
  if (!is.null(options$min_zoom)) {
    if (!is.numeric(options$min_zoom) || length(options$min_zoom) != 1) {
      stop("min_zoom must be a single numeric value")
    }
    validated_options$min_zoom <- options$min_zoom
  }
  
  # Validate pitch limits
  if (!is.null(options$max_pitch)) {
    if (!is.numeric(options$max_pitch) || length(options$max_pitch) != 1) {
      stop("max_pitch must be a single numeric value")
    }
    validated_options$max_pitch <- options$max_pitch
  }
  
  if (!is.null(options$min_pitch)) {
    if (!is.numeric(options$min_pitch) || length(options$min_pitch) != 1) {
      stop("min_pitch must be a single numeric value")
    }
    validated_options$min_pitch <- options$min_pitch
  }
  
  # Validate style
  if (!is.null(options$style)) {
    validated_options$style <- options$style
  }
  
  # Validate libraries
  if (!is.null(options$libraries)) {
    if (!is.character(options$libraries)) {
      stop("Libraries must be a character vector")
    }
    validated_options$libraries <- options$libraries
  }
  
  # Validate boolean options
  if (!is.null(options$show_view_state)) {
    if (!is.logical(options$show_view_state) || length(options$show_view_state) != 1) {
      stop("show_view_state must be a single logical value")
    }
    validated_options$show_view_state <- options$show_view_state
  }
  
  if (!is.null(options$repeat_view)) {
    if (!is.logical(options$repeat_view) || length(options$repeat_view) != 1) {
      stop("repeat_view must be a single logical value")
    }
    validated_options$repeat_view <- options$repeat_view
  }
  
  # Provider-specific validation
  validated_options <- validate_provider_specific_options(validated_options, provider)
  
  return(validated_options)
}

#' Validate Layer Configuration
#'
#' Validate layer configuration for a specific provider.
#'
#' @param layer List containing layer configuration
#' @param provider Character string identifying the provider
#' @return Validated layer configuration
#'
#' @examples
#' \donttest{
#' # Validate layer configuration
#' layer <- validate_layer_config(list(type = "scatterplot"), "mapbox")
#' }
#'
#' @export
validate_layer_config <- function(layer, provider) {
  
  if (!is.list(layer)) {
    stop("Layer must be a list")
  }
  
  if (!is.character(provider) || length(provider) != 1) {
    stop("Provider must be a single character string")
  }
  
  # Validate required fields
  if (is.null(layer$type)) {
    stop("Layer must have a 'type' field")
  }
  
  if (!is.character(layer$type) || length(layer$type) != 1) {
    stop("Layer type must be a single character string")
  }
  
  # Validate layer ID
  if (is.null(layer$id)) {
    layer$id <- paste0(layer$type, "_", as.integer(Sys.time()))
  }
  
  if (!is.character(layer$id) || length(layer$id) != 1) {
    stop("Layer ID must be a single character string")
  }
  
  # Validate data
  if (!is.null(layer$data)) {
    # Basic data validation - could be expanded
    if (!is.data.frame(layer$data) && !is.list(layer$data)) {
      stop("Layer data must be a data frame or list")
    }
  }
  
  # Provider-specific layer validation
  layer <- validate_provider_specific_layer(layer, provider)
  
  return(layer)
}

#' Normalize Style Name
#'
#' Normalize style names for different providers.
#'
#' @param style Character string or list containing style specification
#' @param provider Character string identifying the provider
#' @return Normalized style specification
#'
#' @examples
#' \donttest{
#' # Normalize style names
#' style <- normalize_style_name("dark", "mapbox")
#' }
#'
#' @export
normalize_style_name <- function(style, provider) {
  
  if (is.null(style)) {
    return(get_default_style(provider))
  }
  
  if (!is.character(provider) || length(provider) != 1) {
    stop("Provider must be a single character string")
  }
  
  # Handle different style input types
  if (is.character(style) && length(style) == 1) {
    return(normalize_character_style(style, provider))
  }
  
  if (is.list(style)) {
    return(normalize_custom_style(style, provider))
  }
  
  # Return as-is if we can't normalize
  return(style)
}

#' Get Default Style for Provider
#'
#' @param provider Character string identifying the provider
#' @return Default style for the provider
get_default_style <- function(provider) {
  
  default_styles <- list(
    "mapbox" = "mapbox://styles/mapbox/streets-v11",
    "leaflet" = "OpenStreetMap",
    "openlayers" = "osm",
    "gaode" = "normal",
    "baidu" = "normal"
  )
  
  return(default_styles[[provider]] %||% "default")
}

#' Normalize Character Style
#'
#' @param style Character string containing style name
#' @param provider Character string identifying the provider
#' @return Normalized style string
normalize_character_style <- function(style, provider) {
  
  # Provider-specific style normalization
  switch(provider,
    "mapbox" = normalize_mapbox_style(style),
    "leaflet" = normalize_leaflet_style(style),
    "openlayers" = normalize_openlayers_style(style),
    "gaode" = normalize_gaode_style(style),
    "baidu" = normalize_baidu_style(style),
    style  # Return as-is for unknown providers
  )
}

#' Normalize Custom Style
#'
#' @param style List containing custom style specification
#' @param provider Character string identifying the provider
#' @return Normalized style list
normalize_custom_style <- function(style, provider) {
  
  # Basic validation for custom styles
  if (!is.list(style)) {
    stop("Custom style must be a list")
  }
  
  # Provider-specific custom style handling
  switch(provider,
    "mapbox" = {
      # Mapbox custom styles should have version, sources, layers
      required_fields <- c("version", "sources", "layers")
      if (!all(required_fields %in% names(style))) {
        warning("Custom Mapbox style missing required fields")
      }
    },
    "leaflet" = {
      # Leaflet custom styles are tile configurations
      if (is.null(style$url)) {
        warning("Custom Leaflet style missing URL")
      }
    }
  )
  
  return(style)
}

#' Normalize Mapbox Style
#'
#' @param style Character string containing Mapbox style name
#' @return Normalized Mapbox style URL
normalize_mapbox_style <- function(style) {
  
  # Common Mapbox style aliases
  aliases <- list(
    "streets" = "mapbox://styles/mapbox/streets-v11",
    "outdoors" = "mapbox://styles/mapbox/outdoors-v11",
    "light" = "mapbox://styles/mapbox/light-v10",
    "dark" = "mapbox://styles/mapbox/dark-v10",
    "satellite" = "mapbox://styles/mapbox/satellite-v9",
    "satellite-streets" = "mapbox://styles/mapbox/satellite-streets-v11",
    "navigation-day" = "mapbox://styles/mapbox/navigation-day-v1",
    "navigation-night" = "mapbox://styles/mapbox/navigation-night-v1"
  )
  
  # Convert to lowercase for matching
  style_lower <- tolower(style)
  
  if (style_lower %in% names(aliases)) {
    return(aliases[[style_lower]])
  }
  
  # Return as-is if it's already a full URL or unknown alias
  return(style)
}

#' Normalize Leaflet Style
#'
#' @param style Character string containing Leaflet tile provider name
#' @return Normalized Leaflet tile provider name
normalize_leaflet_style <- function(style) {
  
  # Use the existing normalize_leaflet_tile_provider function
  tryCatch({
    return(normalize_leaflet_tile_provider(style))
  }, error = function(e) {
    return(style)
  })
}

#' Normalize Leaflet Tile Provider
#'
#' @param provider Character string containing tile provider name
#' @return Normalized tile provider name
normalize_leaflet_tile_provider <- function(provider) {
  
  # Common Leaflet tile provider aliases
  aliases <- list(
    "osm" = "OpenStreetMap",
    "openstreetmap" = "OpenStreetMap",
    "cartodb" = "CartoDB.Positron",
    "stamen" = "Stamen.Terrain",
    "esri" = "Esri.WorldImagery"
  )
  
  provider_lower <- tolower(provider)
  
  if (provider_lower %in% names(aliases)) {
    return(aliases[[provider_lower]])
  }
  
  return(provider)
}

#' Normalize OpenLayers Style
#'
#' @param style Character string containing OpenLayers source name
#' @return Normalized OpenLayers source name
normalize_openlayers_style <- function(style) {
  
  # Common OpenLayers source aliases
  aliases <- list(
    "osm" = "osm",
    "openstreetmap" = "osm",
    "bing" = "bing",
    "google" = "google",
    "esri" = "esri"
  )
  
  style_lower <- tolower(style)
  
  if (style_lower %in% names(aliases)) {
    return(aliases[[style_lower]])
  }
  
  return(style)
}

#' Normalize Gaode Style
#'
#' @param style Character string containing Gaode style name
#' @return Normalized Gaode style name
normalize_gaode_style <- function(style) {
  
  # Common Gaode style aliases
  aliases <- list(
    "normal" = "normal",
    "satellite" = "satellite",
    "roadnet" = "roadnet",
    "dark" = "dark"
  )
  
  style_lower <- tolower(style)
  
  if (style_lower %in% names(aliases)) {
    return(aliases[[style_lower]])
  }
  
  return(style)
}

#' Normalize Baidu Style
#'
#' @param style Character string containing Baidu style name
#' @return Normalized Baidu style name
normalize_baidu_style <- function(style) {
  
  # Common Baidu style aliases
  aliases <- list(
    "normal" = "normal",
    "satellite" = "satellite",
    "hybrid" = "hybrid",
    "dark" = "dark"
  )
  
  style_lower <- tolower(style)
  
  if (style_lower %in% names(aliases)) {
    return(aliases[[style_lower]])
  }
  
  return(style)
}

#' Validate Provider-Specific Options
#'
#' @param options List containing validated common options
#' @param provider Character string identifying the provider
#' @return Options with provider-specific validation applied
validate_provider_specific_options <- function(options, provider) {
  
  switch(provider,
    "mapbox" = validate_mapbox_options(options),
    "leaflet" = validate_leaflet_options(options),
    "openlayers" = validate_openlayers_options(options),
    "gaode" = validate_gaode_options(options),
    "baidu" = validate_baidu_options(options),
    options  # Return as-is for unknown providers
  )
}

#' Validate Mapbox Options
#'
#' @param options List containing options
#' @return Validated Mapbox options
validate_mapbox_options <- function(options) {
  
  # Mapbox-specific validation
  if (!is.null(options$pitch)) {
    if (options$pitch < 0 || options$pitch > 60) {
      stop("Mapbox pitch must be between 0 and 60 degrees")
    }
  }
  
  if (!is.null(options$bearing)) {
    if (options$bearing < 0 || options$bearing >= 360) {
      stop("Mapbox bearing must be between 0 and 360 degrees")
    }
  }
  
  return(options)
}

#' Validate Leaflet Options
#'
#' @param options List containing options
#' @return Validated Leaflet options
validate_leaflet_options <- function(options) {
  
  # Leaflet doesn't support pitch and bearing
  if (!is.null(options$pitch) && options$pitch != 0) {
    warning("Leaflet does not support pitch - setting to 0")
    options$pitch <- 0
  }
  
  if (!is.null(options$bearing) && options$bearing != 0) {
    warning("Leaflet does not support bearing - setting to 0")
    options$bearing <- 0
  }
  
  # Validate tile provider specific options
  if (!is.null(options$tile_provider)) {
    if (!is.character(options$tile_provider) || length(options$tile_provider) != 1) {
      stop("tile_provider must be a single character string")
    }
    options$tile_provider <- normalize_leaflet_tile_provider(options$tile_provider)
  }
  
  return(options)
}

#' Validate OpenLayers Options
#'
#' @param options List containing options
#' @return Validated OpenLayers options
validate_openlayers_options <- function(options) {
  
  # OpenLayers-specific validation
  if (!is.null(options$projection)) {
    if (!is.character(options$projection) || length(options$projection) != 1) {
      stop("projection must be a single character string")
    }
  }
  
  return(options)
}

#' Validate Gaode Options
#'
#' @param options List containing options
#' @return Validated Gaode options
validate_gaode_options <- function(options) {
  
  # Gaode-specific validation
  if (!is.null(options$api_key)) {
    if (!is.character(options$api_key) || length(options$api_key) != 1) {
      stop("api_key must be a single character string")
    }
  }
  
  return(options)
}

#' Validate Baidu Options
#'
#' @param options List containing options
#' @return Validated Baidu options
validate_baidu_options <- function(options) {
  
  # Baidu-specific validation
  if (!is.null(options$api_key)) {
    if (!is.character(options$api_key) || length(options$api_key) != 1) {
      stop("api_key must be a single character string")
    }
  }
  
  return(options)
}

#' Validate Provider-Specific Layer
#'
#' @param layer List containing layer configuration
#' @param provider Character string identifying the provider
#' @return Validated layer configuration
validate_provider_specific_layer <- function(layer, provider) {
  
  switch(provider,
    "mapbox" = validate_mapbox_layer(layer),
    "leaflet" = validate_leaflet_layer(layer),
    "openlayers" = validate_openlayers_layer(layer),
    "gaode" = validate_gaode_layer(layer),
    "baidu" = validate_baidu_layer(layer),
    layer  # Return as-is for unknown providers
  )
}

#' Validate Mapbox Layer
#'
#' @param layer List containing layer configuration
#' @return Validated Mapbox layer
validate_mapbox_layer <- function(layer) {
  
  # Mapbox supports all deck.gl layer types
  return(layer)
}

#' Validate Leaflet Layer
#'
#' @param layer List containing layer configuration
#' @return Validated Leaflet layer
validate_leaflet_layer <- function(layer) {
  
  # Leaflet might have limitations with 3D features
  if (!is.null(layer$elevation)) {
    warning("Leaflet has limited support for 3D elevation - layer may not render as expected")
  }
  
  return(layer)
}

#' Validate OpenLayers Layer
#'
#' @param layer List containing layer configuration
#' @return Validated OpenLayers layer
validate_openlayers_layer <- function(layer) {
  
  # OpenLayers-specific layer validation
  return(layer)
}

#' Validate Gaode Layer
#'
#' @param layer List containing layer configuration
#' @return Validated Gaode layer
validate_gaode_layer <- function(layer) {
  
  # Gaode-specific layer validation
  return(layer)
}

#' Validate Baidu Layer
#'
#' @param layer List containing layer configuration
#' @return Validated Baidu layer
validate_baidu_layer <- function(layer) {
  
  # Baidu-specific layer validation
  return(layer)
}

#' List Available Providers
#'
#' Get a list of all available map providers that can be used with mapdeck.
#'
#' @description
#' This function returns a character vector of all map providers that are
#' currently available and registered in the provider system. It includes
#' both built-in providers and any custom providers that have been registered.
#'
#' @param include_status Logical indicating if provider status should be included
#' @return Character vector of available provider names, or named list with status if include_status is TRUE
#'
#' @examples
#' \donttest{
#' # Get list of available providers
#' providers <- list_available_providers()
#' print(providers)
#' 
#' # Get providers with status information
#' provider_status <- list_available_providers(include_status = TRUE)
#' print(provider_status)
#' }
#'
#' @export
list_available_providers <- function(include_status = FALSE) {
  
  if (!is.logical(include_status) || length(include_status) != 1) {
    stop("include_status must be a single logical value")
  }
  
  tryCatch({
    factory <- get_provider_factory()
    providers <- factory$get_available_providers()
    
    if (!include_status) {
      return(providers)
    }
    
    # Get status for each provider
    provider_status <- list()
    
    for (provider in providers) {
      config <- factory$registry$get_provider_config(provider)
      provider_class <- factory$registry$get_provider_factory(provider)
      
      status <- list(
        available = !is.null(provider_class),
        authentication_required = if (!is.null(config)) config$authentication_required else FALSE,
        coordinate_system = if (!is.null(config)) config$coordinate_system else "EPSG:4326",
        supported_features = if (!is.null(config)) config$supported_features else list()
      )
      
      provider_status[[provider]] <- status
    }
    
    return(provider_status)
    
  }, error = function(e) {
    # Fallback to basic list if provider system is not available
    warning("Provider system not fully available, returning basic provider list")
    return(c("mapbox", "leaflet", "openlayers", "gaode", "baidu"))
  })
}

# Define %||% operator if not already defined
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}