#' Provider Configuration Management
#'
#' This file contains classes and functions for managing provider-specific
#' configuration settings, including authentication, capabilities, and defaults.
#'
#' @name provider-config
NULL

#' Provider Configuration Class
#'
#' R6 class for managing provider-specific configuration settings.
#'
#' @description
#' The ProviderConfig class encapsulates all configuration settings for a
#' map provider, including authentication requirements, supported features,
#' coordinate systems, and default styles.
#'
#' @field name Character string identifying the provider
#' @field type Character string indicating provider type
#' @field authentication_required Logical indicating if authentication is needed
#' @field supported_features List of supported features and capabilities
#' @field coordinate_system Character string indicating default coordinate system
#' @field default_style Character string indicating default map style
#' @field api_endpoints List of API endpoints for the provider
#' @field rate_limits List of rate limiting information
#'
#' @examples
#' \donttest{
#' # Create a provider configuration
#' config <- ProviderConfig$new(
#'   name = "mapbox",
#'   type = "webgl",
#'   authentication_required = TRUE,
#'   supported_features = list(
#'     "3d_terrain" = TRUE,
#'     "custom_styles" = TRUE,
#'     "vector_tiles" = TRUE
#'   ),
#'   coordinate_system = "EPSG:4326",
#'   default_style = "mapbox://styles/mapbox/streets-v11"
#' )
#' }
#' @export
ProviderConfig <- R6::R6Class("ProviderConfig",
  public = list(
    name = NULL,
    type = NULL,
    authentication_required = NULL,
    supported_features = NULL,
    coordinate_system = NULL,
    default_style = NULL,
    api_endpoints = NULL,
    rate_limits = NULL,
    
    #' Initialize Provider Configuration
    #'
    #' Create a new provider configuration instance.
    #'
    #' @param name Character string identifying the provider
    #' @param type Character string indicating provider type
    #' @param authentication_required Logical indicating if auth is needed
    #' @param supported_features List of supported features
    #' @param coordinate_system Character string for coordinate system
    #' @param default_style Character string for default style
    #' @param api_endpoints List of API endpoints (optional)
    #' @param rate_limits List of rate limiting info (optional)
    #' @return New ProviderConfig instance
    initialize = function(name, type, authentication_required, supported_features,
                         coordinate_system, default_style, api_endpoints = list(),
                         rate_limits = list()) {
      self$name <- name
      self$type <- type
      self$authentication_required <- authentication_required
      self$supported_features <- supported_features
      self$coordinate_system <- coordinate_system
      self$default_style <- default_style
      self$api_endpoints <- api_endpoints
      self$rate_limits <- rate_limits
      
      # Validate configuration
      self$validate()
    },
    
    #' Validate Configuration
    #'
    #' Validate that all required configuration fields are present and valid.
    #'
    #' @return Logical indicating if configuration is valid
    validate = function() {
      # Check required fields
      required_fields <- c("name", "type", "authentication_required", 
                          "supported_features", "coordinate_system", "default_style")
      
      for (field in required_fields) {
        if (is.null(self[[field]])) {
          stop(sprintf("Required configuration field '%s' is missing", field))
        }
      }
      
      # Validate field types
      if (!is.character(self$name) || length(self$name) != 1) {
        stop("Configuration field 'name' must be a single character string")
      }
      
      if (!is.character(self$type) || length(self$type) != 1) {
        stop("Configuration field 'type' must be a single character string")
      }
      
      if (!is.logical(self$authentication_required) || length(self$authentication_required) != 1) {
        stop("Configuration field 'authentication_required' must be a single logical value")
      }
      
      if (!is.list(self$supported_features)) {
        stop("Configuration field 'supported_features' must be a list")
      }
      
      return(TRUE)
    },
    
    #' Check Feature Support
    #'
    #' Check if a specific feature is supported by this provider.
    #'
    #' @param feature Character string identifying the feature
    #' @return Logical indicating if feature is supported
    supports_feature = function(feature) {
      if (!is.character(feature) || length(feature) != 1) {
        stop("Feature must be a single character string")
      }
      
      return(isTRUE(self$supported_features[[feature]]))
    },
    
    #' Get Configuration Summary
    #'
    #' Get a summary of the provider configuration.
    #'
    #' @return List containing configuration summary
    get_summary = function() {
      list(
        name = self$name,
        type = self$type,
        authentication_required = self$authentication_required,
        coordinate_system = self$coordinate_system,
        default_style = self$default_style,
        supported_features = names(self$supported_features)[
          sapply(self$supported_features, isTRUE)
        ]
      )
    },
    
    #' Update Configuration
    #'
    #' Update specific configuration fields.
    #'
    #' @param ... Named arguments for fields to update
    #' @return Invisible self for method chaining
    update = function(...) {
      updates <- list(...)
      
      for (field_name in names(updates)) {
        if (field_name %in% names(self)) {
          self[[field_name]] <- updates[[field_name]]
        } else {
          warning(sprintf("Unknown configuration field: %s", field_name))
        }
      }
      
      # Re-validate after updates
      self$validate()
      
      invisible(self)
    }
  )
)

#' Create Default Provider Configurations
#'
#' Create default configurations for all supported providers.
#'
#' @return Named list of ProviderConfig objects
#'
#' @examples
#' \donttest{
#' # Get default configurations
#' configs <- create_default_provider_configs()
#' mapbox_config <- configs$mapbox
#' }
#'
#' @export
create_default_provider_configs <- function() {
  configs <- list()
  
  # Mapbox configuration
  configs$mapbox <- ProviderConfig$new(
    name = "mapbox",
    type = "webgl",
    authentication_required = TRUE,
    supported_features = list(
      "3d_terrain" = TRUE,
      "custom_styles" = TRUE,
      "vector_tiles" = TRUE,
      "raster_tiles" = TRUE,
      "satellite_imagery" = TRUE,
      "real_time_updates" = TRUE,
      "clustering" = TRUE,
      "animations" = TRUE
    ),
    coordinate_system = "EPSG:4326",
    default_style = "mapbox://styles/mapbox/streets-v11",
    api_endpoints = list(
      "styles" = "https://api.mapbox.com/styles/v1",
      "tiles" = "https://api.mapbox.com/v4",
      "geocoding" = "https://api.mapbox.com/geocoding/v5"
    ),
    rate_limits = list(
      "requests_per_minute" = 600,
      "requests_per_hour" = 50000
    )
  )
  
  # Leaflet configuration
  configs$leaflet <- ProviderConfig$new(
    name = "leaflet",
    type = "dom",
    authentication_required = FALSE,
    supported_features = list(
      "3d_terrain" = FALSE,
      "custom_styles" = TRUE,
      "vector_tiles" = FALSE,
      "raster_tiles" = TRUE,
      "satellite_imagery" = TRUE,
      "real_time_updates" = TRUE,
      "clustering" = TRUE,
      "animations" = FALSE
    ),
    coordinate_system = "EPSG:4326",
    default_style = "OpenStreetMap",
    api_endpoints = list(),
    rate_limits = list()
  )
  
  # OpenLayers configuration
  configs$openlayers <- ProviderConfig$new(
    name = "openlayers",
    type = "canvas",
    authentication_required = FALSE,
    supported_features = list(
      "3d_terrain" = FALSE,
      "custom_styles" = TRUE,
      "vector_tiles" = TRUE,
      "raster_tiles" = TRUE,
      "satellite_imagery" = TRUE,
      "real_time_updates" = TRUE,
      "clustering" = TRUE,
      "animations" = TRUE
    ),
    coordinate_system = "EPSG:4326",
    default_style = "OSM",
    api_endpoints = list(),
    rate_limits = list()
  )
  
  # Gaode Maps configuration
  configs$gaode <- ProviderConfig$new(
    name = "gaode",
    type = "webgl",
    authentication_required = TRUE,
    supported_features = list(
      "3d_terrain" = TRUE,
      "custom_styles" = TRUE,
      "vector_tiles" = TRUE,
      "raster_tiles" = TRUE,
      "satellite_imagery" = TRUE,
      "real_time_updates" = TRUE,
      "clustering" = TRUE,
      "animations" = TRUE,
      "coordinate_transform" = TRUE
    ),
    coordinate_system = "GCJ02",
    default_style = "amap://styles/normal",
    api_endpoints = list(
      "maps" = "https://webapi.amap.com/maps",
      "geocoding" = "https://restapi.amap.com/v3/geocode"
    ),
    rate_limits = list(
      "requests_per_day" = 300000,
      "requests_per_minute" = 6000
    )
  )
  
  # Baidu Maps configuration
  configs$baidu <- ProviderConfig$new(
    name = "baidu",
    type = "webgl",
    authentication_required = TRUE,
    supported_features = list(
      "3d_terrain" = TRUE,
      "custom_styles" = TRUE,
      "vector_tiles" = FALSE,
      "raster_tiles" = TRUE,
      "satellite_imagery" = TRUE,
      "real_time_updates" = TRUE,
      "clustering" = TRUE,
      "animations" = TRUE,
      "coordinate_transform" = TRUE
    ),
    coordinate_system = "BD09",
    default_style = "normal",
    api_endpoints = list(
      "maps" = "https://api.map.baidu.com",
      "geocoding" = "https://api.map.baidu.com/geocoding/v3"
    ),
    rate_limits = list(
      "requests_per_day" = 100000,
      "requests_per_minute" = 6000
    )
  )
  
  return(configs)
}

#' Get Provider Configuration
#'
#' Get configuration for a specific provider.
#'
#' @param provider_name Character string identifying the provider
#' @return ProviderConfig object or NULL if not found
#'
#' @examples
#' \donttest{
#' # Get Mapbox configuration
#' config <- get_provider_config("mapbox")
#' }
#'
#' @export
get_provider_config <- function(provider_name) {
  if (!is.character(provider_name) || length(provider_name) != 1) {
    stop("Provider name must be a single character string")
  }
  
  configs <- create_default_provider_configs()
  
  if (provider_name %in% names(configs)) {
    return(configs[[provider_name]])
  } else {
    return(NULL)
  }
}

#' List Available Providers
#'
#' Get list of all available provider names.
#'
#' @return Character vector of provider names
#'
#' @examples
#' \donttest{
#' # List all available providers
#' providers <- list_available_providers()
#' }
#'
#' @export
list_available_providers <- function() {
  configs <- create_default_provider_configs()
  return(names(configs))
}
