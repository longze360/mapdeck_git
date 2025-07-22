#' Provider Configuration Management
#'
#' This file contains classes and functions for managing provider-specific
#' configuration settings, including authentication, capabilities, and
#' default options.
#'
#' @name provider-config
NULL

#' Provider Configuration Class
#'
#' R6 class for managing provider-specific configuration settings.
#'
#' @field name Character string provider name
#' @field type Character string provider type
#' @field authentication_required Logical indicating if authentication is required
#' @field supported_features Character vector of supported features
#' @field coordinate_system Character string default coordinate system
#' @field default_style Character string default map style
#' @field api_endpoints List of API endpoints for the provider
#' @field rate_limits List of rate limiting configuration
#'
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
    #' @param name Character string provider name
    #' @param type Character string provider type
    #' @param auth_required Logical indicating if authentication is required
    #' @param features Character vector of supported features
    #' @param coord_sys Character string coordinate system (default: "EPSG:4326")
    #' @param style Character string default style
    #' @param endpoints List of API endpoints
    #' @param limits List of rate limiting configuration
    #'
    #' @return New ProviderConfig instance
    initialize = function(name, type, auth_required = FALSE, features = character(0),
                         coord_sys = "EPSG:4326", style = "default",
                         endpoints = list(), limits = list()) {
      self$name <- name
      self$type <- type
      self$authentication_required <- auth_required
      self$supported_features <- features
      self$coordinate_system <- coord_sys
      self$default_style <- style
      self$api_endpoints <- endpoints
      self$rate_limits <- limits
      
      private$validate_config()
    },
    
    #' Check if Feature is Supported
    #'
    #' @param feature Character string feature name to check
    #'
    #' @return Logical indicating if feature is supported
    supports_feature = function(feature) {
      return(feature %in% self$supported_features)
    },
    
    #' Get Configuration as List
    #'
    #' @return List containing all configuration settings
    to_list = function() {
      return(list(
        name = self$name,
        type = self$type,
        authentication_required = self$authentication_required,
        supported_features = self$supported_features,
        coordinate_system = self$coordinate_system,
        default_style = self$default_style,
        api_endpoints = self$api_endpoints,
        rate_limits = self$rate_limits
      ))
    },
    
    #' Update Configuration
    #'
    #' @param updates List of configuration updates
    #'
    #' @return Invisible self for method chaining
    update = function(updates) {
      for (key in names(updates)) {
        if (key %in% names(self)) {
          self[[key]] <- updates[[key]]
        }
      }
      private$validate_config()
      invisible(self)
    }
  ),
  
  private = list(
    #' Validate Configuration Settings
    #'
    #' @return Invisible self
    validate_config = function() {
      if (is.null(self$name) || !is.character(self$name) || length(self$name) != 1) {
        stop("Provider name must be a single character string")
      }
      
      if (is.null(self$type) || !is.character(self$type) || length(self$type) != 1) {
        stop("Provider type must be a single character string")
      }
      
      if (!is.logical(self$authentication_required)) {
        stop("authentication_required must be logical")
      }
      
      if (!is.character(self$supported_features)) {
        stop("supported_features must be a character vector")
      }
      
      invisible(self)
    }
  )
)

#' Create Default Provider Configurations
#'
#' Creates default configuration objects for all supported providers.
#'
#' @return Named list of ProviderConfig objects
#'
#' @export
create_default_provider_configs <- function() {
  configs <- list()
  
  # Mapbox configuration
  configs$mapbox <- ProviderConfig$new(
    name = "mapbox",
    type = "mapbox-gl",
    auth_required = TRUE,
    features = c("vector_tiles", "raster_tiles", "3d_terrain", "custom_styles", 
                "animations", "interactions", "clustering"),
    coord_sys = "EPSG:4326",
    style = "mapbox://styles/mapbox/streets-v11",
    endpoints = list(
      tiles = "https://api.mapbox.com/v4/",
      styles = "https://api.mapbox.com/styles/v1/",
      geocoding = "https://api.mapbox.com/geocoding/v5/"
    ),
    limits = list(
      requests_per_minute = 600,
      requests_per_hour = 50000
    )
  )
  
  # Leaflet configuration
  configs$leaflet <- ProviderConfig$new(
    name = "leaflet",
    type = "leaflet",
    auth_required = FALSE,
    features = c("tile_layers", "overlays", "markers", "popups", "controls"),
    coord_sys = "EPSG:4326",
    style = "OpenStreetMap",
    endpoints = list(
      osm = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
    ),
    limits = list()
  )
  
  # OpenLayers configuration
  configs$openlayers <- ProviderConfig$new(
    name = "openlayers",
    type = "openlayers",
    auth_required = FALSE,
    features = c("vector_layers", "raster_layers", "wms", "wfs", "projections"),
    coord_sys = "EPSG:4326",
    style = "default",
    endpoints = list(),
    limits = list()
  )
  
  # Gaode Maps configuration
  configs$gaode <- ProviderConfig$new(
    name = "gaode",
    type = "gaode",
    auth_required = TRUE,
    features = c("chinese_maps", "poi_search", "route_planning", "traffic"),
    coord_sys = "GCJ02",
    style = "normal",
    endpoints = list(
      api = "https://webapi.amap.com/maps",
      js = "https://webapi.amap.com/maps?v=1.4.15"
    ),
    limits = list(
      requests_per_day = 300000,
      requests_per_minute = 6000
    )
  )
  
  # Baidu Maps configuration
  configs$baidu <- ProviderConfig$new(
    name = "baidu",
    type = "baidu",
    auth_required = TRUE,
    features = c("chinese_maps", "poi_search", "route_planning", "street_view"),
    coord_sys = "BD09",
    style = "normal",
    endpoints = list(
      api = "https://api.map.baidu.com/",
      js = "https://api.map.baidu.com/api?v=3.0"
    ),
    limits = list(
      requests_per_day = 100000,
      requests_per_minute = 6000
    )
  )
  
  return(configs)
}

#' Get Provider Configuration
#'
#' Retrieves configuration for a specific provider.
#'
#' @param provider_name Character string name of the provider
#'
#' @return ProviderConfig object or NULL if not found
#'
#' @export
get_provider_config <- function(provider_name) {
  configs <- create_default_provider_configs()
  
  if (provider_name %in% names(configs)) {
    return(configs[[provider_name]])
  }
  
  return(NULL)
}

#' List Available Providers
#'
#' Returns a character vector of all available provider names.
#'
#' @return Character vector of provider names
#'
#' @export
list_available_providers <- function() {
  configs <- create_default_provider_configs()
  return(names(configs))
}

#' Validate Provider Configuration
#'
#' Validates that a provider configuration is complete and valid.
#'
#' @param config ProviderConfig object to validate
#'
#' @return Logical indicating if configuration is valid
#'
#' @export
validate_provider_config <- function(config) {
  if (!inherits(config, "ProviderConfig")) {
    return(FALSE)
  }
  
  required_fields <- c("name", "type", "authentication_required", 
                      "supported_features", "coordinate_system", "default_style")
  
  for (field in required_fields) {
    if (is.null(config[[field]])) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}