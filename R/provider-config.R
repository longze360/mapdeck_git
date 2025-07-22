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
#' @field name Character string identifying the provider
#' @field type Character string indicating provider type
#' @field authentication_required Logical indicating if authentication is required
#' @field supported_features List of supported features
#' @field coordinate_system Character string indicating coordinate system
#' @field default_style Character string for default style
#' @field api_endpoints List of API endpoints for the provider
#' @field rate_limits List of rate limiting information
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
    #' @param name Character string identifying the provider
    #' @param type Character string indicating provider type
    #' @param auth_required Logical indicating if authentication is required
    #' @param features List of supported features
    #' @param coord_sys Character string indicating coordinate system
    #' @param style Character string for default style
    #' @param endpoints List of API endpoints (optional)
    #' @param limits List of rate limiting information (optional)
    #'
    #' @return New ProviderConfig instance
    initialize = function(name, type, auth_required, features, coord_sys, 
                         style, endpoints = list(), limits = list()) {
      self$name <- name
      self$type <- type
      self$authentication_required <- auth_required
      self$supported_features <- features
      self$coordinate_system <- coord_sys
      self$default_style <- style
      self$api_endpoints <- endpoints
      self$rate_limits <- limits
      
      # Validate configuration
      self$validate()
    },
    
    #' Validate Configuration
    #'
    #' Validates the provider configuration for completeness and correctness.
    #'
    #' @return Logical indicating if configuration is valid
    validate = function() {
      if (is.null(self$name) || !is.character(self$name) || length(self$name) != 1) {
        stop("Provider name must be a single character string")
      }
      
      if (is.null(self$type) || !is.character(self$type) || length(self$type) != 1) {
        stop("Provider type must be a single character string")
      }
      
      valid_types <- c("mapbox", "leaflet", "openlayers", "gaode", "baidu")
      if (!self$type %in% valid_types) {
        stop(paste("Provider type must be one of:", paste(valid_types, collapse = ", ")))
      }
      
      if (!is.logical(self$authentication_required)) {
        stop("authentication_required must be logical")
      }
      
      if (!is.list(self$supported_features)) {
        stop("supported_features must be a list")
      }
      
      valid_coord_systems <- c("WGS84", "GCJ02", "BD09")
      if (!self$coordinate_system %in% valid_coord_systems) {
        stop(paste("coordinate_system must be one of:", 
                   paste(valid_coord_systems, collapse = ", ")))
      }
      
      return(TRUE)
    },
    
    #' Get Configuration as List
    #'
    #' Returns the configuration as a named list.
    #'
    #' @return Named list containing all configuration settings
    to_list = function() {
      list(
        name = self$name,
        type = self$type,
        authentication_required = self$authentication_required,
        supported_features = self$supported_features,
        coordinate_system = self$coordinate_system,
        default_style = self$default_style,
        api_endpoints = self$api_endpoints,
        rate_limits = self$rate_limits
      )
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
    type = "mapbox",
    auth_required = TRUE,
    features = list(
      vector_tiles = TRUE,
      custom_styles = TRUE,
      3d_terrain = TRUE,
      satellite_imagery = TRUE,
      traffic_data = TRUE
    ),
    coord_sys = "WGS84",
    style = "mapbox://styles/mapbox/streets-v11",
    endpoints = list(
      styles = "https://api.mapbox.com/styles/v1",
      tiles = "https://api.mapbox.com/v4"
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
    features = list(
      tile_layers = TRUE,
      custom_markers = TRUE,
      popup_support = TRUE,
      layer_control = TRUE,
      plugin_support = TRUE
    ),
    coord_sys = "WGS84",
    style = "OpenStreetMap",
    endpoints = list(
      osm_tiles = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
    ),
    limits = list(
      requests_per_minute = 300,
      tile_usage_policy = "fair_use"
    )
  )
  
  # OpenLayers configuration
  configs$openlayers <- ProviderConfig$new(
    name = "openlayers",
    type = "openlayers",
    auth_required = FALSE,
    features = list(
      vector_layers = TRUE,
      raster_layers = TRUE,
      wms_support = TRUE,
      wfs_support = TRUE,
      custom_projections = TRUE
    ),
    coord_sys = "WGS84",
    style = "OSM",
    endpoints = list(
      osm_tiles = "https://{a-c}.tile.openstreetmap.org/{z}/{x}/{y}.png"
    ),
    limits = list(
      requests_per_minute = 300,
      tile_usage_policy = "fair_use"
    )
  )
  
  # Gaode Maps configuration
  configs$gaode <- ProviderConfig$new(
    name = "gaode",
    type = "gaode",
    auth_required = TRUE,
    features = list(
      chinese_maps = TRUE,
      traffic_data = TRUE,
      poi_search = TRUE,
      route_planning = TRUE,
      geocoding = TRUE
    ),
    coord_sys = "GCJ02",
    style = "normal",
    endpoints = list(
      api_base = "https://webapi.amap.com/maps",
      js_api = "https://webapi.amap.com/maps?v=1.4.15"
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
    features = list(
      chinese_maps = TRUE,
      street_view = TRUE,
      traffic_data = TRUE,
      poi_search = TRUE,
      route_planning = TRUE
    ),
    coord_sys = "BD09",
    style = "normal",
    endpoints = list(
      api_base = "https://api.map.baidu.com",
      js_api = "https://api.map.baidu.com/api?v=3.0"
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
#' @param provider_name Character string identifying the provider
#'
#' @return ProviderConfig object for the specified provider
#'
#' @export
get_provider_config <- function(provider_name) {
  configs <- create_default_provider_configs()
  
  if (!provider_name %in% names(configs)) {
    stop(paste("Unknown provider:", provider_name, 
               ". Available providers:", paste(names(configs), collapse = ", ")))
  }
  
  return(configs[[provider_name]])
}

#' List Available Providers
#'
#' Returns a list of all available provider names.
#'
#' @return Character vector of available provider names
#'
#' @export
list_available_providers <- function() {
  configs <- create_default_provider_configs()
  return(names(configs))
}