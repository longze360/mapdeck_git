#' Provider Interface Definition
#'
#' Defines the common interface that all map providers must implement.
#' This ensures consistent behavior across different mapping backends.
#'
#' @name IMapProvider
#' @keywords internal
NULL

#' Map Provider Interface
#'
#' R6 class defining the interface that all map providers must implement.
#' This abstract base class ensures consistent behavior across different
#' mapping backends while allowing provider-specific optimizations.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(config)}}{Initialize the provider with configuration}
#'   \item{\code{create_map(container, options)}}{Create a map instance}
#'   \item{\code{update_style(style)}}{Update the map style}
#'   \item{\code{add_layer(layer)}}{Add a deck.gl layer to the map}
#'   \item{\code{remove_layer(layer_id)}}{Remove a layer from the map}
#'   \item{\code{set_view(location, zoom, pitch, bearing)}}{Set the map view}
#'   \item{\code{get_capabilities()}}{Get provider capabilities}
#'   \item{\code{validate_config(config)}}{Validate provider configuration}
#'   \item{\code{destroy()}}{Clean up provider resources}
#' }
#'
#' @importFrom R6 R6Class
#' @keywords internal
IMapProvider <- R6::R6Class(
  "IMapProvider",
  public = list(
    #' @field provider_name Character string identifying the provider
    provider_name = NULL,
    
    #' @field config Provider-specific configuration
    config = NULL,
    
    #' @field capabilities List of provider capabilities
    capabilities = NULL,
    
    #' Initialize Provider
    #'
    #' Initialize the map provider with the given configuration.
    #'
    #' @param config List containing provider-specific configuration options
    #' @return Invisible self for method chaining
    initialize = function(config = list()) {
      stop("initialize() must be implemented by provider subclass")
    },
    
    #' Create Map
    #'
    #' Create a map instance with the specified container and options.
    #'
    #' @param container HTML container element for the map
    #' @param options List of map initialization options
    #' @return Map object specific to the provider
    create_map = function(container, options) {
      stop("create_map() must be implemented by provider subclass")
    },
    
    #' Update Style
    #'
    #' Update the map style to the specified style.
    #'
    #' @param style Character string or list specifying the new style
    #' @return Invisible self for method chaining
    update_style = function(style) {
      stop("update_style() must be implemented by provider subclass")
    },
    
    #' Add Layer
    #'
    #' Add a deck.gl layer to the map.
    #'
    #' @param layer List containing layer configuration and data
    #' @return Invisible self for method chaining
    add_layer = function(layer) {
      stop("add_layer() must be implemented by provider subclass")
    },
    
    #' Remove Layer
    #'
    #' Remove a layer from the map by its identifier.
    #'
    #' @param layer_id Character string identifying the layer to remove
    #' @return Invisible self for method chaining
    remove_layer = function(layer_id) {
      stop("remove_layer() must be implemented by provider subclass")
    },
    
    #' Set View
    #'
    #' Set the map view to the specified location and parameters.
    #'
    #' @param location Numeric vector of longitude and latitude
    #' @param zoom Numeric zoom level
    #' @param pitch Numeric pitch angle in degrees
    #' @param bearing Numeric bearing angle in degrees
    #' @return Invisible self for method chaining
    set_view = function(location = NULL, zoom = NULL, pitch = NULL, bearing = NULL) {
      stop("set_view() must be implemented by provider subclass")
    },
    
    #' Get Capabilities
    #'
    #' Get the capabilities supported by this provider.
    #'
    #' @return List of provider capabilities
    get_capabilities = function() {
      return(self$capabilities)
    },
    
    #' Validate Configuration
    #'
    #' Validate the provider configuration and return any errors.
    #'
    #' @param config List containing configuration to validate
    #' @return List with 'valid' (logical) and 'errors' (character vector)
    validate_config = function(config) {
      stop("validate_config() must be implemented by provider subclass")
    },
    
    #' Destroy Provider
    #'
    #' Clean up provider resources and destroy the map instance.
    #'
    #' @return Invisible NULL
    destroy = function() {
      stop("destroy() must be implemented by provider subclass")
    }
  )
)

#' Provider Capabilities
#'
#' Standard capabilities that providers can support.
#'
#' @export
PROVIDER_CAPABILITIES <- list(
  AUTHENTICATION_REQUIRED = "authentication_required",
  CUSTOM_STYLES = "custom_styles",
  COORDINATE_TRANSFORM = "coordinate_transform",
  TILE_LAYERS = "tile_layers",
  VECTOR_LAYERS = "vector_layers",
  RASTER_LAYERS = "raster_layers",
  TERRAIN_LAYERS = "terrain_layers",
  SATELLITE_IMAGERY = "satellite_imagery",
  TRAFFIC_LAYERS = "traffic_layers",
  TRANSIT_LAYERS = "transit_layers",
  INDOOR_MAPPING = "indoor_mapping",
  OFFLINE_SUPPORT = "offline_support",
  CLUSTERING = "clustering",
  HEATMAPS = "heatmaps",
  ANIMATIONS = "animations",
  THREE_D_BUILDINGS = "3d_buildings",
  CUSTOM_MARKERS = "custom_markers",
  POPUP_SUPPORT = "popup_support",
  DRAWING_TOOLS = "drawing_tools",
  MEASUREMENT_TOOLS = "measurement_tools"
)

#' Provider Types
#'
#' Standard provider type identifiers.
#'
#' @export
PROVIDER_TYPES <- list(
  MAPBOX = "mapbox",
  LEAFLET = "leaflet",
  OPENLAYERS = "openlayers",
  GAODE = "gaode",
  BAIDU = "baidu"
)