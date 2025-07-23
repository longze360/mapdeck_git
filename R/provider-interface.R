#' Provider Interface Definition
#'
#' This file defines the abstract interface that all map providers must implement.
#' It ensures consistent behavior across different mapping backends while allowing
#' provider-specific optimizations.
#'
#' @name provider-interface
NULL

#' Base Provider Interface
#'
#' Abstract base class that defines the interface all map providers must implement.
#' This ensures consistent behavior across different mapping backends.
#'
#' @description
#' The IMapProvider class defines the contract that all map providers must follow.
#' It includes methods for initialization, map creation, layer management, and cleanup.
#'
#' @field provider_name Character string identifying the provider type
#' @field config List containing provider-specific configuration
#' @field capabilities List of supported features and limitations
#' @field initialized Logical indicating if provider is initialized
#'
#' @examples
#' \donttest{
#' # This is an abstract class - use concrete implementations like MapboxProvider
#' # provider <- MapboxProvider$new()
#' # provider$initialize(config = list(token = "your_token"))
#' }
#'
#' @export
IMapProvider <- R6::R6Class("IMapProvider",
  public = list(
    provider_name = NULL,
    config = NULL,
    capabilities = NULL,
    initialized = FALSE,
    
    #' Initialize Provider
    #'
    #' Initialize the map provider with configuration settings.
    #' Note: This is separate from R6's initialize() constructor method.
    #'
    #' @param config List containing provider-specific configuration
    #' @return Invisible self for method chaining
    initialize_provider = function(config = list()) {
      stop("initialize_provider() must be implemented by concrete provider classes")
    },
    
    #' Create Map Instance
    #'
    #' Create a new map instance with specified options.
    #'
    #' @param container Character string identifying the HTML container
    #' @param options List of map initialization options
    #' @return Map instance object
    create_map = function(container, options = list()) {
      stop("create_map() must be implemented by concrete provider classes")
    },
    
    #' Update Map Style
    #'
    #' Update the map style/theme.
    #'
    #' @param style Character string or list specifying the new style
    #' @return Invisible self for method chaining
    update_style = function(style) {
      stop("update_style() must be implemented by concrete provider classes")
    },
    
    #' Add Layer to Map
    #'
    #' Add a deck.gl layer to the map.
    #'
    #' @param layer List containing layer configuration
    #' @return Invisible self for method chaining
    add_layer = function(layer) {
      stop("add_layer() must be implemented by concrete provider classes")
    },
    
    #' Remove Layer from Map
    #'
    #' Remove a layer from the map by its ID.
    #'
    #' @param layer_id Character string identifying the layer to remove
    #' @return Invisible self for method chaining
    remove_layer = function(layer_id) {
      stop("remove_layer() must be implemented by concrete provider classes")
    },
    
    #' Set Map View
    #'
    #' Set the map view state (location, zoom, pitch, bearing).
    #'
    #' @param longitude Numeric longitude coordinate
    #' @param latitude Numeric latitude coordinate
    #' @param zoom Numeric zoom level
    #' @param pitch Numeric pitch angle (0-60 degrees)
    #' @param bearing Numeric bearing angle (0-360 degrees)
    #' @return Invisible self for method chaining
    set_view = function(longitude, latitude, zoom, pitch = 0, bearing = 0) {
      stop("set_view() must be implemented by concrete provider classes")
    },
    
    #' Get Available Styles
    #'
    #' Get list of available styles for this provider.
    #'
    #' @return Character vector of available style names
    get_available_styles = function() {
      stop("get_available_styles() must be implemented by concrete provider classes")
    },
    
    #' Validate Configuration
    #'
    #' Validate provider-specific configuration.
    #'
    #' @param config List containing configuration to validate
    #' @return Logical indicating if configuration is valid
    validate_config = function(config) {
      stop("validate_config() must be implemented by concrete provider classes")
    },
    
    #' Destroy Provider
    #'
    #' Clean up resources and destroy the provider instance.
    #'
    #' @return Invisible NULL
    destroy = function() {
      stop("destroy() must be implemented by concrete provider classes")
    },
    
    #' Get Provider Capabilities
    #'
    #' Get the capabilities and limitations of this provider.
    #'
    #' @return List containing capability information
    get_capabilities = function() {
      return(self$capabilities)
    },
    
    #' Check if Provider is Initialized
    #'
    #' Check if the provider has been properly initialized.
    #'
    #' @return Logical indicating initialization status
    is_initialized = function() {
      return(self$initialized)
    }
  )
)

#' Validate Provider Interface Implementation
#'
#' Utility function to validate that a provider class properly implements
#' the IMapProvider interface.
#'
#' @param provider_class R6 class object to validate
#' @return Logical indicating if implementation is valid
#'
#' @examples
#' \donttest{
#' # Validate a provider implementation
#' # is_valid <- validate_provider_interface(MapboxProvider)
#' }
#'
#' @export
validate_provider_interface <- function(provider_class) {
  if (!inherits(provider_class, "R6ClassGenerator")) {
    return(FALSE)
  }
  
  required_methods <- c(
    "initialize_provider", "create_map", "update_style", "add_layer",
    "remove_layer", "set_view", "get_available_styles",
    "validate_config", "destroy"
  )
  
  class_methods <- names(provider_class$public_methods)
  missing_methods <- setdiff(required_methods, class_methods)
  
  if (length(missing_methods) > 0) {
    warning("Provider class missing required methods: ", 
            paste(missing_methods, collapse = ", "))
    return(FALSE)
  }
  
  return(TRUE)
}
