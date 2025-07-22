#' Provider Interface Definition
#'
#' This file defines the abstract interface that all map providers must implement.
#' It ensures consistent behavior across different mapping backends while allowing
#' provider-specific optimizations.
#'
#' @name provider-interface
NULL

#' Abstract Map Provider Interface
#'
#' R6 class defining the interface that all map providers must implement.
#' This ensures consistent behavior across different mapping backends.
#'
#' @field provider_name Character string identifying the provider
#' @field provider_type Character string indicating provider type
#' @field config List containing provider-specific configuration
#' @field capabilities List of supported features and capabilities
#'
#' @export
IMapProvider <- R6::R6Class("IMapProvider",
  public = list(
    provider_name = NULL,
    provider_type = NULL,
    config = NULL,
    capabilities = NULL,
    
    #' Initialize Provider
    #'
    #' Initialize the map provider with configuration settings.
    #'
    #' @param config List containing provider-specific configuration
    #'
    #' @return Invisible self for method chaining
    initialize = function(config = list()) {
      stop("initialize() must be implemented by provider subclass")
    },
    
    #' Create Map Instance
    #'
    #' Create a new map instance with the specified container and options.
    #'
    #' @param container Character string identifying the HTML container
    #' @param options List of map initialization options
    #'
    #' @return Map instance object
    create_map = function(container, options = list()) {
      stop("create_map() must be implemented by provider subclass")
    },
    
    #' Update Map Style
    #'
    #' Update the map style to the specified style.
    #'
    #' @param style Character string or list specifying the map style
    #'
    #' @return Invisible self for method chaining
    update_style = function(style) {
      stop("update_style() must be implemented by provider subclass")
    },
    
    #' Add Layer to Map
    #'
    #' Add a deck.gl layer to the map.
    #'
    #' @param layer List containing layer configuration
    #'
    #' @return Invisible self for method chaining
    add_layer = function(layer) {
      stop("add_layer() must be implemented by provider subclass")
    },
    
    #' Remove Layer from Map
    #'
    #' Remove a layer from the map by its ID.
    #'
    #' @param layer_id Character string identifying the layer to remove
    #'
    #' @return Invisible self for method chaining
    remove_layer = function(layer_id) {
      stop("remove_layer() must be implemented by provider subclass")
    },
    
    #' Set Map View
    #'
    #' Set the map view to the specified location and parameters.
    #'
    #' @param longitude Numeric longitude coordinate
    #' @param latitude Numeric latitude coordinate
    #' @param zoom Numeric zoom level
    #' @param pitch Numeric pitch angle (optional)
    #' @param bearing Numeric bearing angle (optional)
    #'
    #' @return Invisible self for method chaining
    set_view = function(longitude, latitude, zoom, pitch = 0, bearing = 0) {
      stop("set_view() must be implemented by provider subclass")
    },
    
    #' Get Available Styles
    #'
    #' Get list of available styles for this provider.
    #'
    #' @return Character vector of available style names
    get_available_styles = function() {
      stop("get_available_styles() must be implemented by provider subclass")
    },
    
    #' Validate Configuration
    #'
    #' Validate provider-specific configuration settings.
    #'
    #' @param config List containing configuration to validate
    #'
    #' @return Logical indicating if configuration is valid
    validate_config = function(config) {
      stop("validate_config() must be implemented by provider subclass")
    },
    
    #' Get Provider Capabilities
    #'
    #' Get list of capabilities supported by this provider.
    #'
    #' @return List of supported capabilities
    get_capabilities = function() {
      return(self$capabilities)
    },
    
    #' Destroy Map Instance
    #'
    #' Clean up and destroy the map instance.
    #'
    #' @return Invisible self
    destroy = function() {
      stop("destroy() must be implemented by provider subclass")
    }
  )
)

#' Validate Provider Interface Implementation
#'
#' Validates that a provider class properly implements the IMapProvider interface.
#'
#' @param provider_class R6 class to validate
#'
#' @return Logical indicating if the provider implements the interface correctly
#'
#' @export
validate_provider_interface <- function(provider_class) {
  if (!R6::is.R6Class(provider_class)) {
    return(FALSE)
  }
  
  required_methods <- c(
    "initialize", "create_map", "update_style", "add_layer", 
    "remove_layer", "set_view", "get_available_styles", 
    "validate_config", "get_capabilities", "destroy"
  )
  
  class_methods <- names(provider_class$public_methods)
  missing_methods <- setdiff(required_methods, class_methods)
  
  if (length(missing_methods) > 0) {
    warning(paste("Provider missing required methods:", 
                  paste(missing_methods, collapse = ", ")))
    return(FALSE)
  }
  
  return(TRUE)
}