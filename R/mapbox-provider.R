#' Mapbox Provider Implementation
#'
#' This file contains the MapboxProvider class that implements the IMapProvider
#' interface for Mapbox GL JS integration. It maintains backward compatibility
#' with existing mapdeck functionality while providing the new provider interface.
#'
#' @name mapbox-provider
NULL

#' Mapbox Provider Class
#'
#' R6 class implementing the IMapProvider interface for Mapbox GL JS.
#'
#' @description
#' The MapboxProvider class provides Mapbox GL JS integration through the
#' provider interface. It maintains full backward compatibility with existing
#' mapdeck functionality while enabling multi-provider support.
#'
#'
#' @examples
#' \donttest{
#' # Create Mapbox provider
#' provider <- MapboxProvider$new()
#' provider$initialize(list(token = "pk.your_token_here"))
#' }
#'
#' @export
MapboxProvider <- R6::R6Class("MapboxProvider",
  inherit = IMapProvider,
  public = list(
    #' @field mapbox_config Mapbox configuration
    mapbox_config = NULL,
    
    #' @field access_token Mapbox access token
    access_token = NULL,
    
    #' @field current_style Current map style
    current_style = NULL,
    
    #' @field layers Active layers list
    layers = NULL,
    
    #' @field map_instance Map instance reference
    map_instance = NULL,
    
    #' Initialize Constructor
    #'
    #' R6 constructor - sets up basic structure without full initialization.
    #'
    #' @return New MapboxProvider instance
    initialize = function() {
      # Set provider identification
      self$provider_name <- "mapbox"
      self$layers <- list()
      self$initialized <- FALSE
      
      # Get provider capabilities from configuration
      tryCatch({
        provider_config <- get_provider_config("mapbox")
        if (!is.null(provider_config)) {
          self$capabilities <- provider_config$supported_features
        }
      }, error = function(e) {
        # Silently handle missing provider config during construction
        self$capabilities <- list()
      })
    },
    
    #' Initialize Provider
    #'
    #' Initialize the Mapbox provider with configuration settings.
    #'
    #' @param config List containing Mapbox-specific configuration
    #' @return Invisible self for method chaining
    initialize_provider = function(config = list()) {
      self$config <- config
      
      # Extract access token from config or token store
      if (!is.null(config$token)) {
        self$access_token <- config$token
        # Also store in token store for consistency
        tryCatch({
          token_store <- get_token_store()
          token_store$set_token("mapbox", config$token, config$scope %||% "default")
        }, error = function(e) {
          # Silently handle token store errors
        })
      } else {
        # Try to get token from token store
        tryCatch({
          token_store <- get_token_store()
          self$access_token <- token_store$get_token("mapbox", config$scope %||% "default")
        }, error = function(e) {
          self$access_token <- NULL
        })
      }
      
      # Set default style
      if (!is.null(config$style)) {
        self$current_style <- config$style
      } else {
        self$current_style <- "mapbox://styles/mapbox/streets-v11"
      }
      
      # Store Mapbox-specific configuration
      self$mapbox_config <- list(
        token = self$access_token,
        style = self$current_style,
        pitch = config$pitch %||% 0,
        zoom = config$zoom %||% 0,
        bearing = config$bearing %||% 0,
        location = config$location %||% c(0, 0),
        max_zoom = config$max_zoom %||% 20,
        min_zoom = config$min_zoom %||% 0,
        max_pitch = config$max_pitch %||% 60,
        min_pitch = config$min_pitch %||% 0
      )
      
      # Validate configuration
      if (!self$validate_config(self$config)) {
        stop("Invalid Mapbox provider configuration")
      }
      
      self$initialized <- TRUE
      invisible(self)
    },
    
    #' Create Map Instance
    #'
    #' Create a new Mapbox map instance with specified options.
    #'
    #' @param container Character string identifying the HTML container
    #' @param options List of map initialization options
    #' @return Map instance object (htmlwidget)
    create_map = function(container = NULL, options = list()) {
      if (!self$initialized) {
        stop("Provider must be initialized before creating map")
      }
      
      # Validate and merge options
      validated_options <- validate_map_options(options, "mapbox")
      
      # Merge with provider configuration
      map_options <- list(
        access_token = self$access_token,
        style = validated_options$style %||% self$current_style,
        pitch = validated_options$pitch %||% self$mapbox_config$pitch,
        zoom = validated_options$zoom %||% self$mapbox_config$zoom,
        bearing = validated_options$bearing %||% self$mapbox_config$bearing,
        location = validated_options$location %||% self$mapbox_config$location,
        max_zoom = validated_options$max_zoom %||% self$mapbox_config$max_zoom,
        min_zoom = validated_options$min_zoom %||% self$mapbox_config$min_zoom,
        max_pitch = validated_options$max_pitch %||% self$mapbox_config$max_pitch,
        min_pitch = validated_options$min_pitch %||% self$mapbox_config$min_pitch,
        show_view_state = validated_options$show_view_state %||% FALSE,
        repeat_view = validated_options$repeat_view %||% FALSE
      )
      
      # Create the htmlwidget using existing mapdeck infrastructure
      map_widget <- htmlwidgets::createWidget(
        name = "mapdeck",
        x = structure(
          map_options,
          mapdeck_data = validated_options$data
        ),
        width = validated_options$width,
        height = validated_options$height,
        package = "mapdeck",
        sizingPolicy = htmlwidgets::sizingPolicy(
          defaultWidth = "100%",
          defaultHeight = 800,
          padding = validated_options$padding %||% 0,
          browser.fill = FALSE
        )
      )
      
      # Add dependencies
      map_widget <- add_dependencies(map_widget)
      
      # Add required JavaScript dependencies
      map_widget$dependencies <- c(
        if (!is.null(validated_options$libraries) && 
            "h3" %in% validated_options$libraries) mapdeckH3JSDependency() else NULL,
        map_widget$dependencies,
        mapboxgl(),
        mapdeck_js(),
        htmlwidgets_js()
      )
      
      # Store reference to map instance
      self$map_instance <- map_widget
      
      return(map_widget)
    },
    
    #' Update Map Style
    #'
    #' Update the Mapbox map style.
    #'
    #' @param style Character string or list specifying the new style
    #' @return Invisible self for method chaining
    update_style = function(style) {
      if (!self$initialized) {
        stop("Provider must be initialized before updating style")
      }
      
      # Normalize style for Mapbox
      normalized_style <- normalize_style_name(style, "mapbox")
      
      # Update internal state
      self$current_style <- normalized_style
      self$mapbox_config$style <- normalized_style
      
      # Update map if instance exists
      if (!is.null(self$map_instance)) {
        self$map_instance <- invoke_method(
          self$map_instance, "md_update_style", normalized_style
        )
      }
      
      invisible(self)
    },
    
    #' Add Layer to Map
    #'
    #' Add a deck.gl layer to the Mapbox map.
    #'
    #' @param layer List containing layer configuration
    #' @return Invisible self for method chaining
    add_layer = function(layer) {
      if (!self$initialized) {
        stop("Provider must be initialized before adding layers")
      }
      
      # Validate layer configuration
      validated_layer <- validate_layer_config(layer, "mapbox")
      
      # Store layer reference
      self$layers[[validated_layer$id]] <- validated_layer
      
      # Add layer to map if instance exists
      if (!is.null(self$map_instance)) {
        # This would typically call the appropriate add_* function
        # For now, we'll store the layer and let the existing layer
        # functions handle the actual addition
      }
      
      invisible(self)
    },
    
    #' Remove Layer from Map
    #'
    #' Remove a layer from the Mapbox map by its ID.
    #'
    #' @param layer_id Character string identifying the layer to remove
    #' @return Invisible self for method chaining
    remove_layer = function(layer_id) {
      if (!self$initialized) {
        stop("Provider must be initialized before removing layers")
      }
      
      if (!is.character(layer_id) || length(layer_id) != 1) {
        stop("Layer ID must be a single character string")
      }
      
      # Remove from internal storage
      if (layer_id %in% names(self$layers)) {
        self$layers[[layer_id]] <- NULL
      }
      
      # Remove from map if instance exists
      if (!is.null(self$map_instance)) {
        # This would typically call a clear function
        # The existing clear_* functions handle layer removal
      }
      
      invisible(self)
    },
    
    #' Set Map View
    #'
    #' Set the Mapbox map view state.
    #'
    #' @param longitude Numeric longitude coordinate
    #' @param latitude Numeric latitude coordinate
    #' @param zoom Numeric zoom level
    #' @param pitch Numeric pitch angle (0-60 degrees)
    #' @param bearing Numeric bearing angle (0-360 degrees)
    #' @return Invisible self for method chaining
    set_view = function(longitude, latitude, zoom, pitch = 0, bearing = 0) {
      if (!self$initialized) {
        stop("Provider must be initialized before setting view")
      }
      
      # Validate coordinates
      if (!is.numeric(longitude) || length(longitude) != 1 ||
          longitude < -180 || longitude > 180) {
        stop("Longitude must be a single numeric value between -180 and 180")
      }
      
      if (!is.numeric(latitude) || length(latitude) != 1 ||
          latitude < -90 || latitude > 90) {
        stop("Latitude must be a single numeric value between -90 and 90")
      }
      
      if (!is.numeric(zoom) || length(zoom) != 1 ||
          zoom < 0 || zoom > 24) {
        stop("Zoom must be a single numeric value between 0 and 24")
      }
      
      if (!is.numeric(pitch) || length(pitch) != 1 ||
          pitch < 0 || pitch > 60) {
        stop("Pitch must be a single numeric value between 0 and 60")
      }
      
      if (!is.numeric(bearing) || length(bearing) != 1 ||
          bearing < 0 || bearing >= 360) {
        stop("Bearing must be a single numeric value between 0 and 360")
      }
      
      # Update internal configuration
      self$mapbox_config$location <- c(longitude, latitude)
      self$mapbox_config$zoom <- zoom
      self$mapbox_config$pitch <- pitch
      self$mapbox_config$bearing <- bearing
      
      # Update map view if instance exists
      if (!is.null(self$map_instance)) {
        self$map_instance <- mapdeck_view(
          self$map_instance,
          location = c(longitude, latitude),
          zoom = zoom,
          pitch = pitch,
          bearing = bearing
        )
      }
      
      invisible(self)
    },
    
    #' Get Available Styles
    #'
    #' Get list of available Mapbox styles with enhanced categorization.
    #'
    #' @param category Character string to filter by category (optional)
    #' @return Character vector of available style names or named list if categorized
    get_available_styles = function(category = NULL) {
      styles <- list(
        "basic" = c(
          "mapbox://styles/mapbox/streets-v11",
          "mapbox://styles/mapbox/outdoors-v11",
          "mapbox://styles/mapbox/light-v10",
          "mapbox://styles/mapbox/dark-v10"
        ),
        "satellite" = c(
          "mapbox://styles/mapbox/satellite-v9",
          "mapbox://styles/mapbox/satellite-streets-v11"
        ),
        "navigation" = c(
          "mapbox://styles/mapbox/navigation-day-v1",
          "mapbox://styles/mapbox/navigation-night-v1"
        )
      )
      
      if (is.null(category)) {
        return(unlist(styles, use.names = FALSE))
      } else {
        if (category %in% names(styles)) {
          return(styles[[category]])
        } else {
          warning(sprintf("Unknown style category: %s", category))
          return(character(0))
        }
      }
    },
    
    #' Validate Style
    #'
    #' Validate that a style is compatible with Mapbox.
    #'
    #' @param style Character string or list containing style specification
    #' @return Logical indicating if style is valid
    validate_style = function(style) {
      if (is.null(style)) {
        return(TRUE)  # NULL style will use default
      }
      
      if (is.character(style) && length(style) == 1) {
        # Check if it's a valid Mapbox style URL
        if (grepl("^mapbox://styles/", style)) {
          return(TRUE)
        }
        
        # Check if it's a valid HTTP(S) URL for custom styles
        if (grepl("^https?://", style)) {
          return(TRUE)
        }
        
        # Check if it's a style alias that can be normalized
        tryCatch({
          normalized <- normalize_style_name(style, "mapbox")
          # Only return TRUE if the normalized style is different from input
          # or if it's a known alias
          return(normalized != style || style %in% c("streets", "satellite", "dark"))
        }, error = function(e) {
          return(FALSE)
        })
      }
      
      if (is.list(style)) {
        # Custom style object - basic validation
        required_fields <- c("version", "sources", "layers")
        return(all(required_fields %in% names(style)))
      }
      
      return(FALSE)
    },
    
    #' Create Custom Style
    #'
    #' Create a custom Mapbox style configuration.
    #'
    #' @param base_style Character string for base style to extend
    #' @param modifications List of style modifications
    #' @return List containing custom style configuration
    create_custom_style = function(base_style = "mapbox://styles/mapbox/streets-v11", 
                                  modifications = list()) {
      if (!self$validate_style(base_style)) {
        stop("Invalid base style provided")
      }
      
      # Create custom style configuration
      custom_style <- list(
        base = base_style,
        modifications = modifications,
        created_at = Sys.time(),
        provider = "mapbox"
      )
      
      # Add common modification helpers
      if (!is.null(modifications$colors)) {
        custom_style$color_overrides <- modifications$colors
      }
      
      if (!is.null(modifications$fonts)) {
        custom_style$font_overrides <- modifications$fonts
      }
      
      return(custom_style)
    },
    
    #' Transform Coordinates
    #'
    #' Transform coordinates for Mapbox (WGS84 is native).
    #'
    #' @param data Data frame or matrix containing coordinates
    #' @param from_crs Character string indicating source coordinate system
    #' @param to_crs Character string indicating target coordinate system
    #' @return Transformed coordinate data
    transform_coordinates = function(data, from_crs = "EPSG:4326", to_crs = "EPSG:4326") {
      # Mapbox uses WGS84 (EPSG:4326) natively
      if (from_crs == "EPSG:4326" && to_crs == "EPSG:4326") {
        return(data)  # No transformation needed
      }
      
      # For other coordinate systems, use the coordinate transformation service
      tryCatch({
        transformer <- get_coordinate_transformer()
        return(transformer$transform_coordinates(data, from_crs, to_crs))
      }, error = function(e) {
        warning("Coordinate transformation failed, returning original data: ", e$message)
        return(data)
      })
    },
    
    #' Auto-detect Coordinate System
    #'
    #' Automatically detect the coordinate system of input data.
    #'
    #' @param data Data frame or matrix containing coordinates
    #' @return Character string indicating detected coordinate system
    detect_coordinate_system = function(data) {
      tryCatch({
        detector <- get_coordinate_detector()
        return(detector$detect_coordinate_system(data))
      }, error = function(e) {
        # Default to WGS84 for Mapbox
        return("EPSG:4326")
      })
    },
    
    #' Prepare Data for Mapbox
    #'
    #' Prepare spatial data for use with Mapbox, including coordinate transformation.
    #'
    #' @param data Spatial data (data.frame, sf object, etc.)
    #' @param auto_transform Logical indicating if coordinates should be auto-transformed
    #' @return Prepared data in WGS84 coordinate system
    prepare_data = function(data, auto_transform = TRUE) {
      if (is.null(data)) {
        return(NULL)
      }
      
      # If auto_transform is enabled, detect and transform coordinates
      if (auto_transform) {
        detected_crs <- self$detect_coordinate_system(data)
        
        if (detected_crs != "EPSG:4326") {
          message(sprintf("Auto-transforming coordinates from %s to EPSG:4326", detected_crs))
          data <- self$transform_coordinates(data, detected_crs, "EPSG:4326")
        }
      }
      
      return(data)
    },
    
    #' Validate Configuration
    #'
    #' Validate Mapbox-specific configuration.
    #'
    #' @param config List containing configuration to validate
    #' @return Logical indicating if configuration is valid
    validate_config = function(config) {
      if (!is.list(config)) {
        return(FALSE)
      }
      
      # Check if token is available (either in config or token store)
      has_token <- !is.null(config$token) || !is.null(self$access_token)
      
      if (!has_token) {
        warning("No Mapbox access token found. Map will render without base tiles.")
        # Don't fail validation - allow maps without tokens for layer-only usage
      }
      
      # Validate token format if present
      token_to_validate <- config$token %||% self$access_token
      if (!is.null(token_to_validate)) {
        if (!is.character(token_to_validate) || length(token_to_validate) != 1) {
          return(FALSE)
        }
        
        # Basic Mapbox token format validation
        if (!grepl("^(pk|sk)\\.", token_to_validate)) {
          warning("Mapbox token should start with 'pk.' or 'sk.'")
        }
      }
      
      return(TRUE)
    },
    
    #' Destroy Provider
    #'
    #' Clean up resources and destroy the Mapbox provider instance.
    #'
    #' @return Invisible NULL
    destroy = function() {
      # Clear layers
      self$layers <- list()
      
      # Clear map instance reference
      self$map_instance <- NULL
      
      # Clear configuration
      self$mapbox_config <- NULL
      self$access_token <- NULL
      self$current_style <- NULL
      
      # Mark as not initialized
      self$initialized <- FALSE
      
      invisible(NULL)
    }
  )
)

# Define %||% operator if not already defined
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}