#' Gaode Maps Provider Implementation
#'
#' This file contains the GaodeProvider class that implements the IMapProvider
#' interface for Gaode Maps (AutoNavi) integration with deck.gl overlay.
#' It supports GCJ02 coordinate system and Chinese mapping features.
#'
#' @name gaode-provider
NULL

#' Gaode Maps Provider Class
#'
#' R6 class implementing the IMapProvider interface for Gaode Maps.
#'
#' @description
#' The GaodeProvider class provides Gaode Maps integration through the
#' provider interface with deck.gl overlay functionality. It handles
#' GCJ02 coordinate system transformations and Chinese mapping features.
#'
#' @field gaode_config Gaode-specific configuration
#' @field api_key Gaode Maps API key
#' @field current_style Current map style
#' @field layers List of active layers
#' @field map_instance Reference to the map instance
#' @field coordinate_transformer Coordinate transformation service
#'
#' @examples
#' \donttest{
#' # Create Gaode provider
#' provider <- GaodeProvider$new()
#' provider$initialize_provider(list(api_key = "your_api_key_here"))
#' }
#'
#' @export
GaodeProvider <- R6::R6Class("GaodeProvider",
  inherit = IMapProvider,
  public = list(
    #' @field gaode_config Gaode configuration
    gaode_config = NULL,
    
    #' @field api_key Gaode Maps API key
    api_key = NULL,
    
    #' @field current_style Current map style
    current_style = NULL,
    
    #' @field layers Active layers list
    layers = NULL,
    
    #' @field map_instance Map instance reference
    map_instance = NULL,
    
    #' @field coordinate_transformer Coordinate transformation service
    coordinate_transformer = NULL,
    
    #' Initialize Constructor
    #'
    #' R6 constructor - sets up basic structure without full initialization.
    #'
    #' @return New GaodeProvider instance
    initialize = function() {
      # Set provider identification
      self$provider_name <- "gaode"
      self$layers <- list()
      self$initialized <- FALSE
      
      # Initialize coordinate transformer for GCJ02 support
      self$coordinate_transformer <- CoordinateTransformer$new()
      
      # Get provider capabilities from configuration
      tryCatch({
        provider_config <- get_provider_config("gaode")
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
    #' Initialize the Gaode provider with configuration settings.
    #'
    #' @param config List containing Gaode-specific configuration
    #' @return Invisible self for method chaining
    initialize_provider = function(config = list()) {
      self$config <- config
      
      # Extract API key from config or token store
      if (!is.null(config$api_key)) {
        self$api_key <- config$api_key
        # Also store in token store for consistency
        tryCatch({
          token_store <- get_token_store()
          token_store$set_token("gaode", config$api_key, 
                               config$scope %||% "default")
        }, error = function(e) {
          # Silently handle token store errors
        })
      } else if (!is.null(config$token)) {
        # Support 'token' parameter for consistency
        self$api_key <- config$token
        tryCatch({
          token_store <- get_token_store()
          token_store$set_token("gaode", config$token, 
                               config$scope %||% "default")
        }, error = function(e) {
          # Silently handle token store errors
        })
      } else {
        # Try to get API key from token store
        tryCatch({
          token_store <- get_token_store()
          self$api_key <- token_store$get_token("gaode", 
                                               config$scope %||% "default")
        }, error = function(e) {
          self$api_key <- NULL
        })
      }
      
      # Set default style
      if (!is.null(config$style)) {
        self$current_style <- config$style
      } else {
        self$current_style <- "amap://styles/normal"
      }
      
      # Store Gaode-specific configuration
      self$gaode_config <- list(
        api_key = self$api_key,
        style = self$current_style,
        pitch = config$pitch %||% 0,
        zoom = config$zoom %||% 10,
        bearing = config$bearing %||% 0,
        location = config$location %||% c(116.397, 39.909), # Beijing
        max_zoom = config$max_zoom %||% 18,
        min_zoom = config$min_zoom %||% 3,
        max_pitch = config$max_pitch %||% 60,
        min_pitch = config$min_pitch %||% 0,
        lang = config$lang %||% "zh_cn",
        features = config$features %||% list("bg", "road", "building", "point")
      )
      
      # Validate configuration
      if (!self$validate_config(self$config)) {
        stop("Invalid Gaode provider configuration")
      }
      
      self$initialized <- TRUE
      invisible(self)
    },
    
    #' Create Map Instance
    #'
    #' Create a new Gaode map instance with deck.gl overlay.
    #'
    #' @param container Character string identifying the HTML container
    #' @param options List of map initialization options
    #' @return Map instance object (htmlwidget)
    create_map = function(container = NULL, options = list()) {
      if (!self$initialized) {
        stop("Provider must be initialized before creating map")
      }
      
      # Validate and merge options
      validated_options <- validate_map_options(options, "gaode")
      
      # Transform coordinates to GCJ02 if needed
      location <- validated_options$location %||% self$gaode_config$location
      if (!is.null(location)) {
        # Assume input is WGS84, transform to GCJ02 for Gaode
        location_gcj02 <- self$coordinate_transformer$transform(
          location, "WGS84", "GCJ02"
        )
      } else {
        location_gcj02 <- self$gaode_config$location
      }
      
      # Merge with provider configuration
      map_options <- list(
        provider = "gaode",
        api_key = self$api_key,
        style = validated_options$style %||% self$current_style,
        pitch = validated_options$pitch %||% self$gaode_config$pitch,
        zoom = validated_options$zoom %||% self$gaode_config$zoom,
        bearing = validated_options$bearing %||% self$gaode_config$bearing,
        location = location_gcj02,
        max_zoom = validated_options$max_zoom %||% self$gaode_config$max_zoom,
        min_zoom = validated_options$min_zoom %||% self$gaode_config$min_zoom,
        max_pitch = validated_options$max_pitch %||% self$gaode_config$max_pitch,
        min_pitch = validated_options$min_pitch %||% self$gaode_config$min_pitch,
        lang = validated_options$lang %||% self$gaode_config$lang,
        features = validated_options$features %||% self$gaode_config$features,
        coordinate_system = "GCJ02",
        show_view_state = validated_options$show_view_state %||% FALSE,
        repeat_view = validated_options$repeat_view %||% FALSE
      )
      
      # Create the htmlwidget using mapdeck infrastructure
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
      
      # Add required JavaScript dependencies for Gaode
      map_widget$dependencies <- c(
        if (!is.null(validated_options$libraries) && 
            "h3" %in% validated_options$libraries) mapdeckH3JSDependency() else NULL,
        map_widget$dependencies,
        gaode_js_dependency(),
        mapdeck_js(),
        htmlwidgets_js()
      )
      
      # Store reference to map instance
      self$map_instance <- map_widget
      
      return(map_widget)
    }
  )
)    #' U
pdate Map Style
    #'
    #' Update the Gaode map style.
    #'
    #' @param style Character string or list specifying the new style
    #' @return Invisible self for method chaining
    update_style = function(style) {
      if (!self$initialized) {
        stop("Provider must be initialized before updating style")
      }
      
      # Normalize style for Gaode
      normalized_style <- normalize_style_name(style, "gaode")
      
      # Update internal state
      self$current_style <- normalized_style
      self$gaode_config$style <- normalized_style
      
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
    #' Add a deck.gl layer to the Gaode map with coordinate transformation.
    #'
    #' @param layer List containing layer configuration
    #' @return Invisible self for method chaining
    add_layer = function(layer) {
      if (!self$initialized) {
        stop("Provider must be initialized before adding layers")
      }
      
      # Validate layer configuration
      validated_layer <- validate_layer_config(layer, "gaode")
      
      # Transform layer data coordinates if needed
      if (!is.null(validated_layer$data)) {
        validated_layer$data <- self$prepare_data(validated_layer$data)
      }
      
      # Store layer reference
      self$layers[[validated_layer$id]] <- validated_layer
      
      # Add layer to map if instance exists
      if (!is.null(self$map_instance)) {
        # Layer addition is handled by existing add_* functions
        # which will use the transformed data
      }
      
      invisible(self)
    },
    
    #' Remove Layer from Map
    #'
    #' Remove a layer from the Gaode map by its ID.
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
        # Layer removal is handled by existing clear_* functions
      }
      
      invisible(self)
    },
    
    #' Set Map View
    #'
    #' Set the Gaode map view state with coordinate transformation.
    #'
    #' @param longitude Numeric longitude coordinate (WGS84)
    #' @param latitude Numeric latitude coordinate (WGS84)
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
          zoom < 0 || zoom > 20) {
        stop("Zoom must be a single numeric value between 0 and 20")
      }
      
      if (!is.numeric(pitch) || length(pitch) != 1 ||
          pitch < 0 || pitch > 60) {
        stop("Pitch must be a single numeric value between 0 and 60")
      }
      
      if (!is.numeric(bearing) || length(bearing) != 1 ||
          bearing < 0 || bearing >= 360) {
        stop("Bearing must be a single numeric value between 0 and 360")
      }
      
      # Transform coordinates from WGS84 to GCJ02 for Gaode
      wgs84_coords <- c(longitude, latitude)
      gcj02_coords <- self$coordinate_transformer$transform(
        wgs84_coords, "WGS84", "GCJ02"
      )
      
      # Update internal configuration
      self$gaode_config$location <- gcj02_coords
      self$gaode_config$zoom <- zoom
      self$gaode_config$pitch <- pitch
      self$gaode_config$bearing <- bearing
      
      # Update map view if instance exists
      if (!is.null(self$map_instance)) {
        self$map_instance <- mapdeck_view(
          self$map_instance,
          location = gcj02_coords,
          zoom = zoom,
          pitch = pitch,
          bearing = bearing
        )
      }
      
      invisible(self)
    },
    
    #' Get Available Styles
    #'
    #' Get list of available Gaode map styles.
    #'
    #' @param category Character string to filter by category (optional)
    #' @return Character vector of available style names
    get_available_styles = function(category = NULL) {
      styles <- list(
        "basic" = c(
          "amap://styles/normal",
          "amap://styles/light",
          "amap://styles/dark",
          "amap://styles/fresh"
        ),
        "satellite" = c(
          "amap://styles/satellite",
          "amap://styles/hybrid"
        ),
        "navigation" = c(
          "amap://styles/navigation-day",
          "amap://styles/navigation-night"
        ),
        "specialty" = c(
          "amap://styles/grey",
          "amap://styles/graffiti",
          "amap://styles/macaron",
          "amap://styles/blue",
          "amap://styles/darkblue",
          "amap://styles/wine"
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
    
    #' Transform Coordinates
    #'
    #' Transform coordinates for Gaode Maps (GCJ02 coordinate system).
    #'
    #' @param data Data frame or matrix containing coordinates
    #' @param from_crs Character string indicating source coordinate system
    #' @param to_crs Character string indicating target coordinate system
    #' @return Transformed coordinate data
    transform_coordinates = function(data, from_crs = "WGS84", to_crs = "GCJ02") {
      if (is.null(data)) {
        return(NULL)
      }
      
      # Use the coordinate transformer
      return(self$coordinate_transformer$transform(data, from_crs, to_crs))
    },
    
    #' Auto-detect Coordinate System
    #'
    #' Automatically detect the coordinate system of input data.
    #'
    #' @param data Data frame or matrix containing coordinates
    #' @return Character string indicating detected coordinate system
    detect_coordinate_system = function(data) {
      if (is.null(data)) {
        return("WGS84")
      }
      
      # Extract coordinate columns
      coords <- extract_coordinates(data)
      if (is.null(coords)) {
        return("WGS84")
      }
      
      # Simple heuristic for Chinese coordinate systems
      lon_range <- range(coords[, 1], na.rm = TRUE)
      lat_range <- range(coords[, 2], na.rm = TRUE)
      
      # Check if coordinates are within China bounds
      china_bounds <- list(
        lon = c(72.004, 137.8347),
        lat = c(0.8293, 55.8271)
      )
      
      in_china <- lon_range[1] >= china_bounds$lon[1] && 
                  lon_range[2] <= china_bounds$lon[2] &&
                  lat_range[1] >= china_bounds$lat[1] && 
                  lat_range[2] <= china_bounds$lat[2]
      
      if (in_china) {
        # Additional heuristics for GCJ02 vs WGS84 vs BD09
        # GCJ02 coordinates are typically slightly offset from WGS84
        # This is a simplified detection - more sophisticated methods exist
        return("GCJ02")
      }
      
      return("WGS84")
    },
    
    #' Prepare Data for Gaode
    #'
    #' Prepare spatial data for use with Gaode Maps, including coordinate 
    #' transformation to GCJ02.
    #'
    #' @param data Spatial data (data.frame, sf object, etc.)
    #' @param auto_transform Logical indicating if coordinates should be 
    #'   auto-transformed
    #' @return Prepared data in GCJ02 coordinate system
    prepare_data = function(data, auto_transform = TRUE) {
      if (is.null(data)) {
        return(NULL)
      }
      
      # If auto_transform is enabled, detect and transform coordinates
      if (auto_transform) {
        detected_crs <- self$detect_coordinate_system(data)
        
        if (detected_crs != "GCJ02") {
          message(sprintf("Auto-transforming coordinates from %s to GCJ02", 
                         detected_crs))
          data <- self$transform_coordinates(data, detected_crs, "GCJ02")
        }
      }
      
      return(data)
    },
    
    #' Validate Configuration
    #'
    #' Validate Gaode-specific configuration.
    #'
    #' @param config List containing configuration to validate
    #' @return Logical indicating if configuration is valid
    validate_config = function(config) {
      if (!is.list(config)) {
        return(FALSE)
      }
      
      # Check if API key is available
      has_api_key <- !is.null(config$api_key) || 
                     !is.null(config$token) || 
                     !is.null(self$api_key)
      
      if (!has_api_key) {
        stop("Gaode Maps API key is required. Set via config$api_key or token store.")
      }
      
      # Validate API key format if present
      key_to_validate <- config$api_key %||% config$token %||% self$api_key
      if (!is.null(key_to_validate)) {
        if (!is.character(key_to_validate) || length(key_to_validate) != 1) {
          return(FALSE)
        }
        
        # Basic Gaode API key format validation
        if (nchar(key_to_validate) < 20) {
          warning("Gaode API key appears to be too short")
        }
      }
      
      return(TRUE)
    },
    
    #' Destroy Provider
    #'
    #' Clean up resources and destroy the Gaode provider instance.
    #'
    #' @return Invisible NULL
    destroy = function() {
      # Clear layers
      self$layers <- list()
      
      # Clear map instance reference
      self$map_instance <- NULL
      
      # Clear configuration
      self$gaode_config <- NULL
      self$api_key <- NULL
      self$current_style <- NULL
      
      # Clear coordinate transformer
      self$coordinate_transformer <- NULL
      
      # Mark as not initialized
      self$initialized <- FALSE
      
      invisible(NULL)
    }
  )
)

#' Extract Coordinates from Data
#'
#' Helper function to extract coordinate columns from various data formats.
#'
#' @param data Data frame, matrix, or sf object
#' @return Matrix with longitude and latitude columns, or NULL if not found
#'
#' @keywords internal
extract_coordinates <- function(data) {
  if (is.null(data)) {
    return(NULL)
  }
  
  # Handle sf objects
  if (inherits(data, "sf")) {
    coords <- sf::st_coordinates(data)
    if (ncol(coords) >= 2) {
      return(coords[, 1:2])
    }
  }
  
  # Handle data frames
  if (is.data.frame(data)) {
    # Look for common coordinate column names
    lon_cols <- c("longitude", "lon", "lng", "x", "X")
    lat_cols <- c("latitude", "lat", "y", "Y")
    
    lon_col <- intersect(names(data), lon_cols)[1]
    lat_col <- intersect(names(data), lat_cols)[1]
    
    if (!is.na(lon_col) && !is.na(lat_col)) {
      return(as.matrix(data[, c(lon_col, lat_col)]))
    }
  }
  
  # Handle matrices
  if (is.matrix(data) && ncol(data) >= 2) {
    return(data[, 1:2])
  }
  
  return(NULL)
}

#' Create Gaode JavaScript Dependency
#'
#' Create the JavaScript dependency for Gaode Maps integration.
#'
#' @return htmltools dependency object
#'
#' @keywords internal
gaode_js_dependency <- function() {
  htmltools::htmlDependency(
    name = "gaode-maps",
    version = "2.0",
    src = system.file("htmlwidgets/lib/gaode", package = "mapdeck"),
    script = c("gaode-adapter.js"),
    all_files = FALSE
  )
}

# Define %||% operator if not already defined
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}