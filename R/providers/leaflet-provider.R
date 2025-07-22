#' Leaflet Provider Implementation
#'
#' This file contains the LeafletProvider class that implements the IMapProvider
#' interface for Leaflet.js integration with deck.gl overlay functionality.
#'
#' @name leaflet-provider
NULL

#' Leaflet Provider Class
#'
#' R6 class implementing the IMapProvider interface for Leaflet.js.
#'
#' @description
#' The LeafletProvider class provides Leaflet.js integration through the
#' provider interface with deck.gl overlay support. It supports various
#' tile providers and Leaflet-specific features.
#'
#' @field leaflet_config Leaflet-specific configuration
#' @field tile_provider Current tile provider configuration
#' @field current_style Current map style/tile layer
#' @field layers List of active deck.gl layers
#' @field map_instance Reference to the map instance
#'
#' @examples
#' \donttest{
#' # Create Leaflet provider
#' provider <- LeafletProvider$new()
#' provider$initialize_provider(list(tile_provider = "OpenStreetMap"))
#' }
#'
#' @export
LeafletProvider <- R6::R6Class("LeafletProvider",
  inherit = IMapProvider,
  public = list(
    #' @field leaflet_config Leaflet configuration
    leaflet_config = NULL,
    
    #' @field tile_provider Tile provider configuration
    tile_provider = NULL,
    
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
    #' @return New LeafletProvider instance
    initialize = function() {
      # Set provider identification
      self$provider_name <- "leaflet"
      self$layers <- list()
      self$initialized <- FALSE
      
      # Get provider capabilities from configuration
      tryCatch({
        provider_config <- get_provider_config("leaflet")
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
    #' Initialize the Leaflet provider with configuration settings.
    #'
    #' @param config List containing Leaflet-specific configuration
    #' @return Invisible self for method chaining
    initialize_provider = function(config = list()) {
      self$config <- config
      
      # Set tile provider (default to OpenStreetMap)
      if (!is.null(config$tile_provider)) {
        self$tile_provider <- config$tile_provider
      } else {
        self$tile_provider <- "OpenStreetMap"
      }
      
      # Set current style based on tile provider
      self$current_style <- self$tile_provider
      
      # Store Leaflet-specific configuration
      self$leaflet_config <- list(
        tile_provider = self$tile_provider,
        tile_url = config$tile_url,
        attribution = config$attribution,
        max_zoom = config$max_zoom %||% 18,
        min_zoom = config$min_zoom %||% 1,
        zoom = config$zoom %||% 10,
        center = config$center %||% c(0, 0),
        crs = config$crs %||% "EPSG3857",
        detect_retina = config$detect_retina %||% TRUE,
        world_copy_jump = config$world_copy_jump %||% FALSE,
        max_bounds = config$max_bounds,
        zoom_control = config$zoom_control %||% TRUE,
        attribution_control = config$attribution_control %||% TRUE
      )
      
      # Validate configuration
      if (!self$validate_config(self$config)) {
        stop("Invalid Leaflet provider configuration")
      }
      
      self$initialized <- TRUE
      invisible(self)
    },
    
    #' Create Map Instance
    #'
    #' Create a new Leaflet map instance with deck.gl overlay.
    #'
    #' @param container Character string identifying the HTML container
    #' @param options List of map initialization options
    #' @return Map instance object (htmlwidget)
    create_map = function(container = NULL, options = list()) {
      if (!self$initialized) {
        stop("Provider must be initialized before creating map")
      }
      
      # Validate and merge options
      validated_options <- validate_map_options(options, "leaflet")
      
      # Merge with provider configuration
      map_options <- list(
        provider = "leaflet",
        tile_provider = validated_options$tile_provider %||% 
          self$leaflet_config$tile_provider,
        tile_url = validated_options$tile_url %||% 
          self$leaflet_config$tile_url,
        attribution = validated_options$attribution %||% 
          self$leaflet_config$attribution,
        max_zoom = validated_options$max_zoom %||% 
          self$leaflet_config$max_zoom,
        min_zoom = validated_options$min_zoom %||% 
          self$leaflet_config$min_zoom,
        zoom = validated_options$zoom %||% 
          self$leaflet_config$zoom,
        center = validated_options$center %||% 
          self$leaflet_config$center,
        crs = validated_options$crs %||% 
          self$leaflet_config$crs,
        detect_retina = validated_options$detect_retina %||% 
          self$leaflet_config$detect_retina,
        world_copy_jump = validated_options$world_copy_jump %||% 
          self$leaflet_config$world_copy_jump,
        max_bounds = validated_options$max_bounds %||% 
          self$leaflet_config$max_bounds,
        zoom_control = validated_options$zoom_control %||% 
          self$leaflet_config$zoom_control,
        attribution_control = validated_options$attribution_control %||% 
          self$leaflet_config$attribution_control,
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
      
      # Add Leaflet-specific JavaScript dependencies
      map_widget$dependencies <- c(
        if (!is.null(validated_options$libraries) && 
            "h3" %in% validated_options$libraries) mapdeckH3JSDependency() else NULL,
        map_widget$dependencies,
        leaflet_js(),
        leaflet_deckgl_adapter(),
        mapdeck_js(),
        htmlwidgets_js()
      )
      
      # Store reference to map instance
      self$map_instance <- map_widget
      
      return(map_widget)
    },
    
    #' Update Map Style
    #'
    #' Update the Leaflet map tile provider/style.
    #'
    #' @param style Character string specifying the new tile provider
    #' @return Invisible self for method chaining
    update_style = function(style) {
      if (!self$initialized) {
        stop("Provider must be initialized before updating style")
      }
      
      # Normalize style for Leaflet (tile provider)
      normalized_style <- normalize_leaflet_tile_provider(style)
      
      # Update internal state
      self$current_style <- normalized_style
      self$tile_provider <- normalized_style
      self$leaflet_config$tile_provider <- normalized_style
      
      # Update map if instance exists
      if (!is.null(self$map_instance)) {
        self$map_instance <- invoke_method(
          self$map_instance, "md_update_leaflet_tiles", normalized_style
        )
      }
      
      invisible(self)
    },
    
    #' Add Layer to Map
    #'
    #' Add a deck.gl layer to the Leaflet map.
    #'
    #' @param layer List containing layer configuration
    #' @return Invisible self for method chaining
    add_layer = function(layer) {
      if (!self$initialized) {
        stop("Provider must be initialized before adding layers")
      }
      
      # Validate layer configuration for Leaflet compatibility
      validated_layer <- validate_layer_config(layer, "leaflet")
      
      # Store layer reference
      self$layers[[validated_layer$id]] <- validated_layer
      
      # Add layer to map if instance exists
      if (!is.null(self$map_instance)) {
        # Layers are handled by existing add_* functions
        # The JavaScript adapter will handle deck.gl integration
      }
      
      invisible(self)
    },
    
    #' Remove Layer from Map
    #'
    #' Remove a layer from the Leaflet map by its ID.
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
        # Layer removal handled by existing clear_* functions
      }
      
      invisible(self)
    },
    
    #' Set Map View
    #'
    #' Set the Leaflet map view state.
    #'
    #' @param longitude Numeric longitude coordinate
    #' @param latitude Numeric latitude coordinate
    #' @param zoom Numeric zoom level
    #' @param pitch Numeric pitch angle (ignored for Leaflet)
    #' @param bearing Numeric bearing angle (ignored for Leaflet)
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
      
      # Note: Leaflet doesn't support pitch and bearing like Mapbox
      if (pitch != 0) {
        warning("Leaflet provider does not support pitch - ignoring pitch parameter")
      }
      
      if (bearing != 0) {
        warning("Leaflet provider does not support bearing - ignoring bearing parameter")
      }
      
      # Update internal configuration
      self$leaflet_config$center <- c(longitude, latitude)
      self$leaflet_config$zoom <- zoom
      
      # Update map view if instance exists
      if (!is.null(self$map_instance)) {
        self$map_instance <- mapdeck_view(
          self$map_instance,
          location = c(longitude, latitude),
          zoom = zoom,
          pitch = 0,  # Always 0 for Leaflet
          bearing = 0  # Always 0 for Leaflet
        )
      }
      
      invisible(self)
    },
    
    #' Get Available Styles
    #'
    #' Get list of available Leaflet tile providers with enhanced categorization.
    #'
    #' @param category Character string to filter by category (optional)
    #' @param include_api_key_required Logical indicating if providers requiring API keys should be included
    #' @return Character vector of available tile provider names or named list if categorized
    get_available_styles = function(category = NULL, include_api_key_required = FALSE) {
      styles <- list(
        "basic" = c(
          "OpenStreetMap",
          "OpenStreetMap.Mapnik",
          "OpenStreetMap.BlackAndWhite",
          "OpenStreetMap.DE",
          "OpenStreetMap.France",
          "OpenStreetMap.HOT"
        ),
        "satellite" = c(
          "Esri.WorldImagery",
          "NASAGIBS.ViirsEarthAtNight2012",
          "NASAGIBS.ModisTerraTrueColorCR"
        ),
        "terrain" = c(
          "OpenTopoMap",
          "Esri.WorldTopoMap"
        ),
        "transport" = c(
          "OpenMapSurfer.Roads"
        ),
        "specialty" = c(
          "CartoDB.Positron",
          "CartoDB.PositronNoLabels",
          "CartoDB.DarkMatter",
          "CartoDB.DarkMatterNoLabels",
          "Stamen.Toner",
          "Stamen.TonerBackground",
          "Stamen.Watercolor"
        )
      )
      
      # Add API key required providers if requested
      if (include_api_key_required) {
        styles$terrain <- c(styles$terrain, 
                           "Thunderforest.Landscape",
                           "Thunderforest.Outdoors")
        styles$transport <- c(styles$transport,
                             "Thunderforest.Transport",
                             "Thunderforest.TransportDark")
      }
      
      if (is.null(category)) {
        return(unlist(styles, use.names = FALSE))
      } else {
        if (category %in% names(styles)) {
          return(styles[[category]])
        } else {
          warning(sprintf("Unknown tile provider category: %s", category))
          return(character(0))
        }
      }
    },
    
    #' Get Tile Provider Configuration
    #'
    #' Get detailed configuration for a specific tile provider.
    #'
    #' @param provider_name Character string identifying the tile provider
    #' @return List containing provider configuration details
    get_tile_provider_config = function(provider_name) {
      if (!is.character(provider_name) || length(provider_name) != 1) {
        stop("Provider name must be a single character string")
      }
      
      # Detailed tile provider configurations
      configs <- list(
        "OpenStreetMap" = list(
          url = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
          attribution = "© OpenStreetMap contributors",
          max_zoom = 19,
          requires_api_key = FALSE
        ),
        "CartoDB.Positron" = list(
          url = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
          attribution = "© OpenStreetMap contributors © CARTO",
          max_zoom = 19,
          requires_api_key = FALSE
        ),
        "CartoDB.DarkMatter" = list(
          url = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png",
          attribution = "© OpenStreetMap contributors © CARTO",
          max_zoom = 19,
          requires_api_key = FALSE
        ),
        "Esri.WorldImagery" = list(
          url = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
          attribution = "Tiles © Esri",
          max_zoom = 18,
          requires_api_key = FALSE
        ),
        "Thunderforest.Landscape" = list(
          url = "https://{s}.tile.thunderforest.com/landscape/{z}/{x}/{y}.png?apikey={apikey}",
          attribution = "© Thunderforest, © OpenStreetMap contributors",
          max_zoom = 18,
          requires_api_key = TRUE
        )
      )
      
      if (provider_name %in% names(configs)) {
        return(configs[[provider_name]])
      } else {
        # Return basic configuration for unknown providers
        return(list(
          url = NULL,
          attribution = "",
          max_zoom = 18,
          requires_api_key = FALSE
        ))
      }
    },
    
    #' Create Custom Tile Layer
    #'
    #' Create a custom tile layer configuration.
    #'
    #' @param url Character string containing tile URL template
    #' @param attribution Character string for attribution text
    #' @param max_zoom Numeric maximum zoom level
    #' @param min_zoom Numeric minimum zoom level
    #' @param api_key Character string for API key (if required)
    #' @return List containing custom tile layer configuration
    create_custom_tile_layer = function(url, attribution = "", max_zoom = 18, 
                                       min_zoom = 1, api_key = NULL) {
      if (!is.character(url) || length(url) != 1 || nchar(url) == 0) {
        stop("URL must be a single non-empty character string")
      }
      
      if (!grepl("^https?://", url)) {
        stop("URL must start with http:// or https://")
      }
      
      if (!grepl("\\{z\\}", url) || !grepl("\\{x\\}", url) || !grepl("\\{y\\}", url)) {
        stop("URL must contain {z}, {x}, and {y} placeholders")
      }
      
      # Validate zoom levels
      if (!is.numeric(max_zoom) || length(max_zoom) != 1 || max_zoom < 1 || max_zoom > 20) {
        stop("max_zoom must be a single numeric value between 1 and 20")
      }
      
      if (!is.numeric(min_zoom) || length(min_zoom) != 1 || min_zoom < 0 || min_zoom >= max_zoom) {
        stop("min_zoom must be a single numeric value between 0 and max_zoom")
      }
      
      config <- list(
        url = url,
        attribution = attribution,
        max_zoom = max_zoom,
        min_zoom = min_zoom,
        requires_api_key = !is.null(api_key),
        api_key = api_key
      )
      
      return(config)
    },
    
    #' Set Tile Provider Options
    #'
    #' Set advanced options for the current tile provider.
    #'
    #' @param options List containing tile provider options
    #' @return Invisible self for method chaining
    set_tile_provider_options = function(options) {
      if (!is.list(options)) {
        stop("Options must be a list")
      }
      
      # Merge with existing configuration
      for (key in names(options)) {
        if (key %in% names(self$leaflet_config)) {
          self$leaflet_config[[key]] <- options[[key]]
        } else {
          warning(sprintf("Unknown tile provider option: %s", key))
        }
      }
      
      invisible(self)
    },
    
    #' Validate Configuration
    #'
    #' Validate Leaflet-specific configuration.
    #'
    #' @param config List containing configuration to validate
    #' @return Logical indicating if configuration is valid
    validate_config = function(config) {
      if (!is.list(config)) {
        return(FALSE)
      }
      
      # Leaflet doesn't require authentication by default
      # Validate tile provider if specified
      if (!is.null(config$tile_provider)) {
        if (!is.character(config$tile_provider) || 
            length(config$tile_provider) != 1) {
          return(FALSE)
        }
        
        # Check if it's a known tile provider
        available_providers <- self$get_available_styles()
        if (!config$tile_provider %in% available_providers &&
            is.null(config$tile_url)) {
          warning(sprintf("Unknown tile provider '%s' and no custom tile_url provided", 
                         config$tile_provider))
        }
      }
      
      # Validate custom tile URL if provided
      if (!is.null(config$tile_url)) {
        if (!is.character(config$tile_url) || length(config$tile_url) != 1) {
          return(FALSE)
        }
        
        # Basic URL validation
        if (!grepl("^https?://", config$tile_url)) {
          warning("Custom tile URL should start with http:// or https://")
        }
      }
      
      return(TRUE)
    },
    
    #' Destroy Provider
    #'
    #' Clean up resources and destroy the Leaflet provider instance.
    #'
    #' @return Invisible NULL
    destroy = function() {
      # Clear layers
      self$layers <- list()
      
      # Clear map instance reference
      self$map_instance <- NULL
      
      # Clear configuration
      self$leaflet_config <- NULL
      self$tile_provider <- NULL
      self$current_style <- NULL
      
      # Mark as not initialized
      self$initialized <- FALSE
      
      invisible(NULL)
    }
  )
)

#' Normalize Leaflet Tile Provider
#'
#' Normalize tile provider names for Leaflet.
#'
#' @param provider Character string identifying the tile provider
#' @return Normalized tile provider name
#'
#' @examples
#' \donttest{
#' # Normalize provider names
#' provider <- normalize_leaflet_tile_provider("osm")
#' }
#'
#' @export
normalize_leaflet_tile_provider <- function(provider) {
  if (is.null(provider)) {
    return("OpenStreetMap")
  }
  
  if (!is.character(provider) || length(provider) != 1) {
    stop("Provider must be a single character string")
  }
  
  # Common aliases
  aliases <- list(
    "osm" = "OpenStreetMap",
    "openstreetmap" = "OpenStreetMap",
    "cartodb" = "CartoDB.Positron",
    "positron" = "CartoDB.Positron",
    "dark" = "CartoDB.DarkMatter",
    "darkmatter" = "CartoDB.DarkMatter",
    "satellite" = "Esri.WorldImagery",
    "imagery" = "Esri.WorldImagery",
    "topo" = "OpenTopoMap",
    "topographic" = "OpenTopoMap",
    "terrain" = "Esri.WorldTopoMap",
    "watercolor" = "Stamen.Watercolor",
    "toner" = "Stamen.Toner"
  )
  
  # Convert to lowercase for matching
  provider_lower <- tolower(provider)
  
  if (provider_lower %in% names(aliases)) {
    return(aliases[[provider_lower]])
  }
  
  # Return as-is if no alias found (assume it's a valid provider name)
  return(provider)
}

# Define %||% operator if not already defined
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}