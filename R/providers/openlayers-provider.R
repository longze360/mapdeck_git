#' OpenLayers Provider Implementation
#'
#' This file contains the OpenLayersProvider class that implements the IMapProvider
#' interface for OpenLayers integration with deck.gl overlay functionality.
#'
#' @name openlayers-provider
NULL

#' OpenLayers Provider Class
#'
#' R6 class implementing the IMapProvider interface for OpenLayers.
#'
#' @description
#' The OpenLayersProvider class provides OpenLayers integration through the
#' provider interface with deck.gl overlay support. It supports various
#' sources and layer configurations specific to OpenLayers.
#'
#' @field openlayers_config OpenLayers-specific configuration
#' @field source_config Current source configuration
#' @field current_style Current map style/source
#' @field layers List of active deck.gl layers
#' @field map_instance Reference to the map instance
#'
#' @examples
#' \donttest{
#' # Create OpenLayers provider
#' provider <- OpenLayersProvider$new()
#' provider$initialize_provider(list(source = "OSM"))
#' }
#'
#' @export
OpenLayersProvider <- R6::R6Class("OpenLayersProvider",
  inherit = IMapProvider,
  public = list(
    #' @field openlayers_config OpenLayers configuration
    openlayers_config = NULL,
    
    #' @field source_config Source configuration
    source_config = NULL,
    
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
    #' @return New OpenLayersProvider instance
    initialize = function() {
      # Set provider identification
      self$provider_name <- "openlayers"
      self$layers <- list()
      self$initialized <- FALSE
      
      # Get provider capabilities from configuration
      tryCatch({
        provider_config <- get_provider_config("openlayers")
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
    #' Initialize the OpenLayers provider with configuration settings.
    #'
    #' @param config List containing OpenLayers-specific configuration
    #' @return Invisible self for method chaining
    initialize_provider = function(config = list()) {
      self$config <- config
      
      # Set source (default to OSM)
      if (!is.null(config$source)) {
        self$source_config <- config$source
      } else {
        self$source_config <- "OSM"
      }
      
      # Set current style based on source
      self$current_style <- self$source_config
      
      # Store OpenLayers-specific configuration
      self$openlayers_config <- list(
        source = self$source_config,
        source_url = config$source_url,
        attribution = config$attribution,
        max_zoom = config$max_zoom %||% 19,
        min_zoom = config$min_zoom %||% 0,
        zoom = config$zoom %||% 2,
        center = config$center %||% c(0, 0),
        projection = config$projection %||% "EPSG:3857",
        extent = config$extent,
        rotation = config$rotation %||% 0,
        enable_rotation = config$enable_rotation %||% TRUE,
        keyboard_pan = config$keyboard_pan %||% TRUE,
        keyboard_zoom = config$keyboard_zoom %||% TRUE,
        mouse_wheel_zoom = config$mouse_wheel_zoom %||% TRUE,
        double_click_zoom = config$double_click_zoom %||% TRUE,
        drag_pan = config$drag_pan %||% TRUE,
        pinch_rotate = config$pinch_rotate %||% TRUE,
        pinch_zoom = config$pinch_zoom %||% TRUE,
        alt_shift_drag_rotate = config$alt_shift_drag_rotate %||% FALSE,
        constrain_resolution = config$constrain_resolution %||% FALSE,
        smooth_resolution_constraint = config$smooth_resolution_constraint %||% TRUE,
        smooth_extent_constraint = config$smooth_extent_constraint %||% TRUE
      )
      
      # Validate configuration
      if (!self$validate_config(self$config)) {
        stop("Invalid OpenLayers provider configuration")
      }
      
      self$initialized <- TRUE
      invisible(self)
    },
    
    #' Create Map Instance
    #'
    #' Create a new OpenLayers map instance with deck.gl overlay.
    #'
    #' @param container Character string identifying the HTML container
    #' @param options List of map initialization options
    #' @return Map instance object (htmlwidget)
    create_map = function(container = NULL, options = list()) {
      if (!self$initialized) {
        stop("Provider must be initialized before creating map")
      }
      
      # Validate and merge options
      validated_options <- validate_map_options(options, "openlayers")
      
      # Merge with provider configuration
      map_options <- list(
        provider = "openlayers",
        source = validated_options$source %||% 
          self$openlayers_config$source,
        source_url = validated_options$source_url %||% 
          self$openlayers_config$source_url,
        attribution = validated_options$attribution %||% 
          self$openlayers_config$attribution,
        max_zoom = validated_options$max_zoom %||% 
          self$openlayers_config$max_zoom,
        min_zoom = validated_options$min_zoom %||% 
          self$openlayers_config$min_zoom,
        zoom = validated_options$zoom %||% 
          self$openlayers_config$zoom,
        center = validated_options$center %||% 
          self$openlayers_config$center,
        projection = validated_options$projection %||% 
          self$openlayers_config$projection,
        extent = validated_options$extent %||% 
          self$openlayers_config$extent,
        rotation = validated_options$rotation %||% 
          self$openlayers_config$rotation,
        enable_rotation = validated_options$enable_rotation %||% 
          self$openlayers_config$enable_rotation,
        keyboard_pan = validated_options$keyboard_pan %||% 
          self$openlayers_config$keyboard_pan,
        keyboard_zoom = validated_options$keyboard_zoom %||% 
          self$openlayers_config$keyboard_zoom,
        mouse_wheel_zoom = validated_options$mouse_wheel_zoom %||% 
          self$openlayers_config$mouse_wheel_zoom,
        double_click_zoom = validated_options$double_click_zoom %||% 
          self$openlayers_config$double_click_zoom,
        drag_pan = validated_options$drag_pan %||% 
          self$openlayers_config$drag_pan,
        pinch_rotate = validated_options$pinch_rotate %||% 
          self$openlayers_config$pinch_rotate,
        pinch_zoom = validated_options$pinch_zoom %||% 
          self$openlayers_config$pinch_zoom,
        alt_shift_drag_rotate = validated_options$alt_shift_drag_rotate %||% 
          self$openlayers_config$alt_shift_drag_rotate,
        constrain_resolution = validated_options$constrain_resolution %||% 
          self$openlayers_config$constrain_resolution,
        smooth_resolution_constraint = validated_options$smooth_resolution_constraint %||% 
          self$openlayers_config$smooth_resolution_constraint,
        smooth_extent_constraint = validated_options$smooth_extent_constraint %||% 
          self$openlayers_config$smooth_extent_constraint,
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
      
      # Add OpenLayers-specific JavaScript dependencies
      map_widget$dependencies <- c(
        if (!is.null(validated_options$libraries) && 
            "h3" %in% validated_options$libraries) mapdeckH3JSDependency() else NULL,
        map_widget$dependencies,
        openlayers_js(),
        openlayers_deckgl_adapter(),
        mapdeck_js(),
        htmlwidgets_js()
      )
      
      # Store reference to map instance
      self$map_instance <- map_widget
      
      return(map_widget)
    },
    
    #' Update Map Style
    #'
    #' Update the OpenLayers map source/style.
    #'
    #' @param style Character string specifying the new source
    #' @return Invisible self for method chaining
    update_style = function(style) {
      if (!self$initialized) {
        stop("Provider must be initialized before updating style")
      }
      
      # Normalize style for OpenLayers (source)
      normalized_style <- normalize_openlayers_source(style)
      
      # Update internal state
      self$current_style <- normalized_style
      self$source_config <- normalized_style
      self$openlayers_config$source <- normalized_style
      
      # Update map if instance exists
      if (!is.null(self$map_instance)) {
        self$map_instance <- invoke_method(
          self$map_instance, "md_update_openlayers_source", normalized_style
        )
      }
      
      invisible(self)
    },
    
    #' Add Layer to Map
    #'
    #' Add a deck.gl layer to the OpenLayers map.
    #'
    #' @param layer List containing layer configuration
    #' @return Invisible self for method chaining
    add_layer = function(layer) {
      if (!self$initialized) {
        stop("Provider must be initialized before adding layers")
      }
      
      # Validate layer configuration for OpenLayers compatibility
      validated_layer <- validate_layer_config(layer, "openlayers")
      
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
    #' Remove a layer from the OpenLayers map by its ID.
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
    #' Set the OpenLayers map view state.
    #'
    #' @param longitude Numeric longitude coordinate
    #' @param latitude Numeric latitude coordinate
    #' @param zoom Numeric zoom level
    #' @param pitch Numeric pitch angle (limited support in OpenLayers)
    #' @param bearing Numeric bearing angle (rotation in OpenLayers)
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
          zoom < 0 || zoom > 28) {
        stop("Zoom must be a single numeric value between 0 and 28")
      }
      
      if (!is.numeric(bearing) || length(bearing) != 1 ||
          bearing < 0 || bearing >= 360) {
        stop("Bearing must be a single numeric value between 0 and 360")
      }
      
      # Note: OpenLayers has limited pitch support compared to Mapbox
      if (pitch != 0) {
        warning("OpenLayers provider has limited pitch support - pitch may be ignored")
      }
      
      # Update internal configuration
      self$openlayers_config$center <- c(longitude, latitude)
      self$openlayers_config$zoom <- zoom
      self$openlayers_config$rotation <- bearing * pi / 180  # Convert to radians
      
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
    #' Get list of available OpenLayers sources with enhanced categorization.
    #'
    #' @param category Character string to filter by category (optional)
    #' @param include_api_key_required Logical indicating if sources requiring API keys should be included
    #' @return Character vector of available source names or named list if categorized
    get_available_styles = function(category = NULL, include_api_key_required = FALSE) {
      styles <- list(
        "basic" = c(
          "OSM",
          "OSM.Mapnik",
          "OSM.CycleMap",
          "OSM.Transport",
          "OSM.TransportDark",
          "OSM.Humanitarian"
        ),
        "satellite" = c(
          "BingMaps.Aerial",
          "BingMaps.AerialWithLabels",
          "ESRI.WorldImagery"
        ),
        "terrain" = c(
          "Stamen.Terrain",
          "Stamen.TerrainBackground",
          "ESRI.WorldTopoMap"
        ),
        "specialty" = c(
          "CartoDB.Positron",
          "CartoDB.PositronNoLabels",
          "CartoDB.DarkMatter",
          "CartoDB.DarkMatterNoLabels",
          "Stamen.Toner",
          "Stamen.TonerBackground",
          "Stamen.Watercolor"
        ),
        "vector" = c(
          "OSM.Vector",
          "MapTiler.Streets",
          "MapTiler.Basic"
        )
      )
      
      # Add API key required sources if requested
      if (include_api_key_required) {
        styles$satellite <- c(styles$satellite, 
                             "BingMaps.Road",
                             "Google.Satellite",
                             "Google.Hybrid")
        styles$vector <- c(styles$vector,
                          "MapBox.Streets",
                          "MapBox.Satellite")
      }
      
      if (is.null(category)) {
        return(unlist(styles, use.names = FALSE))
      } else {
        if (category %in% names(styles)) {
          return(styles[[category]])
        } else {
          warning(sprintf("Unknown source category: %s", category))
          return(character(0))
        }
      }
    },
    
    #' Get Source Configuration
    #'
    #' Get detailed configuration for a specific OpenLayers source.
    #'
    #' @param source_name Character string identifying the source
    #' @return List containing source configuration details
    get_source_config = function(source_name) {
      if (!is.character(source_name) || length(source_name) != 1) {
        stop("Source name must be a single character string")
      }
      
      # Detailed source configurations
      configs <- list(
        "OSM" = list(
          type = "XYZ",
          url = "https://{a-c}.tile.openstreetmap.org/{z}/{x}/{y}.png",
          attribution = "© OpenStreetMap contributors",
          max_zoom = 19,
          requires_api_key = FALSE
        ),
        "CartoDB.Positron" = list(
          type = "XYZ",
          url = "https://{1-4}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
          attribution = "© OpenStreetMap contributors © CARTO",
          max_zoom = 19,
          requires_api_key = FALSE
        ),
        "CartoDB.DarkMatter" = list(
          type = "XYZ",
          url = "https://{1-4}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png",
          attribution = "© OpenStreetMap contributors © CARTO",
          max_zoom = 19,
          requires_api_key = FALSE
        ),
        "ESRI.WorldImagery" = list(
          type = "XYZ",
          url = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
          attribution = "Tiles © Esri",
          max_zoom = 18,
          requires_api_key = FALSE
        ),
        "Stamen.Terrain" = list(
          type = "XYZ",
          url = "https://stamen-tiles-{a-d}.a.ssl.fastly.net/terrain/{z}/{x}/{y}{r}.png",
          attribution = "Map tiles by Stamen Design, CC BY 3.0 — Map data © OpenStreetMap contributors",
          max_zoom = 18,
          requires_api_key = FALSE
        ),
        "BingMaps.Aerial" = list(
          type = "BingMaps",
          imagerySet = "Aerial",
          attribution = "© Microsoft Corporation",
          max_zoom = 19,
          requires_api_key = TRUE
        ),
        "OSM.Vector" = list(
          type = "VectorTile",
          url = "https://{a-c}.tile.openstreetmap.org/{z}/{x}/{y}.pbf",
          format = "MVT",
          attribution = "© OpenStreetMap contributors",
          max_zoom = 14,
          requires_api_key = FALSE
        )
      )
      
      if (source_name %in% names(configs)) {
        return(configs[[source_name]])
      } else {
        # Return basic configuration for unknown sources
        return(list(
          type = "XYZ",
          url = NULL,
          attribution = "",
          max_zoom = 18,
          requires_api_key = FALSE
        ))
      }
    },
    
    #' Create Custom Source
    #'
    #' Create a custom OpenLayers source configuration.
    #'
    #' @param type Character string indicating source type (XYZ, WMS, WMTS, etc.)
    #' @param url Character string containing source URL template
    #' @param attribution Character string for attribution text
    #' @param max_zoom Numeric maximum zoom level
    #' @param min_zoom Numeric minimum zoom level
    #' @param api_key Character string for API key (if required)
    #' @param additional_params List of additional source parameters
    #' @return List containing custom source configuration
    create_custom_source = function(type, url, attribution = "", max_zoom = 18, 
                                   min_zoom = 0, api_key = NULL, additional_params = list()) {
      if (!is.character(type) || length(type) != 1 || nchar(type) == 0) {
        stop("Type must be a single non-empty character string")
      }
      
      if (!is.character(url) || length(url) != 1 || nchar(url) == 0) {
        stop("URL must be a single non-empty character string")
      }
      
      # Validate URL format based on type
      if (type == "XYZ") {
        if (!grepl("^https?://", url)) {
          stop("URL must start with http:// or https://")
        }
        
        if (!grepl("\\{z\\}", url) || !grepl("\\{x\\}", url) || !grepl("\\{y\\}", url)) {
          stop("XYZ URL must contain {z}, {x}, and {y} placeholders")
        }
      }
      
      # Validate zoom levels
      if (!is.numeric(max_zoom) || length(max_zoom) != 1 || max_zoom < 1 || max_zoom > 28) {
        stop("max_zoom must be a single numeric value between 1 and 28")
      }
      
      if (!is.numeric(min_zoom) || length(min_zoom) != 1 || min_zoom < 0 || min_zoom >= max_zoom) {
        stop("min_zoom must be a single numeric value between 0 and max_zoom")
      }
      
      config <- list(
        type = type,
        url = url,
        attribution = attribution,
        max_zoom = max_zoom,
        min_zoom = min_zoom,
        requires_api_key = !is.null(api_key),
        api_key = api_key
      )
      
      # Add additional parameters
      if (length(additional_params) > 0) {
        config <- c(config, additional_params)
      }
      
      return(config)
    },
    
    #' Set Source Options
    #'
    #' Set advanced options for the current source.
    #'
    #' @param options List containing source options
    #' @return Invisible self for method chaining
    set_source_options = function(options) {
      if (!is.list(options)) {
        stop("Options must be a list")
      }
      
      # Merge with existing configuration
      for (key in names(options)) {
        if (key %in% names(self$openlayers_config)) {
          self$openlayers_config[[key]] <- options[[key]]
        } else {
          warning(sprintf("Unknown source option: %s", key))
        }
      }
      
      invisible(self)
    },
    
    #' Create WMS Source
    #'
    #' Create a Web Map Service (WMS) source configuration.
    #'
    #' @param url Character string containing WMS service URL
    #' @param layers Character vector of layer names to request
    #' @param version Character string indicating WMS version (default: "1.3.0")
    #' @param format Character string indicating image format (default: "image/png")
    #' @param transparent Logical indicating if images should be transparent
    #' @param attribution Character string for attribution text
    #' @param additional_params List of additional WMS parameters
    #' @return List containing WMS source configuration
    create_wms_source = function(url, layers, version = "1.3.0", format = "image/png",
                                transparent = TRUE, attribution = "", additional_params = list()) {
      if (!is.character(url) || length(url) != 1 || nchar(url) == 0) {
        stop("URL must be a single non-empty character string")
      }
      
      if (!is.character(layers) || length(layers) == 0) {
        stop("Layers must be a non-empty character vector")
      }
      
      if (!grepl("^https?://", url)) {
        stop("URL must start with http:// or https://")
      }
      
      # Build WMS parameters
      wms_params <- list(
        SERVICE = "WMS",
        VERSION = version,
        REQUEST = "GetMap",
        LAYERS = paste(layers, collapse = ","),
        FORMAT = format,
        TRANSPARENT = if (transparent) "TRUE" else "FALSE"
      )
      
      # Add additional parameters
      if (length(additional_params) > 0) {
        wms_params <- c(wms_params, additional_params)
      }
      
      config <- list(
        type = "WMS",
        url = url,
        params = wms_params,
        attribution = attribution,
        layers = layers,
        version = version,
        format = format,
        transparent = transparent
      )
      
      return(config)
    },
    
    #' Create WMTS Source
    #'
    #' Create a Web Map Tile Service (WMTS) source configuration.
    #'
    #' @param url Character string containing WMTS service URL
    #' @param layer Character string indicating layer name
    #' @param matrix_set Character string indicating tile matrix set
    #' @param format Character string indicating tile format (default: "image/png")
    #' @param attribution Character string for attribution text
    #' @param style Character string indicating style (default: "default")
    #' @param additional_params List of additional WMTS parameters
    #' @return List containing WMTS source configuration
    create_wmts_source = function(url, layer, matrix_set, format = "image/png",
                                 attribution = "", style = "default", additional_params = list()) {
      if (!is.character(url) || length(url) != 1 || nchar(url) == 0) {
        stop("URL must be a single non-empty character string")
      }
      
      if (!is.character(layer) || length(layer) != 1 || nchar(layer) == 0) {
        stop("Layer must be a single non-empty character string")
      }
      
      if (!is.character(matrix_set) || length(matrix_set) != 1 || nchar(matrix_set) == 0) {
        stop("Matrix set must be a single non-empty character string")
      }
      
      if (!grepl("^https?://", url)) {
        stop("URL must start with http:// or https://")
      }
      
      config <- list(
        type = "WMTS",
        url = url,
        layer = layer,
        matrixSet = matrix_set,
        format = format,
        style = style,
        attribution = attribution
      )
      
      # Add additional parameters
      if (length(additional_params) > 0) {
        config <- c(config, additional_params)
      }
      
      return(config)
    },
    
    #' Create Vector Tile Source
    #'
    #' Create a vector tile source configuration.
    #'
    #' @param url Character string containing vector tile URL template
    #' @param format Character string indicating vector format (default: "MVT")
    #' @param attribution Character string for attribution text
    #' @param max_zoom Numeric maximum zoom level
    #' @param min_zoom Numeric minimum zoom level
    #' @param additional_params List of additional parameters
    #' @return List containing vector tile source configuration
    create_vector_tile_source = function(url, format = "MVT", attribution = "",
                                        max_zoom = 14, min_zoom = 0, additional_params = list()) {
      if (!is.character(url) || length(url) != 1 || nchar(url) == 0) {
        stop("URL must be a single non-empty character string")
      }
      
      if (!grepl("^https?://", url)) {
        stop("URL must start with http:// or https://")
      }
      
      if (!grepl("\\{z\\}", url) || !grepl("\\{x\\}", url) || !grepl("\\{y\\}", url)) {
        stop("Vector tile URL must contain {z}, {x}, and {y} placeholders")
      }
      
      # Validate zoom levels
      if (!is.numeric(max_zoom) || length(max_zoom) != 1 || max_zoom < 1 || max_zoom > 22) {
        stop("max_zoom must be a single numeric value between 1 and 22")
      }
      
      if (!is.numeric(min_zoom) || length(min_zoom) != 1 || min_zoom < 0 || min_zoom >= max_zoom) {
        stop("min_zoom must be a single numeric value between 0 and max_zoom")
      }
      
      config <- list(
        type = "VectorTile",
        url = url,
        format = format,
        attribution = attribution,
        max_zoom = max_zoom,
        min_zoom = min_zoom
      )
      
      # Add additional parameters
      if (length(additional_params) > 0) {
        config <- c(config, additional_params)
      }
      
      return(config)
    },
    
    #' Get Projection Information
    #'
    #' Get information about supported projections.
    #'
    #' @param projection Character string identifying the projection (optional)
    #' @return List containing projection information
    get_projection_info = function(projection = NULL) {
      projections <- list(
        "EPSG:3857" = list(
          name = "Web Mercator",
          description = "Spherical Mercator projection used by most web mapping services",
          units = "m",
          extent = c(-20037508.34, -20037508.34, 20037508.34, 20037508.34),
          global = TRUE
        ),
        "EPSG:4326" = list(
          name = "WGS 84",
          description = "World Geodetic System 1984, geographic coordinate system",
          units = "degrees",
          extent = c(-180, -90, 180, 90),
          global = TRUE
        ),
        "EPSG:3395" = list(
          name = "World Mercator",
          description = "Mercator projection of the World Geodetic System 1984",
          units = "m",
          extent = c(-20037508.34, -15496570.74, 20037508.34, 18764656.23),
          global = TRUE
        )
      )
      
      if (is.null(projection)) {
        return(projections)
      } else {
        if (projection %in% names(projections)) {
          return(projections[[projection]])
        } else {
          warning(sprintf("Unknown projection: %s", projection))
          return(NULL)
        }
      }
    },
    
    #' Set Map Projection
    #'
    #' Set the map projection for the OpenLayers map.
    #'
    #' @param projection Character string indicating the projection
    #' @param extent Numeric vector of extent coordinates (optional)
    #' @return Invisible self for method chaining
    set_projection = function(projection, extent = NULL) {
      if (!is.character(projection) || length(projection) != 1) {
        stop("Projection must be a single character string")
      }
      
      # Validate projection
      proj_info <- self$get_projection_info(projection)
      if (is.null(proj_info)) {
        warning(sprintf("Unknown projection '%s' - proceeding anyway", projection))
      }
      
      # Update configuration
      self$openlayers_config$projection <- projection
      
      if (!is.null(extent)) {
        if (!is.numeric(extent) || length(extent) != 4) {
          stop("Extent must be a numeric vector of length 4")
        }
        self$openlayers_config$extent <- extent
      } else if (!is.null(proj_info)) {
        self$openlayers_config$extent <- proj_info$extent
      }
      
      invisible(self)
    },
    
    #' Validate Configuration
    #'
    #' Validate OpenLayers-specific configuration.
    #'
    #' @param config List containing configuration to validate
    #' @return Logical indicating if configuration is valid
    validate_config = function(config) {
      if (!is.list(config)) {
        return(FALSE)
      }
      
      # OpenLayers doesn't require authentication by default
      # Validate source if specified
      if (!is.null(config$source)) {
        if (!is.character(config$source) || 
            length(config$source) != 1) {
          return(FALSE)
        }
        
        # Check if it's a known source
        available_sources <- self$get_available_styles()
        if (!config$source %in% available_sources &&
            is.null(config$source_url)) {
          warning(sprintf("Unknown source '%s' and no custom source_url provided", 
                         config$source))
        }
      }
      
      # Validate custom source URL if provided
      if (!is.null(config$source_url)) {
        if (!is.character(config$source_url) || length(config$source_url) != 1) {
          return(FALSE)
        }
        
        # Basic URL validation
        if (!grepl("^https?://", config$source_url)) {
          warning("Custom source URL should start with http:// or https://")
        }
      }
      
      # Validate projection if specified
      if (!is.null(config$projection)) {
        if (!is.character(config$projection) || length(config$projection) != 1) {
          return(FALSE)
        }
        
        # Check for common projections
        valid_projections <- c("EPSG:3857", "EPSG:4326", "EPSG:3395")
        if (!config$projection %in% valid_projections) {
          warning(sprintf("Projection '%s' may not be supported", config$projection))
        }
      }
      
      return(TRUE)
    },
    
    #' Destroy Provider
    #'
    #' Clean up resources and destroy the OpenLayers provider instance.
    #'
    #' @return Invisible NULL
    destroy = function() {
      # Clear layers
      self$layers <- list()
      
      # Clear map instance reference
      self$map_instance <- NULL
      
      # Clear configuration
      self$openlayers_config <- NULL
      self$source_config <- NULL
      self$current_style <- NULL
      
      # Mark as not initialized
      self$initialized <- FALSE
      
      invisible(NULL)
    }
  )
)

#' Normalize OpenLayers Source
#'
#' Normalize source names for OpenLayers.
#'
#' @param source Character string identifying the source
#' @return Normalized source name
#'
#' @examples
#' \donttest{
#' # Normalize source names
#' source <- normalize_openlayers_source("osm")
#' }
#'
#' @export
normalize_openlayers_source <- function(source) {
  if (is.null(source)) {
    return("OSM")
  }
  
  if (!is.character(source) || length(source) != 1) {
    stop("Source must be a single character string")
  }
  
  # Common aliases
  aliases <- list(
    "osm" = "OSM",
    "openstreetmap" = "OSM",
    "cartodb" = "CartoDB.Positron",
    "positron" = "CartoDB.Positron",
    "dark" = "CartoDB.DarkMatter",
    "darkmatter" = "CartoDB.DarkMatter",
    "satellite" = "ESRI.WorldImagery",
    "imagery" = "ESRI.WorldImagery",
    "terrain" = "Stamen.Terrain",
    "topo" = "ESRI.WorldTopoMap",
    "topographic" = "ESRI.WorldTopoMap",
    "watercolor" = "Stamen.Watercolor",
    "toner" = "Stamen.Toner",
    "bing" = "BingMaps.Aerial",
    "aerial" = "BingMaps.Aerial",
    "vector" = "OSM.Vector"
  )
  
  # Convert to lowercase for matching
  source_lower <- tolower(source)
  
  if (source_lower %in% names(aliases)) {
    return(aliases[[source_lower]])
  }
  
  # Return as-is if no alias found (assume it's a valid source name)
  return(source)
}

# Define %||% operator if not already defined
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}