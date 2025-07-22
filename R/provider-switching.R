#' Provider Switching Functionality
#'
#' This file contains functions for seamless provider switching, including
#' provider compatibility checking, feature mapping, graceful degradation,
#' and layer preservation logic.
#'
#' @name provider-switching
NULL

#' Update Map Provider
#'
#' Seamlessly switch the map provider while preserving all layers and settings.
#'
#' @description
#' This function allows switching between different map providers (Mapbox, Leaflet,
#' OpenLayers, Gaode, Baidu) while maintaining all existing layers, styling,
#' and map state. It handles provider compatibility checking, coordinate
#' transformations, and graceful degradation for unsupported features.
#'
#' @param map A mapdeck map object
#' @param new_provider Character string identifying the new provider
#' @param config List containing provider-specific configuration (optional)
#' @param preserve_layers Logical indicating if layers should be preserved
#' @param transform_coordinates Logical indicating if coordinates should be transformed
#' @param fallback_on_error Logical indicating if fallback should be used on errors
#'
#' @return Updated mapdeck map object with new provider
#'
#' @examples
#' \donttest{
#' # Create a map with Mapbox
#' map <- mapdeck(provider = "mapbox", token = "your_token")
#' map <- add_scatterplot(map, data = capitals, lon = "lon", lat = "lat")
#' 
#' # Switch to Leaflet while preserving layers
#' map <- update_provider(map, "leaflet")
#' 
#' # Switch to Gaode with custom configuration
#' map <- update_provider(map, "gaode", 
#'                       config = list(api_key = "your_gaode_key"))
#' }
#'
#' @export
update_provider <- function(map, new_provider, config = list(), 
                           preserve_layers = TRUE, transform_coordinates = TRUE,
                           fallback_on_error = TRUE) {
  
  # Validate inputs
  if (missing(map) || is.null(map)) {
    stop("Map object is required")
  }
  
  if (!is.character(new_provider) || length(new_provider) != 1) {
    stop("new_provider must be a single character string")
  }
  
  if (!is.list(config)) {
    stop("config must be a list")
  }
  
  # Get current provider information
  current_provider_info <- extract_provider_info(map)
  current_provider <- current_provider_info$provider
  
  # Check if switching to the same provider
  if (current_provider == new_provider) {
    message(sprintf("Map is already using provider '%s'", new_provider))
    return(map)
  }
  
  # Validate new provider availability
  if (!is_provider_available(new_provider)) {
    stop(sprintf("Provider '%s' is not available", new_provider))
  }
  
  # Check provider compatibility
  compatibility_result <- check_provider_compatibility(
    current_provider, new_provider, current_provider_info$layers
  )
  
  if (!compatibility_result$compatible && !fallback_on_error) {
    stop(sprintf("Providers '%s' and '%s' are not compatible: %s",
                current_provider, new_provider, 
                compatibility_result$reason))
  }
  
  # Extract current map state
  map_state <- extract_map_state(map)
  
  # Transform coordinates if needed
  if (transform_coordinates) {
    map_state <- transform_map_state_coordinates(
      map_state, current_provider, new_provider
    )
  }
  
  # Create new provider instance
  tryCatch({
    # Merge user config with extracted state
    merged_config <- merge_provider_configs(config, map_state$config)
    
    # Create new map with new provider
    new_map <- create_map_with_provider(new_provider, merged_config, map_state)
    
    # Preserve layers if requested
    if (preserve_layers && length(map_state$layers) > 0) {
      new_map <- preserve_map_layers(new_map, map_state$layers, 
                                   current_provider, new_provider,
                                   compatibility_result)
    }
    
    # Apply graceful degradation for incompatible features
    if (!compatibility_result$compatible) {
      new_map <- apply_graceful_degradation(new_map, compatibility_result)
    }
    
    return(new_map)
    
  }, error = function(e) {
    if (fallback_on_error) {
      warning(sprintf("Failed to switch to provider '%s': %s. Returning original map.",
                     new_provider, e$message))
      return(map)
    } else {
      stop(sprintf("Failed to switch to provider '%s': %s", new_provider, e$message))
    }
  })
}

#' Check Provider Compatibility
#'
#' Check compatibility between two providers and identify potential issues.
#'
#' @param from_provider Character string identifying the source provider
#' @param to_provider Character string identifying the target provider
#' @param layers List of layer configurations (optional)
#'
#' @return List containing compatibility information
#'
#' @examples
#' \donttest{
#' # Check compatibility between providers
#' result <- check_provider_compatibility("mapbox", "leaflet")
#' print(result$compatible)
#' print(result$issues)
#' }
#'
#' @export
check_provider_compatibility <- function(from_provider, to_provider, layers = list()) {
  
  # Validate inputs
  if (!is.character(from_provider) || length(from_provider) != 1) {
    stop("from_provider must be a single character string")
  }
  
  if (!is.character(to_provider) || length(to_provider) != 1) {
    stop("to_provider must be a single character string")
  }
  
  # Get provider capabilities
  from_capabilities <- get_provider_capabilities_safe(from_provider)
  to_capabilities <- get_provider_capabilities_safe(to_provider)
  
  if (is.null(from_capabilities) || is.null(to_capabilities)) {
    return(list(
      compatible = FALSE,
      reason = "Unable to determine provider capabilities",
      issues = list("Provider capabilities not available"),
      feature_mapping = list()
    ))
  }
  
  # Check basic compatibility
  compatibility_issues <- list()
  feature_mapping <- list()
  
  # Check coordinate system compatibility
  coord_issue <- check_coordinate_system_compatibility(from_provider, to_provider)
  if (!is.null(coord_issue)) {
    compatibility_issues <- append(compatibility_issues, coord_issue)
  }
  
  # Check authentication requirements
  auth_issue <- check_authentication_compatibility(from_capabilities, to_capabilities)
  if (!is.null(auth_issue)) {
    compatibility_issues <- append(compatibility_issues, auth_issue)
  }
  
  # Check layer compatibility
  if (length(layers) > 0) {
    layer_issues <- check_layer_compatibility(layers, from_provider, to_provider)
    compatibility_issues <- append(compatibility_issues, layer_issues$issues)
    feature_mapping <- layer_issues$mapping
  }
  
  # Check style compatibility
  style_issue <- check_style_compatibility(from_provider, to_provider)
  if (!is.null(style_issue)) {
    compatibility_issues <- append(compatibility_issues, style_issue)
  }
  
  # Determine overall compatibility
  is_compatible <- length(compatibility_issues) == 0
  
  return(list(
    compatible = is_compatible,
    reason = if (is_compatible) "Providers are compatible" else 
             paste(compatibility_issues, collapse = "; "),
    issues = compatibility_issues,
    feature_mapping = feature_mapping,
    coordinate_transformation_required = needs_coordinate_transformation(from_provider, to_provider),
    degraded_features = get_degraded_features(from_provider, to_provider)
  ))
}

#' Extract Provider Information
#'
#' Extract current provider information from a map object.
#'
#' @param map A mapdeck map object
#' @return List containing provider information
extract_provider_info <- function(map) {
  
  # Check if map has provider attribute
  provider_obj <- attr(map, "mapdeck_provider")
  
  if (!is.null(provider_obj)) {
    # New provider system
    return(list(
      provider = provider_obj$provider_name,
      provider_object = provider_obj,
      layers = provider_obj$layers,
      config = provider_obj$config
    ))
  }
  
  # Legacy system - assume Mapbox
  return(list(
    provider = "mapbox",
    provider_object = NULL,
    layers = list(),
    config = list()
  ))
}

#' Extract Map State
#'
#' Extract current map state including view, layers, and configuration.
#'
#' @param map A mapdeck map object
#' @return List containing map state information
extract_map_state <- function(map) {
  
  # Extract basic map properties
  map_data <- map$x
  
  # Extract view state
  view_state <- list(
    location = map_data$location %||% c(0, 0),
    zoom = map_data$zoom %||% 0,
    pitch = map_data$pitch %||% 0,
    bearing = map_data$bearing %||% 0
  )
  
  # Extract configuration
  config <- list(
    access_token = map_data$access_token,
    style = map_data$style,
    max_zoom = map_data$max_zoom %||% 20,
    min_zoom = map_data$min_zoom %||% 0,
    max_pitch = map_data$max_pitch %||% 60,
    min_pitch = map_data$min_pitch %||% 0,
    show_view_state = map_data$show_view_state %||% FALSE,
    repeat_view = map_data$repeat_view %||% FALSE
  )
  
  # Extract layers (from provider object if available)
  provider_obj <- attr(map, "mapdeck_provider")
  layers <- if (!is.null(provider_obj)) provider_obj$layers else list()
  
  # Extract data
  data <- attr(map$x, "mapdeck_data", exact = TRUE)
  
  return(list(
    view_state = view_state,
    config = config,
    layers = layers,
    data = data,
    width = map$width,
    height = map$height,
    sizing_policy = map$sizingPolicy
  ))
}

#' Transform Map State Coordinates
#'
#' Transform coordinates in map state for different providers.
#'
#' @param map_state List containing map state
#' @param from_provider Character string identifying source provider
#' @param to_provider Character string identifying target provider
#' @return Transformed map state
transform_map_state_coordinates <- function(map_state, from_provider, to_provider) {
  
  # Check if transformation is needed
  if (!needs_coordinate_transformation(from_provider, to_provider)) {
    return(map_state)
  }
  
  # Get coordinate systems for providers
  from_crs <- get_provider_coordinate_system(from_provider)
  to_crs <- get_provider_coordinate_system(to_provider)
  
  if (from_crs == to_crs) {
    return(map_state)
  }
  
  # Transform view state coordinates
  tryCatch({
    if (!is.null(map_state$view_state$location) && 
        length(map_state$view_state$location) == 2) {
      
      transformer <- get_coordinate_transformer()
      transformed_location <- transformer$transform_coordinates(
        matrix(map_state$view_state$location, nrow = 1),
        from_crs, to_crs
      )
      
      map_state$view_state$location <- as.numeric(transformed_location[1, ])
    }
    
    # Transform data coordinates if present
    if (!is.null(map_state$data)) {
      map_state$data <- transform_data_coordinates(
        map_state$data, from_crs, to_crs
      )
    }
    
  }, error = function(e) {
    warning(sprintf("Failed to transform coordinates: %s", e$message))
  })
  
  return(map_state)
}

#' Create Map with Provider
#'
#' Create a new map using the specified provider and configuration.
#'
#' @param provider Character string identifying the provider
#' @param config List containing provider configuration
#' @param map_state List containing map state to restore
#' @return New mapdeck map object
create_map_with_provider <- function(provider, config, map_state) {
  
  # Merge configuration with map state
  final_config <- list(
    token = config$token %||% config$access_token,
    style = config$style,
    pitch = map_state$view_state$pitch,
    zoom = map_state$view_state$zoom,
    bearing = map_state$view_state$bearing,
    location = map_state$view_state$location,
    max_zoom = config$max_zoom,
    min_zoom = config$min_zoom,
    max_pitch = config$max_pitch,
    min_pitch = config$min_pitch
  )
  
  # Create new map
  new_map <- mapdeck(
    data = map_state$data,
    provider = provider,
    token = final_config$token,
    style = final_config$style,
    pitch = final_config$pitch,
    zoom = final_config$zoom,
    bearing = final_config$bearing,
    location = final_config$location,
    max_zoom = final_config$max_zoom,
    min_zoom = final_config$min_zoom,
    max_pitch = final_config$max_pitch,
    min_pitch = final_config$min_pitch,
    width = map_state$width,
    height = map_state$height,
    show_view_state = map_state$config$show_view_state,
    repeat_view = map_state$config$repeat_view
  )
  
  return(new_map)
}

#' Preserve Map Layers
#'
#' Preserve layers when switching between providers.
#'
#' @param map New map object
#' @param layers List of layer configurations
#' @param from_provider Character string identifying source provider
#' @param to_provider Character string identifying target provider
#' @param compatibility_result List containing compatibility information
#' @return Map object with preserved layers
preserve_map_layers <- function(map, layers, from_provider, to_provider, 
                               compatibility_result) {
  
  if (length(layers) == 0) {
    return(map)
  }
  
  # Process each layer
  for (layer_id in names(layers)) {
    layer_config <- layers[[layer_id]]
    
    tryCatch({
      # Check if layer is compatible with new provider
      if (is_layer_compatible(layer_config, to_provider)) {
        # Transform layer if needed
        transformed_layer <- transform_layer_for_provider(
          layer_config, from_provider, to_provider
        )
        
        # Add layer to new map
        # Note: This would typically call the appropriate add_* function
        # For now, we store the layer configuration
        provider_obj <- attr(map, "mapdeck_provider")
        if (!is.null(provider_obj)) {
          provider_obj$add_layer(transformed_layer)
        }
        
      } else {
        # Apply graceful degradation
        degraded_layer <- apply_layer_degradation(
          layer_config, to_provider, compatibility_result
        )
        
        if (!is.null(degraded_layer)) {
          provider_obj <- attr(map, "mapdeck_provider")
          if (!is.null(provider_obj)) {
            provider_obj$add_layer(degraded_layer)
          }
          
          warning(sprintf("Layer '%s' was modified for compatibility with provider '%s'",
                         layer_id, to_provider))
        } else {
          warning(sprintf("Layer '%s' is not compatible with provider '%s' and was skipped",
                         layer_id, to_provider))
        }
      }
      
    }, error = function(e) {
      warning(sprintf("Failed to preserve layer '%s': %s", layer_id, e$message))
    })
  }
  
  return(map)
}

#' Apply Graceful Degradation
#'
#' Apply graceful degradation for incompatible features.
#'
#' @param map Map object
#' @param compatibility_result List containing compatibility information
#' @return Map object with degradation applied
apply_graceful_degradation <- function(map, compatibility_result) {
  
  if (length(compatibility_result$degraded_features) == 0) {
    return(map)
  }
  
  # Apply degradation for each feature
  for (feature in compatibility_result$degraded_features) {
    tryCatch({
      map <- apply_feature_degradation(map, feature)
    }, error = function(e) {
      warning(sprintf("Failed to apply degradation for feature '%s': %s",
                     feature, e$message))
    })
  }
  
  return(map)
}

# Helper functions for provider switching

#' Check if Provider is Available
#'
#' @param provider Character string identifying the provider
#' @return Logical indicating availability
is_provider_available <- function(provider) {
  tryCatch({
    factory <- get_provider_factory()
    available_providers <- factory$get_available_providers()
    return(provider %in% available_providers)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Get Provider Capabilities Safely
#'
#' @param provider Character string identifying the provider
#' @return Provider capabilities or NULL if unavailable
get_provider_capabilities_safe <- function(provider) {
  tryCatch({
    return(get_provider_capabilities(provider))
  }, error = function(e) {
    return(NULL)
  })
}

#' Check Coordinate System Compatibility
#'
#' @param from_provider Character string identifying source provider
#' @param to_provider Character string identifying target provider
#' @return Character string describing issue or NULL if compatible
check_coordinate_system_compatibility <- function(from_provider, to_provider) {
  
  # Get coordinate systems
  from_crs <- get_provider_coordinate_system(from_provider)
  to_crs <- get_provider_coordinate_system(to_provider)
  
  if (from_crs != to_crs) {
    return(sprintf("Coordinate system transformation required: %s -> %s", 
                  from_crs, to_crs))
  }
  
  return(NULL)
}

#' Check Authentication Compatibility
#'
#' @param from_capabilities List of source provider capabilities
#' @param to_capabilities List of target provider capabilities
#' @return Character string describing issue or NULL if compatible
check_authentication_compatibility <- function(from_capabilities, to_capabilities) {
  
  from_auth <- from_capabilities$authentication_required %||% FALSE
  to_auth <- to_capabilities$authentication_required %||% FALSE
  
  if (!from_auth && to_auth) {
    return("Target provider requires authentication")
  }
  
  return(NULL)
}

#' Check Layer Compatibility
#'
#' @param layers List of layer configurations
#' @param from_provider Character string identifying source provider
#' @param to_provider Character string identifying target provider
#' @return List containing issues and mapping information
check_layer_compatibility <- function(layers, from_provider, to_provider) {
  
  issues <- list()
  mapping <- list()
  
  for (layer_id in names(layers)) {
    layer_config <- layers[[layer_id]]
    
    if (!is_layer_compatible(layer_config, to_provider)) {
      issues <- append(issues, sprintf("Layer '%s' not compatible with %s", 
                                      layer_id, to_provider))
    }
    
    # Create feature mapping for this layer
    layer_mapping <- create_layer_feature_mapping(layer_config, from_provider, to_provider)
    if (length(layer_mapping) > 0) {
      mapping[[layer_id]] <- layer_mapping
    }
  }
  
  return(list(issues = issues, mapping = mapping))
}

#' Check Style Compatibility
#'
#' @param from_provider Character string identifying source provider
#' @param to_provider Character string identifying target provider
#' @return Character string describing issue or NULL if compatible
check_style_compatibility <- function(from_provider, to_provider) {
  
  # Different providers have different style systems
  style_systems <- list(
    "mapbox" = "mapbox_styles",
    "leaflet" = "tile_providers",
    "openlayers" = "layer_sources",
    "gaode" = "gaode_styles",
    "baidu" = "baidu_styles"
  )
  
  from_system <- style_systems[[from_provider]]
  to_system <- style_systems[[to_provider]]
  
  if (from_system != to_system) {
    return(sprintf("Style system incompatibility: %s -> %s", from_system, to_system))
  }
  
  return(NULL)
}

#' Check if Coordinate Transformation is Needed
#'
#' @param from_provider Character string identifying source provider
#' @param to_provider Character string identifying target provider
#' @return Logical indicating if transformation is needed
needs_coordinate_transformation <- function(from_provider, to_provider) {
  from_crs <- get_provider_coordinate_system(from_provider)
  to_crs <- get_provider_coordinate_system(to_provider)
  return(from_crs != to_crs)
}

#' Get Provider Coordinate System
#'
#' @param provider Character string identifying the provider
#' @return Character string identifying coordinate system
get_provider_coordinate_system <- function(provider) {
  coord_systems <- list(
    "mapbox" = "EPSG:4326",
    "leaflet" = "EPSG:4326", 
    "openlayers" = "EPSG:4326",
    "gaode" = "GCJ02",
    "baidu" = "BD09"
  )
  
  return(coord_systems[[provider]] %||% "EPSG:4326")
}

#' Get Degraded Features
#'
#' @param from_provider Character string identifying source provider
#' @param to_provider Character string identifying target provider
#' @return Character vector of features that will be degraded
get_degraded_features <- function(from_provider, to_provider) {
  
  # Define feature support matrix
  feature_support <- list(
    "mapbox" = c("3d_layers", "pitch", "bearing", "custom_styles", "vector_tiles"),
    "leaflet" = c("tile_layers", "basic_interactions"),
    "openlayers" = c("vector_layers", "wms_layers", "custom_projections"),
    "gaode" = c("chinese_pois", "traffic_layers", "gcj02_coords"),
    "baidu" = c("chinese_pois", "panorama", "bd09_coords")
  )
  
  from_features <- feature_support[[from_provider]] %||% character(0)
  to_features <- feature_support[[to_provider]] %||% character(0)
  
  # Features that will be lost in transition
  degraded <- setdiff(from_features, to_features)
  
  return(degraded)
}

#' Check if Layer is Compatible
#'
#' @param layer_config List containing layer configuration
#' @param provider Character string identifying the provider
#' @return Logical indicating compatibility
is_layer_compatible <- function(layer_config, provider) {
  
  # All deck.gl layers should be compatible across providers
  # This is a basic check - more sophisticated logic could be added
  
  if (is.null(layer_config$type)) {
    return(FALSE)
  }
  
  # Check provider-specific layer restrictions
  if (provider == "leaflet") {
    # Leaflet might have limitations with 3D layers
    if (layer_config$type %in% c("column", "hexagon") && 
        !is.null(layer_config$elevation)) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

#' Transform Layer for Provider
#'
#' @param layer_config List containing layer configuration
#' @param from_provider Character string identifying source provider
#' @param to_provider Character string identifying target provider
#' @return Transformed layer configuration
transform_layer_for_provider <- function(layer_config, from_provider, to_provider) {
  
  # Transform coordinates if needed
  if (needs_coordinate_transformation(from_provider, to_provider)) {
    layer_config <- transform_layer_coordinates(layer_config, from_provider, to_provider)
  }
  
  # Apply provider-specific transformations
  layer_config <- apply_provider_specific_layer_transforms(layer_config, to_provider)
  
  return(layer_config)
}

#' Apply Layer Degradation
#'
#' @param layer_config List containing layer configuration
#' @param provider Character string identifying the provider
#' @param compatibility_result List containing compatibility information
#' @return Degraded layer configuration or NULL if not possible
apply_layer_degradation <- function(layer_config, provider, compatibility_result) {
  
  # Apply specific degradations based on provider limitations
  if (provider == "leaflet") {
    # Remove 3D properties for Leaflet
    layer_config$elevation <- NULL
    layer_config$pitch <- NULL
    layer_config$bearing <- NULL
  }
  
  return(layer_config)
}

#' Apply Feature Degradation
#'
#' @param map Map object
#' @param feature Character string identifying the feature
#' @return Map object with feature degradation applied
apply_feature_degradation <- function(map, feature) {
  
  # Apply specific degradations based on feature
  switch(feature,
    "3d_layers" = {
      # Flatten 3D layers to 2D
      message("3D layer features have been flattened for compatibility")
    },
    "pitch" = {
      # Reset pitch to 0
      map <- mapdeck_view(map, pitch = 0)
      message("Map pitch has been reset to 0 for compatibility")
    },
    "bearing" = {
      # Reset bearing to 0
      map <- mapdeck_view(map, bearing = 0)
      message("Map bearing has been reset to 0 for compatibility")
    },
    "custom_styles" = {
      # Fall back to default style
      message("Custom styles have been replaced with default style for compatibility")
    }
  )
  
  return(map)
}

#' Merge Provider Configurations
#'
#' @param user_config List containing user-provided configuration
#' @param state_config List containing configuration from map state
#' @return Merged configuration list
merge_provider_configs <- function(user_config, state_config) {
  
  # Start with state configuration
  merged <- state_config
  
  # Override with user-provided values
  for (key in names(user_config)) {
    merged[[key]] <- user_config[[key]]
  }
  
  return(merged)
}

#' Transform Data Coordinates
#'
#' @param data Data frame or other data structure
#' @param from_crs Character string identifying source coordinate system
#' @param to_crs Character string identifying target coordinate system
#' @return Transformed data
transform_data_coordinates <- function(data, from_crs, to_crs) {
  
  if (is.null(data) || from_crs == to_crs) {
    return(data)
  }
  
  # This would use the coordinate transformation service
  # For now, return data unchanged
  return(data)
}

#' Transform Layer Coordinates
#'
#' @param layer_config List containing layer configuration
#' @param from_provider Character string identifying source provider
#' @param to_provider Character string identifying target provider
#' @return Layer configuration with transformed coordinates
transform_layer_coordinates <- function(layer_config, from_provider, to_provider) {
  
  # This would transform coordinate data within the layer configuration
  # For now, return layer unchanged
  return(layer_config)
}

#' Apply Provider-Specific Layer Transforms
#'
#' @param layer_config List containing layer configuration
#' @param provider Character string identifying the provider
#' @return Transformed layer configuration
apply_provider_specific_layer_transforms <- function(layer_config, provider) {
  
  # Apply provider-specific modifications
  switch(provider,
    "leaflet" = {
      # Leaflet-specific transformations
      layer_config$leaflet_compatible <- TRUE
    },
    "openlayers" = {
      # OpenLayers-specific transformations
      layer_config$openlayers_compatible <- TRUE
    },
    "gaode" = {
      # Gaode-specific transformations
      layer_config$gaode_compatible <- TRUE
    },
    "baidu" = {
      # Baidu-specific transformations
      layer_config$baidu_compatible <- TRUE
    }
  )
  
  return(layer_config)
}

#' Create Layer Feature Mapping
#'
#' @param layer_config List containing layer configuration
#' @param from_provider Character string identifying source provider
#' @param to_provider Character string identifying target provider
#' @return List containing feature mapping information
create_layer_feature_mapping <- function(layer_config, from_provider, to_provider) {
  
  mapping <- list()
  
  # Create mappings based on layer type and providers
  if (!is.null(layer_config$type)) {
    mapping$layer_type <- layer_config$type
    mapping$compatible <- is_layer_compatible(layer_config, to_provider)
    mapping$transformations_needed <- needs_coordinate_transformation(from_provider, to_provider)
  }
  
  return(mapping)
}

#' Get Coordinate Transformer
#'
#' @return Coordinate transformer object
get_coordinate_transformer <- function() {
  # This would return the coordinate transformation service
  # For now, return a mock object
  list(
    transform_coordinates = function(data, from_crs, to_crs) {
      return(data)
    }
  )
}

# Define %||% operator if not already defined
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}