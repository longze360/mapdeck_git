#' Provider Switching Examples
#'
#' This file demonstrates how to use the provider switching functionality
#' to seamlessly switch between different map providers while preserving
#' layers and map state.

# Load required libraries
library(mapdeck)

# Example 1: Basic Provider Switching
#' This example shows how to switch from Mapbox to Leaflet
basic_provider_switching <- function() {
  
  # Create initial map with Mapbox
  map <- mapdeck(
    provider = "mapbox",
    token = "your_mapbox_token",
    location = c(-74.006, 40.7128),
    zoom = 10,
    style = "streets"
  )
  
  # Add some layers
  map <- add_scatterplot(
    map,
    data = capitals,
    lon = "lon",
    lat = "lat",
    radius = 50000,
    fill_colour = "country"
  )
  
  # Switch to Leaflet provider while preserving layers
  map_leaflet <- update_provider(map, "leaflet")
  
  return(map_leaflet)
}

# Example 2: Provider Switching with Configuration
#' This example shows how to provide custom configuration when switching providers
provider_switching_with_config <- function() {
  
  # Create initial map
  map <- mapdeck(
    provider = "mapbox",
    token = "your_mapbox_token",
    location = c(116.3974, 39.9093),  # Beijing
    zoom = 12
  )
  
  # Add layers
  map <- add_polygon(
    map,
    data = melbourne,
    fill_colour = "SA2_NAME",
    stroke_colour = "white"
  )
  
  # Switch to Gaode with custom configuration
  map_gaode <- update_provider(
    map, 
    "gaode",
    config = list(
      api_key = "your_gaode_api_key",
      style = "satellite"
    )
  )
  
  return(map_gaode)
}

# Example 3: Checking Provider Compatibility
#' This example shows how to check compatibility before switching
check_compatibility_example <- function() {
  
  # Check compatibility between providers
  compatibility <- check_provider_compatibility("mapbox", "leaflet")
  
  cat("Compatibility Check Results:\n")
  cat("Compatible:", compatibility$compatible, "\n")
  cat("Reason:", compatibility$reason, "\n")
  
  if (length(compatibility$issues) > 0) {
    cat("Issues:\n")
    for (issue in compatibility$issues) {
      cat("  -", issue, "\n")
    }
  }
  
  if (length(compatibility$degraded_features) > 0) {
    cat("Features that will be degraded:\n")
    for (feature in compatibility$degraded_features) {
      cat("  -", feature, "\n")
    }
  }
  
  cat("Coordinate transformation required:", 
      compatibility$coordinate_transformation_required, "\n")
  
  return(compatibility)
}

# Example 4: Handling Provider Switching Errors
#' This example shows how to handle errors during provider switching
error_handling_example <- function() {
  
  # Create a map
  map <- mapdeck(
    provider = "mapbox",
    token = "your_mapbox_token"
  )
  
  # Try to switch to an unavailable provider with fallback
  tryCatch({
    map_switched <- update_provider(
      map, 
      "unavailable_provider",
      fallback_on_error = TRUE
    )
    
    cat("Provider switching completed successfully\n")
    return(map_switched)
    
  }, error = function(e) {
    cat("Error during provider switching:", e$message, "\n")
    return(map)  # Return original map
  })
}

# Example 5: Coordinate System Transformation
#' This example demonstrates coordinate transformation during provider switching
coordinate_transformation_example <- function() {
  
  # Create map with WGS84 data (Mapbox)
  wgs84_data <- data.frame(
    lon = c(116.3974, 121.4737),  # Beijing, Shanghai
    lat = c(39.9093, 31.2304),
    city = c("Beijing", "Shanghai")
  )
  
  map_mapbox <- mapdeck(
    provider = "mapbox",
    token = "your_mapbox_token",
    location = c(116.3974, 39.9093),
    zoom = 5
  )
  
  map_mapbox <- add_scatterplot(
    map_mapbox,
    data = wgs84_data,
    lon = "lon",
    lat = "lat",
    radius = 100000,
    fill_colour = "city"
  )
  
  # Switch to Gaode (which uses GCJ02 coordinate system)
  # The system will automatically transform coordinates
  map_gaode <- update_provider(
    map_mapbox,
    "gaode",
    config = list(api_key = "your_gaode_key"),
    transform_coordinates = TRUE
  )
  
  cat("Coordinates automatically transformed from WGS84 to GCJ02\n")
  return(map_gaode)
}

# Example 6: Layer Preservation with Degradation
#' This example shows how layers are preserved with graceful degradation
layer_preservation_example <- function() {
  
  # Create Mapbox map with 3D features
  map_3d <- mapdeck(
    provider = "mapbox",
    token = "your_mapbox_token",
    location = c(-74.006, 40.7128),
    zoom = 12,
    pitch = 45,  # 3D view
    bearing = 30
  )
  
  # Add 3D column layer
  map_3d <- add_column(
    map_3d,
    data = road_safety,
    lon = "lng",
    lat = "lat",
    elevation = "accidents",
    fill_colour = "accidents"
  )
  
  # Switch to Leaflet (which doesn't support 3D)
  # The system will gracefully degrade 3D features
  map_leaflet <- update_provider(
    map_3d,
    "leaflet",
    preserve_layers = TRUE
  )
  
  cat("3D features have been gracefully degraded for Leaflet compatibility\n")
  return(map_leaflet)
}

# Example 7: Style Mapping Across Providers
#' This example shows how styles are mapped between providers
style_mapping_example <- function() {
  
  # Create map with dark style
  map_dark <- mapdeck(
    provider = "mapbox",
    token = "your_mapbox_token",
    style = "dark"
  )
  
  # Switch to Leaflet with equivalent dark style
  map_leaflet_dark <- update_provider(
    map_dark,
    "leaflet"
    # Style will be automatically mapped to CartoDB.DarkMatter
  )
  
  cat("Dark style mapped from Mapbox to Leaflet equivalent\n")
  return(map_leaflet_dark)
}

# Example 8: Batch Provider Switching
#' This example shows how to test a map across multiple providers
batch_provider_switching <- function() {
  
  # Create base map
  base_map <- mapdeck(
    provider = "mapbox",
    token = "your_mapbox_token",
    location = c(0, 0),
    zoom = 2
  )
  
  base_map <- add_scatterplot(
    base_map,
    data = capitals,
    lon = "lon",
    lat = "lat",
    radius = 100000,
    fill_colour = "country"
  )
  
  # Test across multiple providers
  providers <- c("leaflet", "openlayers")
  results <- list()
  
  for (provider in providers) {
    cat("Testing provider:", provider, "\n")
    
    # Check compatibility first
    compatibility <- check_provider_compatibility("mapbox", provider)
    
    if (compatibility$compatible || length(compatibility$issues) == 0) {
      tryCatch({
        results[[provider]] <- update_provider(base_map, provider)
        cat("  Success: Switched to", provider, "\n")
      }, error = function(e) {
        cat("  Error:", e$message, "\n")
        results[[provider]] <- NULL
      })
    } else {
      cat("  Skipped: Compatibility issues -", compatibility$reason, "\n")
      results[[provider]] <- NULL
    }
  }
  
  return(results)
}

# Example 9: Custom Provider Configuration
#' This example shows advanced provider configuration during switching
advanced_configuration_example <- function() {
  
  # Create Leaflet map with custom tile provider
  map_leaflet <- mapdeck(
    provider = "leaflet",
    location = c(-74.006, 40.7128),
    zoom = 10
  )
  
  # Switch to OpenLayers with custom configuration
  map_openlayers <- update_provider(
    map_leaflet,
    "openlayers",
    config = list(
      projection = "EPSG:3857",
      source_type = "OSM",
      attribution = "Custom attribution"
    )
  )
  
  return(map_openlayers)
}

# Example 10: Provider Switching in Shiny Applications
#' This example shows how to use provider switching in Shiny apps
shiny_provider_switching_example <- function() {
  
  # This would be used in a Shiny server function
  shiny_server_logic <- function(input, output, session) {
    
    # Reactive map
    map_reactive <- reactive({
      
      # Base map
      map <- mapdeck(
        provider = input$selected_provider,  # From UI selectInput
        token = "your_token",
        location = c(input$longitude, input$latitude),
        zoom = input$zoom
      )
      
      # Add layers based on data
      if (!is.null(input$data_file)) {
        map <- add_scatterplot(
          map,
          data = input$data_file,
          lon = "longitude",
          lat = "latitude"
        )
      }
      
      return(map)
    })
    
    # Observer for provider changes
    observeEvent(input$switch_provider, {
      
      current_map <- map_reactive()
      
      # Switch provider while preserving layers
      new_map <- update_provider(
        current_map,
        input$new_provider,
        preserve_layers = TRUE,
        transform_coordinates = TRUE
      )
      
      # Update the map output
      output$map <- renderMapdeck({
        new_map
      })
    })
  }
  
  cat("Shiny server logic example created\n")
  cat("Use this pattern in your Shiny applications\n")
  
  return(shiny_server_logic)
}

# Helper function to run all examples
run_all_examples <- function() {
  
  cat("Running Provider Switching Examples\n")
  cat("===================================\n\n")
  
  examples <- list(
    "Basic Provider Switching" = basic_provider_switching,
    "Provider Switching with Configuration" = provider_switching_with_config,
    "Checking Provider Compatibility" = check_compatibility_example,
    "Error Handling" = error_handling_example,
    "Coordinate Transformation" = coordinate_transformation_example,
    "Layer Preservation" = layer_preservation_example,
    "Style Mapping" = style_mapping_example,
    "Batch Provider Switching" = batch_provider_switching,
    "Advanced Configuration" = advanced_configuration_example,
    "Shiny Integration" = shiny_provider_switching_example
  )
  
  results <- list()
  
  for (name in names(examples)) {
    cat("Running:", name, "\n")
    cat(paste(rep("-", nchar(name) + 9), collapse = ""), "\n")
    
    tryCatch({
      results[[name]] <- examples[[name]]()
      cat("✓ Completed successfully\n\n")
    }, error = function(e) {
      cat("✗ Error:", e$message, "\n\n")
      results[[name]] <- NULL
    })
  }
  
  return(results)
}

# Print usage information
cat("Provider Switching Examples Loaded\n")
cat("==================================\n")
cat("Available functions:\n")
cat("- basic_provider_switching()\n")
cat("- provider_switching_with_config()\n")
cat("- check_compatibility_example()\n")
cat("- error_handling_example()\n")
cat("- coordinate_transformation_example()\n")
cat("- layer_preservation_example()\n")
cat("- style_mapping_example()\n")
cat("- batch_provider_switching()\n")
cat("- advanced_configuration_example()\n")
cat("- shiny_provider_switching_example()\n")
cat("- run_all_examples()\n")
cat("\nTo run all examples: run_all_examples()\n")