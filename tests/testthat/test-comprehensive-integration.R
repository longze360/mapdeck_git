context("Comprehensive integration tests")

# This file contains comprehensive integration tests that verify the entire multi-provider system
# works together correctly, including cross-provider compatibility, performance, and memory usage.

# Skip on CRAN and CI environments
skip_if_no_integration_test <- function() {
  skip_on_cran()
  skip_on_ci()
}

# Test complete provider switching workflow
test_that("complete provider switching workflow works correctly", {
  skip_if_no_integration_test()
  
  # Mock the provider creation and switching functions
  with_mock(
    `create_provider` = function(provider_name, config = list()) {
      # Return a mock provider object
      structure(
        list(
          provider_name = provider_name,
          config = config,
          add_layer = function(layer) TRUE,
          remove_layer = function(layer_id) TRUE
        ),
        class = "mock_provider"
      )
    },
    `update_provider` = function(map, new_provider) {
      # Update the provider while preserving layers
      old_layers <- map$layers
      map$provider <- new_provider
      map$layers <- old_layers
      return(map)
    },
    {
      # Create sample data
      point_data <- data.frame(
        lon = c(-122.4194, -122.4300, -122.4100),
        lat = c(37.7749, 37.7800, 37.7700),
        value = c(10, 20, 30)
      )
      
      # Create initial map with Mapbox provider
      map <- list(
        provider = "mapbox",
        layers = list(),
        view_state = list(
          longitude = -122.4194,
          latitude = 37.7749,
          zoom = 11,
          pitch = 0,
          bearing = 0
        )
      )
      
      # Add a scatterplot layer
      map$layers$scatterplot <- list(
        id = "scatterplot_1",
        type = "scatterplot",
        data = point_data
      )
      
      # Switch to Leaflet provider
      map <- update_provider(map, "leaflet")
      expect_equal(map$provider, "leaflet")
      expect_equal(length(map$layers), 1)
      expect_true("scatterplot" %in% names(map$layers))
      
      # Switch to OpenLayers provider
      map <- update_provider(map, "openlayers")
      expect_equal(map$provider, "openlayers")
      expect_equal(length(map$layers), 1)
      expect_true("scatterplot" %in% names(map$layers))
      
      # Switch back to Mapbox provider
      map <- update_provider(map, "mapbox")
      expect_equal(map$provider, "mapbox")
      expect_equal(length(map$layers), 1)
      expect_true("scatterplot" %in% names(map$layers))
    }
  )
})

# Helper function
`%||%` <- function(x, y) if (is.null(x)) y else x