# Leaflet Provider Examples
#
# This file demonstrates how to use the Leaflet provider with mapdeck,
# including tile providers, styling, and integration with deck.gl layers.

library(mapdeck)
library(sf)

# ============================================================================
# BASIC LEAFLET SETUP
# ============================================================================

# Leaflet doesn't require authentication tokens by default
# Create basic Leaflet map
leaflet_map <- mapdeck(
  provider = "leaflet",
  location = c(144.9631, -37.8136), # Melbourne
  zoom = 10
)
leaflet_map

# Note: Leaflet doesn't support pitch and bearing
# These parameters will be automatically set to 0 with warnings
leaflet_2d <- mapdeck(
  provider = "leaflet",
  location = c(-74.006, 40.7128), # New York
  zoom = 12,
  pitch = 0, # Must be 0 for Leaflet
  bearing = 0 # Must be 0 for Leaflet
)
leaflet_2d

# ============================================================================
# LEAFLET TILE PROVIDERS
# ============================================================================

# Get available Leaflet styles (tile providers)
leaflet_styles <- get_available_styles("leaflet")
print(leaflet_styles)

# Use different tile providers
osm_map <- mapdeck(provider = "leaflet", style = "streets") # OpenStreetMap
cartodb_light <- mapdeck(provider = "leaflet", style = "light") # CartoDB Positron
cartodb_dark <- mapdeck(provider = "leaflet", style = "dark") # CartoDB DarkMatter
satellite_map <- mapdeck(provider = "leaflet", style = "satellite") # Esri WorldImagery
terrain_map <- mapdeck(provider = "leaflet", style = "terrain") # OpenTopoMap

# Use specific tile provider names
watercolor_map <- mapdeck(
  provider = "leaflet",
  style = "watercolor", # Stamen Watercolor
  location = c(2.3522, 48.8566), # Paris
  zoom = 12
)
watercolor_map

toner_map <- mapdeck(
  provider = "leaflet",
  style = "toner", # Stamen Toner
  location = c(-122.4194, 37.7749), # San Francisco
  zoom = 13
)
toner_map

# ============================================================================
# CUSTOM TILE PROVIDERS
# ============================================================================

# You can also specify custom tile URLs (advanced usage)
# This would require extending the provider system
custom_tile_map <- mapdeck(
  provider = "leaflet",
  style = "OpenStreetMap", # Fallback to standard OSM
  location = c(0, 0),
  zoom = 2
)

# ============================================================================
# DATA VISUALIZATION WITH LEAFLET
# ============================================================================

# Load sample data
data(capitals, package = "mapdeck")
data(melbourne, package = "mapdeck")

# Note: Layer examples are commented out due to C++ compilation requirements
# These will work once the package is properly compiled and installed

# Scatterplot layer on Leaflet (example - requires compiled package)
if (FALSE) {
  scatter_leaflet <- mapdeck(provider = "leaflet", style = "light") %>%
    add_scatterplot(
      data = capitals,
      lon = "lon",
      lat = "lat",
      radius = 50000,
      fill_colour = "country",
      tooltip = c("capital_city", "country")
    )
  scatter_leaflet
}

# Polygon layer with Melbourne data (example - requires compiled package)
if (FALSE) {
  polygon_leaflet <- mapdeck(
    provider = "leaflet",
    style = "streets",
    location = c(144.9631, -37.8136),
    zoom = 10
  ) %>%
    add_polygon(
      data = melbourne,
      fill_colour = "SA2_NAME",
      fill_opacity = 0.7,
      stroke_colour = "#FFFFFF",
      stroke_width = 20
    )
  polygon_leaflet
}

# Line layer (example - requires compiled package)
if (FALSE) {
  line_leaflet <- mapdeck(provider = "leaflet", style = "outdoors") %>%
    add_line(
      data = capitals,
      origin = c("lon", "lat"),
      destination = c("lon", "lat"),
      stroke_colour = "country",
      stroke_width = 3
    )
  line_leaflet
}

# ============================================================================
# LEAFLET LIMITATIONS AND WORKAROUNDS
# ============================================================================

# Leaflet has some limitations compared to Mapbox:
# 1. No 3D support (pitch/bearing)
# 2. Limited elevation/column layers
# 3. No custom vector styles

# For 3D-like effects, use alternative visualizations (example - requires compiled package)
if (FALSE) {
  # Instead of column layer with elevation, use scaled circles
  pseudo_3d <- mapdeck(
    provider = "leaflet",
    style = "dark",
    location = c(144.9631, -37.8136),
    zoom = 10
  ) %>%
    add_scatterplot(
      data = capitals,
      lon = "lon",
      lat = "lat",
      radius = "population", # Scale by population for pseudo-3D effect
      fill_colour = "country",
      radius_min_pixels = 5,
      radius_max_pixels = 50
    )
  pseudo_3d
}

# Use heatmap for density visualization (example - requires compiled package)
if (FALSE) {
  heatmap_leaflet <- mapdeck(
    provider = "leaflet",
    style = "light",
    location = c(144.9631, -37.8136),
    zoom = 8
  ) %>%
    add_heatmap(
      data = melbourne,
      weight = 1,
      radius_pixels = 60
    )
  heatmap_leaflet
}

# ============================================================================
# MULTIPLE LAYERS WITH LEAFLET
# ============================================================================

# Combine multiple compatible layers (example - requires compiled package)
if (FALSE) {
  multi_layer_leaflet <- mapdeck(
    provider = "leaflet",
    style = "streets",
    location = c(144.9631, -37.8136),
    zoom = 10
  ) %>%
    add_polygon(
      data = melbourne,
      fill_colour = "SA2_NAME",
      fill_opacity = 0.3,
      stroke_colour = "#333333",
      stroke_width = 10,
      layer_id = "boundaries"
    ) %>%
    add_scatterplot(
      data = capitals,
      lon = "lon",
      lat = "lat",
      radius = 30000,
      fill_colour = "#FF4444",
      layer_id = "cities"
    ) %>%
    add_text(
      data = capitals,
      lon = "lon",
      lat = "lat",
      text = "capital_city",
      size = 12,
      colour = "#000000",
      layer_id = "labels"
    )
  multi_layer_leaflet
}

# ============================================================================
# LEAFLET PERFORMANCE CONSIDERATIONS
# ============================================================================

# For large datasets, Leaflet may perform differently than Mapbox
# Use appropriate aggregation methods

large_data <- data.frame(
  lon = runif(5000, 144.5, 145.5), # Smaller dataset for Leaflet
  lat = runif(5000, -38.2, -37.4),
  value = rnorm(5000, 100, 20)
)

# Screengrid aggregation works well with Leaflet (example - requires compiled package)
if (FALSE) {
  screengrid_leaflet <- mapdeck(
    provider = "leaflet",
    style = "light",
    location = c(145, -37.8),
    zoom = 9
  ) %>%
    add_screengrid(
      data = large_data,
      lon = "lon",
      lat = "lat",
      weight = "value",
      cell_size = 50,
      opacity = 0.8
    )
  screengrid_leaflet
}

# ============================================================================
# LEAFLET STYLING AND THEMES
# ============================================================================

# Apply themes to Leaflet maps
light_themed <- mapdeck(provider = "leaflet") %>%
  apply_map_theme("light")

dark_themed <- mapdeck(provider = "leaflet") %>%
  apply_map_theme("dark")

# Validate Leaflet styles
validate_map_style("streets", "leaflet")
validate_map_style("OpenStreetMap", "leaflet")
validate_map_style("CartoDB.Positron", "leaflet")

# Get detailed style validation
leaflet_validation <- validate_map_style("watercolor", "leaflet", detailed = TRUE)
print(leaflet_validation)

# ============================================================================
# LEAFLET PROVIDER SWITCHING
# ============================================================================

# Start with Mapbox map
mapbox_map <- mapdeck(
  provider = "mapbox",
  token = get_access_token("mapbox"),
  style = "streets",
  location = c(144.9631, -37.8136),
  zoom = 10
) %>%
  add_scatterplot(
    data = capitals,
    lon = "lon",
    lat = "lat",
    radius = 50000,
    fill_colour = "country"
  )

# Switch to Leaflet (preserves layers, adjusts 3D settings)
leaflet_switched <- update_provider(mapbox_map, "leaflet")

# ============================================================================
# LEAFLET INTEGRATION WITH SHINY
# ============================================================================

# Example Shiny app for Leaflet provider (not run)
if (FALSE) {
  library(shiny)

  ui <- fluidPage(
    titlePanel("Leaflet Provider Example"),
    sidebarLayout(
      sidebarPanel(
        selectInput("tile_provider", "Tile Provider:",
          choices = get_available_styles("leaflet"),
          selected = "streets"
        ),
        sliderInput("zoom", "Zoom Level:", min = 1, max = 18, value = 10),
        checkboxInput("show_labels", "Show City Labels", value = TRUE)
      ),
      mainPanel(
        mapdeckOutput("map", height = "600px")
      )
    )
  )

  server <- function(input, output) {
    output$map <- renderMapdeck({
      map <- mapdeck(
        provider = "leaflet",
        style = input$tile_provider,
        zoom = input$zoom,
        location = c(144.9631, -37.8136)
      ) %>%
        add_scatterplot(
          data = capitals,
          lon = "lon",
          lat = "lat",
          radius = 50000,
          fill_colour = "country"
        )

      if (input$show_labels) {
        map <- map %>%
          add_text(
            data = capitals,
            lon = "lon",
            lat = "lat",
            text = "capital_city",
            size = 10
          )
      }

      map
    })
  }

  shinyApp(ui = ui, server = server)
}

# ============================================================================
# TROUBLESHOOTING LEAFLET ISSUES
# ============================================================================

# Check Leaflet provider availability
providers <- list_available_providers()
"leaflet" %in% providers

# Get Leaflet capabilities
leaflet_capabilities <- get_provider_capabilities("leaflet")
print(leaflet_capabilities)

# Leaflet doesn't require authentication
leaflet_auth_required <- get_token_store()$requires_authentication("leaflet")
print(paste("Authentication required:", leaflet_auth_required))

# Check provider configuration
leaflet_config <- get_provider_config("leaflet")
print(leaflet_config)

# Test provider creation
tryCatch(
  {
    leaflet_provider <- create_provider("leaflet", list())
    print("Leaflet provider created successfully")
  },
  error = function(e) {
    print(paste("Provider creation failed:", e$message))
  }
)

# ============================================================================
# LEAFLET BEST PRACTICES
# ============================================================================

# 1. Use appropriate tile providers for your use case
#    - OpenStreetMap: General purpose, free
#    - CartoDB: Clean, minimal design
#    - Stamen: Artistic styles
#    - Esri: Satellite imagery

# 2. Consider performance with large datasets
#    - Use aggregation layers (screengrid, heatmap)
#    - Limit point density for scatterplot layers

# 3. Remember Leaflet limitations
#    - No 3D visualization (pitch/bearing always 0)
#    - Limited elevation effects
#    - Tile-based styling only

# 4. Leverage Leaflet strengths
#    - Wide variety of free tile providers
#    - Good performance for 2D visualizations
#    - Familiar interface for web mapping users

print("Leaflet provider examples completed!")
