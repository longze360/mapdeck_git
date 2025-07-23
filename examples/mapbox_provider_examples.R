# Mapbox Provider Examples
# 
# This file demonstrates how to use the Mapbox provider with mapdeck,
# including authentication, styling, and advanced features.

library(mapdeck)
library(sf)

# ============================================================================
# AUTHENTICATION AND SETUP
# ============================================================================

# Method 1: Set token globally (recommended)
set_token("pk.your_mapbox_token_here", provider = "mapbox")

# Method 2: Set token via environment variable
# Sys.setenv(MAPBOX_TOKEN = "pk.your_mapbox_token_here")

# Method 3: Pass token directly to mapdeck()
# map <- mapdeck(token = "pk.your_mapbox_token_here", provider = "mapbox")

# Verify token is set
mapdeck_tokens()

# ============================================================================
# BASIC MAPBOX MAPS
# ============================================================================

# Create basic Mapbox map (default provider)
basic_map <- mapdeck(
  provider = "mapbox",
  location = c(144.9631, -37.8136),  # Melbourne
  zoom = 10
)
basic_map

# Map with custom style
styled_map <- mapdeck(
  provider = "mapbox",
  style = "dark",
  location = c(-74.006, 40.7128),  # New York
  zoom = 12,
  pitch = 45,
  bearing = 30
)
styled_map

# ============================================================================
# MAPBOX STYLES
# ============================================================================

# Get available Mapbox styles
mapbox_styles <- get_available_styles("mapbox")
print(mapbox_styles)

# Use different Mapbox styles
streets_map <- mapdeck(provider = "mapbox", style = "streets")
satellite_map <- mapdeck(provider = "mapbox", style = "satellite")
outdoors_map <- mapdeck(provider = "mapbox", style = "outdoors")
navigation_map <- mapdeck(provider = "mapbox", style = "navigation_day")

# Use full Mapbox style URL
custom_style_map <- mapdeck(
  provider = "mapbox",
  style = "mapbox://styles/mapbox/satellite-streets-v11",
  location = c(2.3522, 48.8566),  # Paris
  zoom = 14
)

# ============================================================================
# DATA VISUALIZATION WITH MAPBOX
# ============================================================================

# Load sample data
data(capitals, package = "mapdeck")
data(melbourne, package = "mapdeck")

# Scatterplot layer
scatter_map <- mapdeck(provider = "mapbox", style = "light") %>%
  add_scatterplot(
    data = capitals,
    lon = "lon",
    lat = "lat",
    radius = 50000,
    fill_colour = "country",
    tooltip = c("capital_city", "country")
  )
scatter_map

# Polygon layer with Melbourne data
polygon_map <- mapdeck(
  provider = "mapbox",
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
polygon_map

# Arc layer showing connections
arc_map <- mapdeck(provider = "mapbox", style = "dark") %>%
  add_arc(
    data = capitals,
    origin = c("lon", "lat"),
    destination = c("lon", "lat"),
    stroke_from = "country",
    stroke_to = "country",
    stroke_width = 4
  )
arc_map

# ============================================================================
# ADVANCED MAPBOX FEATURES
# ============================================================================

# 3D visualization with pitch and bearing
map_3d <- mapdeck(
  provider = "mapbox",
  style = "satellite",
  location = c(-122.4194, 37.7749),  # San Francisco
  zoom = 15,
  pitch = 60,
  bearing = 45
) %>%
  add_column(
    data = capitals,
    lon = "lon",
    lat = "lat",
    elevation = "population",
    fill_colour = "country",
    radius = 10000
  )
map_3d

# Animated layers
animated_map <- mapdeck(provider = "mapbox", style = "dark") %>%
  add_animated_arc(
    data = capitals,
    origin = c("lon", "lat"),
    destination = c("lon", "lat"),
    stroke_from = "country",
    stroke_to = "country",
    animation_speed = 2000
  )
animated_map

# Multiple layers
multi_layer_map <- mapdeck(
  provider = "mapbox",
  style = "streets",
  location = c(144.9631, -37.8136),
  zoom = 10
) %>%
  add_polygon(
    data = melbourne,
    fill_colour = "SA2_NAME",
    fill_opacity = 0.5,
    layer_id = "polygons"
  ) %>%
  add_scatterplot(
    data = capitals,
    lon = "lon",
    lat = "lat",
    radius = 30000,
    fill_colour = "#FF0000",
    layer_id = "points"
  )
multi_layer_map

# ============================================================================
# MAPBOX PERFORMANCE OPTIMIZATION
# ============================================================================

# For large datasets, use aggregation layers
large_data <- data.frame(
  lon = runif(10000, 144.5, 145.5),
  lat = runif(10000, -38.2, -37.4),
  value = rnorm(10000, 100, 20)
)

# Hexagon aggregation for performance
hex_map <- mapdeck(
  provider = "mapbox",
  style = "light",
  location = c(145, -37.8),
  zoom = 9
) %>%
  add_hexagon(
    data = large_data,
    lon = "lon",
    lat = "lat",
    colour_range = viridisLite::viridis(6),
    elevation_scale = 100
  )
hex_map

# Grid aggregation
grid_map <- mapdeck(
  provider = "mapbox",
  style = "light",
  location = c(145, -37.8),
  zoom = 9
) %>%
  add_grid(
    data = large_data,
    lon = "lon",
    lat = "lat",
    cell_size = 1000,
    elevation_scale = 50
  )
grid_map

# ============================================================================
# MAPBOX CUSTOM STYLING
# ============================================================================

# Apply theme to Mapbox map
themed_map <- mapdeck(provider = "mapbox") %>%
  apply_map_theme("dark")

# Validate Mapbox styles
validate_map_style("streets", "mapbox")
validate_map_style("mapbox://styles/mapbox/outdoors-v11", "mapbox")

# Get detailed style information
style_info <- validate_map_style("satellite", "mapbox", detailed = TRUE)
print(style_info)

# ============================================================================
# MAPBOX INTEGRATION WITH SHINY
# ============================================================================

# Example Shiny app structure (not run)
if (FALSE) {
  library(shiny)
  
  ui <- fluidPage(
    titlePanel("Mapbox Provider Example"),
    sidebarLayout(
      sidebarPanel(
        selectInput("style", "Map Style:",
                   choices = get_available_styles("mapbox"),
                   selected = "streets"),
        sliderInput("zoom", "Zoom Level:", min = 1, max = 20, value = 10),
        sliderInput("pitch", "Pitch:", min = 0, max = 60, value = 0)
      ),
      mainPanel(
        mapdeckOutput("map", height = "600px")
      )
    )
  )
  
  server <- function(input, output) {
    output$map <- renderMapdeck({
      mapdeck(
        provider = "mapbox",
        style = input$style,
        zoom = input$zoom,
        pitch = input$pitch,
        location = c(144.9631, -37.8136)
      ) %>%
        add_scatterplot(
          data = capitals,
          lon = "lon",
          lat = "lat",
          radius = 50000,
          fill_colour = "country"
        )
    })
  }
  
  shinyApp(ui = ui, server = server)
}

# ============================================================================
# TROUBLESHOOTING MAPBOX ISSUES
# ============================================================================

# Check if Mapbox provider is available
providers <- list_available_providers()
"mapbox" %in% providers

# Get Mapbox provider capabilities
mapbox_capabilities <- get_provider_capabilities("mapbox")
print(mapbox_capabilities)

# Validate token
token_valid <- get_token_store()$validate_token("mapbox")
print(paste("Token valid:", token_valid))

# Check provider configuration
mapbox_config <- get_provider_config("mapbox")
print(mapbox_config)

# Debug provider creation
tryCatch({
  provider <- create_provider("mapbox", list(token = get_access_token("mapbox")))
  print("Provider created successfully")
}, error = function(e) {
  print(paste("Provider creation failed:", e$message))
})

print("Mapbox provider examples completed!")