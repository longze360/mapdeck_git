# OpenLayers Provider Examples
# 
# This file demonstrates how to use the OpenLayers provider with mapdeck,
# including various data sources, projections, and integration with deck.gl layers.

library(mapdeck)
library(sf)

# ============================================================================
# BASIC OPENLAYERS SETUP
# ============================================================================

# OpenLayers doesn't require authentication tokens by default
# Create basic OpenLayers map
openlayers_map <- mapdeck(
  provider = "openlayers",
  location = c(2.3522, 48.8566),  # Paris
  zoom = 12
)
openlayers_map

# OpenLayers with custom configuration
# Note: config parameter is for future use when provider system is fully implemented
custom_ol_map <- mapdeck(
  provider = "openlayers",
  location = c(0, 0),
  zoom = 2
  # config = list(
  #   projection = "EPSG:3857",  # Web Mercator (default)
  #   source_type = "OSM",       # OpenStreetMap tiles
  #   attribution = "© OpenStreetMap contributors"
  # )
)
custom_ol_map

# ============================================================================
# OPENLAYERS DATA SOURCES
# ============================================================================

# Get available OpenLayers sources
ol_sources <- get_available_styles("openlayers")
print(ol_sources)

# Use different tile sources
osm_map <- mapdeck(
  provider = "openlayers",
  style = "osm",  # OpenStreetMap
  location = c(-74.006, 40.7128),  # New York
  zoom = 12
)
osm_map

# Stamen tile sources
stamen_terrain <- mapdeck(
  provider = "openlayers",
  style = "stamen_terrain",
  location = c(-122.4194, 37.7749),  # San Francisco
  zoom = 10
)
stamen_terrain

stamen_watercolor <- mapdeck(
  provider = "openlayers",
  style = "stamen_watercolor",
  location = c(2.3522, 48.8566),  # Paris
  zoom = 12
)
stamen_watercolor

# CartoDB sources
cartodb_light <- mapdeck(
  provider = "openlayers",
  style = "cartodb_light",
  location = c(144.9631, -37.8136),  # Melbourne
  zoom = 10
)
cartodb_light

cartodb_dark <- mapdeck(
  provider = "openlayers",
  style = "cartodb_dark",
  location = c(151.2093, -33.8688),  # Sydney
  zoom = 11
)
cartodb_dark

# ============================================================================
# PROJECTION SUPPORT
# ============================================================================

# OpenLayers supports multiple projections
# Web Mercator (EPSG:3857) - default
web_mercator <- mapdeck(
  provider = "openlayers",
  location = c(0, 0),
  zoom = 2,
  config = list(projection = "EPSG:3857")
)

# Geographic (EPSG:4326) - for global datasets
geographic <- mapdeck(
  provider = "openlayers",
  location = c(0, 0),
  zoom = 2,
  config = list(projection = "EPSG:4326")
)

# Custom projection example (UTM Zone 33N for Europe)
if (FALSE) {  # Advanced usage
  utm_europe <- mapdeck(
    provider = "openlayers",
    location = c(10, 50),  # Central Europe
    zoom = 6,
    config = list(
      projection = "EPSG:32633",  # UTM Zone 33N
      extent = c(166021.44, 0.00, 833978.56, 9329005.18)
    )
  )
}

# ============================================================================
# DATA VISUALIZATION WITH OPENLAYERS
# ============================================================================

# Load sample data
data(capitals, package = "mapdeck")
data(melbourne, package = "mapdeck")

# Scatterplot layer on OpenLayers (example - requires compiled package)
if (FALSE) {
  scatter_ol <- mapdeck(
    provider = "openlayers",
    style = "osm",
    location = c(0, 20),
    zoom = 2
  ) %>%
    add_scatterplot(
      data = capitals,
      lon = "lon",
      lat = "lat",
      radius = 100000,
      fill_colour = "country",
      tooltip = c("capital_city", "country", "population")
    )
  scatter_ol
}

# Polygon layer with Melbourne data (example - requires compiled package)
if (FALSE) {
  polygon_ol <- mapdeck(
    provider = "openlayers",
    style = "cartodb_light",
    location = c(144.9631, -37.8136),
    zoom = 10
  ) %>%
    add_polygon(
      data = melbourne,
      fill_colour = "SA2_NAME",
      fill_opacity = 0.6,
      stroke_colour = "#333333",
      stroke_width = 15
    )
  polygon_ol
}

# Line layer showing connections (example - requires compiled package)
if (FALSE) {
  line_ol <- mapdeck(
    provider = "openlayers",
    style = "cartodb_dark",
    location = c(0, 30),
    zoom = 2
  ) %>%
    add_line(
      data = capitals,
      origin = c("lon", "lat"),
      destination = c("lon", "lat"),
      stroke_colour = "country",
      stroke_width = 2
    )
  line_ol
}

# ============================================================================
# ADVANCED OPENLAYERS FEATURES
# ============================================================================

# Multiple layer sources
multi_source_map <- mapdeck(
  provider = "openlayers",
  style = "osm",
  location = c(2.3522, 48.8566),
  zoom = 10,
  config = list(
    additional_sources = list(
      satellite = list(
        type = "XYZ",
        url = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
      )
    )
  )
)

# Vector data sources (advanced)
if (FALSE) {  # Example with vector tiles
  vector_map <- mapdeck(
    provider = "openlayers",
    location = c(-74.006, 40.7128),
    zoom = 12,
    config = list(
      source_type = "VectorTile",
      source_url = "https://example.com/tiles/{z}/{x}/{y}.pbf",
      style_function = "custom_vector_style"
    )
  )
}

# WMS (Web Map Service) integration
wms_map <- mapdeck(
  provider = "openlayers",
  location = c(10, 50),
  zoom = 6,
  config = list(
    source_type = "ImageWMS",
    url = "https://ows.terrestris.de/osm/service",
    params = list(
      LAYERS = "OSM-Overlay-WMS",
      FORMAT = "image/png"
    )
  )
)

# ============================================================================
# OPENLAYERS LAYER COMPATIBILITY
# ============================================================================

# Test various deck.gl layers with OpenLayers (example - requires compiled package)
if (FALSE) {
  comprehensive_ol <- mapdeck(
    provider = "openlayers",
    style = "cartodb_light",
    location = c(144.9631, -37.8136),
    zoom = 8
  ) %>%
    # Scatterplot layer
    add_scatterplot(
      data = capitals,
      lon = "lon",
      lat = "lat",
      radius = 50000,
      fill_colour = "#FF6B6B",
      layer_id = "points"
    ) %>%
    # Polygon layer
    add_polygon(
      data = melbourne,
      fill_colour = "SA2_NAME",
      fill_opacity = 0.3,
      stroke_colour = "#4ECDC4",
      stroke_width = 10,
      layer_id = "polygons"
    ) %>%
    # Text layer
    add_text(
      data = capitals,
      lon = "lon",
      lat = "lat",
      text = "capital_city",
      size = 12,
      colour = "#2C3E50",
      layer_id = "labels"
    )
  comprehensive_ol
}

# Heatmap layer (example - requires compiled package)
if (FALSE) {
  heatmap_ol <- mapdeck(
    provider = "openlayers",
    style = "cartodb_dark",
    location = c(144.9631, -37.8136),
    zoom = 8
  ) %>%
    add_heatmap(
      data = melbourne,
      weight = 1,
      radius_pixels = 60,
      intensity = 1,
      threshold = 0.05
    )
  heatmap_ol
}

# ============================================================================
# OPENLAYERS PERFORMANCE OPTIMIZATION
# ============================================================================

# For large datasets, use appropriate aggregation
large_data <- data.frame(
  lon = runif(8000, 144.5, 145.5),
  lat = runif(8000, -38.2, -37.4),
  value = rnorm(8000, 100, 20),
  category = sample(c("A", "B", "C", "D"), 8000, replace = TRUE)
)

# Screengrid aggregation (example - requires compiled package)
if (FALSE) {
  screengrid_ol <- mapdeck(
    provider = "openlayers",
    style = "cartodb_light",
    location = c(145, -37.8),
    zoom = 9
  ) %>%
    add_screengrid(
      data = large_data,
      lon = "lon",
      lat = "lat",
      weight = "value",
      cell_size = 40,
      opacity = 0.8,
      colour_range = viridisLite::viridis(6)
    )
  screengrid_ol
}

# Hexagon aggregation (example - requires compiled package)
if (FALSE) {
  hexagon_ol <- mapdeck(
    provider = "openlayers",
    style = "osm",
    location = c(145, -37.8),
    zoom = 9
  ) %>%
    add_hexagon(
      data = large_data,
      lon = "lon",
      lat = "lat",
      colour_range = viridisLite::plasma(8),
      elevation_scale = 50,
      radius = 500
    )
  hexagon_ol
}

# Grid aggregation (example - requires compiled package)
if (FALSE) {
  grid_ol <- mapdeck(
    provider = "openlayers",
    style = "stamen_terrain",
    location = c(145, -37.8),
    zoom = 9
  ) %>%
    add_grid(
      data = large_data,
      lon = "lon",
      lat = "lat",
      cell_size = 1000,
      elevation_scale = 30,
      colour_range = c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", 
                       "#FB6A4A", "#EF3B2C", "#CB181D", "#99000D")
    )
  grid_ol
}

# ============================================================================
# CUSTOM STYLING AND THEMES
# ============================================================================

# Apply themes to OpenLayers maps (example - requires compiled package)
if (FALSE) {
  light_themed_ol <- mapdeck(provider = "openlayers", style = "cartodb_light") %>%
    apply_map_theme("light")

  dark_themed_ol <- mapdeck(provider = "openlayers", style = "cartodb_dark") %>%
    apply_map_theme("dark")
}

# Validate OpenLayers styles
validate_map_style("osm", "openlayers")
validate_map_style("stamen_watercolor", "openlayers")
validate_map_style("cartodb_light", "openlayers")

# Get detailed style validation
ol_validation <- validate_map_style("stamen_terrain", "openlayers", detailed = TRUE)
print(ol_validation)

# ============================================================================
# OPENLAYERS PROVIDER SWITCHING
# ============================================================================

# Start with Mapbox map
mapbox_original <- mapdeck(
  provider = "mapbox",
  token = get_access_token("mapbox"),
  style = "streets",
  location = c(2.3522, 48.8566),
  zoom = 12
) %>%
  add_scatterplot(
    data = capitals,
    lon = "lon",
    lat = "lat",
    radius = 50000,
    fill_colour = "country"
  )

# Switch to OpenLayers (preserves layers)
ol_switched <- update_provider(mapbox_original, "openlayers")

# Switch from Leaflet to OpenLayers
leaflet_original <- mapdeck(
  provider = "leaflet",
  style = "streets",
  location = c(2.3522, 48.8566),
  zoom = 12
) %>%
  add_polygon(
    data = melbourne,
    fill_colour = "SA2_NAME",
    fill_opacity = 0.5
  )

ol_from_leaflet <- update_provider(leaflet_original, "openlayers")

# ============================================================================
# OPENLAYERS INTEGRATION WITH SHINY
# ============================================================================

# Example Shiny app for OpenLayers provider (not run)
if (FALSE) {
  library(shiny)
  
  ui <- fluidPage(
    titlePanel("OpenLayers Provider Example"),
    sidebarLayout(
      sidebarPanel(
        selectInput("source", "Tile Source:",
                   choices = get_available_styles("openlayers"),
                   selected = "osm"),
        selectInput("projection", "Projection:",
                   choices = c("EPSG:3857", "EPSG:4326"),
                   selected = "EPSG:3857"),
        sliderInput("zoom", "Zoom Level:", min = 1, max = 18, value = 10),
        checkboxInput("show_attribution", "Show Attribution", value = TRUE)
      ),
      mainPanel(
        mapdeckOutput("map", height = "600px")
      )
    )
  )
  
  server <- function(input, output) {
    output$map <- renderMapdeck({
      config <- list(
        projection = input$projection,
        source_type = "OSM"
      )
      
      if (input$show_attribution) {
        config$attribution <- "© OpenStreetMap contributors"
      }
      
      map <- mapdeck(
        provider = "openlayers",
        style = input$source,
        zoom = input$zoom,
        location = c(2.3522, 48.8566),
        config = config
      ) %>%
        add_scatterplot(
          data = capitals,
          lon = "lon",
          lat = "lat",
          radius = 50000,
          fill_colour = "country"
        )
      
      map
    })
  }
  
  shinyApp(ui = ui, server = server)
}

# ============================================================================
# GEOSPATIAL DATA INTEGRATION
# ============================================================================

# OpenLayers works well with various geospatial data formats
if (require(sf, quietly = TRUE)) {
  
  # Create sample SF object
  cities_sf <- st_as_sf(
    capitals,
    coords = c("lon", "lat"),
    crs = 4326
  )
  
  # Use SF data with OpenLayers
  sf_map <- mapdeck(
    provider = "openlayers",
    style = "osm",
    location = c(0, 20),
    zoom = 2
  ) %>%
    add_sf(
      data = cities_sf,
      fill_colour = "country",
      radius = 100000
    )
  sf_map
}

# GeoJSON integration
if (FALSE) {  # Example with GeoJSON data
  geojson_map <- mapdeck(
    provider = "openlayers",
    style = "cartodb_light"
  ) %>%
    add_geojson(
      data = "path/to/your/data.geojson",
      fill_colour = "property_name",
      stroke_colour = "#FFFFFF"
    )
}

# ============================================================================
# TROUBLESHOOTING OPENLAYERS ISSUES
# ============================================================================

# Check OpenLayers provider availability
providers <- list_available_providers()
"openlayers" %in% providers

# Get OpenLayers capabilities
ol_capabilities <- get_provider_capabilities("openlayers")
print(ol_capabilities)

# OpenLayers doesn't require authentication
ol_auth_required <- get_token_store()$requires_authentication("openlayers")
print(paste("Authentication required:", ol_auth_required))

# Check provider configuration
ol_config <- get_provider_config("openlayers")
print(ol_config)

# Test provider creation
tryCatch({
  ol_provider <- create_provider("openlayers", list())
  print("OpenLayers provider created successfully")
}, error = function(e) {
  print(paste("Provider creation failed:", e$message))
})

# Debug projection issues
test_projection <- function(proj_code) {
  tryCatch({
    map <- mapdeck(
      provider = "openlayers",
      config = list(projection = proj_code)
    )
    print(paste("Projection", proj_code, "works"))
  }, error = function(e) {
    print(paste("Projection", proj_code, "failed:", e$message))
  })
}

# Test common projections
test_projection("EPSG:3857")  # Web Mercator
test_projection("EPSG:4326")  # Geographic

# ============================================================================
# OPENLAYERS BEST PRACTICES
# ============================================================================

cat("OpenLayers Provider Best Practices:\n")
cat("===================================\n")
cat("1. Choose appropriate tile sources for your use case\n")
cat("   - OSM: General purpose, free\n")
cat("   - CartoDB: Clean, minimal design\n")
cat("   - Stamen: Artistic and terrain styles\n")
cat("2. Consider projection requirements for your data\n")
cat("3. Use aggregation layers for large datasets\n")
cat("4. Leverage OpenLayers' vector data capabilities\n")
cat("5. Test different tile sources for performance\n")
cat("6. Consider caching for production applications\n")
cat("7. Use appropriate attribution for tile sources\n")

# Performance comparison function
compare_ol_performance <- function(data, layer_type = "scatterplot") {
  
  sources <- c("osm", "cartodb_light", "stamen_terrain")
  results <- list()
  
  for (source in sources) {
    cat("Testing", source, "performance...\n")
    
    start_time <- Sys.time()
    
    map <- mapdeck(
      provider = "openlayers",
      style = source,
      location = c(0, 0),
      zoom = 2
    )
    
    if (layer_type == "scatterplot") {
      map <- map %>%
        add_scatterplot(
          data = data,
          lon = "lon",
          lat = "lat",
          radius = 50000,
          fill_colour = "country"
        )
    }
    
    end_time <- Sys.time()
    results[[source]] <- as.numeric(end_time - start_time)
    
    cat("  Time:", round(results[[source]], 3), "seconds\n")
  }
  
  return(results)
}

# Run performance comparison
if (exists("capitals")) {
  performance_results <- compare_ol_performance(capitals)
  cat("\nPerformance Summary:\n")
  print(performance_results)
}

# ============================================================================
# ADVANCED CONFIGURATION EXAMPLES
# ============================================================================

# Custom tile server configuration
custom_tile_server <- mapdeck(
  provider = "openlayers",
  location = c(0, 0),
  zoom = 2,
  config = list(
    source_type = "XYZ",
    url = "https://{a-c}.tile.openstreetmap.org/{z}/{x}/{y}.png",
    attribution = "© OpenStreetMap contributors",
    max_zoom = 19,
    min_zoom = 1
  )
)

# Multiple overlay sources
multi_overlay <- mapdeck(
  provider = "openlayers",
  location = c(-74.006, 40.7128),
  zoom = 10,
  config = list(
    base_source = "osm",
    overlay_sources = list(
      traffic = list(
        type = "XYZ",
        url = "https://example.com/traffic/{z}/{x}/{y}.png",
        opacity = 0.7
      ),
      weather = list(
        type = "XYZ",
        url = "https://example.com/weather/{z}/{x}/{y}.png",
        opacity = 0.5
      )
    )
  )
)

print("OpenLayers provider examples completed!")