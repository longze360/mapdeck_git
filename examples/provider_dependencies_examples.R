# Provider Dependencies Examples
# This file demonstrates how to use different map providers with mapdeck

library(mapdeck)

# Set tokens for different providers
# Note: Replace with your actual API keys
set_token(provider = "mapbox", token = "YOUR_MAPBOX_TOKEN")
set_token(provider = "gaode", token = "YOUR_GAODE_API_KEY")
set_token(provider = "baidu", token = "YOUR_BAIDU_API_KEY")

# Sample data
data <- data.frame(
  lon = c(-122.4194, -122.4300, -122.4100),
  lat = c(37.7749, 37.7800, 37.7700),
  value = c(10, 20, 30)
)

# Example 1: Mapbox provider (default)
mapbox_map <- mapdeck(
  provider = "mapbox",
  style = "mapbox://styles/mapbox/dark-v10",
  location = c(-122.4194, 37.7749),
  zoom = 11
) %>%
  add_scatterplot(
    data = data,
    lon = "lon",
    lat = "lat",
    radius = 100,
    fill_colour = "value",
    tooltip = "value"
  )

# Example 2: Leaflet provider
leaflet_map <- mapdeck(
  provider = "leaflet",
  style = "CartoDB.DarkMatter",
  location = c(-122.4194, 37.7749),
  zoom = 11
) %>%
  add_scatterplot(
    data = data,
    lon = "lon",
    lat = "lat",
    radius = 100,
    fill_colour = "value",
    tooltip = "value"
  )

# Example 3: OpenLayers provider
openlayers_map <- mapdeck(
  provider = "openlayers",
  style = "CartoDB.DarkMatter",
  location = c(-122.4194, 37.7749),
  zoom = 11
) %>%
  add_scatterplot(
    data = data,
    lon = "lon",
    lat = "lat",
    radius = 100,
    fill_colour = "value",
    tooltip = "value"
  )

# Example 4: Gaode provider (requires API key)
gaode_map <- mapdeck(
  provider = "gaode",
  style = "dark",
  location = c(116.397, 39.909),  # Beijing
  zoom = 11
) %>%
  add_scatterplot(
    data = data.frame(
      lon = c(116.397, 116.410, 116.380),
      lat = c(39.909, 39.920, 39.900),
      value = c(10, 20, 30)
    ),
    lon = "lon",
    lat = "lat",
    radius = 100,
    fill_colour = "value",
    tooltip = "value"
  )

# Example 5: Baidu provider (requires API key)
baidu_map <- mapdeck(
  provider = "baidu",
  style = "dark",
  location = c(116.397, 39.909),  # Beijing
  zoom = 11
) %>%
  add_scatterplot(
    data = data.frame(
      lon = c(116.397, 116.410, 116.380),
      lat = c(39.909, 39.920, 39.900),
      value = c(10, 20, 30)
    ),
    lon = "lon",
    lat = "lat",
    radius = 100,
    fill_colour = "value",
    tooltip = "value"
  )