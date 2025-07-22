---
title: Mapdeck Package Specifics
inclusion: always
---

# Mapdeck Package Specifics

## Package Purpose
- Mapdeck provides interactive maps using Mapbox GL JS and Deck.gl
- Allows visualization of large geospatial datasets using WebGL
- Supports various layer types for different visualization needs

## API Structure
- Main function is `mapdeck()` to initialize the map
- Layer functions follow the pattern `add_*()`
- Clear functions follow the pattern `clear_*()`
- Each layer has specific parameters relevant to its visualization type

## Authentication
- Requires a Mapbox access token for full functionality
- Token can be set via:
  - Direct parameter: `mapdeck(token = "your_token")`
  - Global setting: `set_token("your_token")`
  - Environment variables: `MAPBOX_TOKEN`, `MAPBOX_KEY`, etc.

## Layer Types
- Supports numerous layer types including:
  - `add_arc()` - Arc connections between points
  - `add_line()` - Line segments
  - `add_polygon()` - Filled polygons
  - `add_scatterplot()` - Point data
  - `add_heatmap()` - Heatmap visualization
  - `add_grid()` - Grid aggregation
  - `add_hexagon()` - Hexagonal aggregation
  - And many more specialized layers

## Data Handling
- Works with various data formats:
  - data.frames with coordinate columns
  - sf objects (Simple Features)
  - sfencoded objects (encoded geometries)
  - geojson data

## Styling
- Map styles can be set using `mapdeck_style()`
- Supports standard Mapbox styles and custom styles
- Layer styling is controlled by parameters in each `add_*()` function

## Interactivity
- Supports tooltips via the `tooltip` parameter
- Supports click events and highlighting
- Can be used in Shiny applications with `mapdeckOutput()` and `renderMapdeck()`

## Performance Considerations
- Designed for large datasets using WebGL
- Consider using encoded geometries for better performance
- For very large datasets, consider aggregation layers (grid, hexagon)