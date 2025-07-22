---
title: Spatial Data Handling
inclusion: fileMatch
fileMatchPattern: '*.R'
---

# Spatial Data Handling in Mapdeck

## Supported Data Formats
- Simple Features (sf) objects
- Data frames with coordinate columns
- Encoded geometries (sfencoded)
- GeoJSON (as character strings or parsed objects)
- Mesh3d and quadmesh objects for 3D visualizations

## Coordinate Systems
- All coordinates should be in EPSG:4326 (WGS 84) coordinate system
- The package does not handle coordinate transformations internally
- Use `sf::st_transform()` to convert to WGS 84 before plotting

## Data Processing Flow
1. User provides data in one of the supported formats
2. Data is processed by R functions in the package
3. For sf objects, geometries are extracted and processed
4. Data is converted to JSON format for JavaScript
5. Large datasets may be encoded for efficiency

## Performance Considerations
- For large datasets, consider using encoded geometries
- Use `googlePolylines::encode_sf()` to encode sf objects
- Aggregation layers (grid, hexagon) can improve performance for point data
- Consider downsampling very large datasets when appropriate

## Common Data Parameters
- `data`: The dataset containing geometries and attributes
- For point data:
  - `lon`/`lat`: Column names containing coordinates
- For line data:
  - `origin`/`destination`: Column names or coordinate pairs
- For polygon data:
  - Polygons are extracted directly from sf objects
  - For data frames, specify coordinate columns

## Attribute Mapping
- `fill_colour`/`stroke_colour`: Column names for color mapping
- `fill_opacity`/`stroke_opacity`: Column names for opacity mapping
- `stroke_width`: Column name for line width mapping
- `elevation`/`radius`: Column names for 3D mapping

## Data Type Handling
The package includes S3 methods for different data types:
- `resolve_data.sf` for sf objects
- `resolve_data.data.frame` for data frames
- `resolve_data.sfencoded` for encoded geometries
- `resolve_geojson_data` for GeoJSON data

## Error Handling
- Check that required columns exist in the data
- Validate that geometries are the correct type for the layer
- Handle missing values appropriately
- Provide informative error messages for data issues