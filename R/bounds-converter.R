#' Bounds Conversion System
#'
#' Handles transformation of bounding boxes and spatial extents between different
#' coordinate systems. This is essential for map view management and spatial
#' queries across different mapping providers.
#'
#' @details
#' The BoundsConverter class provides functionality to transform bounding boxes,
#' spatial extents, and viewport definitions between different coordinate systems.
#' It ensures that map bounds are correctly transformed when switching between
#' providers that use different coordinate reference systems.
#'
#' @examples
#' \donttest{
#' # Create bounds converter
#' bounds_converter <- BoundsConverter$new()
#' 
#' # Transform a bounding box from WGS84 to GCJ02
#' wgs84_bounds <- c(116.3, 39.8, 116.5, 40.0)  # [west, south, east, north]
#' gcj02_bounds <- bounds_converter$transform_bounds(wgs84_bounds, "WGS84", "GCJ02")
#' 
#' # Transform viewport for provider switching
#' viewport <- list(longitude = 116.3974, latitude = 39.9093, zoom = 10)
#' gaode_viewport <- bounds_converter$transform_viewport(viewport, "WGS84", "GCJ02")
#' }
#'
#' @export
BoundsConverter <- R6::R6Class("BoundsConverter",
  public = list(
    #' @description
    #' Initialize the bounds converter
    initialize = function() {
      private$.coordinate_transformer <- CoordinateTransformer$new()
    },
    
    #' @description
    #' Transform a bounding box between coordinate systems
    #' @param bounds Numeric vector of length 4: [west, south, east, north] or [xmin, ymin, xmax, ymax]
    #' @param from_crs Source coordinate reference system
    #' @param to_crs Target coordinate reference system
    #' @param sample_points Number of points to sample along bounds for accurate transformation (default: 10)
    #' @return Transformed bounding box in same format
    transform_bounds = function(bounds, from_crs, to_crs, sample_points = 10) {
      # Validate inputs
      if (length(bounds) != 4) {
        stop("Bounds must be a numeric vector of length 4: [west, south, east, north]")
      }
      
      if (!is.numeric(bounds)) {
        stop("Bounds must be numeric")
      }
      
      # If same CRS, return unchanged
      if (from_crs == to_crs) {
        return(bounds)
      }
      
      # Extract bounds components
      west <- bounds[1]
      south <- bounds[2]
      east <- bounds[3]
      north <- bounds[4]
      
      # Validate bounds
      if (west >= east || south >= north) {
        stop("Invalid bounds: west must be < east and south must be < north")
      }
      
      # Sample points along the boundary for accurate transformation
      boundary_points <- private$.sample_boundary_points(west, south, east, north, sample_points)
      
      # Transform all boundary points
      transformed_points <- private$.coordinate_transformer$transform(boundary_points, from_crs, to_crs)
      
      # Calculate new bounds from transformed points
      new_west <- min(transformed_points[, 1])
      new_east <- max(transformed_points[, 1])
      new_south <- min(transformed_points[, 2])
      new_north <- max(transformed_points[, 2])
      
      return(c(new_west, new_south, new_east, new_north))
    },
    
    #' @description
    #' Transform a viewport definition between coordinate systems
    #' @param viewport List with longitude, latitude, and optionally zoom, pitch, bearing
    #' @param from_crs Source coordinate reference system
    #' @param to_crs Target coordinate reference system
    #' @return Transformed viewport with same structure
    transform_viewport = function(viewport, from_crs, to_crs) {
      # Validate viewport structure
      required_fields <- c("longitude", "latitude")
      if (!all(required_fields %in% names(viewport))) {
        stop("Viewport must contain 'longitude' and 'latitude' fields")
      }
      
      # If same CRS, return unchanged
      if (from_crs == to_crs) {
        return(viewport)
      }
      
      # Transform center coordinates
      center_coords <- c(viewport$longitude, viewport$latitude)
      transformed_center <- private$.coordinate_transformer$transform(center_coords, from_crs, to_crs)
      
      # Create new viewport with transformed coordinates
      new_viewport <- viewport
      new_viewport$longitude <- transformed_center[1]
      new_viewport$latitude <- transformed_center[2]
      
      return(new_viewport)
    },
    
    #' @description
    #' Transform multiple bounding boxes efficiently
    #' @param bounds_list List of bounding box vectors
    #' @param from_crs Source coordinate reference system
    #' @param to_crs Target coordinate reference system
    #' @param sample_points Number of points to sample per bounds (default: 10)
    #' @return List of transformed bounding boxes
    transform_bounds_batch = function(bounds_list, from_crs, to_crs, sample_points = 10) {
      if (!is.list(bounds_list)) {
        stop("bounds_list must be a list of bounding box vectors")
      }
      
      # Transform each bounds
      transformed_bounds <- lapply(bounds_list, function(bounds) {
        self$transform_bounds(bounds, from_crs, to_crs, sample_points)
      })
      
      return(transformed_bounds)
    },
    
    #' @description
    #' Calculate the area of a bounding box in square meters
    #' @param bounds Bounding box vector [west, south, east, north]
    #' @param crs Coordinate reference system of the bounds
    #' @return Area in square meters
    calculate_bounds_area = function(bounds, crs) {
      if (length(bounds) != 4) {
        stop("Bounds must be a numeric vector of length 4")
      }
      
      west <- bounds[1]
      south <- bounds[2]
      east <- bounds[3]
      north <- bounds[4]
      
      # For accurate area calculation, we need to handle different coordinate systems
      if (crs == "WGS84") {
        # Use spherical calculation for WGS84
        return(private$.calculate_spherical_area(west, south, east, north))
      } else {
        # Transform to WGS84 for area calculation
        wgs84_bounds <- self$transform_bounds(bounds, crs, "WGS84")
        return(private$.calculate_spherical_area(wgs84_bounds[1], wgs84_bounds[2], 
                                               wgs84_bounds[3], wgs84_bounds[4]))
      }
    },
    
    #' @description
    #' Check if a point is within a bounding box
    #' @param point Numeric vector of length 2 [longitude, latitude]
    #' @param bounds Bounding box vector [west, south, east, north]
    #' @param crs Coordinate reference system (must be same for point and bounds)
    #' @return Logical indicating if point is within bounds
    point_in_bounds = function(point, bounds, crs) {
      if (length(point) != 2) {
        stop("Point must be a numeric vector of length 2")
      }
      if (length(bounds) != 4) {
        stop("Bounds must be a numeric vector of length 4")
      }
      
      lon <- point[1]
      lat <- point[2]
      west <- bounds[1]
      south <- bounds[2]
      east <- bounds[3]
      north <- bounds[4]
      
      return(lon >= west && lon <= east && lat >= south && lat <= north)
    },
    
    #' @description
    #' Expand bounds by a specified margin
    #' @param bounds Bounding box vector [west, south, east, north]
    #' @param margin_degrees Margin to add in degrees (or coordinate units)
    #' @return Expanded bounding box
    expand_bounds = function(bounds, margin_degrees) {
      if (length(bounds) != 4) {
        stop("Bounds must be a numeric vector of length 4")
      }
      
      return(c(
        bounds[1] - margin_degrees,  # west
        bounds[2] - margin_degrees,  # south
        bounds[3] + margin_degrees,  # east
        bounds[4] + margin_degrees   # north
      ))
    },
    
    #' @description
    #' Get the center point of a bounding box
    #' @param bounds Bounding box vector [west, south, east, north]
    #' @return Center point as [longitude, latitude]
    get_bounds_center = function(bounds) {
      if (length(bounds) != 4) {
        stop("Bounds must be a numeric vector of length 4")
      }
      
      center_lon <- (bounds[1] + bounds[3]) / 2
      center_lat <- (bounds[2] + bounds[4]) / 2
      
      return(c(center_lon, center_lat))
    }
  ),
  
  private = list(
    .coordinate_transformer = NULL,
    
    # Sample points along boundary for accurate transformation
    .sample_boundary_points = function(west, south, east, north, sample_points) {
      # Create points along the boundary
      # Top edge
      top_lons <- seq(west, east, length.out = sample_points)
      top_lats <- rep(north, sample_points)
      
      # Bottom edge
      bottom_lons <- seq(west, east, length.out = sample_points)
      bottom_lats <- rep(south, sample_points)
      
      # Left edge (excluding corners to avoid duplication)
      left_lats <- seq(south, north, length.out = sample_points + 2)[2:(sample_points + 1)]
      left_lons <- rep(west, sample_points)
      
      # Right edge (excluding corners to avoid duplication)
      right_lats <- seq(south, north, length.out = sample_points + 2)[2:(sample_points + 1)]
      right_lons <- rep(east, sample_points)
      
      # Combine all boundary points
      all_lons <- c(top_lons, bottom_lons, left_lons, right_lons)
      all_lats <- c(top_lats, bottom_lats, left_lats, right_lats)
      
      return(cbind(all_lons, all_lats))
    },
    
    # Calculate spherical area for WGS84 coordinates
    .calculate_spherical_area = function(west, south, east, north) {
      # Earth radius in meters
      R <- 6371000
      
      # Convert to radians
      west_rad <- west * pi / 180
      east_rad <- east * pi / 180
      south_rad <- south * pi / 180
      north_rad <- north * pi / 180
      
      # Calculate area using spherical geometry
      # Area = R^2 * |sin(north) - sin(south)| * |east - west|
      area <- R^2 * abs(sin(north_rad) - sin(south_rad)) * abs(east_rad - west_rad)
      
      return(area)
    }
  )
)

#' Transform bounding box between coordinate systems
#'
#' Convenience function to transform a bounding box without creating a class instance.
#'
#' @param bounds Numeric vector of length 4: [west, south, east, north]
#' @param from_crs Source coordinate reference system
#' @param to_crs Target coordinate reference system
#' @param sample_points Number of points to sample along bounds (default: 10)
#' @return Transformed bounding box
#'
#' @examples
#' \donttest{
#' # Transform Beijing area bounds from WGS84 to GCJ02
#' beijing_bounds_wgs84 <- c(116.3, 39.8, 116.5, 40.0)
#' beijing_bounds_gcj02 <- transform_bounds(beijing_bounds_wgs84, "WGS84", "GCJ02")
#' }
#'
#' @export
transform_bounds <- function(bounds, from_crs, to_crs, sample_points = 10) {
  bounds_converter <- BoundsConverter$new()
  return(bounds_converter$transform_bounds(bounds, from_crs, to_crs, sample_points))
}

#' Transform viewport between coordinate systems
#'
#' Convenience function to transform a map viewport without creating a class instance.
#'
#' @param viewport List with longitude, latitude, and optionally zoom, pitch, bearing
#' @param from_crs Source coordinate reference system
#' @param to_crs Target coordinate reference system
#' @return Transformed viewport
#'
#' @examples
#' \donttest{
#' # Transform viewport from WGS84 to GCJ02
#' viewport_wgs84 <- list(longitude = 116.3974, latitude = 39.9093, zoom = 10)
#' viewport_gcj02 <- transform_viewport(viewport_wgs84, "WGS84", "GCJ02")
#' }
#'
#' @export
transform_viewport <- function(viewport, from_crs, to_crs) {
  bounds_converter <- BoundsConverter$new()
  return(bounds_converter$transform_viewport(viewport, from_crs, to_crs))
}