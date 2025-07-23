#' Coordinate Transformation Engine
#'
#' A comprehensive coordinate transformation system supporting conversions between
#' WGS84, GCJ02 (Mars Coordinates), and BD09 (Baidu Coordinates) coordinate systems.
#' This is essential for Chinese mapping providers that use different coordinate systems.
#'
#' @details
#' The CoordinateTransformer class provides high-precision coordinate transformations
#' with accuracy within 1 meter tolerance. It supports:
#' - WGS84: World Geodetic System 1984 (standard GPS coordinates)
#' - GCJ02: Guojia Cehui Ju 02 (Chinese Mars Coordinates used by Gaode/AutoNavi)
#' - BD09: Baidu 09 (Baidu's proprietary coordinate system)
#'
#' @examples
#' \donttest{
#' # Create transformer instance
#' transformer <- CoordinateTransformer$new()
#' 
#' # Transform single point from WGS84 to GCJ02
#' wgs84_point <- c(116.3974, 39.9093)  # Beijing coordinates
#' gcj02_point <- transformer$transform(wgs84_point, "WGS84", "GCJ02")
#' 
#' # Transform multiple points
#' wgs84_points <- matrix(c(116.3974, 39.9093, 121.4737, 31.2304), ncol = 2, byrow = TRUE)
#' gcj02_points <- transformer$transform_batch(wgs84_points, "WGS84", "GCJ02")
#' 
#' # Validate transformation accuracy
#' is_accurate <- transformer$validate_accuracy(wgs84_point, gcj02_point, "WGS84", "GCJ02")
#' }
#'
#' @export
CoordinateTransformer <- R6::R6Class("CoordinateTransformer",
  public = list(
    #' @description
    #' Initialize the coordinate transformer
    #' @param precision Numerical precision for calculations (default: 1e-10)
    initialize = function(precision = 1e-10) {
      private$.precision <- precision
      private$.setup_constants()
    },
    
    #' @description
    #' Transform coordinates between different coordinate systems
    #' @param coords Numeric vector of length 2 (longitude, latitude) or matrix with 2 columns
    #' @param from_crs Source coordinate reference system ("WGS84", "GCJ02", "BD09")
    #' @param to_crs Target coordinate reference system ("WGS84", "GCJ02", "BD09")
    #' @return Transformed coordinates in same format as input
    transform = function(coords, from_crs, to_crs) {
      # Validate inputs
      private$.validate_crs(from_crs)
      private$.validate_crs(to_crs)
      private$.validate_coordinates(coords)
      
      # Handle same CRS case
      if (from_crs == to_crs) {
        return(coords)
      }
      
      # Convert to matrix for consistent processing
      is_vector <- is.vector(coords) && length(coords) == 2
      if (is_vector) {
        coords <- matrix(coords, nrow = 1)
      }
      
      # Perform transformation
      result <- private$.transform_coordinates(coords, from_crs, to_crs)
      
      # Return in original format
      if (is_vector) {
        return(as.numeric(result[1, ]))
      }
      return(result)
    },
    
    #' @description
    #' Transform multiple coordinate points efficiently
    #' @param coords_matrix Matrix with coordinates (longitude, latitude) in columns
    #' @param from_crs Source coordinate reference system
    #' @param to_crs Target coordinate reference system
    #' @return Matrix of transformed coordinates
    transform_batch = function(coords_matrix, from_crs, to_crs) {
      if (!is.matrix(coords_matrix) || ncol(coords_matrix) != 2) {
        stop("coords_matrix must be a matrix with 2 columns (longitude, latitude)")
      }
      
      return(self$transform(coords_matrix, from_crs, to_crs))
    },
    
    #' @description
    #' Validate transformation accuracy within tolerance
    #' @param original_coords Original coordinates
    #' @param transformed_coords Transformed coordinates
    #' @param from_crs Source CRS
    #' @param to_crs Target CRS
    #' @param tolerance_meters Tolerance in meters (default: 1.0)
    #' @return Logical indicating if transformation is within tolerance
    validate_accuracy = function(original_coords, transformed_coords, from_crs, to_crs, tolerance_meters = 1.0) {
      # Transform back to original CRS
      back_transformed <- self$transform(transformed_coords, to_crs, from_crs)
      
      # Calculate distance difference in meters
      distance_error <- private$.calculate_distance_meters(original_coords, back_transformed)
      
      return(distance_error <= tolerance_meters)
    },
    
    #' @description
    #' Get supported coordinate reference systems
    #' @return Character vector of supported CRS names
    get_supported_crs = function() {
      return(c("WGS84", "GCJ02", "BD09"))
    },
    
    #' @description
    #' Check if a coordinate system is supported
    #' @param crs Coordinate reference system name
    #' @return Logical indicating support
    is_supported_crs = function(crs) {
      return(crs %in% self$get_supported_crs())
    }
  ),
  
  private = list(
    .precision = NULL,
    .constants = NULL,
    
    # Setup mathematical constants for transformations
    .setup_constants = function() {
      private$.constants <- list(
        # Earth parameters
        a = 6378245.0,  # Semi-major axis
        ee = 0.00669342162296594323,  # Eccentricity squared
        
        # Transformation parameters
        x_pi = 3.14159265358979324 * 3000.0 / 180.0,
        pi = 3.1415926535897932384626,
        
        # Offset parameters for GCJ02
        gcj02_lat_offset = 20.0,
        gcj02_lon_offset = 20.0
      )
    },
    
    # Validate coordinate reference system
    .validate_crs = function(crs) {
      if (!self$is_supported_crs(crs)) {
        stop(sprintf("Unsupported coordinate system: %s. Supported systems: %s", 
                     crs, paste(self$get_supported_crs(), collapse = ", ")))
      }
    },
    
    # Validate coordinate format
    .validate_coordinates = function(coords) {
      if (is.vector(coords)) {
        if (length(coords) != 2) {
          stop("Coordinate vector must have exactly 2 elements (longitude, latitude)")
        }
        if (!is.numeric(coords)) {
          stop("Coordinates must be numeric")
        }
      } else if (is.matrix(coords)) {
        if (ncol(coords) != 2) {
          stop("Coordinate matrix must have exactly 2 columns (longitude, latitude)")
        }
        if (!is.numeric(coords)) {
          stop("Coordinates must be numeric")
        }
      } else {
        stop("Coordinates must be a numeric vector of length 2 or a matrix with 2 columns")
      }
    },
    
    # Main coordinate transformation logic
    .transform_coordinates = function(coords_matrix, from_crs, to_crs) {
      # Define transformation path
      if (from_crs == "WGS84" && to_crs == "GCJ02") {
        return(private$.wgs84_to_gcj02(coords_matrix))
      } else if (from_crs == "GCJ02" && to_crs == "WGS84") {
        return(private$.gcj02_to_wgs84(coords_matrix))
      } else if (from_crs == "GCJ02" && to_crs == "BD09") {
        return(private$.gcj02_to_bd09(coords_matrix))
      } else if (from_crs == "BD09" && to_crs == "GCJ02") {
        return(private$.bd09_to_gcj02(coords_matrix))
      } else if (from_crs == "WGS84" && to_crs == "BD09") {
        # WGS84 -> GCJ02 -> BD09
        gcj02_coords <- private$.wgs84_to_gcj02(coords_matrix)
        return(private$.gcj02_to_bd09(gcj02_coords))
      } else if (from_crs == "BD09" && to_crs == "WGS84") {
        # BD09 -> GCJ02 -> WGS84
        gcj02_coords <- private$.bd09_to_gcj02(coords_matrix)
        return(private$.gcj02_to_wgs84(gcj02_coords))
      }
      
      stop(sprintf("Transformation from %s to %s not implemented", from_crs, to_crs))
    },
    
    # WGS84 to GCJ02 transformation
    .wgs84_to_gcj02 = function(coords_matrix) {
      lon <- coords_matrix[, 1]
      lat <- coords_matrix[, 2]
      
      # Check if coordinates are in China
      in_china <- private$.is_in_china(lon, lat)
      
      result_lon <- lon
      result_lat <- lat
      
      # Apply transformation only for coordinates in China
      china_indices <- which(in_china)
      if (length(china_indices) > 0) {
        china_lon <- lon[china_indices]
        china_lat <- lat[china_indices]
        
        delta <- private$.calculate_gcj02_delta(china_lon, china_lat)
        
        result_lon[china_indices] <- china_lon + delta$dlon
        result_lat[china_indices] <- china_lat + delta$dlat
      }
      
      return(cbind(result_lon, result_lat))
    },
    
    # GCJ02 to WGS84 transformation (inverse) - using iterative method for accuracy
    .gcj02_to_wgs84 = function(coords_matrix) {
      lon <- coords_matrix[, 1]
      lat <- coords_matrix[, 2]
      
      # Check if coordinates are in China
      in_china <- private$.is_in_china(lon, lat)
      
      result_lon <- lon
      result_lat <- lat
      
      # Apply inverse transformation only for coordinates in China
      china_indices <- which(in_china)
      if (length(china_indices) > 0) {
        china_lon <- lon[china_indices]
        china_lat <- lat[china_indices]
        
        # Use iterative method for more accurate inverse transformation
        for (i in seq_along(china_indices)) {
          gcj02_point <- c(china_lon[i], china_lat[i])
          wgs84_point <- private$.gcj02_to_wgs84_iterative(gcj02_point)
          
          result_lon[china_indices[i]] <- wgs84_point[1]
          result_lat[china_indices[i]] <- wgs84_point[2]
        }
      }
      
      return(cbind(result_lon, result_lat))
    },
    
    # GCJ02 to BD09 transformation
    .gcj02_to_bd09 = function(coords_matrix) {
      lon <- coords_matrix[, 1]
      lat <- coords_matrix[, 2]
      
      z <- sqrt(lon * lon + lat * lat) + 0.00002 * sin(lat * private$.constants$x_pi)
      theta <- atan2(lat, lon) + 0.000003 * cos(lon * private$.constants$x_pi)
      
      bd_lon <- z * cos(theta) + 0.0065
      bd_lat <- z * sin(theta) + 0.006
      
      return(cbind(bd_lon, bd_lat))
    },
    
    # BD09 to GCJ02 transformation
    .bd09_to_gcj02 = function(coords_matrix) {
      lon <- coords_matrix[, 1]
      lat <- coords_matrix[, 2]
      
      x <- lon - 0.0065
      y <- lat - 0.006
      z <- sqrt(x * x + y * y) - 0.00002 * sin(y * private$.constants$x_pi)
      theta <- atan2(y, x) - 0.000003 * cos(x * private$.constants$x_pi)
      
      gcj_lon <- z * cos(theta)
      gcj_lat <- z * sin(theta)
      
      return(cbind(gcj_lon, gcj_lat))
    },
    
    # Calculate GCJ02 transformation delta
    .calculate_gcj02_delta = function(lon, lat) {
      dlat <- private$.transform_lat(lon - 105.0, lat - 35.0)
      dlon <- private$.transform_lon(lon - 105.0, lat - 35.0)
      
      radlat <- lat / 180.0 * private$.constants$pi
      magic <- sin(radlat)
      magic <- 1 - private$.constants$ee * magic * magic
      sqrtmagic <- sqrt(magic)
      
      dlat <- (dlat * 180.0) / ((private$.constants$a * (1 - private$.constants$ee)) / (magic * sqrtmagic) * private$.constants$pi)
      dlon <- (dlon * 180.0) / (private$.constants$a / sqrtmagic * cos(radlat) * private$.constants$pi)
      
      return(list(dlat = dlat, dlon = dlon))
    },
    
    # Transform latitude component
    .transform_lat = function(lon, lat) {
      ret <- -100.0 + 2.0 * lon + 3.0 * lat + 0.2 * lat * lat + 
             0.1 * lon * lat + 0.2 * sqrt(abs(lon))
      ret <- ret + (20.0 * sin(6.0 * lon * private$.constants$pi) + 20.0 * sin(2.0 * lon * private$.constants$pi)) * 2.0 / 3.0
      ret <- ret + (20.0 * sin(lat * private$.constants$pi) + 40.0 * sin(lat / 3.0 * private$.constants$pi)) * 2.0 / 3.0
      ret <- ret + (160.0 * sin(lat / 12.0 * private$.constants$pi) + 320 * sin(lat * private$.constants$pi / 30.0)) * 2.0 / 3.0
      return(ret)
    },
    
    # Transform longitude component
    .transform_lon = function(lon, lat) {
      ret <- 300.0 + lon + 2.0 * lat + 0.1 * lon * lon + 
             0.1 * lon * lat + 0.1 * sqrt(abs(lon))
      ret <- ret + (20.0 * sin(6.0 * lon * private$.constants$pi) + 20.0 * sin(2.0 * lon * private$.constants$pi)) * 2.0 / 3.0
      ret <- ret + (20.0 * sin(lon * private$.constants$pi) + 40.0 * sin(lon / 3.0 * private$.constants$pi)) * 2.0 / 3.0
      ret <- ret + (150.0 * sin(lon / 12.0 * private$.constants$pi) + 300.0 * sin(lon / 30.0 * private$.constants$pi)) * 2.0 / 3.0
      return(ret)
    },
    
    # Check if coordinates are within China boundaries
    .is_in_china = function(lon, lat) {
      return(lon >= 72.004 & lon <= 137.8347 & lat >= 0.8293 & lat <= 55.8271)
    },
    
    # Iterative GCJ02 to WGS84 transformation for higher accuracy
    .gcj02_to_wgs84_iterative = function(gcj02_point) {
      # Initial guess - simple delta subtraction
      delta <- private$.calculate_gcj02_delta(gcj02_point[1], gcj02_point[2])
      wgs84_guess <- c(gcj02_point[1] - delta$dlon, gcj02_point[2] - delta$dlat)
      
      # Iterative refinement (Newton-Raphson style)
      max_iterations <- 10
      tolerance <- 1e-8
      
      for (i in 1:max_iterations) {
        # Transform the guess forward to GCJ02
        forward_transformed <- private$.wgs84_to_gcj02(matrix(wgs84_guess, nrow = 1))
        forward_point <- c(forward_transformed[1, 1], forward_transformed[1, 2])
        
        # Calculate error
        error_lon <- gcj02_point[1] - forward_point[1]
        error_lat <- gcj02_point[2] - forward_point[2]
        
        # Check convergence
        if (abs(error_lon) < tolerance && abs(error_lat) < tolerance) {
          break
        }
        
        # Update guess by adding the error
        wgs84_guess[1] <- wgs84_guess[1] + error_lon
        wgs84_guess[2] <- wgs84_guess[2] + error_lat
      }
      
      return(wgs84_guess)
    },
    
    # Calculate distance between two points in meters
    .calculate_distance_meters = function(coords1, coords2) {
      if (length(coords1) != 2 || length(coords2) != 2) {
        stop("Both coordinate sets must have exactly 2 elements")
      }
      
      # Haversine formula for distance calculation
      R <- 6371000  # Earth radius in meters
      
      lat1_rad <- coords1[2] * private$.constants$pi / 180
      lat2_rad <- coords2[2] * private$.constants$pi / 180
      delta_lat <- (coords2[2] - coords1[2]) * private$.constants$pi / 180
      delta_lon <- (coords2[1] - coords1[1]) * private$.constants$pi / 180
      
      a <- sin(delta_lat/2) * sin(delta_lat/2) +
           cos(lat1_rad) * cos(lat2_rad) *
           sin(delta_lon/2) * sin(delta_lon/2)
      c <- 2 * atan2(sqrt(a), sqrt(1-a))
      
      return(R * c)
    }
  )
)

#' Transform coordinates between different coordinate systems
#'
#' A convenience function for coordinate transformation without creating a class instance.
#' Supports transformations between WGS84, GCJ02, and BD09 coordinate systems.
#'
#' @param coords Numeric vector of length 2 (longitude, latitude) or matrix with 2 columns
#' @param from_crs Source coordinate reference system ("WGS84", "GCJ02", "BD09")
#' @param to_crs Target coordinate reference system ("WGS84", "GCJ02", "BD09")
#' @return Transformed coordinates in same format as input
#'
#' @examples
#' \donttest{
#' # Transform Beijing coordinates from WGS84 to GCJ02
#' beijing_wgs84 <- c(116.3974, 39.9093)
#' beijing_gcj02 <- transform_coordinates(beijing_wgs84, "WGS84", "GCJ02")
#' 
#' # Transform multiple points
#' points_wgs84 <- matrix(c(116.3974, 39.9093, 121.4737, 31.2304), ncol = 2, byrow = TRUE)
#' points_gcj02 <- transform_coordinates(points_wgs84, "WGS84", "GCJ02")
#' }
#'
#' @export
transform_coordinates <- function(coords, from_crs, to_crs) {
  transformer <- CoordinateTransformer$new()
  return(transformer$transform(coords, from_crs, to_crs))
}