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

#' Transform Coordinates Between Coordinate Systems
#'
#' Transform coordinates between different coordinate systems using a global transformer instance.
#'
#' @description
#' This function provides a convenient interface to transform coordinates between
#' WGS84, GCJ02 (Mars Coordinates), and BD09 (Baidu Coordinates) coordinate systems.
#' It uses a global CoordinateTransformer instance and maintains high precision
#' with accuracy within 1 meter tolerance.
#'
#' @param coords Numeric vector of length 2 (longitude, latitude) or matrix with 2 columns
#' @param from_crs Source coordinate reference system ("WGS84", "GCJ02", "BD09")
#' @param to_crs Target coordinate reference system ("WGS84", "GCJ02", "BD09")
#'
#' @return Transformed coordinates in same format as input
#'
#' @details
#' Supported coordinate systems:
#' \itemize{
#'   \item \strong{WGS84}: World Geodetic System 1984 (standard GPS coordinates)
#'   \item \strong{GCJ02}: Guojia Cehui Ju 02 (Chinese Mars Coordinates used by Gaode/AutoNavi)
#'   \item \strong{BD09}: Baidu 09 (Baidu's proprietary coordinate system)
#' }
#'
#' The transformation algorithms implement the official conversion formulas with
#' optimizations for accuracy and performance. All transformations maintain
#' precision within 1 meter tolerance as required for mapping applications.
#'
#' @examples
#' \donttest{
#' # Transform Beijing coordinates from WGS84 to GCJ02
#' beijing_wgs84 <- c(116.3974, 39.9093)
#' beijing_gcj02 <- transform_coordinates(beijing_wgs84, "WGS84", "GCJ02")
#' print(beijing_gcj02)
#' 
#' # Transform multiple points
#' points_wgs84 <- matrix(c(116.3974, 39.9093, 121.4737, 31.2304), ncol = 2, byrow = TRUE)
#' points_gcj02 <- transform_coordinates(points_wgs84, "WGS84", "GCJ02")
#' print(points_gcj02)
#' 
#' # Chain transformations: WGS84 -> GCJ02 -> BD09
#' wgs84_point <- c(116.3974, 39.9093)
#' gcj02_point <- transform_coordinates(wgs84_point, "WGS84", "GCJ02")
#' bd09_point <- transform_coordinates(gcj02_point, "GCJ02", "BD09")
#' 
#' # Verify round-trip accuracy
#' back_to_wgs84 <- transform_coordinates(bd09_point, "BD09", "GCJ02")
#' back_to_wgs84 <- transform_coordinates(back_to_wgs84, "GCJ02", "WGS84")
#' distance_error <- sqrt(sum((wgs84_point - back_to_wgs84)^2)) * 111320  # Convert to meters
#' print(paste("Round-trip error:", round(distance_error, 2), "meters"))
#' }
#'
#' @seealso \code{\link{detect_coordinate_system}}, \code{\link{auto_transform_for_provider}}, 
#'   \code{\link{CoordinateTransformer}}
#'
#' @export
transform_coordinates <- function(coords, from_crs, to_crs) {
  transformer <- get_coordinate_transformer()
  return(transformer$transform(coords, from_crs, to_crs))
}

#' Detect Coordinate System
#'
#' Automatically detect the coordinate system of spatial data based on coordinate ranges and patterns.
#'
#' @description
#' This function analyzes coordinate data to automatically determine whether it uses
#' WGS84, GCJ02, or BD09 coordinate system. It uses statistical analysis of coordinate
#' ranges and patterns to make the determination.
#'
#' @param data Spatial data (sf object, data.frame with coordinates, or coordinate matrix)
#' @param confidence_threshold Numeric threshold for detection confidence (0-1, default: 0.8)
#' @param sample_size Integer number of points to sample for analysis (default: 1000)
#'
#' @return List containing:
#'   \itemize{
#'     \item detected_crs: Most likely coordinate system ("WGS84", "GCJ02", "BD09", or "UNKNOWN")
#'     \item confidence: Confidence score (0-1)
#'     \item analysis: Detailed analysis results
#'   }
#'
#' @details
#' The detection algorithm analyzes:
#' \itemize{
#'   \item Coordinate ranges and bounds
#'   \item Statistical distribution patterns
#'   \item Offset patterns characteristic of each coordinate system
#'   \item Geographic clustering within China boundaries
#' }
#'
#' Detection is most accurate for coordinates within China where the different
#' coordinate systems show distinct patterns. For coordinates outside China,
#' WGS84 is typically assumed.
#'
#' @examples
#' \donttest{
#' # Detect coordinate system from data frame
#' data <- data.frame(
#'   lon = c(116.3974, 116.4074, 116.3874),
#'   lat = c(39.9093, 39.9193, 39.8993)
#' )
#' detection <- detect_coordinate_system(data)
#' print(detection$detected_crs)
#' print(detection$confidence)
#' 
#' # Detect from sf object
#' library(sf)
#' sf_data <- st_as_sf(data, coords = c("lon", "lat"))
#' detection <- detect_coordinate_system(sf_data)
#' 
#' # Detect with custom confidence threshold
#' detection <- detect_coordinate_system(data, confidence_threshold = 0.9)
#' }
#'
#' @export
detect_coordinate_system <- function(data, confidence_threshold = 0.8, sample_size = 1000) {
  detector <- get_coordinate_detector()
  return(detector$detect(data, confidence_threshold, sample_size))
}

#' Auto-Transform for Provider
#'
#' Automatically transform data coordinates to match the coordinate system required by a map provider.
#'
#' @description
#' This function automatically detects the coordinate system of input data and
#' transforms it to the coordinate system required by the specified map provider.
#' It provides seamless coordinate handling for multi-provider mapping.
#'
#' @param data Spatial data to transform
#' @param provider Character string identifying the target provider
#' @param source_crs Optional source coordinate system (auto-detected if NULL)
#' @param validate_accuracy Logical indicating if transformation accuracy should be validated
#'
#' @return Transformed spatial data in the provider's coordinate system
#'
#' @details
#' Provider coordinate systems:
#' \itemize{
#'   \item \strong{mapbox, leaflet, openlayers}: WGS84 (EPSG:4326)
#'   \item \strong{gaode}: GCJ02 (Mars Coordinates)
#'   \item \strong{baidu}: BD09 (Baidu Coordinates)
#' }
#'
#' The function performs the following steps:
#' \enumerate{
#'   \item Detect source coordinate system (if not specified)
#'   \item Determine target coordinate system for provider
#'   \item Transform coordinates if systems differ
#'   \item Validate transformation accuracy (if requested)
#' }
#'
#' @examples
#' \donttest{
#' # Auto-transform data for Gaode provider
#' wgs84_data <- data.frame(lon = 116.3974, lat = 39.9093)
#' gaode_data <- auto_transform_for_provider(wgs84_data, "gaode")
#' 
#' # Auto-transform with explicit source CRS
#' baidu_data <- auto_transform_for_provider(wgs84_data, "baidu", source_crs = "WGS84")
#' 
#' # Auto-transform with accuracy validation
#' leaflet_data <- auto_transform_for_provider(
#'   gaode_data, 
#'   "leaflet", 
#'   validate_accuracy = TRUE
#' )
#' }
#'
#' @export
auto_transform_for_provider <- function(data, provider, source_crs = NULL, validate_accuracy = FALSE) {
  
  # Validate provider
  if (!is.character(provider) || length(provider) != 1) {
    stop("Provider must be a single character string")
  }
  
  # Get target coordinate system for provider
  target_crs <- get_provider_crs(provider)
  
  # Detect source coordinate system if not provided
  if (is.null(source_crs)) {
    detection <- detect_coordinate_system(data)
    source_crs <- detection$detected_crs
    
    if (source_crs == "UNKNOWN") {
      warning("Could not reliably detect coordinate system, assuming WGS84")
      source_crs <- "WGS84"
    }
  }
  
  # Transform if coordinate systems differ
  if (source_crs != target_crs) {
    # Extract coordinates for transformation
    if (inherits(data, "sf")) {
      coords <- sf::st_coordinates(data)
      transformed_coords <- transform_coordinates(coords, source_crs, target_crs)
      
      # Update sf object with transformed coordinates
      data <- sf::st_set_geometry(data, NULL)  # Remove geometry
      data <- sf::st_as_sf(data, coords = transformed_coords, crs = 4326)
      
    } else if (is.data.frame(data)) {
      # Detect coordinate columns
      coord_cols <- detect_coordinate_columns(data)
      coords <- as.matrix(data[, c(coord_cols$x, coord_cols$y)])
      
      transformed_coords <- transform_coordinates(coords, source_crs, target_crs)
      
      # Update data frame
      data[, coord_cols$x] <- transformed_coords[, 1]
      data[, coord_cols$y] <- transformed_coords[, 2]
      
    } else {
      stop("Unsupported data format. Use sf objects or data.frame with coordinates.")
    }
    
    # Validate accuracy if requested
    if (validate_accuracy) {
      # Sample a few points for validation
      sample_indices <- sample(nrow(coords), min(10, nrow(coords)))
      sample_coords <- coords[sample_indices, , drop = FALSE]
      sample_transformed <- transformed_coords[sample_indices, , drop = FALSE]
      
      transformer <- get_coordinate_transformer()
      for (i in seq_len(nrow(sample_coords))) {
        is_accurate <- transformer$validate_accuracy(
          sample_coords[i, ], sample_transformed[i, ], 
          source_crs, target_crs
        )
        if (!is_accurate) {
          warning("Transformation accuracy validation failed for some points")
          break
        }
      }
    }
  }
  
  return(data)
}

# Global coordinate transformer instance
.coordinate_transformer <- NULL

#' Get Coordinate Transformer
#'
#' Get the global coordinate transformer instance, creating it if necessary.
#'
#' @return CoordinateTransformer instance
#'
#' @examples
#' \donttest{
#' # Get global transformer
#' transformer <- get_coordinate_transformer()
#' coords <- transformer$transform(c(116.3974, 39.9093), "WGS84", "GCJ02")
#' }
#'
#' @export
get_coordinate_transformer <- function() {
  if (is.null(.coordinate_transformer)) {
    .coordinate_transformer <<- CoordinateTransformer$new()
  }
  return(.coordinate_transformer)
}

# Global coordinate detector instance
.coordinate_detector <- NULL

#' Get Coordinate Detector
#'
#' Get the global coordinate detector instance, creating it if necessary.
#'
#' @return CoordinateDetector instance
#'
#' @examples
#' \donttest{
#' # Get global detector
#' detector <- get_coordinate_detector()
#' result <- detector$detect(data)
#' }
#'
#' @export
get_coordinate_detector <- function() {
  if (is.null(.coordinate_detector)) {
    .coordinate_detector <<- CoordinateDetector$new()
  }
  return(.coordinate_detector)
}

#' Get Provider Coordinate System
#'
#' Get the coordinate system used by a specific map provider.
#'
#' @param provider Character string identifying the provider
#' @return Character string identifying the coordinate system
#'
#' @examples
#' \donttest{
#' # Get coordinate systems for different providers
#' mapbox_crs <- get_provider_crs("mapbox")  # Returns "WGS84"
#' gaode_crs <- get_provider_crs("gaode")    # Returns "GCJ02"
#' baidu_crs <- get_provider_crs("baidu")    # Returns "BD09"
#' }
#'
#' @export
get_provider_crs <- function(provider) {
  crs_mapping <- list(
    "mapbox" = "WGS84",
    "leaflet" = "WGS84",
    "openlayers" = "WGS84",
    "gaode" = "GCJ02",
    "baidu" = "BD09"
  )
  
  if (provider %in% names(crs_mapping)) {
    return(crs_mapping[[provider]])
  } else {
    warning(sprintf("Unknown provider '%s', assuming WGS84", provider))
    return("WGS84")
  }
}

# Helper function to detect coordinate columns in data frame
detect_coordinate_columns <- function(data) {
  col_names <- tolower(names(data))
  
  lon_patterns <- c("lon", "lng", "longitude", "x")
  lat_patterns <- c("lat", "latitude", "y")
  
  x_col <- NULL
  y_col <- NULL
  
  for (pattern in lon_patterns) {
    matches <- grep(pattern, col_names, value = TRUE)
    if (length(matches) > 0) {
      x_col <- names(data)[col_names == matches[1]]
      break
    }
  }
  
  for (pattern in lat_patterns) {
    matches <- grep(pattern, col_names, value = TRUE)
    if (length(matches) > 0) {
      y_col <- names(data)[col_names == matches[1]]
      break
    }
  }
  
  if (is.null(x_col) || is.null(y_col)) {
    stop("Could not detect coordinate columns. Please ensure data has longitude/latitude or x/y columns.")
  }
  
  return(list(x = x_col, y = y_col))
}
