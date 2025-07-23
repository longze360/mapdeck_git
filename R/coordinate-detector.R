#' Coordinate System Detection and Auto-Transformation
#'
#' Automatically detects coordinate systems in spatial data and provides
#' seamless transformation functions for provider switching. This enables
#' automatic handling of different coordinate systems without manual intervention.
#'
#' @details
#' The CoordinateDetector class analyzes spatial data to determine the most
#' likely coordinate reference system based on coordinate ranges, patterns,
#' and geographic context. It supports detection of WGS84, GCJ02, and BD09
#' coordinate systems commonly used in mapping applications.
#'
#' @examples
#' \donttest{
#' # Create coordinate detector
#' detector <- CoordinateDetector$new()
#' 
#' # Detect coordinate system from data
#' data <- data.frame(lon = c(116.3974, 121.4737), lat = c(39.9093, 31.2304))
#' detected_crs <- detector$detect_crs(data)
#' 
#' # Auto-transform data for a provider
#' gaode_data <- detector$auto_transform_for_provider(data, "gaode")
#' }
#'
#' @export
CoordinateDetector <- R6::R6Class("CoordinateDetector",
  public = list(
    #' @description
    #' Initialize the coordinate detector
    initialize = function() {
      private$.coordinate_transformer <- CoordinateTransformer$new()
      private$.projection_manager <- ProjectionManager$new()
      private$.setup_detection_rules()
    },
    
    #' @description
    #' Detect the coordinate reference system of spatial data
    #' @param data Data frame, matrix, or vector with coordinate data
    #' @param lon_col Name or index of longitude column (default: auto-detect)
    #' @param lat_col Name or index of latitude column (default: auto-detect)
    #' @param confidence_threshold Minimum confidence level for detection (0-1, default: 0.7)
    #' @return List with detected CRS and confidence score
    detect_crs = function(data, lon_col = NULL, lat_col = NULL, confidence_threshold = 0.7) {
      # Extract coordinates
      coords <- private$.extract_coordinates(data, lon_col, lat_col)
      
      if (nrow(coords) == 0) {
        stop("No valid coordinates found in data")
      }
      
      # Calculate detection scores for each CRS
      scores <- private$.calculate_detection_scores(coords)
      
      # Find the CRS with highest score
      best_crs <- names(scores)[which.max(scores)]
      best_score <- max(scores)
      
      # Check confidence threshold
      if (best_score < confidence_threshold) {
        warning(sprintf("Low confidence in CRS detection (%.2f < %.2f). Consider manual specification.", 
                       best_score, confidence_threshold))
      }
      
      return(list(
        crs = best_crs,
        confidence = best_score,
        scores = scores
      ))
    },
    
    #' @description
    #' Auto-transform data for a specific provider
    #' @param data Spatial data to transform
    #' @param provider Target provider name
    #' @param source_crs Source CRS (if NULL, will be auto-detected)
    #' @param lon_col Longitude column name/index
    #' @param lat_col Latitude column name/index
    #' @return Transformed data appropriate for the provider
    auto_transform_for_provider = function(data, provider, source_crs = NULL, lon_col = NULL, lat_col = NULL) {
      # Detect source CRS if not provided
      if (is.null(source_crs)) {
        detection_result <- self$detect_crs(data, lon_col, lat_col)
        source_crs <- detection_result$crs
        
        if (detection_result$confidence < 0.7) {
          warning(sprintf("Auto-detected CRS '%s' with low confidence (%.2f). Results may be inaccurate.", 
                         source_crs, detection_result$confidence))
        }
      }
      
      # Get target CRS for provider
      target_crs <- private$.projection_manager$get_provider_projection(provider)
      
      # Transform data if needed
      if (source_crs == target_crs) {
        return(data)
      }
      
      # Check if coordinates are in China - only transform if they are
      coords <- private$.extract_coordinates(data, lon_col, lat_col)
      if (!self$is_china_region(coords) && source_crs == "WGS84" && target_crs %in% c("GCJ02", "BD09")) {
        # Don't transform coordinates outside China from WGS84 to Chinese coordinate systems
        return(data)
      }
      
      return(private$.projection_manager$prepare_data_for_provider(data, provider, lon_col, lat_col, source_crs))
    },
    
    #' @description
    #' Validate coordinate accuracy after transformation
    #' @param original_data Original data
    #' @param transformed_data Transformed data
    #' @param source_crs Source coordinate system
    #' @param target_crs Target coordinate system
    #' @param tolerance_meters Tolerance in meters (default: 1.0)
    #' @param sample_size Number of points to validate (default: min(100, nrow))
    #' @return List with validation results
    validate_transformation_accuracy = function(original_data, transformed_data, source_crs, target_crs, 
                                              tolerance_meters = 1.0, sample_size = NULL) {
      # Extract coordinates from both datasets
      original_coords <- private$.extract_coordinates(original_data)
      transformed_coords <- private$.extract_coordinates(transformed_data)
      
      if (nrow(original_coords) != nrow(transformed_coords)) {
        stop("Original and transformed data must have the same number of rows")
      }
      
      # Sample points for validation if dataset is large
      n_points <- nrow(original_coords)
      if (is.null(sample_size)) {
        sample_size <- min(100, n_points)
      }
      
      if (sample_size < n_points) {
        sample_indices <- sample(n_points, sample_size)
        original_coords <- original_coords[sample_indices, , drop = FALSE]
        transformed_coords <- transformed_coords[sample_indices, , drop = FALSE]
      }
      
      # Validate each point
      validation_results <- vector("logical", nrow(original_coords))
      errors <- numeric(nrow(original_coords))
      
      for (i in seq_len(nrow(original_coords))) {
        original_point <- original_coords[i, ]
        transformed_point <- transformed_coords[i, ]
        
        # Transform back to original CRS
        back_transformed <- private$.coordinate_transformer$transform(transformed_point, target_crs, source_crs)
        
        # Calculate error
        error_distance <- private$.coordinate_transformer$validate_accuracy(
          original_point, back_transformed, source_crs, source_crs, tolerance_meters
        )
        
        validation_results[i] <- error_distance
        if (!error_distance) {
          errors[i] <- private$.coordinate_transformer$.__enclos_env__$private$.calculate_distance_meters(
            original_point, back_transformed
          )
        }
      }
      
      # Calculate summary statistics
      accuracy_rate <- sum(validation_results) / length(validation_results)
      max_error <- if (any(!validation_results)) max(errors[!validation_results]) else 0
      
      return(list(
        accuracy_rate = accuracy_rate,
        points_tested = length(validation_results),
        points_accurate = sum(validation_results),
        max_error_meters = max_error,
        tolerance_meters = tolerance_meters,
        passed = accuracy_rate >= 0.95  # 95% of points should be accurate
      ))
    },
    
    #' @description
    #' Get supported coordinate systems for detection
    #' @return Character vector of supported CRS names
    get_supported_crs = function() {
      return(c("WGS84", "GCJ02", "BD09"))
    },
    
    #' @description
    #' Check if coordinates are likely in China (affects CRS detection)
    #' @param coords Matrix or data frame with coordinates
    #' @return Logical indicating if coordinates are in China region
    is_china_region = function(coords) {
      coords_matrix <- private$.extract_coordinates(coords)
      
      if (nrow(coords_matrix) == 0) {
        return(FALSE)
      }
      
      # Check if majority of points are in China bounds
      china_bounds <- private$.get_china_bounds()
      in_china_count <- 0
      
      for (i in seq_len(nrow(coords_matrix))) {
        lon <- coords_matrix[i, 1]
        lat <- coords_matrix[i, 2]
        
        if (lon >= china_bounds$west && lon <= china_bounds$east &&
            lat >= china_bounds$south && lat <= china_bounds$north) {
          in_china_count <- in_china_count + 1
        }
      }
      
      return(in_china_count / nrow(coords_matrix) > 0.5)
    }
  ),
  
  private = list(
    .coordinate_transformer = NULL,
    .projection_manager = NULL,
    .detection_rules = NULL,
    
    # Setup detection rules for different coordinate systems
    .setup_detection_rules = function() {
      private$.detection_rules <- list(
        WGS84 = list(
          lon_range = c(-180, 180),
          lat_range = c(-90, 90),
          china_offset_expected = FALSE,
          typical_precision = 6
        ),
        GCJ02 = list(
          lon_range = c(-180, 180),
          lat_range = c(-90, 90),
          china_offset_expected = TRUE,
          typical_precision = 6
        ),
        BD09 = list(
          lon_range = c(-180, 180),
          lat_range = c(-90, 90),
          china_offset_expected = TRUE,
          typical_precision = 6
        )
      )
    },
    
    # Extract coordinates from various data formats
    .extract_coordinates = function(data, lon_col = NULL, lat_col = NULL) {
      if (is.vector(data) && length(data) == 2) {
        return(matrix(data, nrow = 1))
      }
      
      if (is.matrix(data)) {
        if (ncol(data) < 2) {
          stop("Matrix must have at least 2 columns")
        }
        return(data[, 1:2, drop = FALSE])
      }
      
      if (is.data.frame(data)) {
        # Auto-detect coordinate columns if not specified
        if (is.null(lon_col) && is.null(lat_col)) {
          coord_cols <- private$.detect_coordinate_columns(data)
          lon_col <- coord_cols[1]
          lat_col <- coord_cols[2]
        }
        
        # Extract coordinates
        coords <- as.matrix(data[, c(lon_col, lat_col), drop = FALSE])
        
        # Remove rows with missing coordinates
        complete_rows <- complete.cases(coords)
        return(coords[complete_rows, , drop = FALSE])
      }
      
      stop("Unsupported data format. Use vector, matrix, or data.frame.")
    },
    
    # Detect coordinate columns in data frame
    .detect_coordinate_columns = function(data) {
      col_names <- names(data)
      
      # Look for common longitude column names
      lon_candidates <- c("lon", "lng", "longitude", "x", "X", "long")
      lat_candidates <- c("lat", "latitude", "y", "Y")
      
      lon_col <- NULL
      lat_col <- NULL
      
      # Find longitude column
      for (candidate in lon_candidates) {
        if (candidate %in% col_names) {
          lon_col <- candidate
          break
        }
      }
      
      # Find latitude column
      for (candidate in lat_candidates) {
        if (candidate %in% col_names) {
          lat_col <- candidate
          break
        }
      }
      
      # Fall back to first two numeric columns if not found
      if (is.null(lon_col) || is.null(lat_col)) {
        numeric_cols <- sapply(data, is.numeric)
        numeric_col_names <- names(data)[numeric_cols]
        
        if (length(numeric_col_names) >= 2) {
          lon_col <- numeric_col_names[1]
          lat_col <- numeric_col_names[2]
        } else {
          stop("Could not detect coordinate columns")
        }
      }
      
      return(c(lon_col, lat_col))
    },
    
    # Calculate detection scores for each CRS
    .calculate_detection_scores = function(coords) {
      scores <- numeric(3)
      names(scores) <- c("WGS84", "GCJ02", "BD09")
      
      # Basic range validation
      lon_range <- range(coords[, 1])
      lat_range <- range(coords[, 2])
      
      # All CRS should have valid coordinate ranges
      valid_ranges <- lon_range[1] >= -180 && lon_range[2] <= 180 &&
                     lat_range[1] >= -90 && lat_range[2] <= 90
      
      if (!valid_ranges) {
        # Invalid coordinate ranges - return low scores
        return(scores)
      }
      
      # Check if coordinates are in China region
      in_china <- self$is_china_region(coords)
      
      if (!in_china) {
        # Outside China - most likely WGS84
        scores["WGS84"] <- 0.9
        scores["GCJ02"] <- 0.1
        scores["BD09"] <- 0.1
      } else {
        # In China - need to distinguish between coordinate systems
        scores <- private$.analyze_china_coordinates(coords)
      }
      
      return(scores)
    },
    
    # Analyze coordinates in China to distinguish between CRS
    .analyze_china_coordinates = function(coords) {
      scores <- c(WGS84 = 0.33, GCJ02 = 0.33, BD09 = 0.33)
      
      # Sample a few points for transformation testing
      n_sample <- min(10, nrow(coords))
      sample_indices <- sample(nrow(coords), n_sample)
      sample_coords <- coords[sample_indices, , drop = FALSE]
      
      # Test transformations and see which makes most sense
      for (i in seq_len(nrow(sample_coords))) {
        point <- sample_coords[i, ]
        
        # Test if point could be WGS84 by transforming to GCJ02 and checking offset
        gcj02_from_wgs84 <- private$.coordinate_transformer$transform(point, "WGS84", "GCJ02")
        wgs84_gcj02_offset <- sqrt(sum((gcj02_from_wgs84 - point)^2))
        
        # Test if point could be GCJ02 by transforming to BD09 and checking offset
        bd09_from_gcj02 <- private$.coordinate_transformer$transform(point, "GCJ02", "BD09")
        gcj02_bd09_offset <- sqrt(sum((bd09_from_gcj02 - point)^2))
        
        # Test if point could be BD09 by transforming to GCJ02 and checking offset
        gcj02_from_bd09 <- private$.coordinate_transformer$transform(point, "BD09", "GCJ02")
        bd09_gcj02_offset <- sqrt(sum((gcj02_from_bd09 - point)^2))
        
        # Score based on expected transformation magnitudes
        # WGS84->GCJ02 typically has offset of 0.003-0.008 degrees
        # GCJ02->BD09 typically has offset of 0.004-0.009 degrees
        
        if (wgs84_gcj02_offset > 0.002 && wgs84_gcj02_offset < 0.01) {
          scores["WGS84"] <- scores["WGS84"] + 0.1
        }
        
        if (gcj02_bd09_offset > 0.003 && gcj02_bd09_offset < 0.01) {
          scores["GCJ02"] <- scores["GCJ02"] + 0.1
        }
        
        if (bd09_gcj02_offset > 0.003 && bd09_gcj02_offset < 0.01) {
          scores["BD09"] <- scores["BD09"] + 0.1
        }
      }
      
      # Normalize scores
      total_score <- sum(scores)
      if (total_score > 0) {
        scores <- scores / total_score
      }
      
      return(scores)
    },
    
    # Get China geographical bounds
    .get_china_bounds = function() {
      return(list(
        west = 72.004,
        east = 137.8347,
        south = 0.8293,
        north = 55.8271
      ))
    }
  )
)

#' Detect coordinate reference system
#'
#' Convenience function to detect the CRS of spatial data without creating a class instance.
#'
#' @param data Spatial data (data.frame, matrix, or vector)
#' @param lon_col Longitude column name/index (default: auto-detect)
#' @param lat_col Latitude column name/index (default: auto-detect)
#' @param confidence_threshold Minimum confidence for detection (default: 0.7)
#' @return List with detected CRS and confidence score
#'
#' @examples
#' \donttest{
#' # Detect CRS of coordinate data
#' data <- data.frame(lon = c(116.3974, 121.4737), lat = c(39.9093, 31.2304))
#' detection <- detect_coordinate_system(data)
#' print(paste("Detected CRS:", detection$crs, "with confidence:", detection$confidence))
#' }
#'
#' @export
detect_coordinate_system <- function(data, lon_col = NULL, lat_col = NULL, confidence_threshold = 0.7) {
  detector <- CoordinateDetector$new()
  return(detector$detect_crs(data, lon_col, lat_col, confidence_threshold))
}

#' Auto-transform data for provider
#'
#' Convenience function to automatically transform spatial data for a mapping provider.
#'
#' @param data Spatial data to transform
#' @param provider Target provider name
#' @param source_crs Source CRS (if NULL, will be auto-detected)
#' @param lon_col Longitude column name/index
#' @param lat_col Latitude column name/index
#' @return Transformed data appropriate for the provider
#'
#' @examples
#' \donttest{
#' # Auto-transform data for Gaode Maps
#' data <- data.frame(lon = c(116.3974, 121.4737), lat = c(39.9093, 31.2304))
#' gaode_data <- auto_transform_for_provider(data, "gaode")
#' }
#'
#' @export
auto_transform_for_provider <- function(data, provider, source_crs = NULL, lon_col = NULL, lat_col = NULL) {
  detector <- CoordinateDetector$new()
  return(detector$auto_transform_for_provider(data, provider, source_crs, lon_col, lat_col))
}