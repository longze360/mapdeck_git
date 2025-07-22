#' Projection Management System
#'
#' Manages map projections and coordinate system transformations for different
#' mapping providers. This class handles the complexity of working with different
#' coordinate reference systems and ensures proper projection handling.
#'
#' @details
#' The ProjectionManager provides a unified interface for handling map projections
#' across different providers. It automatically detects appropriate projections
#' and handles transformations between different coordinate systems.
#'
#' @examples
#' \donttest{
#' # Create projection manager
#' proj_manager <- ProjectionManager$new()
#' 
#' # Get projection for a provider
#' mapbox_proj <- proj_manager$get_provider_projection("mapbox")
#' gaode_proj <- proj_manager$get_provider_projection("gaode")
#' 
#' # Transform data for a specific provider
#' data <- data.frame(lon = c(116.3974, 121.4737), lat = c(39.9093, 31.2304))
#' transformed_data <- proj_manager$prepare_data_for_provider(data, "gaode")
#' }
#'
#' @export
ProjectionManager <- R6::R6Class("ProjectionManager",
  public = list(
    #' @description
    #' Initialize the projection manager
    initialize = function() {
      private$.setup_provider_projections()
      private$.coordinate_transformer <- CoordinateTransformer$new()
    },
    
    #' @description
    #' Get the coordinate system used by a specific provider
    #' @param provider Provider name ("mapbox", "leaflet", "openlayers", "gaode", "baidu")
    #' @return Character string of coordinate system ("WGS84", "GCJ02", "BD09")
    get_provider_projection = function(provider) {
      provider <- tolower(provider)
      if (!provider %in% names(private$.provider_projections)) {
        stop(sprintf("Unknown provider: %s. Supported providers: %s", 
                     provider, paste(names(private$.provider_projections), collapse = ", ")))
      }
      return(private$.provider_projections[[provider]])
    },
    
    #' @description
    #' Get all supported providers and their projections
    #' @return Named list of provider projections
    get_all_provider_projections = function() {
      return(private$.provider_projections)
    },
    
    #' @description
    #' Prepare spatial data for a specific provider by transforming coordinates
    #' @param data Data frame or matrix with coordinate columns
    #' @param provider Target provider name
    #' @param lon_col Name or index of longitude column (default: "lon" or 1)
    #' @param lat_col Name or index of latitude column (default: "lat" or 2)
    #' @param source_crs Source coordinate system (default: "WGS84")
    #' @return Data with coordinates transformed for the target provider
    prepare_data_for_provider = function(data, provider, lon_col = NULL, lat_col = NULL, source_crs = "WGS84") {
      # Validate inputs
      if (!is.data.frame(data) && !is.matrix(data)) {
        stop("Data must be a data frame or matrix")
      }
      
      target_crs <- self$get_provider_projection(provider)
      
      # If source and target CRS are the same, return data unchanged
      if (source_crs == target_crs) {
        return(data)
      }
      
      # Detect coordinate columns
      coord_cols <- private$.detect_coordinate_columns(data, lon_col, lat_col)
      
      # Extract coordinates
      if (is.data.frame(data)) {
        coords <- as.matrix(data[, coord_cols, drop = FALSE])
        result_data <- data
      } else {
        coords <- data[, coord_cols, drop = FALSE]
        result_data <- data
      }
      
      # Transform coordinates
      transformed_coords <- private$.coordinate_transformer$transform(coords, source_crs, target_crs)
      
      # Update data with transformed coordinates
      if (is.data.frame(result_data)) {
        result_data[, coord_cols[1]] <- transformed_coords[, 1]
        result_data[, coord_cols[2]] <- transformed_coords[, 2]
      } else {
        result_data[, coord_cols[1]] <- transformed_coords[, 1]
        result_data[, coord_cols[2]] <- transformed_coords[, 2]
      }
      
      return(result_data)
    },
    
    #' @description
    #' Transform coordinates from one provider's system to another
    #' @param coords Coordinates to transform (vector or matrix)
    #' @param from_provider Source provider name
    #' @param to_provider Target provider name
    #' @return Transformed coordinates
    transform_between_providers = function(coords, from_provider, to_provider) {
      from_crs <- self$get_provider_projection(from_provider)
      to_crs <- self$get_provider_projection(to_provider)
      
      return(private$.coordinate_transformer$transform(coords, from_crs, to_crs))
    },
    
    #' @description
    #' Check if a provider requires coordinate transformation from WGS84
    #' @param provider Provider name
    #' @return Logical indicating if transformation is needed
    requires_transformation = function(provider) {
      provider_crs <- self$get_provider_projection(provider)
      return(provider_crs != "WGS84")
    },
    
    #' @description
    #' Get supported providers
    #' @return Character vector of supported provider names
    get_supported_providers = function() {
      return(names(private$.provider_projections))
    },
    
    #' @description
    #' Validate that coordinates are appropriate for a provider
    #' @param coords Coordinates to validate
    #' @param provider Provider name
    #' @param tolerance_meters Tolerance for validation in meters (default: 1.0)
    #' @return Logical indicating if coordinates are valid
    validate_coordinates_for_provider = function(coords, provider, tolerance_meters = 1.0) {
      provider_crs <- self$get_provider_projection(provider)
      
      # For WGS84, check basic coordinate bounds
      if (provider_crs == "WGS84") {
        return(private$.validate_wgs84_bounds(coords))
      }
      
      # For other systems, transform to WGS84 and back to check accuracy
      if (is.vector(coords) && length(coords) == 2) {
        coords_matrix <- matrix(coords, nrow = 1)
      } else {
        coords_matrix <- coords
      }
      
      # Transform to WGS84 and back
      wgs84_coords <- private$.coordinate_transformer$transform(coords_matrix, provider_crs, "WGS84")
      back_transformed <- private$.coordinate_transformer$transform(wgs84_coords, "WGS84", provider_crs)
      
      # Check if transformation is accurate
      for (i in seq_len(nrow(coords_matrix))) {
        original <- coords_matrix[i, ]
        back_trans <- back_transformed[i, ]
        
        if (!private$.coordinate_transformer$validate_accuracy(original, back_trans, provider_crs, provider_crs, tolerance_meters)) {
          return(FALSE)
        }
      }
      
      return(TRUE)
    }
  ),
  
  private = list(
    .provider_projections = NULL,
    .coordinate_transformer = NULL,
    
    # Setup provider projection mappings
    .setup_provider_projections = function() {
      private$.provider_projections <- list(
        "mapbox" = "WGS84",
        "leaflet" = "WGS84", 
        "openlayers" = "WGS84",
        "gaode" = "GCJ02",
        "baidu" = "BD09"
      )
    },
    
    # Detect coordinate columns in data
    .detect_coordinate_columns = function(data, lon_col, lat_col) {
      if (is.null(lon_col) && is.null(lat_col)) {
        # Auto-detect coordinate columns
        if (is.data.frame(data)) {
          col_names <- names(data)
          
          # Look for common longitude column names
          lon_candidates <- c("lon", "lng", "longitude", "x", "X")
          lat_candidates <- c("lat", "latitude", "y", "Y")
          
          lon_col <- NULL
          lat_col <- NULL
          
          for (candidate in lon_candidates) {
            if (candidate %in% col_names) {
              lon_col <- candidate
              break
            }
          }
          
          for (candidate in lat_candidates) {
            if (candidate %in% col_names) {
              lat_col <- candidate
              break
            }
          }
          
          if (is.null(lon_col) || is.null(lat_col)) {
            # Fall back to first two numeric columns
            numeric_cols <- sapply(data, is.numeric)
            numeric_col_names <- names(data)[numeric_cols]
            
            if (length(numeric_col_names) >= 2) {
              lon_col <- numeric_col_names[1]
              lat_col <- numeric_col_names[2]
            } else {
              stop("Could not detect coordinate columns. Please specify lon_col and lat_col parameters.")
            }
          }
        } else {
          # For matrices, use first two columns
          if (ncol(data) < 2) {
            stop("Data matrix must have at least 2 columns for coordinates")
          }
          lon_col <- 1
          lat_col <- 2
        }
      }
      
      # Validate column existence
      if (is.data.frame(data)) {
        if (is.character(lon_col) && !lon_col %in% names(data)) {
          stop(sprintf("Longitude column '%s' not found in data", lon_col))
        }
        if (is.character(lat_col) && !lat_col %in% names(data)) {
          stop(sprintf("Latitude column '%s' not found in data", lat_col))
        }
      } else {
        if (is.numeric(lon_col) && (lon_col < 1 || lon_col > ncol(data))) {
          stop(sprintf("Longitude column index %d is out of bounds", lon_col))
        }
        if (is.numeric(lat_col) && (lat_col < 1 || lat_col > ncol(data))) {
          stop(sprintf("Latitude column index %d is out of bounds", lat_col))
        }
      }
      
      return(c(lon_col, lat_col))
    },
    
    # Validate WGS84 coordinate bounds
    .validate_wgs84_bounds = function(coords) {
      if (is.vector(coords) && length(coords) == 2) {
        coords <- matrix(coords, nrow = 1)
      }
      
      lon <- coords[, 1]
      lat <- coords[, 2]
      
      # Check longitude bounds (-180 to 180)
      if (any(lon < -180 | lon > 180)) {
        return(FALSE)
      }
      
      # Check latitude bounds (-90 to 90)
      if (any(lat < -90 | lat > 90)) {
        return(FALSE)
      }
      
      return(TRUE)
    }
  )
)

#' Get provider coordinate system
#'
#' Convenience function to get the coordinate system used by a mapping provider.
#'
#' @param provider Provider name ("mapbox", "leaflet", "openlayers", "gaode", "baidu")
#' @return Character string of coordinate system ("WGS84", "GCJ02", "BD09")
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
  proj_manager <- ProjectionManager$new()
  return(proj_manager$get_provider_projection(provider))
}

#' Prepare data for mapping provider
#'
#' Convenience function to transform spatial data for a specific mapping provider.
#'
#' @param data Data frame or matrix with coordinate columns
#' @param provider Target provider name
#' @param lon_col Name or index of longitude column (default: auto-detect)
#' @param lat_col Name or index of latitude column (default: auto-detect)
#' @param source_crs Source coordinate system (default: "WGS84")
#' @return Data with coordinates transformed for the target provider
#'
#' @examples
#' \donttest{
#' # Prepare data for Gaode Maps (requires GCJ02 coordinates)
#' data <- data.frame(lon = c(116.3974, 121.4737), lat = c(39.9093, 31.2304))
#' gaode_data <- prepare_data_for_provider(data, "gaode")
#' 
#' # Prepare data with custom column names
#' custom_data <- data.frame(longitude = 116.3974, latitude = 39.9093)
#' gaode_custom <- prepare_data_for_provider(custom_data, "gaode", 
#'                                          lon_col = "longitude", 
#'                                          lat_col = "latitude")
#' }
#'
#' @export
prepare_data_for_provider <- function(data, provider, lon_col = NULL, lat_col = NULL, source_crs = "WGS84") {
  proj_manager <- ProjectionManager$new()
  return(proj_manager$prepare_data_for_provider(data, provider, lon_col, lat_col, source_crs))
}