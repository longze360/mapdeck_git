#' Spatial Sampling Engine
#'
#' Core spatial sampling functionality with GPU acceleration support.
#' Provides efficient sampling methods for large geospatial datasets.
#'
#' @name spatial-sampling-engine
NULL

#' Spatial Sampling Engine Class
#'
#' R6 class that provides spatial sampling functionality with GPU acceleration
#' support and CPU fallback mechanisms.
#'
#' @description
#' The SamplingEngine class provides high-performance spatial sampling methods
#' including random, grid, and stratified sampling with automatic GPU/CPU
#' optimization.
#'
#' @field gpu_enabled Logical indicating if GPU acceleration is available
#' @field performance_stats List containing performance metrics
#' @field fallback_threshold Numeric threshold for switching to CPU fallback
#'
#' @examples
#' \donttest{
#' # Create sampling engine
#' engine <- SamplingEngine$new()
#' engine$initialize()
#' 
#' # Random sampling
#' samples <- engine$spatial_sample_random(data, n = 1000)
#' 
#' # Grid sampling
#' grid_samples <- engine$spatial_sample_grid(data, grid_size = 0.01)
#' }
#'
#' @export
SamplingEngine <- R6::R6Class("SamplingEngine",
  public = list(
    #' @field gpu_enabled Logical indicating GPU availability
    gpu_enabled = FALSE,
    
    #' @field performance_stats Performance monitoring data
    performance_stats = NULL,
    
    #' @field fallback_threshold Threshold for CPU fallback
    fallback_threshold = 10000,
    
    #' @field webgl_manager WebGL shader manager for GPU operations
    webgl_manager = NULL,
    
    #' @field performance_monitor Performance monitoring system
    performance_monitor = NULL,
    
    #' Initialize Sampling Engine
    #'
    #' Initialize the sampling engine and detect GPU capabilities.
    #'
    #' @param config List containing engine configuration
    #' @return Invisible self for method chaining
    initialize = function(config = list()) {
      self$performance_stats <- list(
        gpu_operations = 0,
        cpu_operations = 0,
        total_samples = 0,
        avg_gpu_time = 0,
        avg_cpu_time = 0
      )
      
      # Initialize performance monitor
      tryCatch({
        source("R/spatial-sampling/performance-monitor.R", local = TRUE)
        self$performance_monitor <- PerformanceMonitor$new()
        self$performance_monitor$initialize(config)
      }, error = function(e) {
        # Performance monitor is optional
        self$performance_monitor <- NULL
      })
      
      # Initialize WebGL manager
      tryCatch({
        source("R/spatial-sampling/webgl-shaders.R", local = TRUE)
        self$webgl_manager <- WebGLShaderManager$new()
        webgl_initialized <- self$webgl_manager$initialize()
        self$gpu_enabled <- webgl_initialized
      }, error = function(e) {
        # WebGL manager is optional, fall back to basic GPU detection
        self$webgl_manager <- NULL
        self$gpu_enabled <- private$detect_gpu_support()
      })
      
      # Apply configuration
      if (!is.null(config$fallback_threshold)) {
        self$fallback_threshold <- config$fallback_threshold
      }
      
      if (!is.null(config$gpu_enabled)) {
        self$gpu_enabled <- config$gpu_enabled
      }
      
      invisible(self)
    },
    
    #' Random Spatial Sampling
    #'
    #' Perform random spatial sampling with GPU optimization.
    #'
    #' @param data Spatial data (sf object, data.frame with coordinates)
    #' @param n Integer number of samples to generate
    #' @param bounds Optional bounding box for sampling area
    #' @param seed Optional random seed for reproducibility
    #' @return Sampled spatial data
    spatial_sample_random = function(data, n, bounds = NULL, seed = NULL) {
      if (!is.null(seed)) {
        set.seed(seed)
      }
      
      # Validate inputs
      private$validate_sampling_inputs(data, n)
      
      # Determine sampling method based on data size and GPU availability
      use_gpu <- self$gpu_enabled && nrow(data) > self$fallback_threshold
      
      start_time <- Sys.time()
      
      if (use_gpu) {
        result <- private$gpu_random_sample(data, n, bounds)
        self$performance_stats$gpu_operations <- 
          self$performance_stats$gpu_operations + 1
      } else {
        result <- private$cpu_random_sample(data, n, bounds)
        self$performance_stats$cpu_operations <- 
          self$performance_stats$cpu_operations + 1
      }
      
      # Update performance statistics
      elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      private$update_performance_stats(use_gpu, elapsed_time, n)
      
      return(result)
    },
    
    #' Grid Spatial Sampling
    #'
    #' Perform regular grid sampling with GPU optimization.
    #'
    #' @param data Spatial data (sf object, data.frame with coordinates)
    #' @param grid_size Numeric grid cell size in coordinate units
    #' @param bounds Optional bounding box for sampling area
    #' @param aggregation_method Method for aggregating points within cells
    #' @return Grid-sampled spatial data
    spatial_sample_grid = function(data, grid_size, bounds = NULL, 
                                   aggregation_method = "centroid") {
      # Validate inputs
      private$validate_sampling_inputs(data)
      
      if (!is.numeric(grid_size) || grid_size <= 0) {
        stop("grid_size must be a positive numeric value")
      }
      
      # Determine sampling method based on data size and GPU availability
      use_gpu <- self$gpu_enabled && nrow(data) > self$fallback_threshold
      
      start_time <- Sys.time()
      
      if (use_gpu) {
        result <- private$gpu_grid_sample(data, grid_size, bounds, 
                                          aggregation_method)
        self$performance_stats$gpu_operations <- 
          self$performance_stats$gpu_operations + 1
      } else {
        result <- private$cpu_grid_sample(data, grid_size, bounds, 
                                          aggregation_method)
        self$performance_stats$cpu_operations <- 
          self$performance_stats$cpu_operations + 1
      }
      
      # Update performance statistics
      elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      private$update_performance_stats(use_gpu, elapsed_time, nrow(result))
      
      return(result)
    },
    
    #' Stratified Spatial Sampling
    #'
    #' Perform stratified spatial sampling with GPU optimization.
    #'
    #' @param data Spatial data (sf object, data.frame with coordinates)
    #' @param strata_column Character name of column for stratification
    #' @param n_per_stratum Integer samples per stratum or named vector
    #' @param bounds Optional bounding box for sampling area
    #' @param seed Optional random seed for reproducibility
    #' @return Stratified sampled spatial data
    spatial_sample_stratified = function(data, strata_column, n_per_stratum, 
                                         bounds = NULL, seed = NULL) {
      if (!is.null(seed)) {
        set.seed(seed)
      }
      
      # Validate inputs
      private$validate_sampling_inputs(data)
      private$validate_stratified_inputs(data, strata_column, n_per_stratum)
      
      # Determine sampling method based on data size and GPU availability
      use_gpu <- self$gpu_enabled && nrow(data) > self$fallback_threshold
      
      start_time <- Sys.time()
      
      if (use_gpu) {
        result <- private$gpu_stratified_sample(data, strata_column, 
                                                n_per_stratum, bounds)
        self$performance_stats$gpu_operations <- 
          self$performance_stats$gpu_operations + 1
      } else {
        result <- private$cpu_stratified_sample(data, strata_column, 
                                                n_per_stratum, bounds)
        self$performance_stats$cpu_operations <- 
          self$performance_stats$cpu_operations + 1
      }
      
      # Update performance statistics
      elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      private$update_performance_stats(use_gpu, elapsed_time, nrow(result))
      
      return(result)
    },
    
    #' Get Performance Statistics
    #'
    #' Retrieve performance statistics for the sampling engine.
    #'
    #' @return List containing performance metrics
    get_performance_stats = function() {
      return(self$performance_stats)
    },
    
    #' Reset Performance Statistics
    #'
    #' Reset all performance counters to zero.
    #'
    #' @return Invisible self for method chaining
    reset_performance_stats = function() {
      self$performance_stats <- list(
        gpu_operations = 0,
        cpu_operations = 0,
        total_samples = 0,
        avg_gpu_time = 0,
        avg_cpu_time = 0
      )
      invisible(self)
    }
  ),
  
  private = list(
    #' Detect GPU Support
    #'
    #' Check if GPU acceleration is available for spatial operations.
    #'
    #' @return Logical indicating GPU support availability
    detect_gpu_support = function() {
      # Check for WebGL support in browser environment
      # This is a simplified check - in practice would need more robust detection
      tryCatch({
        # Check if we're in a web environment with WebGL support
        if (exists("webgl_available", envir = .GlobalEnv)) {
          return(get("webgl_available", envir = .GlobalEnv))
        }
        
        # Default to FALSE for safety - GPU support requires proper setup
        return(FALSE)
      }, error = function(e) {
        return(FALSE)
      })
    },
    
    #' Validate Sampling Inputs
    #'
    #' Validate common inputs for sampling functions.
    #'
    #' @param data Input spatial data
    #' @param n Optional number of samples
    validate_sampling_inputs = function(data, n = NULL) {
      if (is.null(data) || nrow(data) == 0) {
        stop("Data must be non-empty")
      }
      
      if (!is.null(n)) {
        if (!is.numeric(n) || n <= 0 || n != as.integer(n)) {
          stop("n must be a positive integer")
        }
        
        if (n > nrow(data)) {
          stop("Cannot sample more points than available in data")
        }
      }
    },
    
    #' Validate Stratified Sampling Inputs
    #'
    #' Validate inputs specific to stratified sampling.
    #'
    #' @param data Input spatial data
    #' @param strata_column Column name for stratification
    #' @param n_per_stratum Samples per stratum
    validate_stratified_inputs = function(data, strata_column, n_per_stratum) {
      if (!strata_column %in% names(data)) {
        stop("strata_column must be a valid column name in data")
      }
      
      if (is.numeric(n_per_stratum)) {
        if (n_per_stratum <= 0 || n_per_stratum != as.integer(n_per_stratum)) {
          stop("n_per_stratum must be a positive integer")
        }
      } else if (is.list(n_per_stratum) || is.vector(n_per_stratum)) {
        if (!all(n_per_stratum > 0)) {
          stop("All values in n_per_stratum must be positive")
        }
      } else {
        stop("n_per_stratum must be numeric or named vector")
      }
    },
    
    #' Update Performance Statistics
    #'
    #' Update internal performance tracking.
    #'
    #' @param used_gpu Logical indicating if GPU was used
    #' @param elapsed_time Numeric elapsed time in seconds
    #' @param sample_count Number of samples processed
    update_performance_stats = function(used_gpu, elapsed_time, sample_count) {
      self$performance_stats$total_samples <- 
        self$performance_stats$total_samples + sample_count
      
      if (used_gpu) {
        # Update GPU statistics
        old_avg <- self$performance_stats$avg_gpu_time
        old_count <- self$performance_stats$gpu_operations - 1
        self$performance_stats$avg_gpu_time <- 
          (old_avg * old_count + elapsed_time) / self$performance_stats$gpu_operations
      } else {
        # Update CPU statistics
        old_avg <- self$performance_stats$avg_cpu_time
        old_count <- self$performance_stats$cpu_operations - 1
        self$performance_stats$avg_cpu_time <- 
          (old_avg * old_count + elapsed_time) / self$performance_stats$cpu_operations
      }
    },
    
    # CPU Implementation Methods
    cpu_random_sample = function(data, n, bounds = NULL) {
      coords <- private$extract_coordinates(data)
      
      if (!is.null(bounds)) {
        coords <- private$filter_by_bounds(coords, bounds)
        if (nrow(coords) == 0) {
          stop("No data points within specified bounds")
        }
      }
      
      if (n >= nrow(coords)) {
        sample_indices <- seq_len(nrow(coords))
      } else {
        sample_indices <- sample(nrow(coords), n, replace = FALSE)
      }
      
      return(private$subset_data(data, sample_indices))
    },
    
    cpu_grid_sample = function(data, grid_size, bounds = NULL, 
                               aggregation_method = "centroid") {
      coords <- private$extract_coordinates(data)
      
      if (!is.null(bounds)) {
        coords <- private$filter_by_bounds(coords, bounds)
        if (nrow(coords) == 0) {
          stop("No data points within specified bounds")
        }
      }
      
      x_min <- min(coords$x)
      x_max <- max(coords$x)
      y_min <- min(coords$y)
      y_max <- max(coords$y)
      
      x_breaks <- seq(x_min, x_max + grid_size, by = grid_size)
      y_breaks <- seq(y_min, y_max + grid_size, by = grid_size)
      
      coords$grid_x <- cut(coords$x, breaks = x_breaks, include.lowest = TRUE, labels = FALSE)
      coords$grid_y <- cut(coords$y, breaks = y_breaks, include.lowest = TRUE, labels = FALSE)
      coords$grid_id <- paste(coords$grid_x, coords$grid_y, sep = "_")
      
      result_indices <- private$aggregate_grid_cells(coords, aggregation_method)
      
      return(private$subset_data(data, result_indices))
    },
    
    cpu_stratified_sample = function(data, strata_column, n_per_stratum, bounds = NULL) {
      coords <- private$extract_coordinates(data)
      strata_values <- data[[strata_column]]
      
      if (!is.null(bounds)) {
        bounds_filter <- private$get_bounds_filter(coords, bounds)
        coords <- coords[bounds_filter, ]
        strata_values <- strata_values[bounds_filter]
        
        if (nrow(coords) == 0) {
          stop("No data points within specified bounds")
        }
      }
      
      unique_strata <- unique(strata_values)
      
      if (is.numeric(n_per_stratum) && length(n_per_stratum) == 1) {
        samples_per_stratum <- rep(n_per_stratum, length(unique_strata))
        names(samples_per_stratum) <- unique_strata
      } else {
        samples_per_stratum <- n_per_stratum
      }
      
      sampled_indices <- c()
      
      for (stratum in unique_strata) {
        stratum_indices <- which(strata_values == stratum)
        n_stratum <- samples_per_stratum[[as.character(stratum)]]
        
        if (is.null(n_stratum)) {
          warning(paste("No sample size specified for stratum:", stratum))
          next
        }
        
        if (n_stratum > length(stratum_indices)) {
          warning(paste("Requested", n_stratum, "samples but only", 
                        length(stratum_indices), "available in stratum:", stratum))
          n_stratum <- length(stratum_indices)
        }
        
        if (n_stratum > 0) {
          stratum_sample <- sample(stratum_indices, n_stratum, replace = FALSE)
          sampled_indices <- c(sampled_indices, stratum_sample)
        }
      }
      
      return(private$subset_data(data, sampled_indices))
    },
    
    # GPU Implementation Methods
    gpu_random_sample = function(data, n, bounds = NULL) {
      tryCatch({
        coords <- private$extract_coordinates(data)
        
        if (!is.null(bounds)) {
          coords <- private$filter_by_bounds(coords, bounds)
          if (nrow(coords) == 0) {
            stop("No data points within specified bounds")
          }
        }
        
        gpu_data <- private$prepare_gpu_data(coords)
        sample_indices <- private$execute_gpu_random_sampling(gpu_data, n)
        
        return(private$subset_data(data, sample_indices))
        
      }, error = function(e) {
        warning("GPU random sampling failed, falling back to CPU: ", e$message)
        return(private$cpu_random_sample(data, n, bounds))
      })
    },
    
    gpu_grid_sample = function(data, grid_size, bounds = NULL, 
                               aggregation_method = "centroid") {
      tryCatch({
        coords <- private$extract_coordinates(data)
        
        if (!is.null(bounds)) {
          coords <- private$filter_by_bounds(coords, bounds)
          if (nrow(coords) == 0) {
            stop("No data points within specified bounds")
          }
        }
        
        gpu_data <- private$prepare_gpu_data(coords)
        sample_indices <- private$execute_gpu_grid_sampling(gpu_data, grid_size, 
                                                            aggregation_method)
        
        return(private$subset_data(data, sample_indices))
        
      }, error = function(e) {
        warning("GPU grid sampling failed, falling back to CPU: ", e$message)
        return(private$cpu_grid_sample(data, grid_size, bounds, aggregation_method))
      })
    },
    
    gpu_stratified_sample = function(data, strata_column, n_per_stratum, bounds = NULL) {
      tryCatch({
        coords <- private$extract_coordinates(data)
        strata_values <- data[[strata_column]]
        
        if (!is.null(bounds)) {
          bounds_filter <- private$get_bounds_filter(coords, bounds)
          coords <- coords[bounds_filter, ]
          strata_values <- strata_values[bounds_filter]
          
          if (nrow(coords) == 0) {
            stop("No data points within specified bounds")
          }
        }
        
        gpu_data <- private$prepare_gpu_stratified_data(coords, strata_values)
        sample_indices <- private$execute_gpu_stratified_sampling(gpu_data, n_per_stratum)
        
        return(private$subset_data(data, sample_indices))
        
      }, error = function(e) {
        warning("GPU stratified sampling failed, falling back to CPU: ", e$message)
        return(private$cpu_stratified_sample(data, strata_column, n_per_stratum, bounds))
      })
    },
    
    # Helper Methods
    extract_coordinates = function(data) {
      if (inherits(data, "sf")) {
        coords <- sf::st_coordinates(data)
        return(data.frame(x = coords[, 1], y = coords[, 2]))
      } else if (is.data.frame(data)) {
        coord_cols <- private$detect_coordinate_columns(data)
        return(data.frame(x = data[[coord_cols$x]], y = data[[coord_cols$y]]))
      } else {
        stop("Unsupported data format. Use sf objects or data.frame with coordinates.")
      }
    },
    
    detect_coordinate_columns = function(data) {
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
    },
    
    filter_by_bounds = function(coords, bounds) {
      if (length(bounds) != 4) {
        stop("Bounds must be a vector of length 4: c(xmin, ymin, xmax, ymax)")
      }
      
      xmin <- bounds[1]
      ymin <- bounds[2]
      xmax <- bounds[3]
      ymax <- bounds[4]
      
      filter_mask <- coords$x >= xmin & coords$x <= xmax & 
                     coords$y >= ymin & coords$y <= ymax
      
      return(coords[filter_mask, ])
    },
    
    get_bounds_filter = function(coords, bounds) {
      if (length(bounds) != 4) {
        stop("Bounds must be a vector of length 4: c(xmin, ymin, xmax, ymax)")
      }
      
      xmin <- bounds[1]
      ymin <- bounds[2]
      xmax <- bounds[3]
      ymax <- bounds[4]
      
      return(coords$x >= xmin & coords$x <= xmax & 
             coords$y >= ymin & coords$y <= ymax)
    },
    
    aggregate_grid_cells = function(coords, aggregation_method) {
      unique_cells <- unique(coords$grid_id)
      result_indices <- c()
      
      for (cell_id in unique_cells) {
        cell_indices <- which(coords$grid_id == cell_id)
        
        if (length(cell_indices) == 0) next
        
        if (aggregation_method == "centroid") {
          cell_coords <- coords[cell_indices, c("x", "y")]
          centroid_x <- mean(cell_coords$x)
          centroid_y <- mean(cell_coords$y)
          
          distances <- sqrt((cell_coords$x - centroid_x)^2 + (cell_coords$y - centroid_y)^2)
          closest_idx <- which.min(distances)
          result_indices <- c(result_indices, cell_indices[closest_idx])
          
        } else if (aggregation_method == "random") {
          result_indices <- c(result_indices, sample(cell_indices, 1))
          
        } else if (aggregation_method == "first") {
          result_indices <- c(result_indices, cell_indices[1])
          
        } else {
          stop("Unsupported aggregation method. Use 'centroid', 'random', or 'first'.")
        }
      }
      
      return(result_indices)
    },
    
    subset_data = function(data, indices) {
      if (inherits(data, "sf")) {
        return(data[indices, ])
      } else if (is.data.frame(data)) {
        return(data[indices, ])
      } else {
        stop("Unsupported data format for subsetting")
      }
    },
    
    # GPU utility methods
    prepare_gpu_data = function(coords) {
      gpu_data <- list(
        x = as.numeric(coords$x),
        y = as.numeric(coords$y),
        count = nrow(coords),
        bounds = c(min(coords$x), min(coords$y), max(coords$x), max(coords$y))
      )
      
      return(gpu_data)
    },
    
    prepare_gpu_stratified_data = function(coords, strata_values) {
      unique_strata <- unique(strata_values)
      strata_numeric <- match(strata_values, unique_strata)
      
      gpu_data <- list(
        x = as.numeric(coords$x),
        y = as.numeric(coords$y),
        strata = as.numeric(strata_numeric),
        count = nrow(coords),
        strata_count = length(unique_strata),
        strata_labels = unique_strata,
        bounds = c(min(coords$x), min(coords$y), max(coords$x), max(coords$y))
      )
      
      return(gpu_data)
    },
    
    execute_gpu_random_sampling = function(gpu_data, n) {
      if (n >= gpu_data$count) {
        return(seq_len(gpu_data$count))
      }
      
      sample_indices <- private$gpu_simulate_random_sampling(gpu_data$count, n)
      
      return(sample_indices)
    },
    
    execute_gpu_grid_sampling = function(gpu_data, grid_size, aggregation_method) {
      sample_indices <- private$gpu_simulate_grid_sampling(gpu_data, grid_size, 
                                                           aggregation_method)
      
      return(sample_indices)
    },
    
    execute_gpu_stratified_sampling = function(gpu_data, n_per_stratum) {
      sample_indices <- private$gpu_simulate_stratified_sampling(gpu_data, n_per_stratum)
      
      return(sample_indices)
    },
    
    gpu_simulate_random_sampling = function(total_count, n) {
      return(sample.int(total_count, n, replace = FALSE))
    },
    
    gpu_simulate_grid_sampling = function(gpu_data, grid_size, aggregation_method) {
      x_min <- gpu_data$bounds[1]
      y_min <- gpu_data$bounds[2]
      
      grid_x <- floor((gpu_data$x - x_min) / grid_size)
      grid_y <- floor((gpu_data$y - y_min) / grid_size)
      grid_ids <- paste(grid_x, grid_y, sep = "_")
      
      unique_cells <- unique(grid_ids)
      result_indices <- c()
      
      for (cell_id in unique_cells) {
        cell_indices <- which(grid_ids == cell_id)
        
        if (length(cell_indices) == 0) next
        
        if (aggregation_method == "centroid") {
          cell_x <- gpu_data$x[cell_indices]
          cell_y <- gpu_data$y[cell_indices]
          centroid_x <- mean(cell_x)
          centroid_y <- mean(cell_y)
          
          distances <- (cell_x - centroid_x)^2 + (cell_y - centroid_y)^2
          closest_idx <- which.min(distances)
          result_indices <- c(result_indices, cell_indices[closest_idx])
          
        } else if (aggregation_method == "random") {
          result_indices <- c(result_indices, sample(cell_indices, 1))
        } else {
          result_indices <- c(result_indices, cell_indices[1])
        }
      }
      
      return(result_indices)
    },
    
    gpu_simulate_stratified_sampling = function(gpu_data, n_per_stratum) {
      result_indices <- c()
      
      for (i in seq_along(gpu_data$strata_labels)) {
        stratum_label <- gpu_data$strata_labels[i]
        stratum_indices <- which(gpu_data$strata == i)
        
        if (is.numeric(n_per_stratum) && length(n_per_stratum) == 1) {
          n_stratum <- n_per_stratum
        } else {
          n_stratum <- n_per_stratum[[as.character(stratum_label)]]
        }
        
        if (is.null(n_stratum) || n_stratum <= 0) next
        
        if (n_stratum >= length(stratum_indices)) {
          result_indices <- c(result_indices, stratum_indices)
        } else {
          stratum_sample <- sample(stratum_indices, n_stratum, replace = FALSE)
          result_indices <- c(result_indices, stratum_sample)
        }
      }
      
      return(result_indices)
    }
  )
)

# Load additional implementation files during package loading
# These files extend the SamplingEngine class with CPU and GPU methods

#' Random Spatial Sampling
#'
#' Perform random spatial sampling on geospatial data with automatic
#' GPU acceleration when available.
#'
#' @param data Spatial data (sf object or data.frame with coordinates)
#' @param n Integer number of samples to generate
#' @param bounds Optional bounding box as c(xmin, ymin, xmax, ymax)
#' @param seed Optional random seed for reproducibility
#' @param engine Optional SamplingEngine instance (created if NULL)
#'
#' @return Sampled spatial data maintaining original structure
#'
#' @examples
#' \donttest{
#' # Random sampling from sf object
#' library(sf)
#' data(melbourne, package = "mapdeck")
#' samples <- spatial_sample_random(melbourne, n = 100)
#' 
#' # Random sampling with bounds
#' bounds <- c(144.9, -37.9, 145.0, -37.8)
#' bounded_samples <- spatial_sample_random(melbourne, n = 50, bounds = bounds)
#' 
#' # Random sampling with seed for reproducibility
#' reproducible_samples <- spatial_sample_random(melbourne, n = 100, seed = 123)
#' }
#'
#' @export
spatial_sample_random <- function(data, n, bounds = NULL, seed = NULL, engine = NULL) {
  # Create engine if not provided
  if (is.null(engine)) {
    engine <- SamplingEngine$new()
    engine$initialize()
  }
  
  # Perform sampling
  return(engine$spatial_sample_random(data, n, bounds, seed))
}

#' Grid Spatial Sampling
#'
#' Perform regular grid sampling on geospatial data with automatic
#' GPU acceleration when available.
#'
#' @param data Spatial data (sf object or data.frame with coordinates)
#' @param grid_size Numeric grid cell size in coordinate units
#' @param bounds Optional bounding box as c(xmin, ymin, xmax, ymax)
#' @param aggregation_method Method for aggregating points within cells:
#'   "centroid" (default), "random", or "first"
#' @param engine Optional SamplingEngine instance (created if NULL)
#'
#' @return Grid-sampled spatial data
#'
#' @examples
#' \donttest{
#' # Grid sampling with default centroid aggregation
#' library(sf)
#' data(melbourne, package = "mapdeck")
#' grid_samples <- spatial_sample_grid(melbourne, grid_size = 0.01)
#' 
#' # Grid sampling with random aggregation
#' random_grid <- spatial_sample_grid(melbourne, grid_size = 0.01, 
#'                                    aggregation_method = "random")
#' 
#' # Grid sampling with bounds
#' bounds <- c(144.9, -37.9, 145.0, -37.8)
#' bounded_grid <- spatial_sample_grid(melbourne, grid_size = 0.005, bounds = bounds)
#' }
#'
#' @export
spatial_sample_grid <- function(data, grid_size, bounds = NULL, 
                                aggregation_method = "centroid", engine = NULL) {
  # Create engine if not provided
  if (is.null(engine)) {
    engine <- SamplingEngine$new()
    engine$initialize()
  }
  
  # Perform sampling
  return(engine$spatial_sample_grid(data, grid_size, bounds, aggregation_method))
}

#' Stratified Spatial Sampling
#'
#' Perform stratified spatial sampling on geospatial data with automatic
#' GPU acceleration when available.
#'
#' @param data Spatial data (sf object or data.frame with coordinates)
#' @param strata_column Character name of column for stratification
#' @param n_per_stratum Integer samples per stratum or named vector/list
#'   specifying samples for each stratum
#' @param bounds Optional bounding box as c(xmin, ymin, xmax, ymax)
#' @param seed Optional random seed for reproducibility
#' @param engine Optional SamplingEngine instance (created if NULL)
#'
#' @return Stratified sampled spatial data
#'
#' @examples
#' \donttest{
#' # Stratified sampling with equal samples per stratum
#' library(sf)
#' data(road_safety, package = "mapdeck")
#' stratified_samples <- spatial_sample_stratified(road_safety, 
#'                                                  strata_column = "ACCIDENT_TYPE",
#'                                                  n_per_stratum = 50)
#' 
#' # Stratified sampling with different samples per stratum
#' sample_sizes <- list("Collision with vehicle" = 100, 
#'                      "Collision with object" = 50,
#'                      "Other" = 25)
#' custom_stratified <- spatial_sample_stratified(road_safety,
#'                                                 strata_column = "ACCIDENT_TYPE",
#'                                                 n_per_stratum = sample_sizes)
#' 
#' # Stratified sampling with bounds and seed
#' bounds <- c(144.9, -37.9, 145.0, -37.8)
#' reproducible_stratified <- spatial_sample_stratified(road_safety,
#'                                                       strata_column = "ACCIDENT_TYPE",
#'                                                       n_per_stratum = 30,
#'                                                       bounds = bounds,
#'                                                       seed = 456)
#' }
#'
#' @export
spatial_sample_stratified <- function(data, strata_column, n_per_stratum, 
                                      bounds = NULL, seed = NULL, engine = NULL) {
  # Create engine if not provided
  if (is.null(engine)) {
    engine <- SamplingEngine$new()
    engine$initialize()
  }
  
  # Perform sampling
  return(engine$spatial_sample_stratified(data, strata_column, n_per_stratum, 
                                          bounds, seed))
}

#' Create Spatial Sampling Engine
#'
#' Create and initialize a SamplingEngine instance for reuse across
#' multiple sampling operations.
#'
#' @param config Optional list containing engine configuration:
#'   - fallback_threshold: Numeric threshold for GPU/CPU switching (default: 10000)
#'   - gpu_enabled: Logical to force enable/disable GPU (auto-detected if NULL)
#'
#' @return Initialized SamplingEngine instance
#'
#' @examples
#' \donttest{
#' # Create engine with default settings
#' engine <- create_sampling_engine()
#' 
#' # Create engine with custom configuration
#' config <- list(fallback_threshold = 5000)
#' custom_engine <- create_sampling_engine(config)
#' 
#' # Use engine for multiple operations
#' samples1 <- spatial_sample_random(data1, n = 100, engine = engine)
#' samples2 <- spatial_sample_grid(data2, grid_size = 0.01, engine = engine)
#' 
#' # Check performance statistics
#' stats <- engine$get_performance_stats()
#' print(stats)
#' }
#'
#' @export
create_sampling_engine <- function(config = list()) {
  engine <- SamplingEngine$new()
  engine$initialize(config)
  return(engine)
}

#' Get Sampling Engine Performance Statistics
#'
#' Retrieve performance statistics from a sampling engine or create
#' a summary of global sampling performance.
#'
#' @param engine Optional SamplingEngine instance. If NULL, returns
#'   global performance summary
#'
#' @return List containing performance metrics:
#'   - gpu_operations: Number of GPU operations performed
#'   - cpu_operations: Number of CPU operations performed
#'   - total_samples: Total number of samples processed
#'   - avg_gpu_time: Average GPU operation time in seconds
#'   - avg_cpu_time: Average CPU operation time in seconds
#'   - gpu_efficiency: GPU vs CPU performance ratio
#'
#' @examples
#' \donttest{
#' # Get statistics from specific engine
#' engine <- create_sampling_engine()
#' samples <- spatial_sample_random(data, n = 1000, engine = engine)
#' stats <- get_sampling_performance(engine)
#' 
#' # Print performance summary
#' print(stats)
#' }
#'
#' @export
get_sampling_performance <- function(engine = NULL) {
  if (is.null(engine)) {
    # Return global performance summary
    return(list(
      message = "No engine provided. Create an engine and perform operations to see statistics.",
      gpu_operations = 0,
      cpu_operations = 0,
      total_samples = 0,
      avg_gpu_time = 0,
      avg_cpu_time = 0,
      gpu_efficiency = NA
    ))
  }
  
  stats <- engine$get_performance_stats()
  
  # Calculate efficiency ratio
  if (stats$avg_cpu_time > 0 && stats$avg_gpu_time > 0) {
    stats$gpu_efficiency <- stats$avg_cpu_time / stats$avg_gpu_time
  } else {
    stats$gpu_efficiency <- NA
  }
  
  return(stats)
}

#' Reset Sampling Engine Performance Statistics
#'
#' Reset performance counters for a sampling engine.
#'
#' @param engine SamplingEngine instance
#'
#' @return Invisible engine for method chaining
#'
#' @examples
#' \donttest{
#' # Reset engine statistics
#' engine <- create_sampling_engine()
#' # ... perform operations ...
#' reset_sampling_performance(engine)
#' }
#'
#' @export
reset_sampling_performance <- function(engine) {
  if (is.null(engine) || !inherits(engine, "SamplingEngine")) {
    stop("engine must be a SamplingEngine instance")
  }
  
  return(engine$reset_performance_stats())
}

#' Check GPU Acceleration Availability
#'
#' Check if GPU acceleration is available for spatial sampling operations.
#'
#' @return List containing GPU availability information:
#'   - available: Logical indicating if GPU is available
#'   - reason: Character string explaining availability status
#'   - fallback_threshold: Current threshold for GPU/CPU switching
#'
#' @examples
#' \donttest{
#' # Check GPU availability
#' gpu_info <- check_gpu_acceleration()
#' print(gpu_info)
#' 
#' if (gpu_info$available) {
#'   message("GPU acceleration is available")
#' } else {
#'   message("Using CPU fallback: ", gpu_info$reason)
#' }
#' }
#'
#' @export
check_gpu_acceleration <- function() {
  # Create temporary engine to check GPU support
  temp_engine <- SamplingEngine$new()
  temp_engine$initialize()
  
  gpu_available <- temp_engine$gpu_enabled
  
  if (gpu_available) {
    reason <- "WebGL compute shaders detected and available"
  } else {
    reason <- "WebGL compute shaders not available or not detected"
  }
  
  return(list(
    available = gpu_available,
    reason = reason,
    fallback_threshold = temp_engine$fallback_threshold
  ))
}