#' Proportional Sampling for Administrative Regions
#'
#' Enhanced proportional sampling functionality specifically designed for
#' common administrative region sampling scenarios, including ratio-based
#' sampling using numeric ratios based on variables like population, cases, etc.
#'
#' @name proportional-sampler
NULL

#' Proportional Regional Sampler Class
#'
#' R6 class that provides enhanced proportional sampling functionality
#' specifically designed for administrative region sampling with support
#' for numeric ratio-based sampling scenarios.
#'
#' @description
#' The ProportionalRegionalSampler class provides methods for generating
#' samples based on regional variables (population, cases, etc.) with
#' support for numeric sampling ratios like 1.0, 0.1, 0.01, 2.0, etc.
#'
#' @export
ProportionalRegionalSampler <- R6::R6Class("ProportionalRegionalSampler",
  public = list(
    #' @field sampling_ratio Numeric sampling ratio (e.g., 0.1 for 10% sampling)
    sampling_ratio = NULL,
    
    #' @field base_sampler Underlying administrative sampler
    base_sampler = NULL,
    
    #' Initialize Proportional Regional Sampler
    #'
    #' Initialize the proportional regional sampling system.
    #'
    #' @param config Optional configuration list
    #' @return Invisible self for method chaining
    initialize = function(config = list()) {
      # Initialize base sampler
      self$base_sampler <- AdministrativeSampler$new()
      self$base_sampler$initialize(config)
      
      # Set default sampling ratio (1:1)
      self$sampling_ratio <- config$sampling_ratio %||% 1.0
      
      invisible(self)
    },
    
    #' Sample by Ratio
    #'
    #' Generate samples based on a specified numeric ratio relative to a variable.
    #'
    #' @param admin_polygons sf object with administrative polygons
    #' @param variable_column Column name containing the variable to sample from
    #' @param ratio Numeric sampling ratio (e.g., 0.1 for 1:10, 1.0 for 1:1, 2.0 for 2:1)
    #' @param min_samples Minimum samples per region (default: 1)
    #' @param max_samples Maximum samples per region (default: Inf)
    #' @param admin_column Column name for administrative unit identifiers
    #' @param concurrent Logical for concurrent processing
    #' @param seed Optional random seed for reproducibility
    #' @return sf object with sampled points
    sample_by_ratio = function(admin_polygons, variable_column, 
                               ratio = 1.0,
                               min_samples = 1, max_samples = Inf,
                               admin_column = NULL, concurrent = FALSE,
                               seed = NULL) {
      # Validate inputs
      private$validate_ratio_inputs(admin_polygons, variable_column, 
                                    ratio, min_samples, max_samples)
      
      # Calculate sample counts based on ratio
      sample_counts <- private$calculate_ratio_samples(
        admin_polygons, variable_column, ratio, 
        min_samples, max_samples
      )
      
      # Calculate total samples needed
      total_samples <- sum(sample_counts)
      
      # Create weight column for custom allocation
      admin_polygons$sampling_weights <- sample_counts
      
      # Use base sampler with custom allocation
      result <- self$base_sampler$sample_administrative(
        admin_polygons = admin_polygons,
        total_samples = total_samples,
        allocation_method = "custom",
        weight_column = "sampling_weights",
        admin_column = admin_column,
        concurrent = concurrent,
        seed = seed
      )
      
      # Add sampling metadata
      if (nrow(result) > 0) {
        admin_col_name <- admin_column %||% names(admin_polygons)[1]
        if (admin_col_name == attr(admin_polygons, "sf_column")) {
          admin_col_name <- names(admin_polygons)[2]
        }
        
        result$original_variable <- admin_polygons[[variable_column]][
          match(result[[admin_col_name]], admin_polygons[[admin_col_name]])
        ]
        result$sampling_ratio <- ratio
        result$expected_samples <- sample_counts[
          match(result[[admin_col_name]], names(sample_counts))
        ]
      }
      
      return(result)
    },
    
    #' Sample Population Proportional
    #'
    #' Convenience function for population-based proportional sampling.
    #'
    #' @param admin_polygons sf object with administrative polygons
    #' @param population_column Column name containing population data
    #' @param ratio Numeric sampling ratio (default: 0.001 for 1:1000)
    #' @param min_samples Minimum samples per region (default: 1)
    #' @param max_samples Maximum samples per region (default: 1000)
    #' @param admin_column Column name for administrative unit identifiers
    #' @param concurrent Logical for concurrent processing
    #' @param seed Optional random seed for reproducibility
    #' @return sf object with sampled points
    sample_population_proportional = function(admin_polygons, population_column,
                                              ratio = 0.001,
                                              min_samples = 1, max_samples = 1000,
                                              admin_column = NULL, 
                                              concurrent = FALSE, seed = NULL) {
      result <- self$sample_by_ratio(
        admin_polygons = admin_polygons,
        variable_column = population_column,
        ratio = ratio,
        min_samples = min_samples,
        max_samples = max_samples,
        admin_column = admin_column,
        concurrent = concurrent,
        seed = seed
      )
      
      # Add population-specific metadata
      result$sampling_type <- "population_proportional"
      
      return(result)
    },
    
    #' Sample Case Proportional
    #'
    #' Convenience function for case-based proportional sampling (e.g., disease cases).
    #'
    #' @param admin_polygons sf object with administrative polygons
    #' @param case_column Column name containing case data
    #' @param ratio Numeric sampling ratio (default: 0.1 for 1:10)
    #' @param min_samples Minimum samples per region (default: 1)
    #' @param max_samples Maximum samples per region (default: 100)
    #' @param admin_column Column name for administrative unit identifiers
    #' @param concurrent Logical for concurrent processing
    #' @param seed Optional random seed for reproducibility
    #' @return sf object with sampled points
    sample_case_proportional = function(admin_polygons, case_column,
                                        ratio = 0.1,
                                        min_samples = 1, max_samples = 100,
                                        admin_column = NULL, 
                                        concurrent = FALSE, seed = NULL) {
      result <- self$sample_by_ratio(
        admin_polygons = admin_polygons,
        variable_column = case_column,
        ratio = ratio,
        min_samples = min_samples,
        max_samples = max_samples,
        admin_column = admin_column,
        concurrent = concurrent,
        seed = seed
      )
      
      # Add case-specific metadata
      result$sampling_type <- "case_proportional"
      
      return(result)
    },
    
    #' Get Sampling Summary
    #'
    #' Get summary statistics for the sampling operation.
    #'
    #' @param sampled_data sf object with sampled points (from sampling functions)
    #' @return List containing sampling summary statistics
    get_sampling_summary = function(sampled_data) {
      if (is.null(sampled_data) || nrow(sampled_data) == 0) {
        return(list(message = "No sampled data provided"))
      }
      
      # Group by administrative unit
      admin_col <- names(sampled_data)[1]  # First column is usually admin ID
      sample_counts <- table(sampled_data[[admin_col]])
      
      summary_stats <- list(
        total_samples = nrow(sampled_data),
        regions_sampled = length(sample_counts),
        samples_per_region = as.list(sample_counts),
        sampling_ratio = unique(sampled_data$sampling_ratio)[1],
        sampling_type = unique(sampled_data$sampling_type)[1] %||% "general"
      )
      
      # Add ratio value if available
      if ("ratio_value" %in% names(sampled_data)) {
        summary_stats$ratio_value <- unique(sampled_data$ratio_value)[1]
      }
      
      # Calculate efficiency metrics
      if ("expected_samples" %in% names(sampled_data) && 
          "original_variable" %in% names(sampled_data)) {
        
        # Group by admin unit for efficiency calculation
        admin_summary <- aggregate(
          cbind(expected_samples, original_variable) ~ get(admin_col),
          data = sampled_data,
          FUN = function(x) x[1]  # Take first value (should be same for each admin)
        )
        
        summary_stats$efficiency <- list(
          total_variable_value = sum(admin_summary$original_variable),
          total_expected_samples = sum(admin_summary$expected_samples),
          actual_samples = nrow(sampled_data),
          sampling_efficiency = nrow(sampled_data) / sum(admin_summary$expected_samples)
        )
      }
      
      return(summary_stats)
    }
  ),
  
  private = list(
    # Validate Ratio Sampling Inputs
    validate_ratio_inputs = function(admin_polygons, variable_column, 
                                     ratio, min_samples, max_samples) {
      if (!variable_column %in% names(admin_polygons)) {
        stop("variable_column must be a valid column in admin_polygons")
      }
      
      if (!is.numeric(ratio) || ratio <= 0) {
        stop("ratio must be a positive numeric value")
      }
      
      if (!is.numeric(min_samples) || min_samples < 0) {
        stop("min_samples must be a non-negative numeric value")
      }
      
      if (!is.numeric(max_samples) || max_samples < min_samples) {
        stop("max_samples must be numeric and >= min_samples")
      }
      
      # Check variable column
      variable_values <- admin_polygons[[variable_column]]
      if (!is.numeric(variable_values)) {
        stop("variable_column must contain numeric values")
      }
      
      if (any(variable_values < 0, na.rm = TRUE)) {
        stop("variable_column must contain non-negative values")
      }
    },
    
    # Calculate Ratio-based Sample Counts
    calculate_ratio_samples = function(admin_polygons, variable_column,
                                       ratio, min_samples, max_samples) {
      variable_values <- admin_polygons[[variable_column]]
      
      # Calculate raw sample counts based on ratio
      raw_samples <- variable_values * ratio
      
      # Apply min/max constraints
      sample_counts <- pmax(min_samples, pmin(max_samples, round(raw_samples)))
      
      # Handle zero values
      sample_counts[is.na(variable_values) | variable_values == 0] <- min_samples
      
      # Create named vector
      admin_col <- names(admin_polygons)[1]  # First non-geometry column
      if (admin_col == attr(admin_polygons, "sf_column")) {
        admin_col <- names(admin_polygons)[2]  # Skip geometry column
      }
      
      names(sample_counts) <- as.character(admin_polygons[[admin_col]])
      
      return(sample_counts)
    }
  )
)

#' Proportional Administrative Sampling by Ratio
#'
#' Generate samples within administrative boundaries based on a variable
#' using specified numeric sampling ratios.
#'
#' @param admin_polygons sf object containing administrative boundary polygons
#' @param variable_column Character column name containing the variable to sample from
#' @param ratio Numeric sampling ratio (e.g., 0.1 for 10%, 0.01 for 1%, 2.0 for 200%)
#' @param min_samples Integer minimum samples per region (default: 1)
#' @param max_samples Integer maximum samples per region (default: Inf)
#' @param admin_column Character column name for administrative unit identifiers
#' @param concurrent Logical whether to use concurrent processing
#' @param seed Integer optional random seed for reproducibility
#'
#' @return sf object with sampled points including sampling metadata
#'
#' @details
#' This function is specifically designed for common administrative region
#' sampling scenarios where samples are needed proportional to a variable
#' (like population, disease cases, etc.) at specified numeric ratios:
#' 
#' - 1.0 - One sample per unit of the variable (1:1)
#' - 0.1 - One sample per 10 units of the variable (1:10)  
#' - 0.01 - One sample per 100 units of the variable (1:100)
#' - 0.001 - One sample per 1000 units of the variable (1:1000)
#' - 2.0 - Two samples per unit of the variable (2:1)
#' 
#' The function ensures minimum and maximum sample constraints are respected
#' and provides detailed metadata about the sampling process.
#'
#' @examples
#' \donttest{
#' # Create sample administrative data with population
#' library(sf)
#' coords_list <- list(
#'   matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE),
#'   matrix(c(1, 0, 3, 0, 3, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE),
#'   matrix(c(0, 1, 2, 1, 2, 2.5, 0, 2.5, 0, 1), ncol = 2, byrow = TRUE)
#' )
#' polygons <- lapply(coords_list, function(x) st_polygon(list(x)))
#' admin_data <- data.frame(
#'   admin_id = c("A", "B", "C"),
#'   population = c(1000, 5000, 2000),
#'   cases = c(10, 50, 20)
#' )
#' admin_sf <- st_sf(admin_data, geometry = st_sfc(polygons), crs = 4326)
#' 
#' # 0.001 population sampling (1 sample per 1000 people)
#' pop_samples <- spatial_sample_proportional(
#'   admin_sf, 
#'   variable_column = "population",
#'   ratio = 0.001
#' )
#' 
#' # 0.1 case sampling (1 sample per 10 cases)
#' case_samples <- spatial_sample_proportional(
#'   admin_sf,
#'   variable_column = "cases", 
#'   ratio = 0.1
#' )
#' 
#' # 1.0 sampling with constraints (1:1 ratio)
#' constrained_samples <- spatial_sample_proportional(
#'   admin_sf,
#'   variable_column = "cases",
#'   ratio = 1.0,
#'   min_samples = 2,
#'   max_samples = 10
#' )
#' }
#'
#' @export
spatial_sample_proportional <- function(admin_polygons, variable_column,
                                         ratio = 1.0,
                                         min_samples = 1, max_samples = Inf,
                                         admin_column = NULL, concurrent = FALSE,
                                         seed = NULL) {
  # Create proportional sampler
  sampler <- ProportionalRegionalSampler$new()
  sampler$initialize()
  
  # Perform proportional sampling
  result <- sampler$sample_by_ratio(
    admin_polygons = admin_polygons,
    variable_column = variable_column,
    ratio = ratio,
    min_samples = min_samples,
    max_samples = max_samples,
    admin_column = admin_column,
    concurrent = concurrent,
    seed = seed
  )
  
  return(result)
}

#' Population Proportional Sampling
#'
#' Convenience function for population-based proportional sampling.
#'
#' @param admin_polygons sf object containing administrative boundary polygons
#' @param population_column Character column name containing population data
#' @param ratio Numeric sampling ratio (default: 0.001 for 1:1000)
#' @param min_samples Integer minimum samples per region (default: 1)
#' @param max_samples Integer maximum samples per region (default: 1000)
#' @param admin_column Character column name for administrative unit identifiers
#' @param concurrent Logical whether to use concurrent processing
#' @param seed Integer optional random seed for reproducibility
#'
#' @return sf object with sampled points including sampling metadata
#'
#' @examples
#' \donttest{
#' # Population-based sampling (1 sample per 1000 people)
#' pop_samples <- spatial_sample_population(
#'   admin_sf, 
#'   population_column = "population",
#'   ratio = 0.001
#' )
#' }
#'
#' @export
spatial_sample_population <- function(admin_polygons, population_column,
                                      ratio = 0.001,
                                      min_samples = 1, max_samples = 1000,
                                      admin_column = NULL, concurrent = FALSE,
                                      seed = NULL) {
  # Create proportional sampler
  sampler <- ProportionalRegionalSampler$new()
  sampler$initialize()
  
  # Perform population proportional sampling
  result <- sampler$sample_population_proportional(
    admin_polygons = admin_polygons,
    population_column = population_column,
    ratio = ratio,
    min_samples = min_samples,
    max_samples = max_samples,
    admin_column = admin_column,
    concurrent = concurrent,
    seed = seed
  )
  
  return(result)
}

#' Case Proportional Sampling
#'
#' Convenience function for case-based proportional sampling (e.g., disease cases).
#'
#' @param admin_polygons sf object containing administrative boundary polygons
#' @param case_column Character column name containing case data
#' @param ratio Numeric sampling ratio (default: 0.1 for 1:10)
#' @param min_samples Integer minimum samples per region (default: 1)
#' @param max_samples Integer maximum samples per region (default: 100)
#' @param admin_column Character column name for administrative unit identifiers
#' @param concurrent Logical whether to use concurrent processing
#' @param seed Integer optional random seed for reproducibility
#'
#' @return sf object with sampled points including sampling metadata
#'
#' @examples
#' \donttest{
#' # Case-based sampling (1 sample per 10 cases)
#' case_samples <- spatial_sample_cases(
#'   admin_sf,
#'   case_column = "cases",
#'   ratio = 0.1
#' )
#' }
#'
#' @export
spatial_sample_cases <- function(admin_polygons, case_column,
                                  ratio = 0.1,
                                  min_samples = 1, max_samples = 100,
                                  admin_column = NULL, concurrent = FALSE,
                                  seed = NULL) {
  # Create proportional sampler
  sampler <- ProportionalRegionalSampler$new()
  sampler$initialize()
  
  # Perform case proportional sampling
  result <- sampler$sample_case_proportional(
    admin_polygons = admin_polygons,
    case_column = case_column,
    ratio = ratio,
    min_samples = min_samples,
    max_samples = max_samples,
    admin_column = admin_column,
    concurrent = concurrent,
    seed = seed
  )
  
  return(result)
}

#' Null-coalescing operator
#'
#' @param x First value
#' @param y Default value if x is NULL
#' @return x if not NULL, otherwise y
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
