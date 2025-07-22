#' Administrative Boundary Sampling
#'
#' This file contains the implementation of spatial sampling within
#' administrative boundaries with support for different allocation strategies.
#'
#' @name administrative-sampler
NULL

#' Administrative Sampler Class
#'
#' R6 class that provides spatial sampling functionality within administrative
#' boundaries with support for proportional, equal, and custom allocation
#' strategies.
#'
#' @description
#' The AdministrativeSampler class provides methods for generating random points
#' within administrative boundaries with different allocation strategies.
#'
#' @field allocation_strategy Strategy for allocating samples across units
#' @field boundary_validator Validator to ensure points fall within boundaries
#' @field concurrent_processor Processor for parallel operations
#'
#' @export
AdministrativeSampler <- R6::R6Class("AdministrativeSampler",
  public = list(
    #' @field allocation_strategy Strategy for sample allocation
    allocation_strategy = NULL,

    #' @field boundary_validator Validator for boundary constraints
    boundary_validator = NULL,

    #' @field concurrent_processor Processor for parallel operations
    concurrent_processor = NULL,

    #' @field sampling_engine Core sampling engine
    sampling_engine = NULL,

    #' Initialize Administrative Sampler
    #'
    #' Initialize the administrative sampling system.
    #'
    #' @param config Optional configuration list
    #' @return Invisible self for method chaining
    initialize = function(config = list()) {
      # Initialize sampling engine
      self$sampling_engine <- SamplingEngine$new()
      self$sampling_engine$initialize(config)

      # Initialize allocation strategy
      allocation_method <- config$allocation_method %||% "proportional"
      self$allocation_strategy <- private$create_allocation_strategy(
        allocation_method
      )

      # Initialize boundary validator
      self$boundary_validator <- private$create_boundary_validator()

      # Initialize concurrent processor if enabled
      if (!is.null(config$concurrent) && config$concurrent) {
        self$concurrent_processor <- private$create_concurrent_processor(config)
      }

      invisible(self)
    },

    #' Sample Within Administrative Boundaries
    #'
    #' Generate random points within administrative boundaries using the
    #' specified allocation strategy.
    #'
    #' @param admin_polygons sf object with administrative polygons
    #' @param total_samples Total number of samples to generate
    #' @param allocation_method Method for allocating samples
    #' @param weight_column Column name for custom weights (optional)
    #' @param admin_column Column name for administrative unit identifiers
    #' @param concurrent Logical for concurrent processing
    #' @param use_gpu Logical for GPU acceleration
    #' @param seed Optional random seed for reproducibility
    #' @return sf object with sampled points
    sample_administrative = function(admin_polygons, total_samples,
                                     allocation_method = "proportional",
                                     weight_column = NULL,
                                     admin_column = NULL,
                                     concurrent = FALSE,
                                     use_gpu = TRUE,
                                     seed = NULL) {
      # Validate inputs
      private$validate_inputs(admin_polygons, total_samples, allocation_method,
                              weight_column, admin_column)

      # Set random seed if provided
      if (!is.null(seed)) {
        set.seed(seed)
      }

      # Prepare administrative data
      admin_data <- private$prepare_admin_data(admin_polygons, admin_column)

      # Update allocation strategy if needed
      private$update_allocation_strategy(allocation_method)

      # Calculate sample allocation
      sample_counts <- self$allocation_strategy$allocate_samples(
        admin_data$polygons,
        total_samples,
        weight_column
      )

      # Generate samples
      samples <- private$generate_samples(
        admin_data, sample_counts, concurrent, use_gpu
      )

      # Validate boundaries
      samples <- private$validate_boundaries(samples, admin_data$polygons)

      return(samples)
    },

    #' Set Allocation Strategy
    #'
    #' Set the allocation strategy for distributing samples.
    #'
    #' @param method Allocation method
    #' @return Invisible self for method chaining
    set_allocation_strategy = function(method) {
      self$allocation_strategy <- private$create_allocation_strategy(method)
      invisible(self)
    },

    #' Enable Concurrent Processing
    #'
    #' Enable concurrent processing for administrative sampling.
    #'
    #' @param config Configuration for concurrent processor
    #' @return Invisible self for method chaining
    enable_concurrent_processing = function(config = list()) {
      self$concurrent_processor <- private$create_concurrent_processor(config)
      invisible(self)
    },

    #' Disable Concurrent Processing
    #'
    #' Disable concurrent processing for administrative sampling.
    #'
    #' @return Invisible self for method chaining
    disable_concurrent_processing = function() {
      self$concurrent_processor <- NULL
      invisible(self)
    }
  ),

  private = list(
    #' Validate Inputs
    #'
    #' Validate inputs for administrative sampling.
    #'
    #' @param admin_polygons sf object with administrative polygons
    #' @param total_samples Total number of samples to generate
    #' @param allocation_method Method for allocating samples
    #' @param weight_column Column name for custom weights
    #' @param admin_column Column name for administrative unit identifiers
    validate_inputs = function(admin_polygons, total_samples, allocation_method,
                               weight_column, admin_column) {
      private$validate_admin_polygons(admin_polygons)
      private$validate_total_samples(total_samples)
      private$validate_allocation_method(allocation_method)
      private$validate_weight_column(admin_polygons, allocation_method,
                                     weight_column)
      private$validate_admin_column(admin_polygons, admin_column)
    },

    #' Validate Administrative Polygons
    #'
    #' @param admin_polygons sf object with administrative polygons
    validate_admin_polygons = function(admin_polygons) {
      if (is.null(admin_polygons) || nrow(admin_polygons) == 0) {
        stop("admin_polygons must be non-empty")
      }

      if (!inherits(admin_polygons, "sf")) {
        stop("admin_polygons must be an sf object")
      }
    },

    #' Validate Total Samples
    #'
    #' @param total_samples Total number of samples to generate
    validate_total_samples = function(total_samples) {
      if (!is.numeric(total_samples) || total_samples <= 0 ||
          total_samples != as.integer(total_samples)) {
        stop("total_samples must be a positive integer")
      }
    },

    #' Validate Allocation Method
    #'
    #' @param allocation_method Method for allocating samples
    validate_allocation_method = function(allocation_method) {
      valid_methods <- c("proportional", "equal", "custom")
      if (!allocation_method %in% valid_methods) {
        stop("allocation_method must be one of: ",
             paste(valid_methods, collapse = ", "))
      }
    },

    #' Validate Weight Column
    #'
    #' @param admin_polygons sf object with administrative polygons
    #' @param allocation_method Method for allocating samples
    #' @param weight_column Column name for custom weights
    validate_weight_column = function(admin_polygons, allocation_method,
                                      weight_column) {
      if (allocation_method == "custom" && is.null(weight_column)) {
        stop("weight_column must be specified when using custom allocation")
      }

      if (!is.null(weight_column) &&
          !weight_column %in% names(admin_polygons)) {
        stop("weight_column must be a valid column in admin_polygons")
      }
    },

    #' Validate Admin Column
    #'
    #' @param admin_polygons sf object with administrative polygons
    #' @param admin_column Column name for administrative unit identifiers
    validate_admin_column = function(admin_polygons, admin_column) {
      if (!is.null(admin_column) &&
          !admin_column %in% names(admin_polygons)) {
        stop("admin_column must be a valid column in admin_polygons")
      }
    },

    #' Prepare Administrative Data
    #'
    #' @param admin_polygons sf object with administrative polygons
    #' @param admin_column Column name for administrative unit identifiers
    #' @return List with prepared administrative data
    prepare_admin_data = function(admin_polygons, admin_column) {
      # Detect admin column if not specified
      if (is.null(admin_column)) {
        admin_column <- private$detect_admin_column(admin_polygons)
      }

      # Ensure admin column exists
      if (!admin_column %in% names(admin_polygons)) {
        admin_polygons$admin_id <- seq_len(nrow(admin_polygons))
        admin_column <- "admin_id"
      }

      list(
        polygons = admin_polygons,
        admin_column = admin_column
      )
    },

    #' Detect Administrative Column
    #'
    #' @param admin_polygons sf object with administrative polygons
    #' @return Name of the administrative column
    detect_admin_column = function(admin_polygons) {
      admin_patterns <- c("id", "name", "code", "admin", "region",
                          "district", "area")

      for (pattern in admin_patterns) {
        matches <- grep(pattern, names(admin_polygons), ignore.case = TRUE)
        if (length(matches) > 0) {
          return(names(admin_polygons)[matches[1]])
        }
      }

      # Use first non-geometry column
      geom_col <- attr(admin_polygons, "sf_column")
      non_geom_cols <- setdiff(names(admin_polygons), geom_col)
      if (length(non_geom_cols) > 0) {
        return(non_geom_cols[1])
      }

      return("admin_id")
    },

    #' Update Allocation Strategy
    #'
    #' @param allocation_method Method for allocating samples
    update_allocation_strategy = function(allocation_method) {
      if (is.null(self$allocation_strategy) ||
          self$allocation_strategy$method != allocation_method) {
        self$allocation_strategy <- private$create_allocation_strategy(
          allocation_method
        )
      }
    },

    #' Generate Samples
    #'
    #' @param admin_data Prepared administrative data
    #' @param sample_counts Named vector of sample counts per unit
    #' @param concurrent Logical for concurrent processing
    #' @param use_gpu Logical for GPU acceleration
    #' @return sf object with sampled points
    generate_samples = function(admin_data, sample_counts, concurrent,
                                use_gpu) {
      if (concurrent && !is.null(self$concurrent_processor)) {
        return(self$concurrent_processor$process_units(
          admin_data$polygons,
          sample_counts,
          admin_data$admin_column,
          use_gpu
        ))
      } else {
        return(private$process_units_sequentially(
          admin_data$polygons,
          sample_counts,
          admin_data$admin_column,
          use_gpu
        ))
      }
    },

    #' Validate Boundaries
    #'
    #' @param samples sf object with sampled points
    #' @param admin_polygons sf object with administrative polygons
    #' @return Validated samples
    validate_boundaries = function(samples, admin_polygons) {
      if (!is.null(self$boundary_validator)) {
        return(self$boundary_validator$validate_points(samples,
                                                       admin_polygons))
      }
      return(samples)
    },

    #' Create Allocation Strategy
    #'
    #' @param method Allocation method
    #' @return AllocationStrategy object
    create_allocation_strategy = function(method) {
      switch(method,
        "proportional" = ProportionalAllocationStrategy$new(),
        "equal" = EqualAllocationStrategy$new(),
        "custom" = CustomAllocationStrategy$new(),
        stop("Unsupported allocation method: ", method)
      )
    },

    #' Create Boundary Validator
    #'
    #' @return BoundaryValidator object
    create_boundary_validator = function() {
      BoundaryValidator$new()
    },

    #' Create Concurrent Processor
    #'
    #' @param config Configuration for concurrent processor
    #' @return ConcurrentProcessor object
    create_concurrent_processor = function(config) {
      tryCatch({
        source("R/spatial-sampling/concurrent-processor.R", local = TRUE)
        processor <- ConcurrentProcessor$new()
        processor$initialize(config)
        return(processor)
      }, error = function(e) {
        warning("Failed to create concurrent processor: ", e$message)
        return(NULL)
      })
    },

    #' Process Units Sequentially
    #'
    #' @param admin_polygons sf object with administrative polygons
    #' @param sample_counts Named vector of sample counts per unit
    #' @param admin_column Column name for administrative unit identifiers
    #' @param use_gpu Logical for GPU acceleration
    #' @return sf object with sampled points
    process_units_sequentially = function(admin_polygons, sample_counts,
                                          admin_column, use_gpu) {
      all_samples <- list()

      for (i in seq_len(nrow(admin_polygons))) {
        unit_samples <- private$process_single_unit(
          admin_polygons[i, ], sample_counts, admin_column, use_gpu
        )

        if (!is.null(unit_samples) && nrow(unit_samples) > 0) {
          all_samples[[i]] <- unit_samples
        }
      }

      return(private$combine_samples(all_samples, admin_polygons))
    },

    #' Process Single Administrative Unit
    #'
    #' @param polygon sf object with a single polygon
    #' @param sample_counts Named vector of sample counts per unit
    #' @param admin_column Column name for administrative unit identifiers
    #' @param use_gpu Logical for GPU acceleration
    #' @return sf object with sampled points for this unit
    process_single_unit = function(polygon, sample_counts, admin_column,
                                   use_gpu) {
      admin_id <- as.character(polygon[[admin_column]])
      n_samples <- sample_counts[[admin_id]]

      if (is.null(n_samples) || n_samples <= 0) {
        return(NULL)
      }

      # Generate random points within polygon
      unit_samples <- private$generate_points_in_polygon(polygon, n_samples,
                                                          use_gpu)

      # Add administrative unit information
      if (!is.null(unit_samples) && nrow(unit_samples) > 0) {
        unit_samples <- sf::st_sf(
          data.frame(admin_id = admin_id),
          geometry = sf::st_geometry(unit_samples)
        )
        names(unit_samples)[1] <- admin_column
      }

      return(unit_samples)
    },

    #' Combine Samples
    #'
    #' @param all_samples List of sample sf objects
    #' @param admin_polygons sf object with administrative polygons
    #' @return Combined sf object
    combine_samples = function(all_samples, admin_polygons) {
      if (length(all_samples) == 0) {
        return(sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs(admin_polygons))))
      }

      combined_samples <- do.call(rbind, all_samples)
      return(combined_samples)
    },

    #' Generate Points in Polygon
    #'
    #' @param polygon sf object with a single polygon
    #' @param n_samples Number of samples to generate
    #' @param use_gpu Logical for GPU acceleration
    #' @return sf object with sampled points
    generate_points_in_polygon = function(polygon, n_samples, use_gpu) {
      bbox <- sf::st_bbox(polygon)
      bounds <- c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])

      # Use rejection sampling to generate points within polygon
      points_in_polygon <- private$rejection_sampling(polygon, bounds,
                                                      n_samples)

      return(points_in_polygon)
    },

    #' Rejection Sampling for Polygon
    #'
    #' @param polygon sf object with a single polygon
    #' @param bounds Bounding box as c(xmin, ymin, xmax, ymax)
    #' @param n_samples Number of samples to generate
    #' @return sf object with sampled points
    rejection_sampling = function(polygon, bounds, n_samples) {
      points_in_polygon <- NULL
      attempts <- 1
      max_attempts <- 5

      while ((is.null(points_in_polygon) || nrow(points_in_polygon) < n_samples) &&
             attempts <= max_attempts) {
        # Generate candidate points
        n_candidates <- n_samples * 2 * attempts
        candidate_points <- private$generate_random_points_in_bounds(bounds,
                                                                     n_candidates)

        # Filter points within polygon
        within_mask <- sf::st_within(candidate_points, polygon, sparse = FALSE)
        new_points <- candidate_points[within_mask, ]

        # Combine with existing points
        if (is.null(points_in_polygon)) {
          points_in_polygon <- new_points
        } else if (nrow(new_points) > 0) {
          points_in_polygon <- rbind(points_in_polygon, new_points)
        }

        attempts <- attempts + 1
      }

      return(private$finalize_polygon_samples(points_in_polygon, n_samples))
    },

    #' Finalize Polygon Samples
    #'
    #' @param points_in_polygon sf object with points in polygon
    #' @param n_samples Number of samples requested
    #' @return Final sf object with correct number of samples
    finalize_polygon_samples = function(points_in_polygon, n_samples) {
      if (is.null(points_in_polygon) || nrow(points_in_polygon) == 0) {
        return(NULL)
      }

      if (nrow(points_in_polygon) < n_samples) {
        warning(sprintf(
          "Could only generate %d of %d requested points in polygon",
          nrow(points_in_polygon), n_samples
        ))
        return(points_in_polygon)
      }

      # Subsample to get exactly n_samples
      if (nrow(points_in_polygon) > n_samples) {
        sample_indices <- sample(nrow(points_in_polygon), n_samples)
        points_in_polygon <- points_in_polygon[sample_indices, ]
      }

      return(points_in_polygon)
    },

    #' Generate Random Points in Bounds
    #'
    #' @param bounds Bounding box as c(xmin, ymin, xmax, ymax)
    #' @param n Number of points to generate
    #' @return sf object with random points
    generate_random_points_in_bounds = function(bounds, n) {
      x <- runif(n, bounds[1], bounds[3])
      y <- runif(n, bounds[2], bounds[4])

      points <- sf::st_as_sf(
        data.frame(x = x, y = y),
        coords = c("x", "y"),
        crs = 4326  # Default to WGS84
      )

      return(points)
    }
  )
)

#' Allocation Strategy Interface
#'
#' Base class for allocation strategies.
#'
#' @description
#' The AllocationStrategy class defines the interface for allocation strategies.
#'
#' @export
AllocationStrategy <- R6::R6Class("AllocationStrategy",
  public = list(
    #' @field method Allocation method name
    method = NULL,

    #' Allocate Samples
    #'
    #' Allocate samples across administrative units.
    #'
    #' @param admin_polygons sf object with administrative polygons
    #' @param total_samples Total number of samples to generate
    #' @param weight_column Column name for custom weights (optional)
    #' @return Named vector of sample counts per unit
    allocate_samples = function(admin_polygons, total_samples,
                                weight_column = NULL) {
      stop("Method not implemented in base class")
    }
  )
)

#' Proportional Allocation Strategy
#'
#' Allocate samples proportionally to polygon area.
#'
#' @description
#' The ProportionalAllocationStrategy class allocates samples proportionally
#' to the area of each administrative unit.
#'
#' @export
ProportionalAllocationStrategy <- R6::R6Class("ProportionalAllocationStrategy",
  inherit = AllocationStrategy,
  public = list(
    #' @field method Allocation method name
    method = "proportional",

    #' Allocate Samples
    #'
    #' Allocate samples across administrative units proportionally to area.
    #'
    #' @param admin_polygons sf object with administrative polygons
    #' @param total_samples Total number of samples to generate
    #' @param weight_column Column name for custom weights (optional)
    #' @return Named vector of sample counts per unit
    allocate_samples = function(admin_polygons, total_samples,
                                weight_column = NULL) {
      weights <- private$get_weights(admin_polygons, weight_column)
      sample_counts <- private$calculate_proportional_allocation(weights,
                                                                 total_samples)
      return(private$create_named_sample_counts(admin_polygons, sample_counts))
    }
  ),

  private = list(
    #' Get Weights for Allocation
    #'
    #' @param admin_polygons sf object with administrative polygons
    #' @param weight_column Column name for custom weights
    #' @return Numeric vector of weights
    get_weights = function(admin_polygons, weight_column) {
      if (is.null(weight_column)) {
        # Calculate areas in square meters
        areas <- sf::st_area(admin_polygons)
        return(as.numeric(areas))
      } else {
        return(admin_polygons[[weight_column]])
      }
    },

    #' Calculate Proportional Allocation
    #'
    #' @param weights Numeric vector of weights
    #' @param total_samples Total number of samples
    #' @return Integer vector of sample counts
    calculate_proportional_allocation = function(weights, total_samples) {
      proportions <- weights / sum(weights)
      sample_counts <- floor(proportions * total_samples)

      # Distribute remaining samples to largest units
      remaining <- total_samples - sum(sample_counts)
      if (remaining > 0) {
        sorted_indices <- order(weights, decreasing = TRUE)
        for (i in seq_len(min(remaining, length(sorted_indices)))) {
          sample_counts[sorted_indices[i]] <- sample_counts[sorted_indices[i]] + 1
        }
      }

      return(sample_counts)
    },

    #' Create Named Sample Counts
    #'
    #' @param admin_polygons sf object with administrative polygons
    #' @param sample_counts Integer vector of sample counts
    #' @return Named vector of sample counts
    create_named_sample_counts = function(admin_polygons, sample_counts) {
      admin_ids <- as.character(admin_polygons[[1]])
      names(sample_counts) <- admin_ids
      return(sample_counts)
    }
  )
)

#' Equal Allocation Strategy
#'
#' Allocate samples equally across administrative units.
#'
#' @description
#' The EqualAllocationStrategy class allocates samples equally
#' across all administrative units.
#'
#' @export
EqualAllocationStrategy <- R6::R6Class("EqualAllocationStrategy",
  inherit = AllocationStrategy,
  public = list(
    #' @field method Allocation method name
    method = "equal",

    #' Allocate Samples
    #'
    #' Allocate samples equally across administrative units.
    #'
    #' @param admin_polygons sf object with administrative polygons
    #' @param total_samples Total number of samples to generate
    #' @param weight_column Column name for custom weights (ignored)
    #' @return Named vector of sample counts per unit
    allocate_samples = function(admin_polygons, total_samples,
                                weight_column = NULL) {
      n_units <- nrow(admin_polygons)
      sample_counts <- private$calculate_equal_allocation(n_units,
                                                          total_samples)
      return(private$create_named_sample_counts(admin_polygons, sample_counts))
    }
  ),

  private = list(
    #' Calculate Equal Allocation
    #'
    #' @param n_units Number of administrative units
    #' @param total_samples Total number of samples
    #' @return Integer vector of sample counts
    calculate_equal_allocation = function(n_units, total_samples) {
      base_samples <- floor(total_samples / n_units)
      remaining <- total_samples - (base_samples * n_units)

      sample_counts <- rep(base_samples, n_units)

      # Distribute remaining samples
      if (remaining > 0) {
        sample_counts[1:remaining] <- sample_counts[1:remaining] + 1
      }

      return(sample_counts)
    },

    #' Create Named Sample Counts
    #'
    #' @param admin_polygons sf object with administrative polygons
    #' @param sample_counts Integer vector of sample counts
    #' @return Named vector of sample counts
    create_named_sample_counts = function(admin_polygons, sample_counts) {
      admin_ids <- as.character(admin_polygons[[1]])
      names(sample_counts) <- admin_ids
      return(sample_counts)
    }
  )
)

#' Custom Allocation Strategy
#'
#' Allocate samples based on custom weights.
#'
#' @description
#' The CustomAllocationStrategy class allocates samples based on
#' custom weights provided in a specified column.
#'
#' @export
CustomAllocationStrategy <- R6::R6Class("CustomAllocationStrategy",
  inherit = AllocationStrategy,
  public = list(
    #' @field method Allocation method name
    method = "custom",

    #' Allocate Samples
    #'
    #' Allocate samples based on custom weights.
    #'
    #' @param admin_polygons sf object with administrative polygons
    #' @param total_samples Total number of samples to generate
    #' @param weight_column Column name for custom weights (required)
    #' @return Named vector of sample counts per unit
    allocate_samples = function(admin_polygons, total_samples,
                                weight_column = NULL) {
      private$validate_weight_column(admin_polygons, weight_column)
      weights <- private$get_and_validate_weights(admin_polygons, weight_column)
      sample_counts <- private$calculate_custom_allocation(weights,
                                                           total_samples)
      return(private$create_named_sample_counts(admin_polygons, sample_counts))
    }
  ),

  private = list(
    #' Validate Weight Column
    #'
    #' @param admin_polygons sf object with administrative polygons
    #' @param weight_column Column name for custom weights
    validate_weight_column = function(admin_polygons, weight_column) {
      if (is.null(weight_column)) {
        stop("weight_column must be specified for custom allocation")
      }

      if (!weight_column %in% names(admin_polygons)) {
        stop("weight_column must be a valid column in admin_polygons")
      }
    },

    #' Get and Validate Weights
    #'
    #' @param admin_polygons sf object with administrative polygons
    #' @param weight_column Column name for custom weights
    #' @return Validated numeric vector of weights
    get_and_validate_weights = function(admin_polygons, weight_column) {
      weights <- admin_polygons[[weight_column]]

      if (!is.numeric(weights)) {
        stop("weight_column must contain numeric values")
      }

      if (any(weights < 0)) {
        stop("weights must be non-negative")
      }

      if (all(weights == 0)) {
        warning("All weights are zero, using equal allocation")
        return(rep(1, length(weights)))
      }

      return(weights)
    },

    #' Calculate Custom Allocation
    #'
    #' @param weights Numeric vector of weights
    #' @param total_samples Total number of samples
    #' @return Integer vector of sample counts
    calculate_custom_allocation = function(weights, total_samples) {
      proportions <- weights / sum(weights)
      sample_counts <- floor(proportions * total_samples)

      # Distribute remaining samples to largest units
      remaining <- total_samples - sum(sample_counts)
      if (remaining > 0) {
        sorted_indices <- order(weights, decreasing = TRUE)
        for (i in seq_len(min(remaining, length(sorted_indices)))) {
          sample_counts[sorted_indices[i]] <- sample_counts[sorted_indices[i]] + 1
        }
      }

      return(sample_counts)
    },

    #' Create Named Sample Counts
    #'
    #' @param admin_polygons sf object with administrative polygons
    #' @param sample_counts Integer vector of sample counts
    #' @return Named vector of sample counts
    create_named_sample_counts = function(admin_polygons, sample_counts) {
      admin_ids <- as.character(admin_polygons[[1]])
      names(sample_counts) <- admin_ids
      return(sample_counts)
    }
  )
)

#' Boundary Validator
#'
#' Validates that points fall within polygon boundaries.
#'
#' @description
#' The BoundaryValidator class ensures that generated points
#' fall strictly within polygon boundaries.
#'
#' @export
BoundaryValidator <- R6::R6Class("BoundaryValidator",
  public = list(
    #' Validate Points
    #'
    #' Ensure points fall within polygon boundaries.
    #'
    #' @param points sf object with points
    #' @param polygons sf object with polygons
    #' @return Validated points
    validate_points = function(points, polygons) {
      if (is.null(points) || nrow(points) == 0) {
        return(points)
      }

      # Check if points are within polygons
      within_matrix <- sf::st_within(points, polygons, sparse = FALSE)

      # Keep only points that are within at least one polygon
      valid_points <- points[rowSums(within_matrix) > 0, ]

      # Report validation results
      if (nrow(valid_points) < nrow(points)) {
        warning(sprintf(
          "Removed %d points that fell outside polygon boundaries",
          nrow(points) - nrow(valid_points)
        ))
      }

      return(valid_points)
    }
  )
)

#' Null-coalescing operator
#'
#' @param x First value
#' @param y Default value if x is NULL
#' @return x if not NULL, otherwise y
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
#' Administrative Boundary Sampling
#'
#' Generate random coordinate points within administrative boundaries with
#' support for different allocation strategies and concurrent processing.
#'
#' @param admin_polygons sf object containing administrative boundary polygons
#' @param total_samples Integer total number of samples to generate
#' @param allocation_method Character allocation method: "proportional", "equal", or "custom"
#' @param weight_column Character column name for custom weights (required for custom allocation)
#' @param admin_column Character column name for administrative unit identifiers
#' @param concurrent Logical whether to use concurrent processing for multiple units
#' @param use_gpu Logical whether to use GPU acceleration when available
#' @param seed Integer optional random seed for reproducibility
#' @param max_workers Integer maximum number of parallel workers (default: auto-detect)
#' @param progress Logical whether to show progress indicators
#'
#' @return sf object with sampled points including administrative unit identifiers
#'
#' @details
#' This function generates random coordinate points within administrative boundaries
#' using different allocation strategies:
#' 
#' - "proportional": Allocates samples proportionally to polygon area
#' - "equal": Distributes samples equally across all administrative units
#' - "custom": Uses custom weights from specified column
#' 
#' Concurrent processing is automatically enabled for datasets with more than 10
#' administrative units and can significantly improve performance for large datasets.
#' Progress indicators help monitor processing of large datasets.
#'
#' @examples
#' \donttest{
#' # Create sample administrative polygons
#' library(sf)
#' coords_list <- list(
#'   matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE),
#'   matrix(c(1, 0, 3, 0, 3, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE)
#' )
#' polygons <- lapply(coords_list, function(x) st_polygon(list(x)))
#' admin_data <- data.frame(admin_id = c("A", "B"))
#' admin_sf <- st_sf(admin_data, geometry = st_sfc(polygons), crs = 4326)
#' 
#' # Proportional allocation (default)
#' samples <- spatial_sample_administrative(admin_sf, total_samples = 100)
#' 
#' # Equal allocation
#' equal_samples <- spatial_sample_administrative(
#'   admin_sf, 
#'   total_samples = 100, 
#'   allocation_method = "equal"
#' )
#' 
#' # Custom allocation with weights
#' admin_sf$population <- c(1000, 2000)
#' custom_samples <- spatial_sample_administrative(
#'   admin_sf,
#'   total_samples = 100,
#'   allocation_method = "custom",
#'   weight_column = "population"
#' )
#' 
#' # Concurrent processing for large datasets
#' large_samples <- spatial_sample_administrative(
#'   admin_sf,
#'   total_samples = 1000,
#'   concurrent = TRUE,
#'   max_workers = 4,
#'   progress = TRUE
#' )
#' }
#'
#' @export
spatial_sample_administrative <- function(admin_polygons, total_samples,
                                          allocation_method = "proportional",
                                          weight_column = NULL,
                                          admin_column = NULL,
                                          concurrent = FALSE,
                                          use_gpu = TRUE,
                                          seed = NULL,
                                          max_workers = NULL,
                                          progress = TRUE) {
  # Create and configure sampler
  config <- list(
    allocation_method = allocation_method,
    concurrent = concurrent,
    max_workers = max_workers,
    progress_enabled = progress
  )
  
  sampler <- AdministrativeSampler$new()
  sampler$initialize(config)
  
  # Perform sampling
  result <- sampler$sample_administrative(
    admin_polygons = admin_polygons,
    total_samples = total_samples,
    allocation_method = allocation_method,
    weight_column = weight_column,
    admin_column = admin_column,
    concurrent = concurrent,
    use_gpu = use_gpu,
    seed = seed
  )
  
  return(result)
}