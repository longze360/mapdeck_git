#' Concurrent Processing for Administrative Sampling
#'
#' This file contains the implementation of concurrent processing capabilities
#' for administrative boundary sampling operations.
#'
#' @name concurrent-processor
NULL

#' Concurrent Processor Class
#'
#' R6 class that provides parallel processing capabilities for administrative
#' sampling operations, enabling efficient processing of multiple administrative
#' units simultaneously.
#'
#' @description
#' The ConcurrentProcessor class manages parallel execution of sampling
#' operations across multiple administrative units, with support for progress
#' monitoring and resource management.
#'
#' @field max_workers Maximum number of parallel workers
#' @field progress_enabled Logical indicating if progress reporting is enabled
#' @field chunk_size Number of units to process per chunk
#'
#' @examples
#' \donttest{
#' # Create concurrent processor
#' processor <- ConcurrentProcessor$new()
#' processor$initialize(max_workers = 4)
#'
#' # Process administrative units in parallel
#' results <- processor$process_units(admin_polygons, sample_counts,
#'   admin_column,
#'   use_gpu = TRUE
#' )
#' }
#'
#' @export
ConcurrentProcessor <- R6::R6Class("ConcurrentProcessor",
  public = list(
    #' @field max_workers Maximum number of parallel workers
    max_workers = NULL,

    #' @field progress_enabled Logical for progress reporting
    progress_enabled = NULL,

    #' @field chunk_size Number of units per processing chunk
    chunk_size = NULL,

    #' @field progress_bar Progress bar object
    progress_bar = NULL,

    #' @field cluster Parallel cluster object
    cluster = NULL,

    #' Initialize Concurrent Processor
    #'
    #' Initialize the concurrent processing system with specified configuration.
    #'
    #' @param config List containing processor configuration
    #' @return Invisible self for method chaining
    initialize = function(config = list()) {
      # Set default configuration
      self$max_workers <- config$max_workers %||%
        min(4, parallel::detectCores() - 1)
      self$progress_enabled <- config$progress_enabled %||% TRUE
      self$chunk_size <- config$chunk_size %||% 10

      # Initialize parallel cluster if multiple workers requested
      if (self$max_workers > 1) {
        private$setup_cluster()
      }

      invisible(self)
    },

    #' Process Administrative Units
    #'
    #' Process multiple administrative units concurrently with progress
    #' monitoring and efficient resource utilization.
    #'
    #' @param admin_polygons sf object with administrative polygons
    #' @param sample_counts Named vector of sample counts per unit
    #' @param admin_column Column name for administrative unit identifiers
    #' @param use_gpu Logical for GPU acceleration
    #' @return sf object with combined sampled points
    process_units = function(admin_polygons, sample_counts, admin_column,
                             use_gpu = TRUE) {
      n_units <- nrow(admin_polygons)

      # Initialize progress tracking
      if (self$progress_enabled) {
        private$initialize_progress(n_units)
      }

      tryCatch(
        {
          if (self$max_workers > 1 && n_units > self$chunk_size) {
            # Use parallel processing for large datasets
            result <- private$process_parallel(
              admin_polygons, sample_counts,
              admin_column, use_gpu
            )
          } else {
            # Use sequential processing for small datasets
            result <- private$process_sequential(
              admin_polygons, sample_counts,
              admin_column, use_gpu
            )
          }

          # Finalize progress
          if (self$progress_enabled && !is.null(self$progress_bar)) {
            private$finalize_progress()
          }

          return(result)
        },
        error = function(e) {
          # Clean up progress bar on error
          if (self$progress_enabled && !is.null(self$progress_bar)) {
            private$cleanup_progress()
          }
          stop("Concurrent processing failed: ", e$message)
        }
      )
    },

    #' Set Maximum Workers
    #'
    #' Update the maximum number of parallel workers.
    #'
    #' @param max_workers Integer maximum number of workers
    #' @return Invisible self for method chaining
    set_max_workers = function(max_workers) {
      if (!is.numeric(max_workers) || max_workers < 1) {
        stop("max_workers must be a positive integer")
      }

      # Clean up existing cluster
      private$cleanup_cluster()

      # Update configuration
      self$max_workers <- as.integer(max_workers)

      # Setup new cluster if needed
      if (self$max_workers > 1) {
        private$setup_cluster()
      }

      invisible(self)
    },

    #' Enable Progress Reporting
    #'
    #' Enable progress bar and status reporting.
    #'
    #' @return Invisible self for method chaining
    enable_progress = function() {
      self$progress_enabled <- TRUE
      invisible(self)
    },

    #' Disable Progress Reporting
    #'
    #' Disable progress bar and status reporting.
    #'
    #' @return Invisible self for method chaining
    disable_progress = function() {
      self$progress_enabled <- FALSE
      if (!is.null(self$progress_bar)) {
        private$cleanup_progress()
      }
      invisible(self)
    },

    #' Get Processing Statistics
    #'
    #' Retrieve statistics about concurrent processing performance.
    #'
    #' @return List containing processing statistics
    get_statistics = function() {
      list(
        max_workers = self$max_workers,
        progress_enabled = self$progress_enabled,
        chunk_size = self$chunk_size,
        cluster_active = !is.null(self$cluster)
      )
    },

    #' Cleanup Resources
    #'
    #' Clean up parallel cluster and progress resources.
    #'
    #' @return Invisible self for method chaining
    cleanup = function() {
      private$cleanup_cluster()
      private$cleanup_progress()
      invisible(self)
    }
  ),
  private = list(
    #' Setup Parallel Cluster
    #'
    #' Initialize parallel processing cluster.
    setup_cluster = function() {
      tryCatch(
        {
          # Create cluster
          self$cluster <- parallel::makeCluster(self$max_workers)

          # Load required packages on workers
          parallel::clusterEvalQ(self$cluster, {
            library(sf)
            library(R6)
          })

          # Export required functions to workers
          parallel::clusterExport(self$cluster,
            c("process_single_unit_worker", "generate_points_in_polygon_worker"),
            envir = environment()
          )
        },
        error = function(e) {
          warning("Failed to setup parallel cluster: ", e$message)
          self$cluster <- NULL
          self$max_workers <- 1
        }
      )
    },

    #' Cleanup Parallel Cluster
    #'
    #' Clean up parallel processing resources.
    cleanup_cluster = function() {
      if (!is.null(self$cluster)) {
        tryCatch(
          {
            parallel::stopCluster(self$cluster)
          },
          error = function(e) {
            # Ignore cleanup errors
          }
        )
        self$cluster <- NULL
      }
    },

    #' Initialize Progress Tracking
    #'
    #' Setup progress bar for processing monitoring.
    #'
    #' @param total_units Total number of units to process
    initialize_progress = function(total_units) {
      if (requireNamespace("progress", quietly = TRUE)) {
        self$progress_bar <- progress::progress_bar$new(
          format = "Processing [:bar] :percent (:current/:total) ETA: :eta",
          total = total_units,
          clear = FALSE,
          width = 80
        )
      } else {
        # Fallback to simple text progress
        cat("Processing", total_units, "administrative units...\n")
      }
    },

    #' Update Progress
    #'
    #' Update progress bar with current status.
    #'
    #' @param increment Number of units completed
    update_progress = function(increment = 1) {
      if (!is.null(self$progress_bar)) {
        self$progress_bar$tick(increment)
      }
    },

    #' Finalize Progress
    #'
    #' Complete progress tracking.
    finalize_progress = function() {
      if (!is.null(self$progress_bar)) {
        if (!self$progress_bar$finished) {
          self$progress_bar$terminate()
        }
        cat("Processing completed successfully.\n")
      }
    },

    #' Cleanup Progress
    #'
    #' Clean up progress tracking resources.
    cleanup_progress = function() {
      if (!is.null(self$progress_bar)) {
        if (!self$progress_bar$finished) {
          self$progress_bar$terminate()
        }
        self$progress_bar <- NULL
      }
    },

    #' Process Units in Parallel
    #'
    #' Execute parallel processing of administrative units.
    #'
    #' @param admin_polygons sf object with administrative polygons
    #' @param sample_counts Named vector of sample counts per unit
    #' @param admin_column Column name for administrative unit identifiers
    #' @param use_gpu Logical for GPU acceleration
    #' @return sf object with combined results
    process_parallel = function(admin_polygons, sample_counts, admin_column,
                                use_gpu) {
      n_units <- nrow(admin_polygons)

      # Create processing chunks
      chunks <- private$create_chunks(n_units, self$chunk_size)

      # Process chunks in parallel
      chunk_results <- parallel::parLapply(self$cluster, chunks, function(chunk) {
        chunk_polygons <- admin_polygons[chunk, ]
        chunk_results <- list()

        for (i in seq_len(nrow(chunk_polygons))) {
          unit_result <- process_single_unit_worker(
            chunk_polygons[i, ], sample_counts, admin_column, use_gpu
          )

          if (!is.null(unit_result) && nrow(unit_result) > 0) {
            chunk_results[[length(chunk_results) + 1]] <- unit_result
          }
        }

        return(chunk_results)
      })

      # Update progress
      if (self$progress_enabled) {
        private$update_progress(length(chunks) * self$chunk_size)
      }

      # Combine results
      all_results <- unlist(chunk_results, recursive = FALSE)
      return(private$combine_results(all_results, admin_polygons))
    },

    #' Process Units Sequentially
    #'
    #' Execute sequential processing with progress monitoring.
    #'
    #' @param admin_polygons sf object with administrative polygons
    #' @param sample_counts Named vector of sample counts per unit
    #' @param admin_column Column name for administrative unit identifiers
    #' @param use_gpu Logical for GPU acceleration
    #' @return sf object with combined results
    process_sequential = function(admin_polygons, sample_counts, admin_column,
                                  use_gpu) {
      all_results <- list()

      for (i in seq_len(nrow(admin_polygons))) {
        unit_result <- private$process_single_unit(
          admin_polygons[i, ], sample_counts, admin_column, use_gpu
        )

        if (!is.null(unit_result) && nrow(unit_result) > 0) {
          all_results[[length(all_results) + 1]] <- unit_result
        }

        # Update progress
        if (self$progress_enabled) {
          private$update_progress(1)
        }
      }

      return(private$combine_results(all_results, admin_polygons))
    },

    #' Create Processing Chunks
    #'
    #' Divide units into chunks for parallel processing.
    #'
    #' @param n_units Total number of units
    #' @param chunk_size Size of each chunk
    #' @return List of index vectors for each chunk
    create_chunks = function(n_units, chunk_size) {
      chunks <- list()
      start_idx <- 1

      while (start_idx <= n_units) {
        end_idx <- min(start_idx + chunk_size - 1, n_units)
        chunks[[length(chunks) + 1]] <- start_idx:end_idx
        start_idx <- end_idx + 1
      }

      return(chunks)
    },

    #' Process Single Administrative Unit
    #'
    #' Process a single administrative unit for sampling.
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
      unit_samples <- private$generate_points_in_polygon(
        polygon, n_samples,
        use_gpu
      )

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

    #' Generate Points in Polygon
    #'
    #' Generate random points within a polygon boundary.
    #'
    #' @param polygon sf object with a single polygon
    #' @param n_samples Number of samples to generate
    #' @param use_gpu Logical for GPU acceleration
    #' @return sf object with sampled points
    generate_points_in_polygon = function(polygon, n_samples, use_gpu) {
      bbox <- sf::st_bbox(polygon)
      bounds <- c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])

      # Use rejection sampling to generate points within polygon
      points_in_polygon <- private$rejection_sampling(
        polygon, bounds,
        n_samples
      )

      return(points_in_polygon)
    },

    #' Rejection Sampling for Polygon
    #'
    #' Use rejection sampling to generate points within polygon boundaries.
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
        candidate_points <- private$generate_random_points_in_bounds(
          bounds,
          n_candidates
        )

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
    #' Finalize the sampled points to match requested count.
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
    #' Generate random points within specified bounds.
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
        crs = 4326 # Default to WGS84
      )

      return(points)
    },

    #' Combine Results
    #'
    #' Combine results from multiple processing units.
    #'
    #' @param all_results List of sf objects with results
    #' @param admin_polygons Original administrative polygons for CRS reference
    #' @return Combined sf object
    combine_results = function(all_results, admin_polygons) {
      if (length(all_results) == 0) {
        return(sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs(admin_polygons))))
      }

      combined_results <- do.call(rbind, all_results)
      return(combined_results)
    }
  )
)

# Worker functions for parallel processing
# These functions are exported to parallel workers

#' Process Single Unit Worker Function
#'
#' Worker function for processing a single administrative unit in parallel.
#'
#' @param polygon sf object with a single polygon
#' @param sample_counts Named vector of sample counts per unit
#' @param admin_column Column name for administrative unit identifiers
#' @param use_gpu Logical for GPU acceleration
#' @return sf object with sampled points for this unit
#' @keywords internal
process_single_unit_worker <- function(polygon, sample_counts, admin_column,
                                       use_gpu) {
  admin_id <- as.character(polygon[[admin_column]])
  n_samples <- sample_counts[[admin_id]]

  if (is.null(n_samples) || n_samples <= 0) {
    return(NULL)
  }

  # Generate random points within polygon
  unit_samples <- generate_points_in_polygon_worker(polygon, n_samples, use_gpu)

  # Add administrative unit information
  if (!is.null(unit_samples) && nrow(unit_samples) > 0) {
    unit_samples <- sf::st_sf(
      data.frame(admin_id = admin_id),
      geometry = sf::st_geometry(unit_samples)
    )
    names(unit_samples)[1] <- admin_column
  }

  return(unit_samples)
}

#' Generate Points in Polygon Worker Function
#'
#' Worker function for generating points within a polygon.
#'
#' @param polygon sf object with a single polygon
#' @param n_samples Number of samples to generate
#' @param use_gpu Logical for GPU acceleration
#' @return sf object with sampled points
#' @keywords internal
generate_points_in_polygon_worker <- function(polygon, n_samples, use_gpu) {
  bbox <- sf::st_bbox(polygon)
  bounds <- c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])

  # Use rejection sampling to generate points within polygon
  points_in_polygon <- NULL
  attempts <- 1
  max_attempts <- 5

  while ((is.null(points_in_polygon) || nrow(points_in_polygon) < n_samples) &&
    attempts <= max_attempts) {
    # Generate candidate points
    n_candidates <- n_samples * 2 * attempts
    x <- runif(n_candidates, bounds[1], bounds[3])
    y <- runif(n_candidates, bounds[2], bounds[4])

    candidate_points <- sf::st_as_sf(
      data.frame(x = x, y = y),
      coords = c("x", "y"),
      crs = sf::st_crs(polygon)
    )

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

  # Finalize samples
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
