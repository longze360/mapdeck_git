# Test Setup Helper
# This file provides common setup functions and utilities for testing

# Load required libraries
library(testthat)
library(R6)

# Source all R files to make them available for testing
source_r_files <- function() {
  r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  for (file in r_files) {
    tryCatch({
      source(file)
    }, error = function(e) {
      # Skip files that can't be sourced due to dependencies
      message("Skipping ", file, ": ", e$message)
    })
  }
}

# Source R files if not in package context
if (!exists("IMapProvider")) {
  source_r_files()
}

# Mock data generators for testing
create_test_data <- function(n = 100) {
  data.frame(
    longitude = runif(n, -180, 180),
    latitude = runif(n, -90, 90),
    value = rnorm(n),
    category = sample(c("A", "B", "C"), n, replace = TRUE)
  )
}

create_test_sf_data <- function(n = 50) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    skip("sf package not available")
  }
  
  coords <- matrix(c(runif(n, -180, 180), runif(n, -90, 90)), ncol = 2)
  sf::st_as_sf(
    data.frame(
      id = seq_len(n),
      value = rnorm(n)
    ),
    coords = c("longitude", "latitude"),
    crs = 4326
  )
}

# Mock provider for testing
create_mock_provider <- function(provider_name = "mock") {
  MockProvider <- R6::R6Class("MockProvider",
    inherit = if (exists("IMapProvider")) IMapProvider else NULL,
    public = list(
      provider_name = provider_name,
      config = list(),
      capabilities = list(
        coordinate_systems = c("WGS84"),
        authentication_required = FALSE,
        supported_features = list(
          layers = TRUE,
          styles = TRUE,
          view_control = TRUE
        )
      ),
      initialized = FALSE,
      layers = list(),
      current_style = "default",
      
      initialize = function() {
        # R6 constructor - different from initialize_provider
        invisible(self)
      },
      
      initialize_provider = function(config = list()) {
        self$config <- config
        self$initialized <- TRUE
        invisible(self)
      },
      
      create_map = function(container, options = list()) {
        if (!self$initialized) {
          stop("Provider must be initialized before creating map")
        }
        list(
          container = container,
          options = options,
          provider = self$provider_name
        )
      },
      
      update_style = function(style) {
        if (!self$initialized) {
          stop("Provider must be initialized before updating style")
        }
        self$current_style <- style
        invisible(self)
      },
      
      add_layer = function(layer) {
        if (!self$initialized) {
          stop("Provider must be initialized before adding layers")
        }
        if (is.null(layer$id)) {
          stop("Layer must have an id")
        }
        self$layers[[layer$id]] <- layer
        invisible(self)
      },
      
      remove_layer = function(layer_id) {
        if (!self$initialized) {
          stop("Provider must be initialized before removing layers")
        }
        self$layers[[layer_id]] <- NULL
        invisible(self)
      },
      
      set_view = function(longitude, latitude, zoom, pitch = 0, bearing = 0) {
        if (!self$initialized) {
          stop("Provider must be initialized before setting view")
        }
        # Basic validation
        if (longitude < -180 || longitude > 180) {
          stop("Longitude must be between -180 and 180")
        }
        if (latitude < -90 || latitude > 90) {
          stop("Latitude must be between -90 and 90")
        }
        if (zoom < 0 || zoom > 24) {
          stop("Zoom must be between 0 and 24")
        }
        
        self$config$view <- list(
          longitude = longitude,
          latitude = latitude,
          zoom = zoom,
          pitch = pitch,
          bearing = bearing
        )
        invisible(self)
      },
      
      get_available_styles = function() {
        c("default", "dark", "light", "satellite")
      },
      
      validate_config = function(config) {
        is.list(config)
      },
      
      destroy = function() {
        self$initialized <- FALSE
        self$config <- list()
        self$layers <- list()
        self$current_style <- NULL
        invisible(NULL)
      }
    )
  )
  
  return(MockProvider)
}

# Performance testing utilities
benchmark_function <- function(func, iterations = 100) {
  times <- numeric(iterations)
  for (i in seq_len(iterations)) {
    start_time <- Sys.time()
    func()
    times[i] <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  }
  
  list(
    mean_time = mean(times),
    median_time = median(times),
    min_time = min(times),
    max_time = max(times),
    sd_time = sd(times)
  )
}

# Memory usage testing
measure_memory_usage <- function(func) {
  gc_before <- gc()
  result <- func()
  gc_after <- gc()
  
  list(
    result = result,
    memory_used = gc_after[2, 2] - gc_before[2, 2]  # Max used memory difference
  )
}

# Skip conditions for optional dependencies
skip_if_no_sf <- function() {
  skip_if_not_installed("sf")
}

skip_if_no_spatial <- function() {
  skip_if_not_installed("sf")
  skip_if_not_installed("sp")
}

skip_if_no_parallel <- function() {
  skip_if_not_installed("parallel")
}

# Test data validation helpers
expect_valid_coordinates <- function(data, lon_col = "longitude", lat_col = "latitude") {
  expect_true(lon_col %in% names(data))
  expect_true(lat_col %in% names(data))
  expect_true(all(data[[lon_col]] >= -180 & data[[lon_col]] <= 180))
  expect_true(all(data[[lat_col]] >= -90 & data[[lat_col]] <= 90))
}

expect_valid_layer <- function(layer) {
  expect_true(is.list(layer))
  expect_true("id" %in% names(layer))
  expect_true("type" %in% names(layer))
  expect_true(is.character(layer$id))
  expect_true(nchar(layer$id) > 0)
}

# Statistical validation for sampling tests
expect_statistical_validity <- function(samples, population, tolerance = 0.1) {
  # Check that sample size is reasonable
  expect_true(nrow(samples) > 0)
  expect_true(nrow(samples) <= nrow(population))
  
  # Check that samples are representative (basic statistical tests)
  if (nrow(samples) > 10 && nrow(population) > 20) {
    # Compare means (should be similar within tolerance)
    if ("value" %in% names(samples) && "value" %in% names(population)) {
      sample_mean <- mean(samples$value, na.rm = TRUE)
      pop_mean <- mean(population$value, na.rm = TRUE)
      
      if (!is.na(sample_mean) && !is.na(pop_mean) && pop_mean != 0) {
        relative_diff <- abs(sample_mean - pop_mean) / abs(pop_mean)
        expect_true(relative_diff <= tolerance, 
                    info = sprintf("Sample mean %.3f differs too much from population mean %.3f", 
                                   sample_mean, pop_mean))
      }
    }
  }
}

# Coordinate transformation testing utilities
expect_coordinate_accuracy <- function(original, transformed, tolerance = 1e-6) {
  expect_equal(length(original), length(transformed))
  expect_true(all(abs(original - transformed) <= tolerance))
}

# Provider interface compliance testing
test_provider_interface_compliance <- function(provider_class, config = list()) {
  # Test that provider implements interface
  expect_true(validate_provider_interface(provider_class))
  
  # Test provider lifecycle
  provider <- provider_class$new()
  expect_false(provider$initialized)
  
  provider$initialize_provider(config)
  expect_true(provider$initialized)
  
  # Test all interface methods exist and work
  expect_true(is.function(provider$create_map))
  expect_true(is.function(provider$update_style))
  expect_true(is.function(provider$add_layer))
  expect_true(is.function(provider$remove_layer))
  expect_true(is.function(provider$set_view))
  expect_true(is.function(provider$get_available_styles))
  expect_true(is.function(provider$validate_config))
  expect_true(is.function(provider$destroy))
  
  # Test cleanup
  provider$destroy()
  expect_false(provider$initialized)
}