#' Performance Monitoring for Spatial Sampling
#'
#' Advanced performance monitoring and benchmarking system for spatial sampling
#' operations with GPU vs CPU comparison capabilities.
#'
#' @name performance-monitor
NULL

#' Performance Monitor Class
#'
#' Comprehensive performance monitoring for spatial sampling operations.
#'
#' @description
#' The PerformanceMonitor class provides detailed performance tracking,
#' benchmarking, and analysis capabilities for spatial sampling operations.
#'
#' @field metrics List of performance metrics
#' @field benchmarks List of benchmark results
#' @field profiling_enabled Logical indicating if profiling is active
#'
#' @export
PerformanceMonitor <- R6::R6Class("PerformanceMonitor",
  public = list(
    #' @field metrics Performance metrics storage
    metrics = NULL,
    
    #' @field benchmarks Benchmark results storage
    benchmarks = NULL,
    
    #' @field profiling_enabled Profiling status
    profiling_enabled = TRUE,
    
    #' Initialize Performance Monitor
    #'
    #' Initialize the performance monitoring system.
    #'
    #' @param config Optional configuration list
    #' @return Invisible self for method chaining
    initialize = function(config = list()) {
      self$metrics <- list(
        operations = data.frame(
          timestamp = as.POSIXct(character(0)),
          operation = character(0),
          method = character(0), # "gpu" or "cpu"
          data_size = numeric(0),
          execution_time = numeric(0),
          memory_used = numeric(0),
          samples_generated = numeric(0),
          stringsAsFactors = FALSE
        ),
        summary = list(
          total_operations = 0,
          gpu_operations = 0,
          cpu_operations = 0,
          total_execution_time = 0,
          avg_gpu_time = 0,
          avg_cpu_time = 0,
          gpu_speedup = NA
        )
      )
      
      self$benchmarks <- list()
      
      if (!is.null(config$profiling_enabled)) {
        self$profiling_enabled <- config$profiling_enabled
      }
      
      invisible(self)
    },
    
    #' Record Operation
    #'
    #' Record performance metrics for a sampling operation.
    #'
    #' @param operation Operation type ("random", "grid", "stratified")
    #' @param method Execution method ("gpu" or "cpu")
    #' @param data_size Size of input dataset
    #' @param execution_time Time taken in seconds
    #' @param samples_generated Number of samples produced
    #' @param memory_used Memory usage in bytes (optional)
    #' @return Invisible self for method chaining
    record_operation = function(operation, method, data_size, execution_time, 
                                samples_generated, memory_used = NA) {
      if (!self$profiling_enabled) {
        return(invisible(self))
      }
      
      # Add new operation record
      new_record <- data.frame(
        timestamp = Sys.time(),
        operation = operation,
        method = method,
        data_size = data_size,
        execution_time = execution_time,
        memory_used = memory_used,
        samples_generated = samples_generated,
        stringsAsFactors = FALSE
      )
      
      self$metrics$operations <- rbind(self$metrics$operations, new_record)
      
      # Update summary statistics
      private$update_summary_stats()
      
      invisible(self)
    },
    
    #' Run Benchmark
    #'
    #' Run comprehensive benchmark comparing GPU vs CPU performance.
    #'
    #' @param data_sizes Vector of data sizes to test
    #' @param operations Vector of operations to benchmark
    #' @param iterations Number of iterations per test
    #' @param engine SamplingEngine instance to use
    #' @return Benchmark results
    run_benchmark = function(data_sizes = c(100, 1000, 10000, 50000), 
                             operations = c("random", "grid", "stratified"),
                             iterations = 3,
                             engine = NULL) {
      if (is.null(engine)) {
        engine <- SamplingEngine$new()
        engine$initialize()
      }
      
      benchmark_id <- paste0("benchmark_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      results <- list()
      
      cat("Running spatial sampling benchmark...\n")
      cat("Data sizes:", paste(data_sizes, collapse = ", "), "\n")
      cat("Operations:", paste(operations, collapse = ", "), "\n")
      cat("Iterations per test:", iterations, "\n\n")
      
      total_tests <- length(data_sizes) * length(operations) * 2 * iterations # 2 for GPU/CPU
      current_test <- 0
      
      for (data_size in data_sizes) {
        # Generate test data
        test_data <- private$generate_test_data(data_size)
        
        for (operation in operations) {
          for (method in c("cpu", "gpu")) {
            method_times <- numeric(iterations)
            
            for (i in seq_len(iterations)) {
              current_test <- current_test + 1
              cat(sprintf("Test %d/%d: %s sampling, %d points, %s method, iteration %d\n",
                          current_test, total_tests, operation, data_size, method, i))
              
              # Force method selection
              original_threshold <- engine$fallback_threshold
              if (method == "cpu") {
                engine$fallback_threshold <- 0  # Force CPU
              } else {
                engine$fallback_threshold <- Inf  # Force GPU attempt
              }
              
              # Run test
              start_time <- Sys.time()
              tryCatch({
                if (operation == "random") {
                  result <- engine$spatial_sample_random(test_data, n = min(100, data_size))
                } else if (operation == "grid") {
                  grid_size <- diff(range(test_data$longitude)) / 10
                  result <- engine$spatial_sample_grid(test_data, grid_size = grid_size)
                } else if (operation == "stratified") {
                  result <- engine$spatial_sample_stratified(test_data, 
                                                             strata_column = "category",
                                                             n_per_stratum = 10)
                }
                
                method_times[i] <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
              }, error = function(e) {
                method_times[i] <- NA
                cat("  Error:", e$message, "\n")
              })
              
              # Restore original threshold
              engine$fallback_threshold <- original_threshold
            }
            
            # Store results
            test_key <- paste(data_size, operation, method, sep = "_")
            results[[test_key]] <- list(
              data_size = data_size,
              operation = operation,
              method = method,
              times = method_times,
              mean_time = mean(method_times, na.rm = TRUE),
              median_time = median(method_times, na.rm = TRUE),
              sd_time = sd(method_times, na.rm = TRUE),
              success_rate = sum(!is.na(method_times)) / iterations
            )
          }
        }
      }
      
      # Store benchmark results
      self$benchmarks[[benchmark_id]] <- list(
        timestamp = Sys.time(),
        config = list(
          data_sizes = data_sizes,
          operations = operations,
          iterations = iterations
        ),
        results = results
      )
      
      cat("\nBenchmark completed!\n")
      return(private$format_benchmark_results(results))
    },
    
    #' Get Performance Summary
    #'
    #' Get comprehensive performance summary.
    #'
    #' @return List containing performance summary
    get_performance_summary = function() {
      if (nrow(self$metrics$operations) == 0) {
        return(list(
          message = "No operations recorded yet",
          total_operations = 0
        ))
      }
      
      summary <- self$metrics$summary
      
      # Add recent operations analysis
      recent_ops <- tail(self$metrics$operations, 10)
      summary$recent_operations <- nrow(recent_ops)
      summary$recent_avg_time <- mean(recent_ops$execution_time, na.rm = TRUE)
      
      # Add operation type breakdown
      op_breakdown <- table(self$metrics$operations$operation)
      summary$operation_breakdown <- as.list(op_breakdown)
      
      # Add method breakdown
      method_breakdown <- table(self$metrics$operations$method)
      summary$method_breakdown <- as.list(method_breakdown)
      
      # Add performance trends
      if (nrow(self$metrics$operations) >= 5) {
        recent_times <- tail(self$metrics$operations$execution_time, 5)
        summary$performance_trend <- private$calculate_trend(recent_times)
      }
      
      return(summary)
    },
    
    #' Generate Performance Report
    #'
    #' Generate detailed performance report.
    #'
    #' @param format Output format ("text" or "html")
    #' @return Performance report string
    generate_report = function(format = "text") {
      summary <- self$get_performance_summary()
      
      if (format == "text") {
        return(private$generate_text_report(summary))
      } else if (format == "html") {
        return(private$generate_html_report(summary))
      } else {
        stop("Unsupported format. Use 'text' or 'html'.")
      }
    },
    
    #' Reset Metrics
    #'
    #' Reset all performance metrics.
    #'
    #' @return Invisible self for method chaining
    reset_metrics = function() {
      self$initialize()
      invisible(self)
    },
    
    #' Export Metrics
    #'
    #' Export performance metrics to file.
    #'
    #' @param filename Output filename
    #' @param format Export format ("csv", "json", "rds")
    #' @return Invisible self for method chaining
    export_metrics = function(filename, format = "csv") {
      if (format == "csv") {
        write.csv(self$metrics$operations, filename, row.names = FALSE)
      } else if (format == "json") {
        jsonlite::write_json(self$metrics, filename, pretty = TRUE)
      } else if (format == "rds") {
        saveRDS(self$metrics, filename)
      } else {
        stop("Unsupported format. Use 'csv', 'json', or 'rds'.")
      }
      
      invisible(self)
    }
  ),
  
  private = list(
    #' Update Summary Statistics
    #'
    #' Update internal summary statistics.
    update_summary_stats = function() {
      ops <- self$metrics$operations
      
      if (nrow(ops) == 0) return()
      
      self$metrics$summary$total_operations <- nrow(ops)
      self$metrics$summary$gpu_operations <- sum(ops$method == "gpu", na.rm = TRUE)
      self$metrics$summary$cpu_operations <- sum(ops$method == "cpu", na.rm = TRUE)
      self$metrics$summary$total_execution_time <- sum(ops$execution_time, na.rm = TRUE)
      
      # Calculate average times
      gpu_times <- ops$execution_time[ops$method == "gpu"]
      cpu_times <- ops$execution_time[ops$method == "cpu"]
      
      if (length(gpu_times) > 0) {
        self$metrics$summary$avg_gpu_time <- mean(gpu_times, na.rm = TRUE)
      }
      
      if (length(cpu_times) > 0) {
        self$metrics$summary$avg_cpu_time <- mean(cpu_times, na.rm = TRUE)
      }
      
      # Calculate speedup
      if (self$metrics$summary$avg_gpu_time > 0 && self$metrics$summary$avg_cpu_time > 0) {
        self$metrics$summary$gpu_speedup <- self$metrics$summary$avg_cpu_time / self$metrics$summary$avg_gpu_time
      }
    },
    
    #' Generate Test Data
    #'
    #' Generate test data for benchmarking.
    #'
    #' @param size Number of points to generate
    #' @return Test data frame
    generate_test_data = function(size) {
      data.frame(
        longitude = runif(size, -180, 180),
        latitude = runif(size, -90, 90),
        category = sample(c("A", "B", "C", "D"), size, replace = TRUE),
        value = rnorm(size),
        stringsAsFactors = FALSE
      )
    },
    
    #' Format Benchmark Results
    #'
    #' Format benchmark results for display.
    #'
    #' @param results Raw benchmark results
    #' @return Formatted results data frame
    format_benchmark_results = function(results) {
      result_df <- data.frame(
        data_size = numeric(0),
        operation = character(0),
        method = character(0),
        mean_time = numeric(0),
        median_time = numeric(0),
        sd_time = numeric(0),
        success_rate = numeric(0),
        stringsAsFactors = FALSE
      )
      
      for (result in results) {
        result_df <- rbind(result_df, data.frame(
          data_size = result$data_size,
          operation = result$operation,
          method = result$method,
          mean_time = result$mean_time,
          median_time = result$median_time,
          sd_time = result$sd_time,
          success_rate = result$success_rate,
          stringsAsFactors = FALSE
        ))
      }
      
      return(result_df)
    },
    
    #' Calculate Trend
    #'
    #' Calculate performance trend from recent times.
    #'
    #' @param times Vector of execution times
    #' @return Trend description
    calculate_trend = function(times) {
      if (length(times) < 3) return("insufficient_data")
      
      # Simple linear trend
      x <- seq_along(times)
      trend_coef <- coef(lm(times ~ x))[2]
      
      if (abs(trend_coef) < 0.001) {
        return("stable")
      } else if (trend_coef > 0) {
        return("degrading")
      } else {
        return("improving")
      }
    },
    
    #' Generate Text Report
    #'
    #' Generate text-based performance report.
    #'
    #' @param summary Performance summary
    #' @return Text report string
    generate_text_report = function(summary) {
      report <- c(
        "=== Spatial Sampling Performance Report ===",
        "",
        sprintf("Total Operations: %d", summary$total_operations),
        sprintf("GPU Operations: %d", summary$gpu_operations),
        sprintf("CPU Operations: %d", summary$cpu_operations),
        sprintf("Total Execution Time: %.3f seconds", summary$total_execution_time),
        "",
        "Average Execution Times:",
        sprintf("  GPU: %.4f seconds", summary$avg_gpu_time),
        sprintf("  CPU: %.4f seconds", summary$avg_cpu_time),
        ""
      )
      
      if (!is.na(summary$gpu_speedup)) {
        report <- c(report, sprintf("GPU Speedup: %.2fx", summary$gpu_speedup), "")
      }
      
      if (!is.null(summary$operation_breakdown)) {
        report <- c(report, "Operation Breakdown:")
        for (op in names(summary$operation_breakdown)) {
          report <- c(report, sprintf("  %s: %d", op, summary$operation_breakdown[[op]]))
        }
        report <- c(report, "")
      }
      
      if (!is.null(summary$performance_trend)) {
        report <- c(report, sprintf("Performance Trend: %s", summary$performance_trend))
      }
      
      return(paste(report, collapse = "\n"))
    },
    
    #' Generate HTML Report
    #'
    #' Generate HTML-based performance report.
    #'
    #' @param summary Performance summary
    #' @return HTML report string
    generate_html_report = function(summary) {
      # Simplified HTML report
      html <- paste0(
        "<html><head><title>Spatial Sampling Performance Report</title></head><body>",
        "<h1>Spatial Sampling Performance Report</h1>",
        "<h2>Summary</h2>",
        "<ul>",
        sprintf("<li>Total Operations: %d</li>", summary$total_operations),
        sprintf("<li>GPU Operations: %d</li>", summary$gpu_operations),
        sprintf("<li>CPU Operations: %d</li>", summary$cpu_operations),
        sprintf("<li>Total Execution Time: %.3f seconds</li>", summary$total_execution_time),
        "</ul>",
        "</body></html>"
      )
      
      return(html)
    }
  )
)