% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatial-sampling-engine.R
\name{get_sampling_performance}
\alias{get_sampling_performance}
\title{Get Sampling Performance Statistics}
\usage{
get_sampling_performance(engine = NULL, reset = FALSE)
}
\arguments{
\item{engine}{Optional SamplingEngine instance (uses global engine if NULL)}

\item{reset}{Logical indicating if statistics should be reset after retrieval}
}
\value{
List containing performance statistics:
  \itemize{
    \item gpu_operations: Number of operations using GPU acceleration
    \item cpu_operations: Number of operations using CPU processing
    \item total_samples: Total number of samples processed
    \item avg_gpu_time: Average time per GPU operation (seconds)
    \item avg_cpu_time: Average time per CPU operation (seconds)
    \item gpu_speedup: Speed improvement factor when using GPU
    \item memory_usage: Peak memory usage during operations
  }
}
\description{
This function returns detailed performance metrics for spatial sampling
operations, including GPU vs CPU usage, timing statistics, and throughput
measurements. Useful for optimizing sampling performance and monitoring
GPU acceleration effectiveness.
}
\details{
Retrieve performance statistics for spatial sampling operations.
}
\examples{
\donttest{
# Get performance statistics
stats <- get_sampling_performance()
print(stats)

# Get statistics and reset counters
stats <- get_sampling_performance(reset = TRUE)

# Check GPU acceleration effectiveness
if (stats$gpu_operations > 0 && stats$cpu_operations > 0) {
  speedup <- stats$avg_cpu_time / stats$avg_gpu_time
  cat("GPU provides", round(speedup, 2), "x speedup\n")
}
}

}
