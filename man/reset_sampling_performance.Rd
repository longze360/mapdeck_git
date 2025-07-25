% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatial-sampling-engine.R
\name{reset_sampling_performance}
\alias{reset_sampling_performance}
\title{Reset Sampling Performance Statistics}
\usage{
reset_sampling_performance(engine = NULL)
}
\arguments{
\item{engine}{Optional SamplingEngine instance (uses global engine if NULL)}
}
\value{
Invisible NULL
}
\description{
This function resets all performance monitoring counters to zero, providing
a clean slate for measuring performance of subsequent sampling operations.
}
\details{
Reset all performance counters and statistics for spatial sampling operations.
}
\examples{
\donttest{
# Reset performance statistics
reset_sampling_performance()

# Verify reset
stats <- get_sampling_performance()
print(stats)  # Should show all zeros
}

}
