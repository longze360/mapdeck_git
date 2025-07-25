% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proportional-sampler.R
\name{ProportionalRegionalSampler}
\alias{ProportionalRegionalSampler}
\title{Proportional Regional Sampler Class}
\description{
The ProportionalRegionalSampler class provides methods for generating
samples based on regional variables (population, cases, etc.) with
support for numeric sampling ratios like 1.0, 0.1, 0.01, 2.0, etc.
}
\details{
R6 class that provides enhanced proportional sampling functionality
specifically designed for administrative region sampling with support
for numeric ratio-based sampling scenarios.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{sampling_ratio}}{Numeric sampling ratio (e.g., 0.1 for 10% sampling)}

\item{\code{base_sampler}}{Underlying administrative sampler
Initialize Proportional Regional Sampler

Initialize the proportional regional sampling system.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ProportionalRegionalSampler-new}{\code{ProportionalRegionalSampler$new()}}
\item \href{#method-ProportionalRegionalSampler-sample_by_ratio}{\code{ProportionalRegionalSampler$sample_by_ratio()}}
\item \href{#method-ProportionalRegionalSampler-sample_population_proportional}{\code{ProportionalRegionalSampler$sample_population_proportional()}}
\item \href{#method-ProportionalRegionalSampler-sample_case_proportional}{\code{ProportionalRegionalSampler$sample_case_proportional()}}
\item \href{#method-ProportionalRegionalSampler-get_sampling_summary}{\code{ProportionalRegionalSampler$get_sampling_summary()}}
\item \href{#method-ProportionalRegionalSampler-clone}{\code{ProportionalRegionalSampler$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProportionalRegionalSampler-new"></a>}}
\if{latex}{\out{\hypertarget{method-ProportionalRegionalSampler-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProportionalRegionalSampler$new(config = list())}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{config}}{Optional configuration list}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Invisible self for method chaining
Sample by Ratio

Generate samples based on a specified numeric ratio relative to a variable.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProportionalRegionalSampler-sample_by_ratio"></a>}}
\if{latex}{\out{\hypertarget{method-ProportionalRegionalSampler-sample_by_ratio}{}}}
\subsection{Method \code{sample_by_ratio()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProportionalRegionalSampler$sample_by_ratio(
  admin_polygons,
  variable_column,
  ratio = 1,
  min_samples = 1,
  max_samples = Inf,
  admin_column = NULL,
  concurrent = FALSE,
  seed = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{admin_polygons}}{sf object with administrative polygons}

\item{\code{variable_column}}{Column name containing the variable to sample from}

\item{\code{ratio}}{Numeric sampling ratio (e.g., 0.1 for 1:10, 1.0 for 1:1, 2.0 for 2:1)}

\item{\code{min_samples}}{Minimum samples per region (default: 1)}

\item{\code{max_samples}}{Maximum samples per region (default: Inf)}

\item{\code{admin_column}}{Column name for administrative unit identifiers}

\item{\code{concurrent}}{Logical for concurrent processing}

\item{\code{seed}}{Optional random seed for reproducibility}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
sf object with sampled points
Sample Population Proportional

Convenience function for population-based proportional sampling.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProportionalRegionalSampler-sample_population_proportional"></a>}}
\if{latex}{\out{\hypertarget{method-ProportionalRegionalSampler-sample_population_proportional}{}}}
\subsection{Method \code{sample_population_proportional()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProportionalRegionalSampler$sample_population_proportional(
  admin_polygons,
  population_column,
  ratio = 0.001,
  min_samples = 1,
  max_samples = 1000,
  admin_column = NULL,
  concurrent = FALSE,
  seed = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{admin_polygons}}{sf object with administrative polygons}

\item{\code{population_column}}{Column name containing population data}

\item{\code{ratio}}{Numeric sampling ratio (default: 0.001 for 1:1000)}

\item{\code{min_samples}}{Minimum samples per region (default: 1)}

\item{\code{max_samples}}{Maximum samples per region (default: 1000)}

\item{\code{admin_column}}{Column name for administrative unit identifiers}

\item{\code{concurrent}}{Logical for concurrent processing}

\item{\code{seed}}{Optional random seed for reproducibility}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
sf object with sampled points
Sample Case Proportional

Convenience function for case-based proportional sampling (e.g., disease cases).
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProportionalRegionalSampler-sample_case_proportional"></a>}}
\if{latex}{\out{\hypertarget{method-ProportionalRegionalSampler-sample_case_proportional}{}}}
\subsection{Method \code{sample_case_proportional()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProportionalRegionalSampler$sample_case_proportional(
  admin_polygons,
  case_column,
  ratio = 0.1,
  min_samples = 1,
  max_samples = 100,
  admin_column = NULL,
  concurrent = FALSE,
  seed = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{admin_polygons}}{sf object with administrative polygons}

\item{\code{case_column}}{Column name containing case data}

\item{\code{ratio}}{Numeric sampling ratio (default: 0.1 for 1:10)}

\item{\code{min_samples}}{Minimum samples per region (default: 1)}

\item{\code{max_samples}}{Maximum samples per region (default: 100)}

\item{\code{admin_column}}{Column name for administrative unit identifiers}

\item{\code{concurrent}}{Logical for concurrent processing}

\item{\code{seed}}{Optional random seed for reproducibility}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
sf object with sampled points
Get Sampling Summary

Get summary statistics for the sampling operation.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProportionalRegionalSampler-get_sampling_summary"></a>}}
\if{latex}{\out{\hypertarget{method-ProportionalRegionalSampler-get_sampling_summary}{}}}
\subsection{Method \code{get_sampling_summary()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProportionalRegionalSampler$get_sampling_summary(sampled_data)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{sampled_data}}{sf object with sampled points (from sampling functions)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
List containing sampling summary statistics
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProportionalRegionalSampler-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ProportionalRegionalSampler-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProportionalRegionalSampler$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
