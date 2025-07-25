% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coordinate-detector.R
\name{CoordinateDetector}
\alias{CoordinateDetector}
\title{Coordinate System Detection and Auto-Transformation}
\description{
Coordinate System Detection and Auto-Transformation

Coordinate System Detection and Auto-Transformation
}
\details{
Automatically detects coordinate systems in spatial data and provides
seamless transformation functions for provider switching. This enables
automatic handling of different coordinate systems without manual intervention.


The CoordinateDetector class analyzes spatial data to determine the most
likely coordinate reference system based on coordinate ranges, patterns,
and geographic context. It supports detection of WGS84, GCJ02, and BD09
coordinate systems commonly used in mapping applications.
}
\examples{
\donttest{
# Create coordinate detector
detector <- CoordinateDetector$new()

# Detect coordinate system from data
data <- data.frame(lon = c(116.3974, 121.4737), lat = c(39.9093, 31.2304))
detected_crs <- detector$detect_crs(data)

# Auto-transform data for a provider
gaode_data <- detector$auto_transform_for_provider(data, "gaode")
}

}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-CoordinateDetector-new}{\code{CoordinateDetector$new()}}
\item \href{#method-CoordinateDetector-detect_crs}{\code{CoordinateDetector$detect_crs()}}
\item \href{#method-CoordinateDetector-auto_transform_for_provider}{\code{CoordinateDetector$auto_transform_for_provider()}}
\item \href{#method-CoordinateDetector-validate_transformation_accuracy}{\code{CoordinateDetector$validate_transformation_accuracy()}}
\item \href{#method-CoordinateDetector-get_supported_crs}{\code{CoordinateDetector$get_supported_crs()}}
\item \href{#method-CoordinateDetector-is_china_region}{\code{CoordinateDetector$is_china_region()}}
\item \href{#method-CoordinateDetector-clone}{\code{CoordinateDetector$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CoordinateDetector-new"></a>}}
\if{latex}{\out{\hypertarget{method-CoordinateDetector-new}{}}}
\subsection{Method \code{new()}}{
Initialize the coordinate detector
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CoordinateDetector$new()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CoordinateDetector-detect_crs"></a>}}
\if{latex}{\out{\hypertarget{method-CoordinateDetector-detect_crs}{}}}
\subsection{Method \code{detect_crs()}}{
Detect the coordinate reference system of spatial data
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CoordinateDetector$detect_crs(
  data,
  lon_col = NULL,
  lat_col = NULL,
  confidence_threshold = 0.7
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{Data frame, matrix, or vector with coordinate data}

\item{\code{lon_col}}{Name or index of longitude column (default: auto-detect)}

\item{\code{lat_col}}{Name or index of latitude column (default: auto-detect)}

\item{\code{confidence_threshold}}{Minimum confidence level for detection (0-1, default: 0.7)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
List with detected CRS and confidence score
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CoordinateDetector-auto_transform_for_provider"></a>}}
\if{latex}{\out{\hypertarget{method-CoordinateDetector-auto_transform_for_provider}{}}}
\subsection{Method \code{auto_transform_for_provider()}}{
Auto-transform data for a specific provider
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CoordinateDetector$auto_transform_for_provider(
  data,
  provider,
  source_crs = NULL,
  lon_col = NULL,
  lat_col = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{Spatial data to transform}

\item{\code{provider}}{Target provider name}

\item{\code{source_crs}}{Source CRS (if NULL, will be auto-detected)}

\item{\code{lon_col}}{Longitude column name/index}

\item{\code{lat_col}}{Latitude column name/index}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Transformed data appropriate for the provider
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CoordinateDetector-validate_transformation_accuracy"></a>}}
\if{latex}{\out{\hypertarget{method-CoordinateDetector-validate_transformation_accuracy}{}}}
\subsection{Method \code{validate_transformation_accuracy()}}{
Validate coordinate accuracy after transformation
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CoordinateDetector$validate_transformation_accuracy(
  original_data,
  transformed_data,
  source_crs,
  target_crs,
  tolerance_meters = 1,
  sample_size = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{original_data}}{Original data}

\item{\code{transformed_data}}{Transformed data}

\item{\code{source_crs}}{Source coordinate system}

\item{\code{target_crs}}{Target coordinate system}

\item{\code{tolerance_meters}}{Tolerance in meters (default: 1.0)}

\item{\code{sample_size}}{Number of points to validate (default: min(100, nrow))}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
List with validation results
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CoordinateDetector-get_supported_crs"></a>}}
\if{latex}{\out{\hypertarget{method-CoordinateDetector-get_supported_crs}{}}}
\subsection{Method \code{get_supported_crs()}}{
Get supported coordinate systems for detection
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CoordinateDetector$get_supported_crs()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Character vector of supported CRS names
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CoordinateDetector-is_china_region"></a>}}
\if{latex}{\out{\hypertarget{method-CoordinateDetector-is_china_region}{}}}
\subsection{Method \code{is_china_region()}}{
Check if coordinates are likely in China (affects CRS detection)
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CoordinateDetector$is_china_region(coords)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{coords}}{Matrix or data frame with coordinates}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Logical indicating if coordinates are in China region
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CoordinateDetector-clone"></a>}}
\if{latex}{\out{\hypertarget{method-CoordinateDetector-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CoordinateDetector$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
