% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coordinate-detector.R,
%   R/coordinate-transformer.R
\name{auto_transform_for_provider}
\alias{auto_transform_for_provider}
\title{Auto-transform data for provider}
\usage{
auto_transform_for_provider(
  data,
  provider,
  source_crs = NULL,
  validate_accuracy = FALSE
)

auto_transform_for_provider(
  data,
  provider,
  source_crs = NULL,
  validate_accuracy = FALSE
)
}
\arguments{
\item{data}{Spatial data to transform}

\item{provider}{Character string identifying the target provider}

\item{source_crs}{Optional source coordinate system (auto-detected if NULL)}

\item{validate_accuracy}{Logical indicating if transformation accuracy should be validated}

\item{lon_col}{Longitude column name/index}

\item{lat_col}{Latitude column name/index}
}
\value{
Transformed data appropriate for the provider

Transformed spatial data in the provider's coordinate system
}
\description{
Convenience function to automatically transform spatial data for a mapping provider.

This function automatically detects the coordinate system of input data and
transforms it to the coordinate system required by the specified map provider.
It provides seamless coordinate handling for multi-provider mapping.
}
\details{
Automatically transform data coordinates to match the coordinate system required by a map provider.


Provider coordinate systems:
\itemize{
  \item \strong{mapbox, leaflet, openlayers}: WGS84 (EPSG:4326)
  \item \strong{gaode}: GCJ02 (Mars Coordinates)
  \item \strong{baidu}: BD09 (Baidu Coordinates)
}

The function performs the following steps:
\enumerate{
  \item Detect source coordinate system (if not specified)
  \item Determine target coordinate system for provider
  \item Transform coordinates if systems differ
  \item Validate transformation accuracy (if requested)
}
}
\examples{
\donttest{
# Auto-transform data for Gaode Maps
data <- data.frame(lon = c(116.3974, 121.4737), lat = c(39.9093, 31.2304))
gaode_data <- auto_transform_for_provider(data, "gaode")
}

\donttest{
# Auto-transform data for Gaode provider
wgs84_data <- data.frame(lon = 116.3974, lat = 39.9093)
gaode_data <- auto_transform_for_provider(wgs84_data, "gaode")

# Auto-transform with explicit source CRS
baidu_data <- auto_transform_for_provider(wgs84_data, "baidu", source_crs = "WGS84")

# Auto-transform with accuracy validation
leaflet_data <- auto_transform_for_provider(
  gaode_data, 
  "leaflet", 
  validate_accuracy = TRUE
)
}

}
