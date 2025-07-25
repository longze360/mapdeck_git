% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/provider-utils.R
\name{validate_layer_config}
\alias{validate_layer_config}
\title{Validate Layer Configuration}
\usage{
validate_layer_config(layer, provider)
}
\arguments{
\item{layer}{List containing layer configuration}

\item{provider}{Character string identifying the provider}
}
\value{
Validated layer configuration
}
\description{
Validate layer configuration for a specific provider.
}
\examples{
\donttest{
# Validate layer configuration
layer <- validate_layer_config(list(type = "scatterplot"), "mapbox")
}

}
