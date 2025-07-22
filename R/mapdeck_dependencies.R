## from htmltools::htmlDependency()
createHtmlDependency <- function(name, version, src, script = NULL, stylesheet = NULL, all_files = FALSE) {
	structure(
		list(
			name = name
			, version = version
			, src = list( file = src )
			, meta = NULL
			, script = script
			, stylesheet = stylesheet
			, head = NULL
			, attachment = NULL
			, package = NULL
			, all_files = all_files
		)
		, class = "html_dependency"
	)
}


addDependency <- function(map, dependencyFunction, priority = FALSE) {

	existingDeps <- sapply(map$dependencies, function(x) x[['name']])
	addingDependency <- sapply(dependencyFunction, function(x) x[['name']])

	if(!addingDependency %in% existingDeps)
		map$dependencies <- if(priority) c(dependencyFunction, map$dependencies) else c(map$dependencies, dependencyFunction)

	return(map)
}

#' Add Dependencies
#'
#' Adds the required mapdeck javascript dependencies to a map when not using a mapdeck map.
#'
#' @param map the map object to which dependencies will be added
#'
#' @examples
#' \donttest{
#'
#' ## use with a google map from googleway
#' library(googleway)
#'
#' set_key("GOOGLE_MAP_KEY")
#'
#' google_map() %>%
#' 	add_dependencies() %>%
#' 	add_scatterplot(
#' 		data = capitals
#' 		, lon = "lon"
#' 		, lat = "lat"
#' 		, fill_colour = "country"
#' 		, radius = 10000
#' 	)
#'
#' }
#'
#'
#'
#' @export
add_dependencies <- function( map ) {
	map$dependencies <- unique( c( map$dependencies, mapdeck_dependencies() ) )
	return( map )
}

#' Mapdeck Dependencies
#'
#' Adds the required mapdeck javascript dependencies to a Shiny UI when you want to use
#' mapdeck layers, but not with a mapdeck map.
#'
#'
#'
#' @export
mapdeck_dependencies <- function() {
	c(
		mapdeck_dep_functions()
		, deckgl_min_js()
		, mapdeck_dep_legend()
		, mapdeck_dep_title()
		, mapdeck_dep_location()
		, mapdeck_dep_colours()
		, mapdeck_dep_coordinates()
		, mapdeck_css()
		)
}

htmlwidgets_js <- function() {
	list(
		createHtmlDependency(
			name = "htmlwidgets",
			version = as.character( utils::packageVersion("htmlwidgets") ),
			src = system.file("www", package = "htmlwidgets"),
			script = c("htmlwidgets.js"),
			all_files = FALSE
		)
	)
}

mapdeck_js <- function() {
	list(
		createHtmlDependency(
			name = "mpadeck-binding",
			version = as.character( utils::packageVersion("mapdeck") ),
			src = system.file("htmlwidgets/", package = "mapdeck"),
			script = c("mapdeck.js"),
			all_files = FALSE
		)
	)
}

mapdeck_dep_functions <- function() {
	list(
		createHtmlDependency(
			name = "mpadeck_functions",
			version = "0.0.1",
			src = system.file("htmlwidgets/", package = "mapdeck"),
			script = c("mapdeck_functions.js"),
			all_files = FALSE
		)
	)
}

mapdeck_dep_coordinates <- function() {
	list(
		createHtmlDependency(
			name = "mapdeck_coordinates",
			version = "0.0.1",
			src = system.file("htmlwidgets/", package = "mapdeck"),
			script = c("mapdeck_coordinates.js"),
			all_files = FALSE
		)
	)
}

mapdeck_dep_colours <- function() {
	list(
		createHtmlDependency(
			name = "mapdeck_colours",
			version = "0.0.1",
			src = system.file("htmlwidgets/", package = "mapdeck"),
			script = c("mapdeck_colours.js"),
			all_files = FALSE
		)
	)
}

mapdeck_dep_location <- function() {
	list(
		createHtmlDependency(
			name = "mapdeck_location",
			version = "0.0.1",
			src = system.file("htmlwidgets/", package = "mapdeck"),
			script = c("mapdeck_location.js"),
			all_files = FALSE
		)
	)
}

# https://unpkg.com/deck.gl@latest/dist.min.js
deckgl_min_js <- function() {
	list(
		createHtmlDependency(
			name = "deckgl",
			version = "8.9.33",
			src = system.file("htmlwidgets/lib/", package = "mapdeck"),
			script = c("deckgl.min.js"),
			all_files = FALSE
		)
	)
}

## https://api.mapbox.com/mapbox-gl-js/v1.13.0/mapbox-gl.js
## https://api.mapbox.com/mapbox-gl-js/v1.13.0/mapbox-gl.css
mapboxgl <- function() {
	list(
		createHtmlDependency(
			name = "mapboxgl",
			version = "1.13.0",
			src = system.file("htmlwidgets/lib/", package = "mapdeck"),
			script = c("mapbox-gl.js"),
			stylesheet = c("mapbox-gl.css"),
			all_files = FALSE
		)
	)
}

# css specific to mapdeck map
mapdeck_css <- function() {
	list(
		createHtmlDependency(
			name = "mapdeck",
			version = "0.0.1",
			src = system.file("htmlwidgets/", package = "mapdeck"),
			stylesheet = c("mapdeck.css")
		)
	)
}

mapdeck_dep_legend <- function() {
	list(
		createHtmlDependency(
			name = "legend",
			version = "0.0.1",
			src = system.file("htmlwidgets/lib/map/", package = "mapdeck"),
			script = c("legend.js"),
			all_files = FALSE
		)
	)
}

mapdeck_dep_title <- function() {
	list(
		createHtmlDependency(
			name = "title",
			version = "0.0.1",
			src = system.file("htmlwidgets/lib/map/", package = "mapdeck"),
			script = c("title.js"),
			all_files = FALSE
		)
	)
}

## Leaflet.js dependencies
## https://unpkg.com/leaflet@1.9.4/dist/leaflet.js
## https://unpkg.com/leaflet@1.9.4/dist/leaflet.css
leaflet_js <- function() {
	list(
		createHtmlDependency(
			name = "leaflet",
			version = "1.9.4",
			src = system.file("htmlwidgets/lib/", package = "mapdeck"),
			script = c("leaflet.js"),
			stylesheet = c("leaflet.css"),
			all_files = FALSE
		)
	)
}

## Leaflet-deck.gl adapter
leaflet_deckgl_adapter <- function() {
	list(
		createHtmlDependency(
			name = "leaflet-deckgl-adapter",
			version = "0.0.1",
			src = system.file("htmlwidgets/lib/", package = "mapdeck"),
			script = c("leaflet-deckgl-adapter.js"),
			all_files = FALSE
		)
	)
}

## OpenLayers.js dependencies
## https://cdn.jsdelivr.net/npm/ol@v7.5.2/dist/ol.js
## https://cdn.jsdelivr.net/npm/ol@v7.5.2/ol.css
openlayers_js <- function() {
	list(
		createHtmlDependency(
			name = "openlayers",
			version = "7.5.2",
			src = system.file("htmlwidgets/lib/openlayers/", package = "mapdeck"),
			script = c("ol.js"),
			stylesheet = c("ol.css"),
			all_files = FALSE
		)
	)
}

## OpenLayers-deck.gl adapter
openlayers_deckgl_adapter <- function() {
	list(
		createHtmlDependency(
			name = "openlayers-deckgl-adapter",
			version = "0.0.1",
			src = system.file("htmlwidgets/lib/openlayers/", package = "mapdeck"),
			script = c("openlayers-adapter.js"),
			all_files = FALSE
		)
	)
}

