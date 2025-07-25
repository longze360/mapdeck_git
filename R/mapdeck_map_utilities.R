#' mapdeck dispatch
#'
#' Extension points for plugins
#'
#' @param map a map object, as returned from \code{\link{mapdeck}}
#' @param funcName the name of the function that the user called that caused
#'   this \code{mapdeck_dispatch} call; for error message purposes
#' @param mapdeck an action to be performed if the map is from
#'   \code{\link{mapdeck}}
#' @param mapdeck_update an action to be performed if the map is from
#'   \code{\link{mapdeck_update}}
#'
#' @return \code{mapdeck_dispatch} returns the value of \code{mapdeck} or
#' or an error. \code{invokeMethod} returns the
#' \code{map} object that was passed in, possibly modified.
#'
#' @export
mapdeck_dispatch = function(
  map,
  funcName,
  mapdeck = stop(paste(funcName, "requires a map update object")),
  mapdeck_update = stop(paste(funcName, "does not support map update objects"))
  ) {

  if (inherits(map, "mapdeck") | inherits(map, "google_map") )
    return(mapdeck)
  else if (inherits(map, "mapdeck_update") | inherits(map, "google_map_update"))
    return(mapdeck_update)
  else
    stop("mapdeck - Invalid map parameter")
}


#' @param method the name of the JavaScript method to invoke
#' @param ... unnamed arguments to be passed to the JavaScript method
#' @rdname mapdeck_dispatch
#' @export
invoke_method = function(map, method, ...) {
	args = evalFormula(list(...))
	mapdeck_dispatch(
		map,
		method,
		mapdeck = {
			x = map$x$calls
			if (is.null(x)) x = list()
			n = length(x)
			x[[n + 1]] = list(functions = method, args = args)
			map$x$calls = x
			map
		},
		mapdeck_update = {
			invoke_remote(map, method, args)
		}
	)
}


invoke_remote = function(map, method, args = list()) {

  if (!( inherits(map, "mapdeck_update") | inherits(map, "google_map_update") ) )
    stop("mapdeck - Invalid map parameter; mapdeck_update object was expected")


	calls <- "mapdeckmap-calls"
	if( inherits(map, "google_map_update")) calls <- "googlemap-calls"

  msg <- list(
    id = map$id,
    calls = list(
      list(
        dependencies = lapply(map$dependencies, shiny::createWebDependency),
        method = method,
        args = args
      )
    )
  )

  sess <- map$session
  if (map$deferUntilFlush) {

    sess$onFlushed(function() {
      sess$sendCustomMessage(calls, msg)
    }, once = TRUE)

  } else {
    sess$sendCustomMessage(calls, msg)
  }
  map
}


# Evaluate list members that are formulae, using the map data as the environment
# (if provided, otherwise the formula environment)
evalFormula = function(list, data) {
	evalAll = function(x) {
		if (is.list(x)) {
			structure(lapply(x, evalAll), class = class(x))
		} else resolveFormula(x, data)
	}
	evalAll(list)
}

resolveFormula = function(f, data) {
	if (!inherits(f, 'formula')) return(f)
	if (length(f) != 2L) stop("mapdeck - Unexpected two-sided formula: ", deparse(f))

	doResolveFormula(data, f)
}

doResolveFormula = function(data, f) {
	UseMethod("doResolveFormula")
}


#' @export
doResolveFormula.data.frame = function(data, f) {
	eval(f[[2]], data, environment(f))
}

# Layer Id
#
# Checks the layer_id parameter, and provides a default one if NULL
# @param layer_id
layerId <- function(
	layer_id,
	layer = layers <- c(
		"animated_arc"
		, "animated_line"
		, "arc"
		, "bitmap"
		, "column"
		, "cesium"
		, "i3s"
		, "geojson"
		, "greatcircle"
		, "grid"
		, "h3"
		, "heatmap"
		, "hexagon"
		, "line"
		, "mesh"
		, "path"
		, "pointcloud"
		, "polygon"
		, "scatterplot"
		, "screengrid"
		, "terrain"
		, "text"
		, "tile3d"
		, "title"
		, "trips"
	)
	) {

	layer <- match.arg( layer )
	if (!is.null(layer_id) & length(layer_id) != 1)
		stop("mapdeck - please provide a single value for 'layer_id'")

	if (is.null(layer_id)) {
		return(paste0(layer, "-defaultLayerId"))
	} else {
		return(layer_id)
	}
}

# returns 0-based index of all the list columns in a data.frame (sf object)
list_columns <- function(x, geometry_col) {
	geom_col <- which(names(x) == geometry_col)
	return( setdiff( which( vapply(x, is.list, T) ), geom_col ) - 1 )
}
