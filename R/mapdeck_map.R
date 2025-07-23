#' mapdeck
#'
#' @import htmlwidgets
#'
#' @param token Access token for the specified provider. Use \code{set_token()} or 
#' \code{Sys.setenv()} to set a global token. See Access Tokens section for details.
#' If left empty layers will still be plotted, but without a base map.
#' @param provider Character string identifying the map provider to use. 
#' Options: "mapbox", "leaflet", "openlayers", "gaode", "baidu". 
#' Defaults to "mapbox" for backward compatibility.
#' @param data data to be used in the layer. All coordinates are expected to be
#' EPSG:4326 (WGS 84) coordinate system
#' @param width the width of the map
#' @param height the height of the map
#' @param padding the padding of the map
#' @param style the style of the map (see \link{mapdeck_style})
#' @param pitch the pitch angle of the map
#' @param zoom zoom level of the map
#' @param bearing bearing of the map between 0 and 360
#' @param max_zoom sets the maximum zoom level
#' @param min_zoom sets the minimum zoom level
#' @param max_pitch sets the maximum pitch
#' @param min_pitch sets the minimum pitch
#' @param location unnamed vector of lon and lat coordinates (in that order)
#' @param libraries additional libraries required by some layers. Currently
#' 'h3' is required for \link{add_h3}.
#' @param show_view_state logical, indicating whether to add the current View State to the map.
#' When \code{TRUE}, the following is added as an overlay to the map
#' \itemize{
#'   \item{width}
#'   \item{height}
#'   \item{latitude & longitude}
#'   \item{zoom}
#'   \item{bearing}
#'   \item{pitch}
#'   \item{altitude}
#'   \item{viewBounds}
#'   \item{interactionState}
#' }
#'
#' @param repeat_view Logical indicating if the layers should repeat at low zoom levels
#'
#' @section Access Tokens:
#'
#' If the \code{token} argument is not used, the map will search for the token, firstly by
#' checking if \code{set_token()} was used, then it will search environment variables.
#' For Mapbox, it searches: c("MAPBOX_TOKEN","MAPBOX_KEY","MAPBOX_API_TOKEN", 
#' "MAPBOX_API_KEY", "MAPBOX", "MAPDECK"). Other providers have their own environment
#' variable patterns.
#'
#' If multiple tokens are found, the first one is used
#'
#' @export
mapdeck <- function(
	data = NULL,
	token = NULL,
	provider = "mapbox",
	width = NULL,
	height = NULL,
	padding = 0,
	style = NULL,
	pitch = 0,
	zoom = 0,
	bearing = 0,
	libraries = NULL,
	max_zoom = 20,
	min_zoom = 0,
	max_pitch = 60,
	min_pitch = 0,
	location = c(0, 0),
	show_view_state = FALSE,
	repeat_view = FALSE
	) {

  # Validate provider
  if (!is.character(provider) || length(provider) != 1) {
    stop("Provider must be a single character string")
  }
  
  # For backward compatibility, if token is NULL, try to get it for the provider
  if (is.null(token)) {
    # Use get_access_token which doesn't depend on token-store.R
    token <- get_access_token(provider)
  }
  
  # Create provider configuration
  provider_config <- list(
    token = token,
    style = style,
    pitch = pitch,
    zoom = zoom,
    bearing = bearing,
    location = location,
    max_zoom = max_zoom,
    min_zoom = min_zoom,
    max_pitch = max_pitch,
    min_pitch = min_pitch
  )
  
  # Map options for provider
  map_options <- list(
    data = data,
    width = width,
    height = height,
    padding = padding,
    libraries = libraries,
    show_view_state = show_view_state,
    repeat_view = repeat_view
  )
  
  # Check if provider system is available and provider is implemented
  provider_system_available <- FALSE
  
  if (provider %in% c("mapbox", "leaflet", "openlayers", "gaode", "baidu")) {
    # Try to use provider system
    tryCatch({
      # Check if provider factory is available
      factory <- get_provider_factory()
      
      # Check if provider is registered and has a factory class
      if (factory$registry$is_registered(provider)) {
        provider_class <- factory$registry$get_provider_factory(provider)
        
        if (!is.null(provider_class)) {
          # Create provider instance
          map_provider <- create_provider(provider, provider_config)
          
          # Create map using provider
          mapdeckmap <- map_provider$create_map(options = map_options)
          
          # Store provider reference in map object for later use
          attr(mapdeckmap, "mapdeck_provider") <- map_provider
          
          provider_system_available <- TRUE
          return(mapdeckmap)
        }
      }
    }, error = function(e) {
      # Log the error but continue to fallback
      if (getOption("mapdeck.debug", FALSE)) {
        message(sprintf("Provider system error for %s: %s", provider, e$message))
      }
    })
  } else {
    stop(sprintf("Unknown provider '%s'. Available providers: mapbox, leaflet, openlayers, gaode, baidu", provider))
  }
  
  # If provider system is not available or failed, use fallback implementation
  if (!provider_system_available) {
    if (provider != "mapbox") {
      # For non-Mapbox providers, inform user that implementation is not complete
      message(sprintf("Provider '%s' implementation is not yet complete. Using legacy Mapbox-compatible implementation.", provider))
    }
  }
  
  # Legacy implementation (backward compatibility)
  # This maintains the exact same behavior as before
  x = list(
    access_token = force( token )
    , style = force( style )
    , pitch = force( pitch )
    , zoom = force( zoom )
    , location = force( as.numeric( location ) )
    , bearing = force( bearing )
    , max_zoom = force( max_zoom )
    , min_zoom = force( min_zoom )
    , max_pitch = force( max_pitch )
    , min_pitch = force( min_pitch )
    , show_view_state = force( show_view_state )
    , repeat_view = force( repeat_view )
  )

  # create widget
  mapdeckmap <- htmlwidgets::createWidget(
    name = 'mapdeck',
    x = structure(
    	x,
    	mapdeck_data = data
    ),
    width = width,
    height = height,
    package = 'mapdeck',
    sizingPolicy = htmlwidgets::sizingPolicy(
    	defaultWidth = '100%',
    	defaultHeight = 800,
    	padding = padding,
    	browser.fill = FALSE
    )
  )

  mapdeckmap <- add_dependencies( mapdeckmap )

  # Add provider-specific dependencies
  provider_deps <- switch(provider,
    "mapbox" = mapboxgl(),
    "leaflet" = c(leaflet_js(), leaflet_deckgl_adapter()),
    "openlayers" = c(openlayers_js(), openlayers_deckgl_adapter()),
    "gaode" = gaode_adapter(),
    "baidu" = baidu_adapter(),
    mapboxgl()  # Default to mapbox
  )
  
  mapdeckmap$dependencies <- c(
    if ('h3' %in% libraries) mapdeckH3JSDependency() else NULL,
    mapdeckmap$dependencies,
    provider_deps,
    provider_dependencies_js(),  # Always include provider dependencies manager
    mapdeck_js(),
    htmlwidgets_js()
  )

  return(mapdeckmap)
}


#' update style
#'
#' @param map a mapdeck map object
#' @param style the style of the map (see \link{mapdeck_style})
#'
#' @export
update_style <- function( map, style ) {
	invoke_method(
		map, "md_update_style", style
	)
}


#' Shiny bindings for mapdeck
#'
#' Output and render functions for using mapdeck within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a mapdeck
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name mapdeck-shiny
#'
#' @export
mapdeckOutput <- function(outputId, width = '100%', height = '400px'){
	htmlwidgets::shinyWidgetOutput(outputId, 'mapdeck', width, height, package = 'mapdeck')
}


#' @rdname mapdeck-shiny
#' @export
renderMapdeck <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, mapdeckOutput, env, quoted = TRUE)
}



#' Mapdeck update
#'
#' Update a Mapdeck map in a shiny app. Use this function whenever the map needs
#' to respond to reactive content.
#'
#' @param map_id string containing the output ID of the map in a shiny application.
#' @param session the Shiny session object to which the map belongs; usually the
#' default value will suffice.
#' @param data data to be used in the layer. All coordinates are expected to be
#' EPSG:4326 (WGS 84) coordinate system
#' @param deferUntilFlush indicates whether actions performed against this
#' instance should be carried out right away, or whether they should be held until
#' after the next time all of the outputs are updated; defaults to TRUE.
#' @param map_type either mapdeck_update or google_map_update
#' @export
mapdeck_update <- function(
	data = NULL,
	map_id,
	session = shiny::getDefaultReactiveDomain(),
	deferUntilFlush = TRUE,
	map_type = c("mapdeck_update", "google_map_update")
	) {

	map_type <- match.arg( map_type )

	if (is.null(session)) {
		stop("mapdeck - mapdeck_update must be called from the server function of a Shiny app")
	}

	structure(
		list(
			session = session,
			id = map_id,
			x = structure(
				list(),
				mapdeck_data = data
			),
			deferUntilFlush = deferUntilFlush,
			dependencies = NULL
		),
		class = c(map_type)
	)
}


#' Mapdeck view
#'
#' Changes the view of the of the map
#'
#' @inheritParams mapdeck
#' @param map a \code{mapdeck} map object
#' @param duration time in milliseconds of the transition
#' @param transition type of transition
#' @export
mapdeck_view <- function(
	map,
	location = NULL,
	zoom = NULL,
	pitch = NULL,
	bearing = NULL,
	duration = NULL,
	transition = c("linear", "fly")
	) {

	transition <- match.arg(transition)
	invoke_method(
		map, 'md_change_location', map_type( map ) , as.numeric( location ), zoom, pitch,
		bearing, duration, transition
		)
}

# Get Map Data
#
# extracts the data attribute from the map
#
# @param map a mapdeck map object
#
get_map_data <- function( map ) {
	attr( map$x, "mapdeck_data", exact = TRUE )
}

# map_type
#
# determines the source/type of map
map_type <- function( map ) {

	map_type <- attr( map, "class")
	if( any( c("mapdeck", "mapdeck_update") %in% map_type ) ) return( "mapdeck" )

	if( any( c("google_map", "google_map_update") %in% map_type ) ) return( "google_map" )

	return(NULL)
}
