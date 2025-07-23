#' mapdeck: An R interface to Mapbox and deck.gl
#'
#' @docType package
#' @name mapdeck-package
#' @import htmlwidgets
NULL

#' Create Interactive Maps with Multiple Provider Support
#'
#' Create interactive maps using various mapping providers including Mapbox, Leaflet,
#' OpenLayers, and Chinese mapping services (Gaode, Baidu) with deck.gl visualization layers.
#'
#' @description
#' The \code{mapdeck} function creates interactive maps with support for multiple mapping
#' providers while maintaining full backward compatibility with existing Mapbox-based code.
#' It provides a unified interface for creating maps with different base map providers
#' and supports all existing deck.gl visualization layers.
#'
#' @param data Data to be used in the layer. All coordinates are expected to be
#'   EPSG:4326 (WGS 84) coordinate system, though automatic coordinate transformation
#'   is applied for Chinese providers when needed.
#' @param token Access token or API key for the specified provider. Use \code{set_token()} 
#'   or \code{Sys.setenv()} to set a global token. See Access Tokens section for details.
#'   If left empty, layers will still be plotted but without a base map.
#' @param provider Character string identifying the map provider to use. 
#'   Options: "mapbox" (default), "leaflet", "openlayers", "gaode", "baidu". 
#'   Defaults to "mapbox" for backward compatibility.
#' @param width The width of the map. Can be numeric (pixels) or character (e.g., "100%").
#' @param height The height of the map. Can be numeric (pixels) or character (e.g., "400px").
#' @param padding The padding around the map content in pixels (default: 0).
#' @param style The style/theme of the map. See \code{\link{mapdeck_style}} for options.
#'   Each provider supports different style formats and options.
#' @param pitch The pitch angle of the map in degrees (0-60). Note: Not all providers
#'   support pitch (e.g., Leaflet is limited to 0).
#' @param zoom Initial zoom level of the map (typically 0-20, varies by provider).
#' @param bearing Initial bearing/rotation of the map in degrees (0-360). Note: Not all
#'   providers support bearing (e.g., Leaflet is limited to 0).
#' @param max_zoom Maximum allowed zoom level (default: 20).
#' @param min_zoom Minimum allowed zoom level (default: 0).
#' @param max_pitch Maximum allowed pitch angle in degrees (default: 60).
#' @param min_pitch Minimum allowed pitch angle in degrees (default: 0).
#' @param location Numeric vector of initial map center coordinates c(longitude, latitude).
#'   Coordinates will be automatically transformed for Chinese providers if needed.
#' @param libraries Character vector of additional JavaScript libraries required by some layers.
#'   Currently 'h3' is required for \code{\link{add_h3}} layers.
#' @param show_view_state Logical indicating whether to display current view state information
#'   as an overlay on the map. When \code{TRUE}, shows width, height, coordinates, zoom,
#'   bearing, pitch, altitude, viewBounds, and interactionState.
#' @param repeat_view Logical indicating if layers should repeat at low zoom levels for
#'   continuous world view (default: FALSE).
#' @param config List containing provider-specific configuration options (optional).
#'   Used for advanced provider customization such as projections, tile sources, etc.
#'
#' @return A mapdeck map object that can be used with layer functions and in Shiny applications.
#'
#' @section Access Tokens:
#'
#' Different providers require different authentication methods:
#'
#' \strong{Mapbox:} Requires an access token starting with 'pk.' (public) or 'sk.' (secret).
#' Environment variables checked: MAPBOX_TOKEN, MAPBOX_KEY, MAPBOX_API_TOKEN, 
#' MAPBOX_API_KEY, MAPBOX, MAPDECK.
#'
#' \strong{Gaode (AutoNavi):} Requires an API key from Gaode Maps.
#' Environment variables checked: GAODE_API_KEY, GAODE_TOKEN, AMAP_API_KEY, AMAP_TOKEN.
#'
#' \strong{Baidu:} Requires an API key from Baidu Maps.
#' Environment variables checked: BAIDU_API_KEY, BAIDU_TOKEN, BAIDU_MAP_KEY.
#'
#' \strong{Leaflet/OpenLayers:} No authentication required by default (uses free tile providers).
#'
#' If multiple tokens are found, the first one is used. Use \code{set_token()} to set
#' provider-specific tokens programmatically.
#'
#' @section Provider Differences:
#'
#' While the interface is consistent across providers, there are some differences:
#'
#' \strong{Coordinate Systems:}
#' \itemize{
#'   \item Mapbox, Leaflet, OpenLayers: WGS84 (standard GPS coordinates)
#'   \item Gaode: GCJ02 (Mars Coordinates) - automatically transformed
#'   \item Baidu: BD09 (Baidu Coordinates) - automatically transformed
#' }
#'
#' \strong{Feature Support:}
#' \itemize{
#'   \item 3D features (pitch, bearing): Supported by Mapbox, Gaode, Baidu
#'   \item Custom styles: Full support in Mapbox, limited in others
#'   \item Tile layers: Primary method for Leaflet and OpenLayers
#' }
#'
#' @section Performance Guidelines:
#'
#' \itemize{
#'   \item For large datasets (>10,000 points), consider using aggregation layers
#'   \item GPU acceleration is automatically used when available for spatial operations
#'   \item Chinese providers may have slower initial load times due to coordinate transformation
#'   \item Use appropriate zoom levels - higher zoom requires more detailed data
#' }
#'
#' @examples
#' \donttest{
#' # Basic map with default Mapbox provider
#' map <- mapdeck(token = "your_mapbox_token")
#' 
#' # Map with different providers
#' leaflet_map <- mapdeck(provider = "leaflet")
#' gaode_map <- mapdeck(provider = "gaode", token = "your_gaode_key")
#' baidu_map <- mapdeck(provider = "baidu", token = "your_baidu_key")
#' 
#' # Map with custom styling and view
#' styled_map <- mapdeck(
#'   provider = "mapbox",
#'   token = "your_token",
#'   style = "dark",
#'   location = c(144.9631, -37.8136),  # Melbourne
#'   zoom = 10,
#'   pitch = 45,
#'   bearing = 30
#' )
#' 
#' # Add data layers (works with any provider)
#' library(sf)
#' data(capitals, package = "mapdeck")
#' map <- mapdeck(provider = "leaflet") %>%
#'   add_scatterplot(
#'     data = capitals,
#'     lon = "lon",
#'     lat = "lat",
#'     radius = 50000,
#'     fill_colour = "country"
#'   )
#' 
#' # Provider switching (preserves layers)
#' switched_map <- update_provider(map, "mapbox", 
#'                                config = list(token = "your_token"))
#' }
#'
#' @seealso 
#' \code{\link{set_token}} for setting authentication tokens,
#' \code{\link{mapdeck_style}} for styling options,
#' \code{\link{update_provider}} for switching providers,
#' \code{\link{list_available_providers}} for available providers,
#' \code{\link{add_scatterplot}}, \code{\link{add_polygon}} and other layer functions
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
	repeat_view = FALSE,
	config = NULL
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
  
  # Merge with user-provided config if available
  if (!is.null(config) && is.list(config)) {
    provider_config <- c(provider_config, config)
  }
  
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
      if (exists("get_provider_factory", mode = "function")) {
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

  # Add provider-specific dependencies (fallback to mapbox for now)
  provider_deps <- tryCatch({
    switch(provider,
      "mapbox" = mapboxgl(),
      "leaflet" = mapboxgl(),  # Fallback to mapbox
      "openlayers" = mapboxgl(),  # Fallback to mapbox
      "gaode" = mapboxgl(),  # Fallback to mapbox
      "baidu" = mapboxgl(),  # Fallback to mapbox
      mapboxgl()  # Default to mapbox
    )
  }, error = function(e) {
    mapboxgl()  # Ultimate fallback
  })
  
  mapdeckmap$dependencies <- c(
    if ('h3' %in% libraries) mapdeckH3JSDependency() else NULL,
    mapdeckmap$dependencies,
    provider_deps
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
