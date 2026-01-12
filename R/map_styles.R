#' Mapdeck Style
#'
#' Various styles available to all Mapbox accounts using a valid access token.
#' Available styles are listed at
#' \url{https://docs.mapbox.com/api/maps/#styles}.
#'
#' @param style one of streets, outdoors, light, dark, satellite, satellite-streets
#'
#' @examples
#' \donttest{
#'
#' ## You need a valid access token from Mapbox
#' key <- 'abc'
#'
#' ## set a map style
#' mapdeck(token = key, style = mapdeck_style("dark"))
#'
#' }
#'
#' @export
mapdeck_style <- function(
  style = c(
    "dark",
    "light",
    "outdoors",
    "streets",
    "satellite",
    "satellite-streets"
  )
) {
  style <- match.arg(style)
  return(
    switch(
      style,
      "dark" = "mapbox://styles/mapbox/dark-v10",
      "light" = "mapbox://styles/mapbox/light-v10",
      "outdoors" = "mapbox://styles/mapbox/outdoors-v11",
      "streets" = "mapbox://styles/mapbox/streets-v11",
      "satellite" = "mapbox://styles/mapbox/satellite-v9",
      "satellite-streets" = "mapbox://styles/mapbox/satellite-streets-v11"
    )
  )
}


#' Leaflet Map Styles
#'
#' Tile styles available for use with Leaflet maps (map_type = "leaflet").
#' These are free to use without an API token.
#'
#' @param style one of openstreetmap, carto-db-positron, carto-db-dark-matter,
#'   stamen-terrain, stadia-outdoor, esri-world-imagery, esri-world-topo
#' @param attribution optional custom attribution text
#'
#' @return A list with elements 'url' and 'attribution' for use with mapdeck()
#'
#' @examples
#' \donttest{
#' ## Use Leaflet with OpenStreetMap (no token required)
#' mapdeck(map_type = "leaflet", style = leaflet_style("openstreetmap")) %>%
#'   add_scatterplot(
#'     data = capitals,
#'     lat = "lat",
#'     lon = "lon"
#'   )
#'
#' ## Use Leaflet with CARTO Dark Matter
#' mapdeck(map_type = "leaflet", style = leaflet_style("carto-db-dark-matter")) %>%
#'   add_arc(
#'     data = flights,
#'     origin = c("start_lon", "start_lat"),
#'     destination = c("end_lon", "end_lat")
#'   )
#' }
#'
#' @export
leaflet_style <- function(
  style = c(
    "openstreetmap",
    "carto-db-positron",
    "carto-db-dark-matter",
    "stamen-terrain",
    "stadia-outdoor",
    "esri-world-imagery",
    "esri-world-topo"
  ),
  attribution = NULL
) {
  style <- match.arg(style)

  styles <- list(
    "openstreetmap" = list(
      url = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
      attribution = '© <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
    ),
    "carto-db-positron" = list(
      url = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
      attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>'
    ),
    "carto-db-dark-matter" = list(
      url = "https://{s}.basemaps.cartocdn.org/dark_all/{z}/{x}/{y}{r}.png",
      attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>'
    ),
    "stamen-terrain" = list(
      url = "https://tiles.stadiamaps.com/tiles/stamen_terrain/{z}/{x}/{y}.png",
      attribution = '&copy; <a href="https://stadiamaps.com/">Stadia Maps</a>, &copy; <a href="https://openmaptiles.org/">OpenMapTiles</a> &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>'
    ),
    "stadia-outdoor" = list(
      url = "https://tiles.stadiamaps.com/tiles/stadia_outdoor/{z}/{x}/{y}.png",
      attribution = '&copy; <a href="https://stadiamaps.com/">Stadia Maps</a>, &copy; <a href="https://openmaptiles.org/">OpenMapTiles</a> &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>'
    ),
    "esri-world-imagery" = list(
      url = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
      attribution = '&copy; <a href="https://www.esri.com/">Esri</a>'
    ),
    "esri-world-topo" = list(
      url = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}",
      attribution = '&copy; <a href="https://www.esri.com/">Esri</a>'
    )
  )

  selected <- styles[[style]]

  if (!is.null(attribution)) {
    selected$attribution <- attribution
  }

  return(selected)
}
