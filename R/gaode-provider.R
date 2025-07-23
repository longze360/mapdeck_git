#' @export
GaodeProvider <- R6::R6Class(
  "GaodeProvider",
  inherit = IMapProvider,
  public = list(
    initialize = function(access_token = NULL) {
      super$initialize(access_token)
      private$.name <- "gaode"
      private$.url <- "https://webapi.amap.com/maps"
    },

    get_name = function() {
      return(private$.name)
    },

    get_url = function() {
      return(private$.url)
    },

    get_access_token = function() {
      return(private$.access_token)
    },

    get_supported_layers = function() {
      return(c("point", "line", "polygon"))
    },

    get_supported_styles = function() {
      return(c("normal", "dark", "light"))
    },

    get_default_style = function() {
      return("normal")
    },

    get_style_url = function(style) {
      return(paste0("amap://styles/", style))
    },

    get_tile_url = function(x, y, z, style) {
      return(paste0(
        "https://wprd02.is.autonavi.com/appmaptile?x=", x,
        "&y=", y, "&z=", z, "&style=",
        private$normalize_style(style)
      ))
    },

    get_wms_url = function(bbox, layers, style) {
      return(paste0(
        "https://wprd02.is.autonavi.com/appmaptile?service=WMS&version=1.1.0&request=GetMap&layers=",
        layers, "&bbox=", bbox, "&width=768&height=768&srs=EPSG:4326&format=image/png&styles=",
        private$normalize_style(style)
      ))
    },

    get_image_url = function(lon, lat, zoom, width, height, style) {
      return(paste0(
        "https://restapi.amap.com/v3/staticmap?location=",
        lon, ",", lat, "&zoom=", zoom, "&size=",
        width, "*", height, "&key=", private$.access_token,
        "&style=", private$normalize_style(style)
      ))
    },

    get_geocode_url = function(address) {
      return(paste0(
        "https://restapi.amap.com/v3/geocode/geo?address=",
        address, "&key=", private$.access_token
      ))
    },

    get_reverse_geocode_url = function(lon, lat) {
      return(paste0(
        "https://restapi.amap.com/v3/geocode/regeo?location=",
        lon, ",", lat, "&key=", private$.access_token
      ))
    },

    get_directions_url = function(origin, destination, mode) {
      return(paste0(
        "https://restapi.amap.com/v3/direction/", mode, "?origin=",
        origin, "&destination=", destination, "&key=", private$.access_token
      ))
    },

    get_elevation_url = function(lon, lat) {
      return(paste0(
        "https://restapi.amap.com/v3/elevation?location=",
        lon, ",", lat, "&key=", private$.access_token
      ))
    },

    get_places_url = function(query, region) {
      return(paste0(
        "https://restapi.amap.com/v3/place/text?keywords=",
        query, "&city=", region, "&key=", private$.access_token
      ))
    },

    get_distance_url = function(origins, destinations) {
      return(paste0(
        "https://restapi.amap.com/v3/distance?origins=",
        origins, "&destination=", destinations, "&key=", private$.access_token
      ))
    },

    get_map_url = function(lon, lat, zoom, style) {
      return(paste0(
        "https://www.amap.com/search?query=s&geoobj=",
        lon, "%7C", lat, "%7C", lon, "%7C", lat, "&zoom=",
        zoom, "&style=", private$normalize_style(style)
      ))
    },

    get_streetview_url = function(lon, lat, heading, pitch) {
      return(paste0(
        "https://www.amap.com/search?query=s&geoobj=",
        lon, "%7C", lat, "%7C", lon, "%7C", lat, "&heading=",
        heading, "&pitch=", pitch
      ))
    },

    get_traffic_url = function(lon, lat, zoom) {
      return(paste0(
        "https://www.amap.com/search?query=s&geoobj=",
        lon, "%7C", lat, "%7C", lon, "%7C", lat, "&zoom=",
        zoom, "&traffic=1"
      ))
    },

    get_satellite_url = function(lon, lat, zoom) {
      return(paste0(
        "https://www.amap.com/search?query=s&geoobj=",
        lon, "%7C", lat, "%7C", lon, "%7C", lat, "&zoom=",
        zoom, "&satellite=1"
      ))
    },

    get_terrain_url = function(lon, lat, zoom) {
      return(paste0(
        "https://www.amap.com/search?query=s&geoobj=",
        lon, "%7C", lat, "%7C", lon, "%7C", lat, "&zoom=",
        zoom, "&terrain=1"
      ))
    },

    get_transit_url = function(origin, destination) {
      return(paste0(
        "https://www.amap.com/search?query=s&origin=",
        origin, "&destination=", destination, "&transit=1"
      ))
    },

    get_driving_url = function(origin, destination) {
      return(paste0(
        "https://www.amap.com/search?query=s&origin=",
        origin, "&destination=", destination, "&driving=1"
      ))
    },

    get_walking_url = function(origin, destination) {
      return(paste0(
        "https://www.amap.com/search?query=s&origin=",
        origin, "&destination=", destination, "&walking=1"
      ))
    },

    get_cycling_url = function(origin, destination) {
      return(paste0(
        "https://www.amap.com/search?query=s&origin=",
        origin, "&destination=", destination, "&cycling=1"
      ))
    }
  ),
  private = list(
    .name = NULL,
    .url = NULL,
    .access_token = NULL,

    normalize_style = function(style) {
      if (style == "normal") {
        return("amap://styles/normal")
      } else if (style == "dark") {
        return("amap://styles/dark")
      } else if (style == "light") {
        return("amap://styles/light")
      } else {
        return(style)
      }
    }
  )
)

#' Add Layer
#'
#' Adds a layer to the map.
#'
#' @param map the map object
#' @param layer the layer to add
#' @return the map object
#' @export
add_layer <- function(map, layer) {
  map$add_layer(layer)
  return(map)
}
