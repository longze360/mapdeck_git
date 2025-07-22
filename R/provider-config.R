#' Provider Configuration Management
#'
#' Configuration management system for map providers, handling
#' provider-specific settings, validation, and defaults.
#'
#' @importFrom R6 R6Class
#' @keywords internal
NULL

#' Provider Configuration
#'
#' R6 class for managing provider-specific configuration options.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(name, type, defaults)}}{Initialize configuration}
#'   \item{\code{set_option(key, value)}}{Set a configuration option}
#'   \item{\code{get_option(key, default)}}{Get a configuration option}
#'   \item{\code{validate()}}{Validate the current configuration}
#'   \item{\code{merge_config(config)}}{Merge with another configuration}
#'   \item{\code{to_list()}}{Convert configuration to list}
#' }
#'
#' @keywords internal
ProviderConfig <- R6::R6Class(
  "ProviderConfig",
  private = list(
    .name = NULL,
    .type = NULL,
    .options = list(),
    .defaults = list(),
    .required_options = character(0),
    .validation_rules = list()
  ),
  
  public = list(
    #' Initialize Configuration
    #'
    #' Initialize a provider configuration with name, type, and defaults.
    #'
    #' @param name Character string identifying the provider
    #' @param type Character string specifying the provider type
    #' @param defaults List of default configuration options
    #' @param required_options Character vector of required option names
    #' @param validation_rules List of validation functions for options
    initialize = function(name, type, defaults = list(), 
                         required_options = character(0),
                         validation_rules = list()) {
      private$.name <- name
      private$.type <- type
      private$.defaults <- defaults
      private$.required_options <- required_options
      private$.validation_rules <- validation_rules
      
      # Initialize with defaults
      private$.options <- defaults
    },
    
    #' Set Option
    #'
    #' Set a configuration option value.
    #'
    #' @param key Character string identifying the option
    #' @param value Value to set for the option
    #' @return Invisible self for method chaining
    set_option = function(key, value) {
      # Validate option if validation rule exists
      if (key %in% names(private$.validation_rules)) {
        validation_fn <- private$.validation_rules[[key]]
        if (!validation_fn(value)) {
          stop(sprintf("Invalid value for option '%s'", key))
        }
      }
      
      private$.options[[key]] <- value
      invisible(self)
    },
    
    #' Get Option
    #'
    #' Get a configuration option value.
    #'
    #' @param key Character string identifying the option
    #' @param default Default value if option is not set
    #' @return Option value or default
    get_option = function(key, default = NULL) {
      if (key %in% names(private$.options)) {
        return(private$.options[[key]])
      }
      return(default)
    },
    
    #' Validate Configuration
    #'
    #' Validate the current configuration against requirements and rules.
    #'
    #' @return List with 'valid' (logical) and 'errors' (character vector)
    validate = function() {
      errors <- character(0)
      
      # Check required options
      missing_required <- setdiff(private$.required_options, names(private$.options))
      if (length(missing_required) > 0) {
        errors <- c(errors, sprintf(
          "Missing required options: %s",
          paste(missing_required, collapse = ", ")
        ))
      }
      
      # Validate individual options
      for (key in names(private$.options)) {
        if (key %in% names(private$.validation_rules)) {
          validation_fn <- private$.validation_rules[[key]]
          if (!validation_fn(private$.options[[key]])) {
            errors <- c(errors, sprintf("Invalid value for option '%s'", key))
          }
        }
      }
      
      return(list(
        valid = length(errors) == 0,
        errors = errors
      ))
    },
    
    #' Merge Configuration
    #'
    #' Merge this configuration with another configuration or list.
    #'
    #' @param config ProviderConfig instance or list to merge
    #' @return Invisible self for method chaining
    merge_config = function(config) {
      if (inherits(config, "ProviderConfig")) {
        config <- config$to_list()
      }
      
      if (!is.list(config)) {
        stop("Config must be a list or ProviderConfig instance")
      }
      
      for (key in names(config)) {
        self$set_option(key, config[[key]])
      }
      
      invisible(self)
    },
    
    #' Convert to List
    #'
    #' Convert the configuration to a standard list.
    #'
    #' @return List containing all configuration options
    to_list = function() {
      return(private$.options)
    },
    
    #' Get Provider Name
    #'
    #' Get the provider name for this configuration.
    #'
    #' @return Character string provider name
    get_name = function() {
      return(private$.name)
    },
    
    #' Get Provider Type
    #'
    #' Get the provider type for this configuration.
    #'
    #' @return Character string provider type
    get_type = function() {
      return(private$.type)
    }
  )
)

#' Create Provider Configuration
#'
#' Create a new provider configuration with the specified parameters.
#'
#' @param name Character string identifying the provider
#' @param type Character string specifying the provider type
#' @param defaults List of default configuration options
#' @param required_options Character vector of required option names
#' @param validation_rules List of validation functions for options
#' @return ProviderConfig instance
#' @export
create_provider_config <- function(name, type, defaults = list(),
                                  required_options = character(0),
                                  validation_rules = list()) {
  return(ProviderConfig$new(name, type, defaults, required_options, validation_rules))
}

#' Standard Provider Configurations
#'
#' Pre-defined configurations for standard provider types.
#'
#' @keywords internal
STANDARD_PROVIDER_CONFIGS <- list(
  mapbox = function() {
    create_provider_config(
      name = "mapbox",
      type = PROVIDER_TYPES$MAPBOX,
      defaults = list(
        style = "mapbox://styles/mapbox/streets-v9",
        pitch = 0,
        zoom = 0,
        bearing = 0,
        max_zoom = 20,
        min_zoom = 0,
        max_pitch = 60,
        min_pitch = 0,
        location = c(0, 0),
        show_view_state = FALSE,
        repeat_view = FALSE
      ),
      required_options = c("token"),
      validation_rules = list(
        token = function(x) is.character(x) && length(x) == 1 && nchar(x) > 0,
        style = function(x) is.character(x) && length(x) == 1,
        pitch = function(x) is.numeric(x) && length(x) == 1 && x >= 0 && x <= 60,
        zoom = function(x) is.numeric(x) && length(x) == 1 && x >= 0,
        bearing = function(x) is.numeric(x) && length(x) == 1 && x >= 0 && x < 360,
        location = function(x) is.numeric(x) && length(x) == 2
      )
    )
  },
  
  leaflet = function() {
    create_provider_config(
      name = "leaflet",
      type = PROVIDER_TYPES$LEAFLET,
      defaults = list(
        tile_layer = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution = "© OpenStreetMap contributors",
        zoom = 10,
        center = c(0, 0),
        min_zoom = 0,
        max_zoom = 18
      ),
      required_options = character(0),
      validation_rules = list(
        tile_layer = function(x) is.character(x) && length(x) == 1,
        attribution = function(x) is.character(x) && length(x) == 1,
        zoom = function(x) is.numeric(x) && length(x) == 1 && x >= 0,
        center = function(x) is.numeric(x) && length(x) == 2
      )
    )
  },
  
  openlayers = function() {
    create_provider_config(
      name = "openlayers",
      type = PROVIDER_TYPES$OPENLAYERS,
      defaults = list(
        source_type = "OSM",
        zoom = 10,
        center = c(0, 0),
        min_zoom = 0,
        max_zoom = 18,
        projection = "EPSG:3857"
      ),
      required_options = character(0),
      validation_rules = list(
        source_type = function(x) is.character(x) && length(x) == 1,
        zoom = function(x) is.numeric(x) && length(x) == 1 && x >= 0,
        center = function(x) is.numeric(x) && length(x) == 2,
        projection = function(x) is.character(x) && length(x) == 1
      )
    )
  },
  
  gaode = function() {
    create_provider_config(
      name = "gaode",
      type = PROVIDER_TYPES$GAODE,
      defaults = list(
        zoom = 10,
        center = c(116.397428, 39.90923), # Beijing
        coordinate_system = "GCJ02",
        map_style = "normal",
        features = c("bg", "point", "road", "building")
      ),
      required_options = c("api_key"),
      validation_rules = list(
        api_key = function(x) is.character(x) && length(x) == 1 && nchar(x) > 0,
        zoom = function(x) is.numeric(x) && length(x) == 1 && x >= 3 && x <= 18,
        center = function(x) is.numeric(x) && length(x) == 2,
        coordinate_system = function(x) x %in% c("WGS84", "GCJ02"),
        map_style = function(x) x %in% c("normal", "dark", "light", "satellite")
      )
    )
  },
  
  baidu = function() {
    create_provider_config(
      name = "baidu",
      type = PROVIDER_TYPES$BAIDU,
      defaults = list(
        zoom = 10,
        center = c(116.404, 39.915), # Beijing
        coordinate_system = "BD09",
        map_style = "normal",
        enable_scroll_wheel_zoom = TRUE
      ),
      required_options = c("api_key"),
      validation_rules = list(
        api_key = function(x) is.character(x) && length(x) == 1 && nchar(x) > 0,
        zoom = function(x) is.numeric(x) && length(x) == 1 && x >= 3 && x <= 19,
        center = function(x) is.numeric(x) && length(x) == 2,
        coordinate_system = function(x) x %in% c("WGS84", "GCJ02", "BD09"),
        map_style = function(x) x %in% c("normal", "dark", "light", "satellite", "googlelite")
      )
    )
  }
)

#' Get Standard Provider Configuration
#'
#' Get a pre-defined configuration for a standard provider type.
#'
#' @param provider_type Character string specifying the provider type
#' @return ProviderConfig instance
#' @export
get_standard_provider_config <- function(provider_type) {
  if (!provider_type %in% names(STANDARD_PROVIDER_CONFIGS)) {
    available <- paste(names(STANDARD_PROVIDER_CONFIGS), collapse = ", ")
    stop(sprintf(
      "Unknown provider type '%s'. Available types: %s",
      provider_type, available
    ))
  }
  
  config_fn <- STANDARD_PROVIDER_CONFIGS[[provider_type]]
  return(config_fn())
}