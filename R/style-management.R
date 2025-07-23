#' Enhanced Style Management System
#'
#' This file contains classes and functions for managing map styles across
#' different providers in a provider-agnostic way, including style resolution,
#' validation, and theming.
#'
#' @name style-management
NULL

#' Style Resolver Class
#'
#' R6 class for resolving generic style names to provider-specific styles.
#'
#' @description
#' The StyleResolver class provides a centralized system for mapping generic
#' style names to provider-specific style identifiers, enabling consistent
#' styling across different map providers.
#'
#'
#' @examples
#' \donttest{
#' # Create style resolver
#' resolver <- StyleResolver$new()
#' style <- resolver$resolve_style("streets", "mapbox")
#' }
#'
#' @export
StyleResolver <- R6::R6Class("StyleResolver",
  public = list(
    #' @field style_mappings Style mappings for providers
    style_mappings = NULL,
    
    #' @field default_styles Default styles for providers
    default_styles = NULL,
    
    #' @field style_categories Style categories
    style_categories = NULL,
    
    #' Initialize Style Resolver
    #'
    #' Create a new style resolver instance.
    #'
    #' @return New StyleResolver instance
    initialize = function() {
      self$style_mappings <- self$create_default_mappings()
      self$default_styles <- self$create_default_styles()
      self$style_categories <- self$create_style_categories()
    },
    
    #' Resolve Style
    #'
    #' Resolve a generic style name to a provider-specific style.
    #'
    #' @param style_name Character string identifying the generic style
    #' @param provider_name Character string identifying the provider
    #' @return Character string containing provider-specific style identifier
    resolve_style = function(style_name, provider_name) {
      if (!is.character(style_name) || length(style_name) != 1) {
        stop("Style name must be a single character string")
      }
      
      if (!is.character(provider_name) || length(provider_name) != 1) {
        stop("Provider name must be a single character string")
      }
      
      # Check if provider exists in mappings
      if (!provider_name %in% names(self$style_mappings)) {
        warning(sprintf("Unknown provider '%s', using default style", provider_name))
        return(self$get_default_style(provider_name))
      }
      
      provider_mappings <- self$style_mappings[[provider_name]]
      
      # Check if style exists for provider
      if (style_name %in% names(provider_mappings)) {
        return(provider_mappings[[style_name]])
      }
      
      # Try case-insensitive match
      style_lower <- tolower(style_name)
      mapping_names_lower <- tolower(names(provider_mappings))
      
      if (style_lower %in% mapping_names_lower) {
        match_index <- which(mapping_names_lower == style_lower)[1]
        return(provider_mappings[[match_index]])
      }
      
      # If no match found, return default style for provider
      warning(sprintf("Style '%s' not found for provider '%s', using default", 
                     style_name, provider_name))
      return(self$get_default_style(provider_name))
    },
    
    #' Get Default Style
    #'
    #' Get the default style for a provider.
    #'
    #' @param provider_name Character string identifying the provider
    #' @return Character string containing default style identifier
    get_default_style = function(provider_name) {
      if (provider_name %in% names(self$default_styles)) {
        return(self$default_styles[[provider_name]])
      }
      
      # Fallback defaults
      fallback_defaults <- list(
        "mapbox" = "mapbox://styles/mapbox/streets-v11",
        "leaflet" = "OpenStreetMap",
        "openlayers" = "OSM",
        "gaode" = "amap://styles/normal",
        "baidu" = "normal"
      )
      
      if (provider_name %in% names(fallback_defaults)) {
        return(fallback_defaults[[provider_name]])
      }
      
      return("default")
    },
    
    #' Get Available Styles
    #'
    #' Get list of available generic style names.
    #'
    #' @param category Character string to filter by category (optional)
    #' @return Character vector of available style names
    get_available_styles = function(category = NULL) {
      if (is.null(category)) {
        # Return all unique style names across all providers
        all_styles <- unique(unlist(lapply(self$style_mappings, names)))
        return(sort(all_styles))
      }
      
      if (category %in% names(self$style_categories)) {
        return(self$style_categories[[category]])
      }
      
      warning(sprintf("Unknown style category '%s'", category))
      return(character(0))
    },
    
    #' Get Style Categories
    #'
    #' Get list of available style categories.
    #'
    #' @return Character vector of category names
    get_style_categories = function() {
      return(names(self$style_categories))
    },
    
    #' Add Style Mapping
    #'
    #' Add a new style mapping for a provider.
    #'
    #' @param generic_name Character string for generic style name
    #' @param provider_name Character string identifying the provider
    #' @param provider_style Character string for provider-specific style
    #' @return Invisible self for method chaining
    add_style_mapping = function(generic_name, provider_name, provider_style) {
      if (!is.character(generic_name) || length(generic_name) != 1) {
        stop("Generic name must be a single character string")
      }
      
      if (!is.character(provider_name) || length(provider_name) != 1) {
        stop("Provider name must be a single character string")
      }
      
      if (!is.character(provider_style) || length(provider_style) != 1) {
        stop("Provider style must be a single character string")
      }
      
      # Initialize provider mappings if not exists
      if (!provider_name %in% names(self$style_mappings)) {
        self$style_mappings[[provider_name]] <- list()
      }
      
      # Add mapping
      self$style_mappings[[provider_name]][[generic_name]] <- provider_style
      
      invisible(self)
    },
    
    #' Create Default Mappings
    #'
    #' Create default style mappings for all providers.
    #'
    #' @return List of style mappings
    create_default_mappings = function() {
      list(
        "mapbox" = list(
          "streets" = "mapbox://styles/mapbox/streets-v11",
          "outdoors" = "mapbox://styles/mapbox/outdoors-v11",
          "light" = "mapbox://styles/mapbox/light-v10",
          "dark" = "mapbox://styles/mapbox/dark-v10",
          "satellite" = "mapbox://styles/mapbox/satellite-v9",
          "satellite_streets" = "mapbox://styles/mapbox/satellite-streets-v11",
          "navigation_day" = "mapbox://styles/mapbox/navigation-day-v1",
          "navigation_night" = "mapbox://styles/mapbox/navigation-night-v1"
        ),
        "leaflet" = list(
          "streets" = "OpenStreetMap",
          "outdoors" = "OpenStreetMap.HOT",
          "light" = "CartoDB.Positron",
          "dark" = "CartoDB.DarkMatter",
          "satellite" = "Esri.WorldImagery",
          "terrain" = "OpenTopoMap",
          "watercolor" = "Stamen.Watercolor",
          "toner" = "Stamen.Toner"
        ),
        "openlayers" = list(
          "streets" = "OSM",
          "outdoors" = "OSM.CycleMap",
          "light" = "CartoDB.Positron",
          "dark" = "CartoDB.DarkMatter",
          "satellite" = "ESRI.WorldImagery",
          "terrain" = "Stamen.Terrain",
          "watercolor" = "Stamen.Watercolor",
          "toner" = "Stamen.Toner"
        ),
        "gaode" = list(
          "streets" = "amap://styles/normal",
          "outdoors" = "amap://styles/fresh",
          "light" = "amap://styles/light",
          "dark" = "amap://styles/dark",
          "satellite" = "amap://styles/satellite",
          "hybrid" = "amap://styles/hybrid",
          "blue" = "amap://styles/blue",
          "wine" = "amap://styles/wine"
        ),
        "baidu" = list(
          "streets" = "normal",
          "outdoors" = "normal",
          "light" = "light",
          "dark" = "dark",
          "satellite" = "satellite",
          "hybrid" = "hybrid",
          "midnight" = "midnight",
          "googlelite" = "googlelite"
        )
      )
    },
    
    #' Create Default Styles
    #'
    #' Create default styles for each provider.
    #'
    #' @return List of default styles
    create_default_styles = function() {
      list(
        "mapbox" = "mapbox://styles/mapbox/streets-v11",
        "leaflet" = "OpenStreetMap",
        "openlayers" = "OSM",
        "gaode" = "amap://styles/normal",
        "baidu" = "normal"
      )
    },
    
    #' Create Style Categories
    #'
    #' Create style categories for organization.
    #'
    #' @return List of style categories
    create_style_categories = function() {
      list(
        "basic" = c("streets", "outdoors"),
        "monochrome" = c("light", "dark"),
        "satellite" = c("satellite", "satellite_streets", "hybrid"),
        "terrain" = c("terrain", "outdoors"),
        "artistic" = c("watercolor", "toner"),
        "navigation" = c("navigation_day", "navigation_night"),
        "themed" = c("blue", "wine", "midnight", "googlelite")
      )
    }
  )
)

#' Style Validator Class
#'
#' R6 class for validating style compatibility across providers.
#'
#' @description
#' The StyleValidator class provides validation functionality to ensure
#' that styles are compatible with specific providers and can be properly
#' resolved and applied.
#'
#'
#' @examples
#' \donttest{
#' # Create style validator
#' validator <- StyleValidator$new()
#' is_valid <- validator$validate_style("streets", "mapbox")
#' }
#'
#' @export
StyleValidator <- R6::R6Class("StyleValidator",
  public = list(
    #' @field supported_features Supported features by provider
    supported_features = NULL,
    
    #' @field validation_rules Validation rules
    validation_rules = NULL,
    
    #' Initialize Style Validator
    #'
    #' Create a new style validator instance.
    #'
    #' @return New StyleValidator instance
    initialize = function() {
      self$supported_features <- self$create_supported_features()
      self$validation_rules <- self$create_validation_rules()
    },
    
    #' Validate Style
    #'
    #' Validate that a style is compatible with a provider.
    #'
    #' @param style Character string or list containing style specification
    #' @param provider_name Character string identifying the provider
    #' @return Logical indicating if style is valid
    validate_style = function(style, provider_name) {
      if (!is.character(provider_name) || length(provider_name) != 1) {
        stop("Provider name must be a single character string")
      }
      
      # Check if provider is supported
      if (!provider_name %in% names(self$supported_features)) {
        warning(sprintf("Unknown provider '%s'", provider_name))
        return(FALSE)
      }
      
      # Handle NULL style
      if (is.null(style)) {
        return(TRUE)  # NULL style will use provider default
      }
      
      # Handle character style
      if (is.character(style) && length(style) == 1) {
        return(self$validate_character_style(style, provider_name))
      }
      
      # Handle list style (custom style object)
      if (is.list(style)) {
        return(self$validate_custom_style(style, provider_name))
      }
      
      return(FALSE)
    },
    
    #' Validate Character Style
    #'
    #' Validate a character-based style specification.
    #'
    #' @param style Character string containing style name or URL
    #' @param provider_name Character string identifying the provider
    #' @return Logical indicating if style is valid
    validate_character_style = function(style, provider_name) {
      provider_features <- self$supported_features[[provider_name]]
      
      # Check provider-specific URL patterns
      if (provider_name == "mapbox") {
        # Mapbox styles should start with mapbox:// or be valid URLs
        if (grepl("^mapbox://styles/", style) || grepl("^https?://", style)) {
          return(TRUE)
        }
      } else if (provider_name == "gaode") {
        # Gaode styles should start with amap://styles/
        if (grepl("^amap://styles/", style)) {
          return(TRUE)
        }
      }
      
      # Check if it's a generic style name that can be resolved
      resolver <- StyleResolver$new()
      available_styles <- resolver$get_available_styles()
      
      if (style %in% available_styles) {
        return(TRUE)
      }
      
      # Check case-insensitive match
      if (tolower(style) %in% tolower(available_styles)) {
        return(TRUE)
      }
      
      return(FALSE)
    },
    
    #' Validate Custom Style
    #'
    #' Validate a custom style object.
    #'
    #' @param style List containing custom style specification
    #' @param provider_name Character string identifying the provider
    #' @return Logical indicating if style is valid
    validate_custom_style = function(style, provider_name) {
      provider_features <- self$supported_features[[provider_name]]
      
      # Basic validation for custom styles
      if (!is.list(style)) {
        return(FALSE)
      }
      
      # Provider-specific validation
      if (provider_name == "mapbox") {
        # Mapbox custom styles should have version, sources, layers
        required_fields <- c("version", "sources", "layers")
        return(all(required_fields %in% names(style)))
      }
      
      # For other providers, basic validation
      return(TRUE)
    },
    
    #' Get Style Compatibility
    #'
    #' Get compatibility information for a style across providers.
    #'
    #' @param style Character string containing style specification
    #' @return List containing compatibility information
    get_style_compatibility = function(style) {
      if (!is.character(style) || length(style) != 1) {
        stop("Style must be a single character string")
      }
      
      compatibility <- list()
      
      for (provider in names(self$supported_features)) {
        compatibility[[provider]] <- self$validate_style(style, provider)
      }
      
      return(compatibility)
    },
    
    #' Create Supported Features
    #'
    #' Create supported features list for each provider.
    #'
    #' @return List of supported features
    create_supported_features = function() {
      list(
        "mapbox" = list(
          "custom_styles" = TRUE,
          "vector_tiles" = TRUE,
          "raster_tiles" = TRUE,
          "3d_terrain" = TRUE,
          "url_styles" = TRUE
        ),
        "leaflet" = list(
          "custom_styles" = FALSE,
          "vector_tiles" = FALSE,
          "raster_tiles" = TRUE,
          "3d_terrain" = FALSE,
          "url_styles" = FALSE
        ),
        "openlayers" = list(
          "custom_styles" = TRUE,
          "vector_tiles" = TRUE,
          "raster_tiles" = TRUE,
          "3d_terrain" = FALSE,
          "url_styles" = TRUE
        ),
        "gaode" = list(
          "custom_styles" = TRUE,
          "vector_tiles" = TRUE,
          "raster_tiles" = TRUE,
          "3d_terrain" = TRUE,
          "url_styles" = TRUE
        ),
        "baidu" = list(
          "custom_styles" = TRUE,
          "vector_tiles" = FALSE,
          "raster_tiles" = TRUE,
          "3d_terrain" = TRUE,
          "url_styles" = FALSE
        )
      )
    },
    
    #' Create Validation Rules
    #'
    #' Create validation rules for different providers.
    #'
    #' @return List of validation rules
    create_validation_rules = function() {
      list(
        "mapbox" = list(
          "url_pattern" = "^(mapbox://styles/|https?://)",
          "required_fields" = c("version", "sources", "layers"),
          "max_zoom" = 22,
          "min_zoom" = 0
        ),
        "leaflet" = list(
          "url_pattern" = "^https?://",
          "required_fields" = character(0),
          "max_zoom" = 20,
          "min_zoom" = 0
        ),
        "openlayers" = list(
          "url_pattern" = "^https?://",
          "required_fields" = character(0),
          "max_zoom" = 28,
          "min_zoom" = 0
        ),
        "gaode" = list(
          "url_pattern" = "^amap://styles/",
          "required_fields" = character(0),
          "max_zoom" = 20,
          "min_zoom" = 3
        ),
        "baidu" = list(
          "url_pattern" = NULL,
          "required_fields" = character(0),
          "max_zoom" = 19,
          "min_zoom" = 3
        )
      )
    }
  )
)

#' Theme Manager Class
#'
#' R6 class for managing consistent theming across providers.
#'
#' @description
#' The ThemeManager class provides functionality for creating and managing
#' consistent visual themes across different map providers, ensuring a
#' unified look and feel regardless of the underlying provider.
#'
#'
#' @examples
#' \donttest{
#' # Create theme manager
#' theme_manager <- ThemeManager$new()
#' theme_manager$apply_theme("dark", "mapbox")
#' }
#'
#' @export
ThemeManager <- R6::R6Class("ThemeManager",
  public = list(
    #' @field themes Available themes
    themes = NULL,
    
    #' @field current_theme Current active theme
    current_theme = NULL,
    
    #' Initialize Theme Manager
    #'
    #' Create a new theme manager instance.
    #'
    #' @return New ThemeManager instance
    initialize = function() {
      self$themes <- self$create_default_themes()
      self$current_theme <- NULL
    },
    
    #' Apply Theme
    #'
    #' Apply a theme to a specific provider.
    #'
    #' @param theme_name Character string identifying the theme
    #' @param provider_name Character string identifying the provider
    #' @return List containing theme configuration for the provider
    apply_theme = function(theme_name, provider_name) {
      if (!is.character(theme_name) || length(theme_name) != 1) {
        stop("Theme name must be a single character string")
      }
      
      if (!is.character(provider_name) || length(provider_name) != 1) {
        stop("Provider name must be a single character string")
      }
      
      # Check if theme exists
      if (!theme_name %in% names(self$themes)) {
        stop(sprintf("Unknown theme '%s'", theme_name))
      }
      
      theme <- self$themes[[theme_name]]
      
      # Get provider-specific configuration
      if (provider_name %in% names(theme$providers)) {
        provider_config <- theme$providers[[provider_name]]
      } else {
        # Use fallback configuration
        provider_config <- theme$fallback
      }
      
      # Set current theme
      self$current_theme <- theme_name
      
      return(provider_config)
    },
    
    #' Get Available Themes
    #'
    #' Get list of available theme names.
    #'
    #' @return Character vector of theme names
    get_available_themes = function() {
      return(names(self$themes))
    },
    
    #' Get Theme Info
    #'
    #' Get information about a specific theme.
    #'
    #' @param theme_name Character string identifying the theme
    #' @return List containing theme information
    get_theme_info = function(theme_name) {
      if (!is.character(theme_name) || length(theme_name) != 1) {
        stop("Theme name must be a single character string")
      }
      
      if (!theme_name %in% names(self$themes)) {
        stop(sprintf("Unknown theme '%s'", theme_name))
      }
      
      theme <- self$themes[[theme_name]]
      
      return(list(
        name = theme_name,
        description = theme$description,
        style_base = theme$style_base,
        supported_providers = names(theme$providers)
      ))
    },
    
    #' Create Custom Theme
    #'
    #' Create a custom theme configuration.
    #'
    #' @param name Character string for theme name
    #' @param description Character string describing the theme
    #' @param style_base Character string for base style
    #' @param provider_configs List of provider-specific configurations
    #' @return Invisible self for method chaining
    create_custom_theme = function(name, description, style_base, provider_configs) {
      if (!is.character(name) || length(name) != 1) {
        stop("Name must be a single character string")
      }
      
      if (!is.character(description) || length(description) != 1) {
        stop("Description must be a single character string")
      }
      
      if (!is.character(style_base) || length(style_base) != 1) {
        stop("Style base must be a single character string")
      }
      
      if (!is.list(provider_configs)) {
        stop("Provider configs must be a list")
      }
      
      # Create theme
      theme <- list(
        description = description,
        style_base = style_base,
        providers = provider_configs,
        fallback = list(
          style = style_base,
          colors = list(),
          options = list()
        )
      )
      
      # Add to themes
      self$themes[[name]] <- theme
      
      invisible(self)
    },
    
    #' Create Default Themes
    #'
    #' Create default theme configurations.
    #'
    #' @return List of default themes
    create_default_themes = function() {
      list(
        "light" = list(
          description = "Light theme with bright colors and high contrast",
          style_base = "light",
          providers = list(
            "mapbox" = list(
              style = "mapbox://styles/mapbox/light-v10",
              colors = list(
                background = "#ffffff",
                text = "#000000",
                accent = "#0066cc"
              ),
              options = list()
            ),
            "leaflet" = list(
              style = "CartoDB.Positron",
              colors = list(
                background = "#ffffff",
                text = "#000000",
                accent = "#0066cc"
              ),
              options = list()
            ),
            "openlayers" = list(
              style = "CartoDB.Positron",
              colors = list(
                background = "#ffffff",
                text = "#000000",
                accent = "#0066cc"
              ),
              options = list()
            ),
            "gaode" = list(
              style = "amap://styles/light",
              colors = list(
                background = "#ffffff",
                text = "#000000",
                accent = "#0066cc"
              ),
              options = list()
            ),
            "baidu" = list(
              style = "light",
              colors = list(
                background = "#ffffff",
                text = "#000000",
                accent = "#0066cc"
              ),
              options = list()
            )
          ),
          fallback = list(
            style = "light",
            colors = list(
              background = "#ffffff",
              text = "#000000",
              accent = "#0066cc"
            ),
            options = list()
          )
        ),
        "dark" = list(
          description = "Dark theme with muted colors and low contrast",
          style_base = "dark",
          providers = list(
            "mapbox" = list(
              style = "mapbox://styles/mapbox/dark-v10",
              colors = list(
                background = "#1a1a1a",
                text = "#ffffff",
                accent = "#66ccff"
              ),
              options = list()
            ),
            "leaflet" = list(
              style = "CartoDB.DarkMatter",
              colors = list(
                background = "#1a1a1a",
                text = "#ffffff",
                accent = "#66ccff"
              ),
              options = list()
            ),
            "openlayers" = list(
              style = "CartoDB.DarkMatter",
              colors = list(
                background = "#1a1a1a",
                text = "#ffffff",
                accent = "#66ccff"
              ),
              options = list()
            ),
            "gaode" = list(
              style = "amap://styles/dark",
              colors = list(
                background = "#1a1a1a",
                text = "#ffffff",
                accent = "#66ccff"
              ),
              options = list()
            ),
            "baidu" = list(
              style = "dark",
              colors = list(
                background = "#1a1a1a",
                text = "#ffffff",
                accent = "#66ccff"
              ),
              options = list()
            )
          ),
          fallback = list(
            style = "dark",
            colors = list(
              background = "#1a1a1a",
              text = "#ffffff",
              accent = "#66ccff"
            ),
            options = list()
          )
        ),
        "satellite" = list(
          description = "Satellite imagery theme with earth tones",
          style_base = "satellite",
          providers = list(
            "mapbox" = list(
              style = "mapbox://styles/mapbox/satellite-v9",
              colors = list(
                background = "#2d4a3e",
                text = "#ffffff",
                accent = "#90ee90"
              ),
              options = list()
            ),
            "leaflet" = list(
              style = "Esri.WorldImagery",
              colors = list(
                background = "#2d4a3e",
                text = "#ffffff",
                accent = "#90ee90"
              ),
              options = list()
            ),
            "openlayers" = list(
              style = "ESRI.WorldImagery",
              colors = list(
                background = "#2d4a3e",
                text = "#ffffff",
                accent = "#90ee90"
              ),
              options = list()
            ),
            "gaode" = list(
              style = "amap://styles/satellite",
              colors = list(
                background = "#2d4a3e",
                text = "#ffffff",
                accent = "#90ee90"
              ),
              options = list()
            ),
            "baidu" = list(
              style = "satellite",
              colors = list(
                background = "#2d4a3e",
                text = "#ffffff",
                accent = "#90ee90"
              ),
              options = list()
            )
          ),
          fallback = list(
            style = "satellite",
            colors = list(
              background = "#2d4a3e",
              text = "#ffffff",
              accent = "#90ee90"
            ),
            options = list()
          )
        )
      )
    }
  )
)

# Global instances
.style_resolver <- NULL
.style_validator <- NULL
.theme_manager <- NULL

#' Get Style Resolver
#'
#' Get the global style resolver instance, creating it if necessary.
#'
#' @return StyleResolver instance
#'
#' @examples
#' \donttest{
#' # Get global style resolver
#' resolver <- get_style_resolver()
#' style <- resolver$resolve_style("streets", "mapbox")
#' }
#'
#' @export
get_style_resolver <- function() {
  tryCatch({
    if (is.null(.style_resolver)) {
      .style_resolver <<- StyleResolver$new()
    }
    return(.style_resolver)
  }, error = function(e) {
    # Handle locked binding error during development
    if (grepl("locked binding", e$message)) {
      # Create a new instance without assigning to global variable
      return(StyleResolver$new())
    } else {
      stop(e)
    }
  })
}

#' Get Style Validator
#'
#' Get the global style validator instance, creating it if necessary.
#'
#' @return StyleValidator instance
#'
#' @examples
#' \donttest{
#' # Get global style validator
#' validator <- get_style_validator()
#' is_valid <- validator$validate_style("streets", "mapbox")
#' }
#'
#' @export
get_style_validator <- function() {
  tryCatch({
    if (is.null(.style_validator)) {
      .style_validator <<- StyleValidator$new()
    }
    return(.style_validator)
  }, error = function(e) {
    # Handle locked binding error during development
    if (grepl("locked binding", e$message)) {
      # Create a new instance without assigning to global variable
      return(StyleValidator$new())
    } else {
      stop(e)
    }
  })
}

#' Get Theme Manager
#'
#' Get the global theme manager instance, creating it if necessary.
#'
#' @return ThemeManager instance
#'
#' @examples
#' \donttest{
#' # Get global theme manager
#' theme_manager <- get_theme_manager()
#' theme_config <- theme_manager$apply_theme("dark", "mapbox")
#' }
#'
#' @export
get_theme_manager <- function() {
  tryCatch({
    if (is.null(.theme_manager)) {
      .theme_manager <<- ThemeManager$new()
    }
    return(.theme_manager)
  }, error = function(e) {
    # Handle locked binding error during development
    if (grepl("locked binding", e$message)) {
      # Create a new instance without assigning to global variable
      return(ThemeManager$new())
    } else {
      stop(e)
    }
  })
}

#' Get Available Styles for Provider
#'
#' Get a list of available map styles for a specific provider or all providers.
#'
#' @description
#' This function returns the available map styles that can be used with a specific
#' provider or across all providers. It provides both generic style names that
#' work across providers and provider-specific style identifiers.
#'
#' @param provider Character string identifying the provider (optional). If NULL,
#'   returns styles available across all providers.
#' @param category Character string to filter styles by category (optional).
#'   Categories include: "basic", "monochrome", "satellite", "terrain", "artistic", 
#'   "navigation", "themed".
#' @param include_provider_specific Logical indicating if provider-specific style
#'   identifiers should be included (default: FALSE).
#'
#' @return Character vector of available style names, or named list if 
#'   include_provider_specific is TRUE.
#'
#' @details
#' The function returns generic style names that are mapped to provider-specific
#' styles internally. Common generic styles include:
#' \itemize{
#'   \item \strong{Basic styles}: "streets", "outdoors"
#'   \item \strong{Monochrome}: "light", "dark"
#'   \item \strong{Satellite}: "satellite", "satellite_streets", "hybrid"
#'   \item \strong{Terrain}: "terrain"
#'   \item \strong{Artistic}: "watercolor", "toner"
#'   \item \strong{Navigation}: "navigation_day", "navigation_night"
#'   \item \strong{Themed}: "blue", "wine", "midnight"
#' }
#'
#' Provider-specific styles use different formats:
#' \itemize{
#'   \item \strong{Mapbox}: URLs like "mapbox://styles/mapbox/streets-v11"
#'   \item \strong{Leaflet}: Tile provider names like "OpenStreetMap", "CartoDB.Positron"
#'   \item \strong{OpenLayers}: Source names like "OSM", "CartoDB.Positron"
#'   \item \strong{Gaode}: URLs like "amap://styles/normal"
#'   \item \strong{Baidu}: Style names like "normal", "satellite"
#' }
#'
#' @examples
#' \donttest{
#' # Get all available generic styles
#' all_styles <- get_available_styles()
#' print(all_styles)
#' 
#' # Get styles for specific provider
#' mapbox_styles <- get_available_styles("mapbox")
#' leaflet_styles <- get_available_styles("leaflet")
#' 
#' # Get styles by category
#' satellite_styles <- get_available_styles(category = "satellite")
#' monochrome_styles <- get_available_styles(category = "monochrome")
#' 
#' # Get provider-specific style identifiers
#' detailed_styles <- get_available_styles("mapbox", include_provider_specific = TRUE)
#' print(detailed_styles)
#' 
#' # Check what styles are available for Chinese providers
#' gaode_styles <- get_available_styles("gaode")
#' baidu_styles <- get_available_styles("baidu")
#' }
#'
#' @seealso \code{\link{mapdeck_style}}, \code{\link{validate_map_style}}, 
#'   \code{\link{apply_map_theme}}
#'
#' @export
get_available_styles <- function(provider = NULL, category = NULL, include_provider_specific = FALSE) {
  
  if (!is.null(provider) && (!is.character(provider) || length(provider) != 1)) {
    stop("Provider must be a single character string or NULL")
  }
  
  if (!is.null(category) && (!is.character(category) || length(category) != 1)) {
    stop("Category must be a single character string or NULL")
  }
  
  if (!is.logical(include_provider_specific) || length(include_provider_specific) != 1) {
    stop("include_provider_specific must be a single logical value")
  }
  
  resolver <- get_style_resolver()
  
  if (is.null(provider)) {
    # Return generic styles available across all providers
    styles <- resolver$get_available_styles(category)
    
    if (include_provider_specific) {
      # Return detailed mapping for all providers
      result <- list()
      for (p in names(resolver$style_mappings)) {
        result[[p]] <- resolver$style_mappings[[p]]
      }
      return(result)
    }
    
    return(styles)
  }
  
  # Validate provider
  if (!provider %in% names(resolver$style_mappings)) {
    stop(sprintf("Unknown provider '%s'. Available providers: %s",
                provider, paste(names(resolver$style_mappings), collapse = ", ")))
  }
  
  if (include_provider_specific) {
    # Return provider-specific style identifiers
    provider_styles <- resolver$style_mappings[[provider]]
    
    if (!is.null(category)) {
      # Filter by category
      category_styles <- resolver$get_available_styles(category)
      provider_styles <- provider_styles[names(provider_styles) %in% category_styles]
    }
    
    return(provider_styles)
  }
  
  # Return generic style names available for this provider
  provider_styles <- names(resolver$style_mappings[[provider]])
  
  if (!is.null(category)) {
    category_styles <- resolver$get_available_styles(category)
    provider_styles <- provider_styles[provider_styles %in% category_styles]
  }
  
  return(provider_styles)
}

#' Apply Map Theme
#'
#' Apply a consistent theme across different map providers.
#'
#' @description
#' This function applies a predefined theme to a map, ensuring consistent
#' visual appearance across different providers. Themes include coordinated
#' color schemes, styles, and visual elements.
#'
#' @param map A mapdeck map object
#' @param theme_name Character string identifying the theme to apply.
#'   Available themes: "light", "dark", "satellite".
#' @param provider Character string identifying the provider (optional, 
#'   auto-detected from map if not specified).
#'
#' @return Updated mapdeck map object with theme applied
#'
#' @details
#' Available themes:
#' \itemize{
#'   \item \strong{light}: Bright theme with high contrast, suitable for detailed data visualization
#'   \item \strong{dark}: Dark theme with muted colors, reduces eye strain and highlights data
#'   \item \strong{satellite}: Earth-toned theme optimized for satellite imagery backgrounds
#' }
#'
#' Each theme includes:
#' \itemize{
#'   \item Provider-appropriate base map style
#'   \item Coordinated color palette for data layers
#'   \item Consistent typography and UI elements
#'   \item Optimized contrast and readability
#' }
#'
#' @examples
#' \donttest{
#' # Apply dark theme to map
#' map <- mapdeck(provider = "mapbox", token = "your_token")
#' dark_map <- apply_map_theme(map, "dark")
#' 
#' # Apply satellite theme
#' satellite_map <- apply_map_theme(map, "satellite")
#' 
#' # Apply theme to different providers
#' leaflet_map <- mapdeck(provider = "leaflet")
#' themed_leaflet <- apply_map_theme(leaflet_map, "light")
#' }
#'
#' @export
apply_map_theme <- function(map, theme_name, provider = NULL) {
  
  if (!is.character(theme_name) || length(theme_name) != 1) {
    stop("theme_name must be a single character string")
  }
  
  # Auto-detect provider if not specified
  if (is.null(provider)) {
    provider_obj <- attr(map, "mapdeck_provider")
    if (!is.null(provider_obj)) {
      provider <- provider_obj$provider_name
    } else {
      provider <- "mapbox"  # Default fallback
    }
  }
  
  # Apply theme using theme manager
  theme_manager <- get_theme_manager()
  theme_config <- theme_manager$apply_theme(theme_name, provider)
  
  # Update map style
  if (!is.null(theme_config$style)) {
    map <- update_style(map, theme_config$style)
  }
  
  # Store theme configuration for layer styling
  attr(map, "mapdeck_theme") <- theme_config
  
  return(map)
}

#' Validate Map Style
#'
#' Validate that a map style is compatible with a specific provider.
#'
#' @description
#' This function validates whether a given style specification is compatible
#' with a specific map provider, checking format, requirements, and feature support.
#'
#' @param style Character string or list containing style specification
#' @param provider Character string identifying the provider
#' @param detailed Logical indicating if detailed validation results should be returned
#'
#' @return Logical indicating validity, or list with detailed results if detailed=TRUE
#'
#' @details
#' The validation checks:
#' \itemize{
#'   \item Style format compatibility with provider
#'   \item Required fields and structure
#'   \item Feature support and limitations
#'   \item URL patterns and accessibility
#' }
#'
#' Provider-specific validation:
#' \itemize{
#'   \item \strong{Mapbox}: Validates Mapbox style URLs and custom style objects
#'   \item \strong{Leaflet}: Validates tile provider names and URLs
#'   \item \strong{OpenLayers}: Validates source configurations
#'   \item \strong{Gaode}: Validates Gaode style URLs and names
#'   \item \strong{Baidu}: Validates Baidu style names
#' }
#'
#' @examples
#' \donttest{
#' # Validate generic style names
#' is_valid <- validate_map_style("streets", "mapbox")
#' print(is_valid)
#' 
#' # Validate provider-specific styles
#' mapbox_valid <- validate_map_style("mapbox://styles/mapbox/dark-v10", "mapbox")
#' leaflet_valid <- validate_map_style("OpenStreetMap", "leaflet")
#' 
#' # Get detailed validation results
#' validation <- validate_map_style("satellite", "gaode", detailed = TRUE)
#' print(validation)
#' 
#' # Validate custom style object
#' custom_style <- list(
#'   version = 8,
#'   sources = list(),
#'   layers = list()
#' )
#' custom_valid <- validate_map_style(custom_style, "mapbox")
#' }
#'
#' @export
validate_map_style <- function(style, provider, detailed = FALSE) {
  
  if (!is.character(provider) || length(provider) != 1) {
    stop("Provider must be a single character string")
  }
  
  if (!is.logical(detailed) || length(detailed) != 1) {
    stop("detailed must be a single logical value")
  }
  
  validator <- get_style_validator()
  
  if (detailed) {
    # Return detailed validation results
    is_valid <- validator$validate_style(style, provider)
    compatibility <- validator$get_style_compatibility(as.character(style)[1])
    
    return(list(
      valid = is_valid,
      provider = provider,
      style = style,
      compatibility = compatibility,
      supported_features = validator$supported_features[[provider]],
      validation_rules = validator$validation_rules[[provider]]
    ))
  }
  
  return(validator$validate_style(style, provider))
}