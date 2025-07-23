#' Mapdeck Tokens
#'
#' Retrieves all tokens that have been set for different providers
#'
#' @param show_values Logical indicating whether to show masked token values (default: FALSE)
#'
#' @details
#' This function now displays tokens for all configured providers, not just Mapbox.
#' It maintains backward compatibility by checking both the new multi-provider system
#' and the legacy options system.
#'
#' @examples
#' \donttest{
#' # Display all configured tokens
#' mapdeck_tokens()
#' 
#' # Display tokens with masked values
#' mapdeck_tokens(show_values = TRUE)
#' }
#'
#' @export
mapdeck_tokens <- function(show_values = FALSE) {
  # Get tokens from new multi-provider system
  token_store <- get_token_store()
  tokens <- token_store$list_tokens(show_values = show_values)
  
  # Check legacy system for backward compatibility
  legacy_options <- getOption("mapdeck")
  legacy_tokens <- list()
  
  if (!is.null(legacy_options) && !is.null(legacy_options[["mapdeck"]])) {
    for (provider in names(legacy_options[["mapdeck"]])) {
      token_value <- legacy_options[["mapdeck"]][[provider]]
      if (!is.null(token_value) && !is.na(token_value)) {
        # Only add if not already in new system
        if (is.null(tokens[[provider]]) || is.null(tokens[[provider]][["default"]])) {
          if (show_values) {
            # Mask token for display
            if (nchar(token_value) > 12) {
              masked_token <- paste0(
                substr(token_value, 1, 8),
                "...",
                substr(token_value, nchar(token_value) - 3, nchar(token_value))
              )
              legacy_tokens[[provider]] <- list(default = masked_token)
            } else {
              legacy_tokens[[provider]] <- list(default = "[HIDDEN]")
            }
          } else {
            legacy_tokens[[provider]] <- list(default = "[SET]")
          }
        }
      }
    }
  }
  
  # Combine tokens from both systems
  all_tokens <- c(tokens, legacy_tokens)
  
  if (length(all_tokens) == 0) {
    cat("no tokens found\n")
    return(invisible())
  }
  
  # Create result object with proper class for printing
  result <- list(mapdeck = all_tokens)
  class(result) <- "mapdeck_api"
  
  return(result)
}

#' @export
print.mapdeck_api <- function(x, ...) {
  for (i in 1:length(x)) {
    cat("Mapdeck tokens\n")
    
    for (provider_name in names(x[[i]])) {
      provider_tokens <- x[[i]][[provider_name]]
      
      if (is.list(provider_tokens)) {
        # New format with scopes
        for (scope_name in names(provider_tokens)) {
          scope_label <- if (scope_name == "default") "" else paste0(" (", scope_name, ")")
          cat(" - ", provider_name, scope_label, ": ", sep = "")
          key <- provider_tokens[[scope_name]]
          cat(ifelse(is.na(key), "", key), "\n")
        }
      } else {
        # Legacy format (single token value)
        cat(" - ", provider_name, ": ", sep = "")
        key <- provider_tokens
        cat(ifelse(is.na(key), "", key), "\n")
      }
    }
  }
}

#' Set Token
#'
#' Sets an access token so it's available for all mapdeck calls. See details
#'
#' @param token Access token or API key for the specified provider
#' @param provider Character string identifying the provider (default: "mapbox")
#' @param scope Character string for token scope (default: "default")
#'
#' @details
#' Use \code{set_token} to make access tokens available for all the \code{mapdeck()}
#' calls in a session so you don't have to keep specifying the \code{token} argument
#' each time. The function now supports multiple providers while maintaining
#' backward compatibility with existing Mapbox-only usage.
#'
#' @examples
#' \donttest{
#' # Backward compatible usage (Mapbox)
#' set_token("pk.your_mapbox_token_here")
#' 
#' # Multi-provider usage
#' set_token("pk.your_mapbox_token", provider = "mapbox")
#' set_token("your_gaode_api_key", provider = "gaode")
#' set_token("your_baidu_api_key", provider = "baidu")
#' }
#'
#' @export
set_token <- function(token, provider = "mapbox", scope = "default") {
  # Input validation
  if (!is.character(token) || length(token) != 1 || nchar(trimws(token)) == 0) {
    stop("Token must be a single non-empty character string")
  }
  
  if (!is.character(provider) || length(provider) != 1) {
    stop("Provider must be a single character string")
  }
  
  if (!is.character(scope) || length(scope) != 1) {
    stop("Scope must be a single character string")
  }
  
  # Store token in new multi-provider system
  token_store <- get_token_store()
  token_store$set_token(provider, token, scope)
  
  # Maintain backward compatibility with existing options system for Mapbox
  if (provider == "mapbox" && scope == "default") {
    options <- getOption("mapdeck")
    if (is.null(options)) {
      options <- list(mapdeck = list())
    }
    options[['mapdeck']][["mapbox"]] <- token
    class(options) <- "mapdeck_api"
    options(mapdeck = options)
  }
  
  invisible(NULL)
}


#' Clear Tokens
#'
#' Clears all access tokens for all providers
#'
#' @param provider Character string identifying specific provider to clear (optional)
#'
#' @details
#' This function clears tokens from both the new multi-provider system and the
#' legacy options system for backward compatibility. If no provider is specified,
#' all tokens are cleared.
#'
#' @examples
#' \donttest{
#' # Clear all tokens
#' clear_tokens()
#' 
#' # Clear tokens for specific provider
#' clear_tokens("mapbox")
#' }
#'
#' @export
clear_tokens <- function(provider = NULL) {
  # Input validation
  if (!is.null(provider) && (!is.character(provider) || length(provider) != 1)) {
    stop("Provider must be a single character string or NULL")
  }
  
  # Clear tokens from new multi-provider system
  token_store <- get_token_store()
  
  if (is.null(provider)) {
    # Clear all tokens
    token_store$clear_all_tokens()
  } else {
    # Clear tokens for specific provider
    token_store$remove_token(provider, "default")
    # Also remove any other scopes for this provider
    tokens <- token_store$list_tokens()
    if (!is.null(tokens[[provider]])) {
      for (scope in names(tokens[[provider]])) {
        token_store$remove_token(provider, scope)
      }
    }
  }
  
  # Clear legacy options system for backward compatibility
  if (is.null(provider) || provider == "mapbox") {
    options <- list(
      mapdeck = list(
        mapbox = NA_character_
      )
    )
    attr(options, "class") <- "mapdeck_api"
    options(mapdeck = options)
  }
  
  invisible(NULL)
}

#' Get Access Token
#'
#' Retrieves the access token for a specified provider
#'
#' @param provider Character string identifying the provider (default: "mapbox")
#' @param scope Character string for token scope (default: "default")
#'
#' @details
#' This function retrieves tokens from the new multi-provider token storage system
#' while maintaining backward compatibility with the existing options-based system
#' for Mapbox tokens.
#'
#' @examples
#' \donttest{
#' # Get Mapbox token (backward compatible)
#' token <- get_access_token()
#' 
#' # Get provider-specific tokens
#' mapbox_token <- get_access_token("mapbox")
#' gaode_token <- get_access_token("gaode")
#' }
#'
#' @export
get_access_token <- function(provider = "mapbox", scope = "default") {
  # Input validation
  if (!is.character(provider) || length(provider) != 1) {
    stop("Provider must be a single character string")
  }
  
  if (!is.character(scope) || length(scope) != 1) {
    stop("Scope must be a single character string")
  }
  
  # Try to get token from new multi-provider system first
  token_store <- get_token_store()
  token <- token_store$get_token(provider, scope)
  
  if (!is.null(token)) {
    return(token)
  }
  
  # Fallback to legacy system for backward compatibility (Mapbox only)
  if (provider == "mapbox" && scope == "default") {
    # Check legacy options system
    options <- getOption("mapdeck")
    if (!is.null(options) && !is.null(options[['mapdeck']]) && !is.null(options[['mapdeck']][[provider]])) {
      api <- options[['mapdeck']][[provider]]
      if (!is.null(api) && !is.na(api)) {
        return(api)
      }
    }
    
    # Check legacy environment variables
    e <- Sys.getenv()
    e <- e[grep("mapbox|mapdeck", names(e), ignore.case = TRUE)]
    
    api <- unique(as.character(e))
    if (length(api) > 1) {
      warning("Multiple MAPBOX API tokens found in Sys.getenv(), using the first one")
    }
    
    if (length(api) == 0) {
      api <- NULL
    } else {
      return(api[1L])
    }
  }
  
  return(NULL)
}




#' Get Provider Environment Variables
#'
#' Get list of environment variables to check for each provider
#'
#' @param provider Character string identifying the provider
#' @return Character vector of environment variable names
#'
#' @examples
#' \donttest{
#' # Get Mapbox environment variables
#' vars <- get_provider_env_vars("mapbox")
#' }
#'
#' @export
get_provider_env_vars <- function(provider) {
  env_vars <- list(
    "mapbox" = c("MAPBOX_TOKEN", "MAPBOX_KEY", "MAPBOX_API_TOKEN", 
                 "MAPBOX_API_KEY", "MAPBOX", "MAPDECK"),
    "leaflet" = c(), # Leaflet doesn't require tokens by default
    "openlayers" = c(), # OpenLayers doesn't require tokens by default
    "gaode" = c("GAODE_API_KEY", "GAODE_TOKEN", "AMAP_API_KEY", "AMAP_TOKEN"),
    "baidu" = c("BAIDU_API_KEY", "BAIDU_TOKEN", "BAIDU_MAP_KEY")
  )
  
  if (provider %in% names(env_vars)) {
    return(env_vars[[provider]])
  } else {
    return(character(0))
  }
}