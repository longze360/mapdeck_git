#' Multi-Provider Token Storage System
#'
#' This file contains the TokenStore class for secure storage and management
#' of API keys and tokens across multiple mapping providers.
#'
#' @name token-store
NULL

#' Token Store Class
#'
#' R6 class for managing API tokens and keys across multiple providers.
#'
#' @description
#' The TokenStore class provides secure storage and retrieval of authentication
#' tokens for different mapping providers. It supports environment variable
#' loading, validation, and provider-specific token management.
#'
#'
#' @examples
#' \donttest{
#' # Create token store
#' store <- TokenStore$new()
#' store$set_token("mapbox", "pk.your_token_here")
#' token <- store$get_token("mapbox")
#' }
#'
#' @export
TokenStore <- R6::R6Class("TokenStore",
  public = list(
    #' @field tokens Provider token storage
    tokens = NULL,
    
    #' @field encrypted Encryption flag
    encrypted = NULL,
    
    #' @field env_vars Environment variable mappings
    env_vars = NULL,
    
    #' Initialize Token Store
    #'
    #' Create a new token store instance.
    #'
    #' @param encrypted Logical indicating if tokens should be encrypted
    #' @return New TokenStore instance
    initialize = function(encrypted = FALSE) {
      self$tokens <- list()
      self$encrypted <- encrypted
      
      # Define environment variable mappings for each provider
      self$env_vars <- list(
        mapbox = c("MAPBOX_TOKEN", "MAPBOX_KEY", "MAPBOX_API_TOKEN", 
                   "MAPBOX_API_KEY", "MAPBOX", "MAPDECK"),
        gaode = c("GAODE_API_KEY", "GAODE_TOKEN", "AMAP_API_KEY", "AMAP_TOKEN"),
        baidu = c("BAIDU_API_KEY", "BAIDU_TOKEN", "BAIDU_MAP_KEY"),
        leaflet = c(),  # No authentication required
        openlayers = c()  # No authentication required
      )
      
      # Load tokens from environment variables
      self$load_from_environment()
    },
    
    #' Set Token
    #'
    #' Store a token for a specific provider.
    #'
    #' @param provider Character string identifying the provider
    #' @param token Character string containing the token
    #' @param scope Character string for token scope (default: "default")
    #' @return Invisible self for method chaining
    set_token = function(provider, token, scope = "default") {
      if (!is.character(provider) || length(provider) != 1) {
        stop("Provider must be a single character string")
      }
      
      if (!is.character(token) || length(token) != 1 || nchar(trimws(token)) == 0) {
        stop("Token must be a single non-empty character string")
      }
      
      if (!is.character(scope) || length(scope) != 1) {
        stop("Scope must be a single character string")
      }
      
      # Validate provider
      provider <- validate_provider_name(provider, allow_null = FALSE)
      
      # Initialize provider storage if needed
      if (is.null(self$tokens[[provider]])) {
        self$tokens[[provider]] <- list()
      }
      
      # Store token (encrypt if enabled)
      if (self$encrypted) {
        self$tokens[[provider]][[scope]] <- private$encrypt_token(token)
      } else {
        self$tokens[[provider]][[scope]] <- token
      }
      
      invisible(self)
    },
    
    #' Get Token
    #'
    #' Retrieve a token for a specific provider.
    #'
    #' @param provider Character string identifying the provider
    #' @param scope Character string for token scope (default: "default")
    #' @return Character string containing the token or NULL if not found
    get_token = function(provider, scope = "default") {
      if (!is.character(provider) || length(provider) != 1) {
        stop("Provider must be a single character string")
      }
      
      if (!is.character(scope) || length(scope) != 1) {
        stop("Scope must be a single character string")
      }
      
      # Validate provider
      provider <- validate_provider_name(provider, allow_null = FALSE)
      
      # Check if provider has tokens
      if (is.null(self$tokens[[provider]])) {
        return(NULL)
      }
      
      # Get token for scope
      token <- self$tokens[[provider]][[scope]]
      
      if (is.null(token)) {
        return(NULL)
      }
      
      # Decrypt if needed
      if (self$encrypted) {
        return(private$decrypt_token(token))
      } else {
        return(token)
      }
    },
    
    #' Remove Token
    #'
    #' Remove a token for a specific provider and scope.
    #'
    #' @param provider Character string identifying the provider
    #' @param scope Character string for token scope (default: "default")
    #' @return Invisible self for method chaining
    remove_token = function(provider, scope = "default") {
      if (!is.character(provider) || length(provider) != 1) {
        stop("Provider must be a single character string")
      }
      
      if (!is.character(scope) || length(scope) != 1) {
        stop("Scope must be a single character string")
      }
      
      # Only validate provider if it exists in our tokens
      # This allows removal of tokens for providers that might not be registered
      if (!is.null(self$tokens[[provider]])) {
        self$tokens[[provider]][[scope]] <- NULL
        
        # Remove provider entry if no tokens left
        if (length(self$tokens[[provider]]) == 0) {
          self$tokens[[provider]] <- NULL
        }
      }
      
      invisible(self)
    },
    
    #' List Tokens
    #'
    #' Get list of all stored tokens by provider.
    #'
    #' @param show_values Logical indicating if token values should be shown
    #' @return Named list of provider tokens
    list_tokens = function(show_values = FALSE) {
      result <- list()
      
      for (provider in names(self$tokens)) {
        result[[provider]] <- list()
        
        for (scope in names(self$tokens[[provider]])) {
          if (show_values) {
            token <- self$get_token(provider, scope)
            # Mask token for security (show first 8 and last 4 characters)
            if (!is.null(token) && nchar(token) > 12) {
              masked_token <- paste0(
                substr(token, 1, 8),
                "...",
                substr(token, nchar(token) - 3, nchar(token))
              )
              result[[provider]][[scope]] <- masked_token
            } else {
              result[[provider]][[scope]] <- "[HIDDEN]"
            }
          } else {
            result[[provider]][[scope]] <- "[SET]"
          }
        }
      }
      
      return(result)
    },
    
    #' Clear All Tokens
    #'
    #' Remove all stored tokens.
    #'
    #' @return Invisible self for method chaining
    clear_all_tokens = function() {
      self$tokens <- list()
      invisible(self)
    },
    
    #' Load from Environment
    #'
    #' Load tokens from environment variables.
    #'
    #' @return Invisible self for method chaining
    load_from_environment = function() {
      env_vars <- Sys.getenv()
      
      for (provider in names(self$env_vars)) {
        var_names <- self$env_vars[[provider]]
        
        for (var_name in var_names) {
          if (var_name %in% names(env_vars) && nchar(env_vars[[var_name]]) > 0) {
            # Only set if not already set
            if (is.null(self$tokens[[provider]])) {
                self$set_token(provider, env_vars[[var_name]])
                break  # Use first found token
            }
          }
        }
      }
      
      invisible(self)
    },
    
    #' Validate Token
    #'
    #' Validate a token for a specific provider.
    #'
    #' @param provider Character string identifying the provider
    #' @param token Character string containing token to validate (optional)
    #' @return Logical indicating if token is valid
    validate_token = function(provider, token = NULL) {
      if (!is.character(provider) || length(provider) != 1) {
        stop("Provider must be a single character string")
      }
      
      # Use stored token if not provided
      if (is.null(token)) {
        token <- self$get_token(provider)
      }
      
      if (is.null(token)) {
        return(FALSE)
      }
      
      # Provider-specific validation
      return(private$validate_provider_token(provider, token))
    },
    
    #' Check Authentication Required
    #'
    #' Check if a provider requires authentication.
    #'
    #' @param provider Character string identifying the provider
    #' @return Logical indicating if authentication is required
    requires_authentication = function(provider) {
      if (!is.character(provider) || length(provider) != 1) {
        stop("Provider must be a single character string")
      }
      
      # Validate provider
      provider <- validate_provider_name(provider, allow_null = FALSE)
      
      # Get provider configuration
      factory <- get_provider_factory()
      config <- factory$registry$get_provider_config(provider)
      
      if (is.null(config)) {
        return(FALSE)
      }
      
      return(config$authentication_required)
    }
  ),
  
  private = list(
    #' Encrypt Token
    #'
    #' Encrypt a token for secure storage.
    #'
    #' @param token Character string to encrypt
    #' @return Encrypted token
    encrypt_token = function(token) {
      # Check if base64enc is available
      if (!requireNamespace("base64enc", quietly = TRUE)) {
        warning("base64enc package not available, storing token unencrypted")
        return(token)
      }
      
      # Simple base64 encoding for now
      # In production, use proper encryption
      return(base64enc::base64encode(charToRaw(token)))
    },
    
    #' Decrypt Token
    #'
    #' Decrypt a stored token.
    #'
    #' @param encrypted_token Encrypted token to decrypt
    #' @return Decrypted token
    decrypt_token = function(encrypted_token) {
      # Check if base64enc is available
      if (!requireNamespace("base64enc", quietly = TRUE)) {
        # Token was stored unencrypted due to missing package
        return(encrypted_token)
      }
      
      # Simple base64 decoding for now
      # In production, use proper decryption
      return(rawToChar(base64enc::base64decode(encrypted_token)))
    },
    
    #' Validate Provider Token
    #'
    #' Perform provider-specific token validation.
    #'
    #' @param provider Character string identifying the provider
    #' @param token Character string containing the token
    #' @return Logical indicating if token is valid
    validate_provider_token = function(provider, token) {
      # Basic validation - check format and length
      switch(provider,
        "mapbox" = {
          # Mapbox tokens start with 'pk.' or 'sk.'
          return(grepl("^(pk|sk)\\.", token) && nchar(token) > 20)
        },
        "gaode" = {
          # Gaode API keys are typically 32 characters
          return(nchar(token) >= 20 && nchar(token) <= 50)
        },
        "baidu" = {
          # Baidu API keys are typically alphanumeric
          return(nchar(token) >= 20 && nchar(token) <= 50)
        },
        "leaflet" = {
          # Leaflet doesn't require authentication
          return(TRUE)
        },
        "openlayers" = {
          # OpenLayers doesn't require authentication
          return(TRUE)
        },
        {
          # Default validation
          return(nchar(token) > 0)
        }
      )
    }
  )
)

# Global token store instance
.token_store <- NULL

#' Get Global Token Store
#'
#' Get the global token store instance, creating it if necessary.
#'
#' @param encrypted Logical indicating if tokens should be encrypted
#' @return TokenStore instance
#'
#' @examples
#' \donttest{
#' # Get global token store
#' store <- get_token_store()
#' }
#'
#' @export
get_token_store <- function(encrypted = FALSE) {
  tryCatch({
    if (is.null(.token_store)) {
      .token_store <<- TokenStore$new(encrypted = encrypted)
    }
    return(.token_store)
  }, error = function(e) {
    # Handle locked binding error during development
    if (grepl("locked binding", e$message)) {
      # Create a new instance without assigning to global variable
      return(TokenStore$new(encrypted = encrypted))
    } else {
      stop(e)
    }
  })
}