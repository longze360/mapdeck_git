#' Provider Factory and Registry System
#'
#' This file contains the factory pattern implementation for creating and
#' managing map provider instances. It includes provider registration,
#' creation, and capability management.
#'
#' @name provider-factory
NULL

#' Provider Registry Class
#'
#' R6 class that maintains a registry of available map providers and their
#' configurations. Implements the registry pattern for provider management.
#'
#' @field providers List of registered provider classes
#' @field configs List of provider configurations
#'
#' @export
ProviderRegistry <- R6::R6Class("ProviderRegistry",
  public = list(
    providers = NULL,
    configs = NULL,
    
    #' Initialize Provider Registry
    #'
    #' @return New ProviderRegistry instance
    initialize = function() {
      self$providers <- list()
      self$configs <- create_default_provider_configs()
    },
    
    #' Register Provider Class
    #'
    #' Register a new provider class with the registry.
    #'
    #' @param name Character string provider name
    #' @param provider_class R6 class implementing IMapProvider interface
    #' @param config ProviderConfig object (optional)
    #'
    #' @return Invisible self for method chaining
    register_provider = function(name, provider_class, config = NULL) {
      # Validate provider class implements interface
      if (!validate_provider_interface(provider_class)) {
        stop("Provider class must implement IMapProvider interface")
      }
      
      # Use provided config or get default
      if (is.null(config)) {
        config <- get_provider_config(name)
        if (is.null(config)) {
          stop("No configuration found for provider: ", name)
        }
      }
      
      # Validate configuration
      if (!validate_provider_config(config)) {
        stop("Invalid provider configuration for: ", name)
      }
      
      self$providers[[name]] <- provider_class
      self$configs[[name]] <- config
      
      invisible(self)
    },
    
    #' Unregister Provider
    #'
    #' Remove a provider from the registry.
    #'
    #' @param name Character string provider name
    #'
    #' @return Invisible self for method chaining
    unregister_provider = function(name) {
      if (name %in% names(self$providers)) {
        self$providers[[name]] <- NULL
        self$configs[[name]] <- NULL
      }
      invisible(self)
    },
    
    #' Check if Provider is Registered
    #'
    #' @param name Character string provider name
    #'
    #' @return Logical indicating if provider is registered
    is_registered = function(name) {
      return(name %in% names(self$providers))
    },
    
    #' Get Registered Providers
    #'
    #' @return Character vector of registered provider names
    get_registered_providers = function() {
      return(names(self$providers))
    },
    
    #' Get Provider Class
    #'
    #' @param name Character string provider name
    #'
    #' @return R6 class or NULL if not found
    get_provider_class = function(name) {
      return(self$providers[[name]])
    },
    
    #' Get Provider Configuration
    #'
    #' @param name Character string provider name
    #'
    #' @return ProviderConfig object or NULL if not found
    get_provider_config = function(name) {
      return(self$configs[[name]])
    },
    
    #' Get Provider Capabilities
    #'
    #' @param name Character string provider name
    #'
    #' @return Character vector of capabilities or NULL if not found
    get_provider_capabilities = function(name) {
      config <- self$get_provider_config(name)
      if (!is.null(config)) {
        return(config$supported_features)
      }
      return(NULL)
    }
  )
)

#' Provider Factory Class
#'
#' R6 class implementing the factory pattern for creating provider instances.
#' Uses the provider registry to create and configure provider instances.
#'
#' @field registry ProviderRegistry instance
#'
#' @export
ProviderFactory <- R6::R6Class("ProviderFactory",
  public = list(
    registry = NULL,
    
    #' Initialize Provider Factory
    #'
    #' @param registry ProviderRegistry instance (optional)
    #'
    #' @return New ProviderFactory instance
    initialize = function(registry = NULL) {
      if (is.null(registry)) {
        self$registry <- ProviderRegistry$new()
      } else {
        self$registry <- registry
      }
    },
    
    #' Create Provider Instance
    #'
    #' Create a new provider instance with the specified configuration.
    #'
    #' @param provider_name Character string provider name
    #' @param config List of additional configuration options
    #'
    #' @return Provider instance or error if provider not found
    create_provider = function(provider_name, config = list()) {
      # Check if provider is registered
      if (!self$registry$is_registered(provider_name)) {
        stop("Provider not registered: ", provider_name, 
             ". Available providers: ", 
             paste(self$registry$get_registered_providers(), collapse = ", "))
      }
      
      # Get provider class and configuration
      provider_class <- self$registry$get_provider_class(provider_name)
      provider_config <- self$registry$get_provider_config(provider_name)
      
      # Merge default config with user config
      merged_config <- private$merge_configs(provider_config$to_list(), config)
      
      # Create and return provider instance
      tryCatch({
        provider_instance <- provider_class$new(merged_config)
        return(provider_instance)
      }, error = function(e) {
        stop("Failed to create provider instance for '", provider_name, "': ", e$message)
      })
    },
    
    #' Get Available Providers
    #'
    #' @return Character vector of available provider names
    get_available_providers = function() {
      return(self$registry$get_registered_providers())
    },
    
    #' Check Provider Availability
    #'
    #' @param provider_name Character string provider name
    #'
    #' @return Logical indicating if provider is available
    is_provider_available = function(provider_name) {
      return(self$registry$is_registered(provider_name))
    },
    
    #' Get Provider Information
    #'
    #' @param provider_name Character string provider name
    #'
    #' @return List containing provider information
    get_provider_info = function(provider_name) {
      if (!self$registry$is_registered(provider_name)) {
        return(NULL)
      }
      
      config <- self$registry$get_provider_config(provider_name)
      capabilities <- self$registry$get_provider_capabilities(provider_name)
      
      return(list(
        name = provider_name,
        type = config$type,
        authentication_required = config$authentication_required,
        coordinate_system = config$coordinate_system,
        default_style = config$default_style,
        capabilities = capabilities
      ))
    }
  ),
  
  private = list(
    #' Merge Configuration Objects
    #'
    #' @param default_config List of default configuration
    #' @param user_config List of user configuration
    #'
    #' @return Merged configuration list
    merge_configs = function(default_config, user_config) {
      merged <- default_config
      
      for (key in names(user_config)) {
        merged[[key]] <- user_config[[key]]
      }
      
      return(merged)
    }
  )
)

# Global provider factory instance
.provider_factory <- NULL

#' Get Global Provider Factory
#'
#' Returns the global provider factory instance, creating it if necessary.
#'
#' @return ProviderFactory instance
#'
#' @export
get_provider_factory <- function() {
  if (is.null(.provider_factory)) {
    .provider_factory <<- ProviderFactory$new()
  }
  return(.provider_factory)
}

#' Register Provider
#'
#' Convenience function to register a provider with the global factory.
#'
#' @param name Character string provider name
#' @param provider_class R6 class implementing IMapProvider interface
#' @param config ProviderConfig object (optional)
#'
#' @return Invisible NULL
#'
#' @export
register_provider <- function(name, provider_class, config = NULL) {
  factory <- get_provider_factory()
  factory$registry$register_provider(name, provider_class, config)
  invisible(NULL)
}

#' Create Provider
#'
#' Convenience function to create a provider instance using the global factory.
#'
#' @param provider_name Character string provider name
#' @param config List of additional configuration options
#'
#' @return Provider instance
#'
#' @export
create_provider <- function(provider_name, config = list()) {
  factory <- get_provider_factory()
  return(factory$create_provider(provider_name, config))
}

#' Get Available Providers
#'
#' Convenience function to get available providers from the global factory.
#'
#' @return Character vector of available provider names
#'
#' @export
get_available_providers <- function() {
  factory <- get_provider_factory()
  return(factory$get_available_providers())
}

#' Get Provider Capabilities
#'
#' Convenience function to get provider capabilities from the global factory.
#'
#' @param provider_name Character string provider name
#'
#' @return Character vector of capabilities or NULL if not found
#'
#' @export
get_provider_capabilities <- function(provider_name) {
  factory <- get_provider_factory()
  return(factory$registry$get_provider_capabilities(provider_name))
}