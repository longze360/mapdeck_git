#' Provider Factory and Registry System
#'
#' This file contains the factory pattern implementation for creating and
#' managing map provider instances, along with a registry system for
#' tracking available providers.
#'
#' @name provider-factory
NULL

#' Provider Registry Class
#'
#' R6 class for managing the registry of available map providers.
#'
#' @description
#' The ProviderRegistry class maintains a registry of all available map providers,
#' their configurations, and factory functions. It provides methods for
#' registering new providers and retrieving provider information.
#'
#' @field providers List of registered provider configurations
#' @field factories List of provider factory functions
#'
#' @examples
#' \donttest{
#' # Create and use provider registry
#' registry <- ProviderRegistry$new()
#' registry$register_provider("custom", CustomProvider, custom_config)
#' }
#'
#' @export
ProviderRegistry <- R6::R6Class("ProviderRegistry",
  public = list(
    #' @field providers Registered provider configurations
    providers = NULL,
    
    #' @field factories Provider factory functions
    factories = NULL,
    
    #' Initialize Provider Registry
    #'
    #' Create a new provider registry instance.
    #'
    #' @return New ProviderRegistry instance
    initialize = function() {
      self$providers <- list()
      self$factories <- list()
      
      # Register default providers
      self$register_default_providers()
    },
    
    #' Register Provider
    #'
    #' Register a new provider with its configuration and factory function.
    #'
    #' @param name Character string identifying the provider
    #' @param provider_class R6 class for the provider
    #' @param config ProviderConfig object for the provider
    #' @return Invisible self for method chaining
    register_provider = function(name, provider_class, config) {
      if (!is.character(name) || length(name) != 1) {
        stop("Provider name must be a single character string")
      }
      
      if (!inherits(provider_class, "R6ClassGenerator")) {
        stop("Provider class must be an R6 class generator")
      }
      
      if (!inherits(config, "ProviderConfig")) {
        stop("Config must be a ProviderConfig object")
      }
      
      # Validate that provider implements required interface
      if (!validate_provider_interface(provider_class)) {
        stop(sprintf("Provider class '%s' does not implement required interface", name))
      }
      
      self$providers[[name]] <- config
      self$factories[[name]] <- provider_class
      
      invisible(self)
    },
    
    #' Unregister Provider
    #'
    #' Remove a provider from the registry.
    #'
    #' @param name Character string identifying the provider to remove
    #' @return Invisible self for method chaining
    unregister_provider = function(name) {
      if (!is.character(name) || length(name) != 1) {
        stop("Provider name must be a single character string")
      }
      
      if (name %in% names(self$providers)) {
        self$providers[[name]] <- NULL
        self$factories[[name]] <- NULL
      } else {
        warning(sprintf("Provider '%s' not found in registry", name))
      }
      
      invisible(self)
    },
    
    #' Check Provider Registration
    #'
    #' Check if a provider is registered.
    #'
    #' @param name Character string identifying the provider
    #' @return Logical indicating if provider is registered
    is_registered = function(name) {
      if (!is.character(name) || length(name) != 1) {
        return(FALSE)
      }
      
      return(name %in% names(self$providers))
    },
    
    #' Get Provider Configuration
    #'
    #' Get configuration for a registered provider.
    #'
    #' @param name Character string identifying the provider
    #' @return ProviderConfig object or NULL if not found
    get_provider_config = function(name) {
      if (!is.character(name) || length(name) != 1) {
        return(NULL)
      }
      
      return(self$providers[[name]])
    },
    
    #' Get Provider Factory
    #'
    #' Get factory function for a registered provider.
    #'
    #' @param name Character string identifying the provider
    #' @return R6 class generator or NULL if not found
    get_provider_factory = function(name) {
      if (!is.character(name) || length(name) != 1) {
        return(NULL)
      }
      
      return(self$factories[[name]])
    },
    
    #' List Registered Providers
    #'
    #' Get list of all registered provider names.
    #'
    #' @return Character vector of provider names
    list_providers = function() {
      return(names(self$providers))
    },
    
    #' Get Provider Capabilities
    #'
    #' Get capabilities for all registered providers.
    #'
    #' @return Named list of provider capabilities
    get_all_capabilities = function() {
      capabilities <- list()
      
      for (name in names(self$providers)) {
        config <- self$providers[[name]]
        capabilities[[name]] <- config$get_summary()
      }
      
      return(capabilities)
    },
    
    #' Register Default Providers
    #'
    #' Register all default providers with their configurations.
    #' This is called automatically during initialization.
    #'
    #' @return Invisible self for method chaining
    register_default_providers = function() {
      # Get default configurations
      configs <- create_default_provider_configs()
      
      # Register each provider with a placeholder factory
      # Actual provider classes will be registered when they are implemented
      for (name in names(configs)) {
        self$providers[[name]] <- configs[[name]]
        # Placeholder factory - will be replaced when actual classes are available
        self$factories[[name]] <- NULL
      }
      
      invisible(self)
    }
  )
)

#' Provider Factory Class
#'
#' R6 class for creating provider instances using the factory pattern.
#'
#' @description
#' The ProviderFactory class provides methods for creating provider instances
#' based on provider names and configurations. It uses the registry to
#' look up provider information and create appropriate instances.
#'
#' @field registry ProviderRegistry instance
#'
#' @examples
#' \donttest{
#' # Create and use provider factory
#' factory <- ProviderFactory$new()
#' provider <- factory$create_provider("mapbox", list(token = "your_token"))
#' }
#'
#' @export
ProviderFactory <- R6::R6Class("ProviderFactory",
  public = list(
    #' @field registry Provider registry instance
    registry = NULL,
    
    #' Initialize Provider Factory
    #'
    #' Create a new provider factory instance.
    #'
    #' @param registry ProviderRegistry instance (optional)
    #' @return New ProviderFactory instance
    initialize = function(registry = NULL) {
      if (is.null(registry)) {
        self$registry <- ProviderRegistry$new()
      } else {
        if (!inherits(registry, "ProviderRegistry")) {
          stop("Registry must be a ProviderRegistry instance")
        }
        self$registry <- registry
      }
    },
    
    #' Create Provider Instance
    #'
    #' Create a new provider instance based on provider name and configuration.
    #'
    #' @param provider_name Character string identifying the provider
    #' @param config List containing provider-specific configuration
    #' @return Provider instance or NULL if creation fails
    create_provider = function(provider_name, config = list()) {
      if (!is.character(provider_name) || length(provider_name) != 1) {
        stop("Provider name must be a single character string")
      }
      
      if (!is.list(config)) {
        stop("Config must be a list")
      }
      
      # Check if provider is registered
      if (!self$registry$is_registered(provider_name)) {
        stop(sprintf("Provider '%s' is not registered", provider_name))
      }
      
      # Get provider factory
      provider_class <- self$registry$get_provider_factory(provider_name)
      
      if (is.null(provider_class)) {
        stop(sprintf("Provider class for '%s' is not available yet", provider_name))
      }
      
      # Get provider configuration
      provider_config <- self$registry$get_provider_config(provider_name)
      
      # Merge user config with default config
      merged_config <- self$merge_configs(provider_config, config)
      
      # Create provider instance
      tryCatch({
        provider <- provider_class$new()
        provider$initialize(merged_config)
        return(provider)
      }, error = function(e) {
        stop(sprintf("Failed to create provider '%s': %s", provider_name, e$message))
      })
    },
    
    #' Merge Configurations
    #'
    #' Merge user-provided configuration with default provider configuration.
    #'
    #' @param default_config ProviderConfig object with defaults
    #' @param user_config List with user-provided settings
    #' @return List with merged configuration
    merge_configs = function(default_config, user_config) {
      # Start with default configuration summary
      merged <- default_config$get_summary()
      
      # Override with user-provided values
      for (key in names(user_config)) {
        merged[[key]] <- user_config[[key]]
      }
      
      return(merged)
    },
    
    #' Get Available Providers
    #'
    #' Get list of available provider names.
    #'
    #' @return Character vector of provider names
    get_available_providers = function() {
      return(self$registry$list_providers())
    },
    
    #' Get Provider Capabilities
    #'
    #' Get capabilities for a specific provider.
    #'
    #' @param provider_name Character string identifying the provider
    #' @return List containing provider capabilities
    get_provider_capabilities = function(provider_name) {
      if (!is.character(provider_name) || length(provider_name) != 1) {
        stop("Provider name must be a single character string")
      }
      
      config <- self$registry$get_provider_config(provider_name)
      
      if (is.null(config)) {
        stop(sprintf("Provider '%s' not found", provider_name))
      }
      
      return(config$get_summary())
    },
    
    #' Validate Provider Configuration
    #'
    #' Validate configuration for a specific provider.
    #'
    #' @param provider_name Character string identifying the provider
    #' @param config List containing configuration to validate
    #' @return Logical indicating if configuration is valid
    validate_provider_config = function(provider_name, config) {
      if (!is.character(provider_name) || length(provider_name) != 1) {
        stop("Provider name must be a single character string")
      }
      
      if (!is.list(config)) {
        stop("Config must be a list")
      }
      
      provider_config <- self$registry$get_provider_config(provider_name)
      
      if (is.null(provider_config)) {
        stop(sprintf("Provider '%s' not found", provider_name))
      }
      
      # Check authentication requirements
      if (provider_config$authentication_required) {
        if (is.null(config$token) && is.null(config$api_key)) {
          return(FALSE)
        }
      }
      
      return(TRUE)
    }
  )
)

# Global provider factory instance
.provider_factory <- NULL

#' Get Global Provider Factory
#'
#' Get the global provider factory instance, creating it if necessary.
#'
#' @return ProviderFactory instance
#'
#' @examples
#' \donttest{
#' # Get global factory
#' factory <- get_provider_factory()
#' providers <- factory$get_available_providers()
#' }
#'
#' @export
get_provider_factory <- function() {
  if (is.null(.provider_factory)) {
    .provider_factory <<- ProviderFactory$new()
  }
  return(.provider_factory)
}

#' Create Provider Instance
#'
#' Convenience function to create a provider instance using the global factory.
#'
#' @param provider_name Character string identifying the provider
#' @param config List containing provider-specific configuration
#' @return Provider instance
#'
#' @examples
#' \donttest{
#' # Create a Mapbox provider instance
#' provider <- create_provider("mapbox", list(token = "your_token"))
#' }
#'
#' @export
create_provider <- function(provider_name, config = list()) {
  factory <- get_provider_factory()
  return(factory$create_provider(provider_name, config))
}

#' Register Provider
#'
#' Convenience function to register a provider using the global factory.
#'
#' @param name Character string identifying the provider
#' @param provider_class R6 class for the provider
#' @param config ProviderConfig object for the provider
#' @return Invisible NULL
#'
#' @examples
#' \donttest{
#' # Register a custom provider
#' register_provider("custom", CustomProvider, custom_config)
#' }
#'
#' @export
register_provider <- function(name, provider_class, config) {
  factory <- get_provider_factory()
  factory$registry$register_provider(name, provider_class, config)
  invisible(NULL)
}

#' Get Provider Capabilities
#'
#' Convenience function to get provider capabilities using the global factory.
#'
#' @param provider_name Character string identifying the provider
#' @return List containing provider capabilities
#'
#' @examples
#' \donttest{
#' # Get Mapbox capabilities
#' capabilities <- get_provider_capabilities("mapbox")
#' }
#'
#' @export
get_provider_capabilities <- function(provider_name) {
  factory <- get_provider_factory()
  return(factory$get_provider_capabilities(provider_name))
}