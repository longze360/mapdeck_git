#' Provider Factory and Registry
#'
#' Factory and registry system for managing multiple map providers.
#' Handles provider creation, registration, and capability management.
#'
#' @importFrom R6 R6Class
#' @keywords internal
NULL

#' Provider Registry
#'
#' Singleton registry for managing available map providers and their capabilities.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{register_provider(name, class, capabilities)}}{Register a new provider}
#'   \item{\code{get_provider_class(name)}}{Get provider class by name}
#'   \item{\code{list_providers()}}{List all registered providers}
#'   \item{\code{get_capabilities(name)}}{Get capabilities for a provider}
#'   \item{\code{is_registered(name)}}{Check if provider is registered}
#' }
#'
#' @keywords internal
ProviderRegistry <- R6::R6Class(
  "ProviderRegistry",
  private = list(
    providers = list(),
    
    # Singleton instance
    .instance = NULL
  ),
  
  public = list(
    #' Register Provider
    #'
    #' Register a new map provider with the registry.
    #'
    #' @param name Character string identifying the provider
    #' @param provider_class R6 class that implements IMapProvider
    #' @param capabilities List of capabilities supported by the provider
    #' @return Invisible self for method chaining
    register_provider = function(name, provider_class, capabilities = list()) {
      if (!is.character(name) || length(name) != 1) {
        stop("Provider name must be a single character string")
      }
      
      if (!inherits(provider_class, "R6ClassGenerator")) {
        stop("Provider class must be an R6 class generator")
      }
      
      private$providers[[name]] <- list(
        class = provider_class,
        capabilities = capabilities
      )
      
      invisible(self)
    },
    
    #' Get Provider Class
    #'
    #' Retrieve the R6 class for a registered provider.
    #'
    #' @param name Character string identifying the provider
    #' @return R6 class generator for the provider
    get_provider_class = function(name) {
      if (!self$is_registered(name)) {
        stop(sprintf("Provider '%s' is not registered", name))
      }
      
      return(private$providers[[name]]$class)
    },
    
    #' List Providers
    #'
    #' Get a list of all registered provider names.
    #'
    #' @return Character vector of provider names
    list_providers = function() {
      return(names(private$providers))
    },
    
    #' Get Capabilities
    #'
    #' Get the capabilities for a registered provider.
    #'
    #' @param name Character string identifying the provider
    #' @return List of provider capabilities
    get_capabilities = function(name) {
      if (!self$is_registered(name)) {
        stop(sprintf("Provider '%s' is not registered", name))
      }
      
      return(private$providers[[name]]$capabilities)
    },
    
    #' Is Registered
    #'
    #' Check if a provider is registered with the registry.
    #'
    #' @param name Character string identifying the provider
    #' @return Logical indicating if provider is registered
    is_registered = function(name) {
      return(name %in% names(private$providers))
    },
    
    #' Clear Registry
    #'
    #' Clear all registered providers (primarily for testing).
    #'
    #' @return Invisible self for method chaining
    clear_registry = function() {
      private$providers <- list()
      invisible(self)
    }
  )
)

# Create singleton instance
.provider_registry <- NULL

#' Get Provider Registry
#'
#' Get the singleton instance of the provider registry.
#'
#' @return ProviderRegistry instance
#' @keywords internal
get_provider_registry <- function() {
  if (is.null(.provider_registry)) {
    .provider_registry <<- ProviderRegistry$new()
  }
  return(.provider_registry)
}

#' Provider Factory
#'
#' Factory class for creating map provider instances.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{create_provider(name, config)}}{Create a provider instance}
#'   \item{\code{validate_provider_config(name, config)}}{Validate provider configuration}
#' }
#'
#' @keywords internal
ProviderFactory <- R6::R6Class(
  "ProviderFactory",
  public = list(
    #' Create Provider
    #'
    #' Create an instance of the specified provider with the given configuration.
    #'
    #' @param name Character string identifying the provider
    #' @param config List containing provider-specific configuration
    #' @return Instance of the requested provider
    create_provider = function(name, config = list()) {
      registry <- get_provider_registry()
      
      if (!registry$is_registered(name)) {
        available <- paste(registry$list_providers(), collapse = ", ")
        stop(sprintf(
          "Provider '%s' is not registered. Available providers: %s",
          name, available
        ))
      }
      
      # Validate configuration
      validation_result <- self$validate_provider_config(name, config)
      if (!validation_result$valid) {
        stop(sprintf(
          "Invalid configuration for provider '%s': %s",
          name, paste(validation_result$errors, collapse = "; ")
        ))
      }
      
      # Create provider instance
      provider_class <- registry$get_provider_class(name)
      provider_instance <- provider_class$new(config)
      
      return(provider_instance)
    },
    
    #' Validate Provider Configuration
    #'
    #' Validate the configuration for a specific provider.
    #'
    #' @param name Character string identifying the provider
    #' @param config List containing configuration to validate
    #' @return List with 'valid' (logical) and 'errors' (character vector)
    validate_provider_config = function(name, config) {
      registry <- get_provider_registry()
      
      if (!registry$is_registered(name)) {
        return(list(
          valid = FALSE,
          errors = sprintf("Provider '%s' is not registered", name)
        ))
      }
      
      # Create temporary instance to validate config
      tryCatch({
        provider_class <- registry$get_provider_class(name)
        temp_instance <- provider_class$new()
        validation_result <- temp_instance$validate_config(config)
        return(validation_result)
      }, error = function(e) {
        return(list(
          valid = FALSE,
          errors = as.character(e$message)
        ))
      })
    }
  )
)

# Create singleton factory instance
.provider_factory <- NULL

#' Get Provider Factory
#'
#' Get the singleton instance of the provider factory.
#'
#' @return ProviderFactory instance
#' @keywords internal
get_provider_factory <- function() {
  if (is.null(.provider_factory)) {
    .provider_factory <<- ProviderFactory$new()
  }
  return(.provider_factory)
}

#' Register Provider
#'
#' Register a new map provider with the global registry.
#'
#' @param name Character string identifying the provider
#' @param provider_class R6 class that implements IMapProvider
#' @param capabilities List of capabilities supported by the provider
#' @return Invisible NULL
#' @export
register_provider <- function(name, provider_class, capabilities = list()) {
  registry <- get_provider_registry()
  registry$register_provider(name, provider_class, capabilities)
  invisible(NULL)
}

#' Create Provider
#'
#' Create an instance of the specified map provider.
#'
#' @param name Character string identifying the provider
#' @param config List containing provider-specific configuration
#' @return Instance of the requested provider
#' @export
create_provider <- function(name, config = list()) {
  factory <- get_provider_factory()
  return(factory$create_provider(name, config))
}

#' List Available Providers
#'
#' Get a list of all registered map providers.
#'
#' @return Character vector of provider names
#' @export
list_providers <- function() {
  registry <- get_provider_registry()
  return(registry$list_providers())
}

#' Get Provider Capabilities
#'
#' Get the capabilities supported by a specific provider.
#'
#' @param name Character string identifying the provider
#' @return List of provider capabilities
#' @export
get_provider_capabilities <- function(name) {
  registry <- get_provider_registry()
  return(registry$get_capabilities(name))
}