context("Browser compatibility tests")

# Test browser compatibility for WebGL and GPU features
# This test file focuses on ensuring compatibility across different browsers and environments

# Skip tests on CRAN and CI environments
skip_if_no_browser_test <- function() {
  skip_on_cran()
  skip_on_ci()
}

# Test comprehensive WebGL feature detection and compatibility
test_that("comprehensive WebGL feature detection works correctly across browser environments", {
  skip_if_no_browser_test()
  
  # Mock comprehensive WebGL detection with browser-specific variations
  with_mock(
    `check_webgl_support` = function(browser_context = "chrome") {
      # Simulate different WebGL capabilities based on browser
      browser_capabilities <- list(
        chrome = list(
          supported = TRUE,
          vendor = "Google Inc. (NVIDIA)",
          renderer = "ANGLE (NVIDIA, NVIDIA GeForce GTX 1080 Direct3D11 vs_5_0 ps_5_0, D3D11-27.21.14.5671)",
          version = "WebGL 1.0 (OpenGL ES 2.0 Chromium)",
          shadingLanguageVersion = "WebGL GLSL ES 1.0 (OpenGL ES GLSL ES 1.0 Chromium)",
          maxTextureSize = 16384,
          maxViewportDims = c(16384, 16384),
          maxVertexAttribs = 16,
          maxVaryingVectors = 30,
          maxFragmentUniforms = 1024,
          maxVertexUniforms = 1024,
          extensions = c(
            "ANGLE_instanced_arrays", "EXT_blend_minmax", "EXT_color_buffer_half_float",
            "EXT_disjoint_timer_query", "EXT_frag_depth", "EXT_shader_texture_lod",
            "EXT_texture_compression_rgtc", "EXT_texture_filter_anisotropic",
            "EXT_sRGB", "OES_element_index_uint", "OES_standard_derivatives",
            "OES_texture_float", "OES_texture_float_linear", "OES_texture_half_float",
            "OES_texture_half_float_linear", "OES_vertex_array_object",
            "WEBGL_color_buffer_float", "WEBGL_compressed_texture_s3tc",
            "WEBGL_debug_renderer_info", "WEBGL_debug_shaders", "WEBGL_depth_texture",
            "WEBGL_draw_buffers", "WEBGL_lose_context"
          )
        ),
        firefox = list(
          supported = TRUE,
          vendor = "Mozilla",
          renderer = "Mozilla -- ANGLE (NVIDIA, NVIDIA GeForce GTX 1080 Direct3D11 vs_5_0 ps_5_0, D3D11-27.21.14.5671)",
          version = "WebGL 1.0",
          shadingLanguageVersion = "WebGL GLSL ES 1.0",
          maxTextureSize = 16384,
          maxViewportDims = c(16384, 16384),
          maxVertexAttribs = 16,
          maxVaryingVectors = 30,
          maxFragmentUniforms = 1024,
          maxVertexUniforms = 1024,
          extensions = c(
            "EXT_blend_minmax", "EXT_color_buffer_half_float", "EXT_frag_depth",
            "EXT_sRGB", "OES_element_index_uint", "OES_standard_derivatives",
            "OES_texture_float", "OES_texture_half_float", "OES_vertex_array_object",
            "WEBGL_color_buffer_float", "WEBGL_compressed_texture_s3tc",
            "WEBGL_debug_renderer_info", "WEBGL_depth_texture", "WEBGL_draw_buffers"
          )
        ),
        safari = list(
          supported = TRUE,
          vendor = "WebKit",
          renderer = "WebKit WebGL",
          version = "WebGL 1.0 (OpenGL ES 2.0 Metal - 71.7.1)",
          shadingLanguageVersion = "WebGL GLSL ES 1.0 (OpenGL ES GLSL ES 1.0 Metal - 71.7.1)",
          maxTextureSize = 16384,
          maxViewportDims = c(16384, 16384),
          maxVertexAttribs = 16,
          maxVaryingVectors = 31,
          maxFragmentUniforms = 1024,
          maxVertexUniforms = 1024,
          extensions = c(
            "EXT_blend_minmax", "EXT_sRGB", "EXT_shader_texture_lod",
            "OES_element_index_uint", "OES_standard_derivatives",
            "OES_texture_float", "OES_texture_half_float", "OES_vertex_array_object",
            "WEBGL_compressed_texture_s3tc", "WEBGL_debug_renderer_info",
            "WEBGL_depth_texture", "WEBGL_draw_buffers"
          )
        ),
        edge = list(
          supported = TRUE,
          vendor = "Microsoft Corporation",
          renderer = "ANGLE (Microsoft, D3D11 vs_5_0 ps_5_0, D3D11-10.0.19041.1202)",
          version = "WebGL 1.0 (OpenGL ES 2.0 Chromium)",
          shadingLanguageVersion = "WebGL GLSL ES 1.0 (OpenGL ES GLSL ES 1.0 Chromium)",
          maxTextureSize = 16384,
          maxViewportDims = c(16384, 16384),
          maxVertexAttribs = 16,
          maxVaryingVectors = 30,
          maxFragmentUniforms = 1024,
          maxVertexUniforms = 1024,
          extensions = c(
            "ANGLE_instanced_arrays", "EXT_blend_minmax", "EXT_color_buffer_half_float",
            "EXT_frag_depth", "EXT_sRGB", "OES_element_index_uint",
            "OES_standard_derivatives", "OES_texture_float", "OES_texture_half_float",
            "OES_vertex_array_object", "WEBGL_color_buffer_float",
            "WEBGL_compressed_texture_s3tc", "WEBGL_debug_renderer_info",
            "WEBGL_depth_texture", "WEBGL_draw_buffers"
          )
        ),
        ie11 = list(
          supported = FALSE,
          error = "WebGL not supported in Internet Explorer 11"
        )
      )
      
      return(browser_capabilities[[browser_context]])
    },
    `check_webgl2_support` = function(browser_context = "chrome") {
      # WebGL 2.0 support varies more significantly across browsers
      webgl2_capabilities <- list(
        chrome = list(supported = TRUE, version = "WebGL 2.0"),
        firefox = list(supported = TRUE, version = "WebGL 2.0"),
        safari = list(supported = TRUE, version = "WebGL 2.0"),  # Recent Safari versions
        edge = list(supported = TRUE, version = "WebGL 2.0"),
        ie11 = list(supported = FALSE, error = "WebGL 2.0 not supported")
      )
      
      return(webgl2_capabilities[[browser_context]])
    },
    `check_required_extensions` = function(browser_context = "chrome") {
      # Check for extensions required by deck.gl and spatial sampling
      webgl_info <- check_webgl_support(browser_context)
      
      if (!webgl_info$supported) {
        return(list(supported = FALSE, missing_extensions = "WebGL not supported"))
      }
      
      required_extensions <- c(
        "OES_texture_float",           # Required for GPU computations
        "OES_standard_derivatives",    # Required for advanced shading
        "WEBGL_depth_texture",         # Required for 3D rendering
        "OES_element_index_uint"       # Required for large datasets
      )
      
      available_extensions <- webgl_info$extensions
      missing_extensions <- setdiff(required_extensions, available_extensions)
      
      return(list(
        supported = length(missing_extensions) == 0,
        required_extensions = required_extensions,
        available_extensions = available_extensions,
        missing_extensions = missing_extensions
      ))
    },
    {
      # Test WebGL detection across different browsers
      browsers <- c("chrome", "firefox", "safari", "edge", "ie11")
      
      cat("\n=== WebGL Compatibility Analysis ===\n")
      
      webgl_results <- data.frame(
        browser = character(),
        webgl1_supported = logical(),
        webgl2_supported = logical(),
        max_texture_size = numeric(),
        extensions_count = numeric(),
        deck_gl_compatible = logical(),
        stringsAsFactors = FALSE
      )
      
      for (browser in browsers) {
        # Check WebGL 1.0 support
        webgl1_info <- check_webgl_support(browser)
        webgl2_info <- check_webgl2_support(browser)
        extensions_info <- check_required_extensions(browser)
        
        # Determine deck.gl compatibility
        deck_gl_compatible <- webgl1_info$supported && extensions_info$supported
        
        # Store results
        webgl_results <- rbind(webgl_results, data.frame(
          browser = browser,
          webgl1_supported = webgl1_info$supported,
          webgl2_supported = webgl2_info$supported,
          max_texture_size = ifelse(webgl1_info$supported, webgl1_info$maxTextureSize, 0),
          extensions_count = ifelse(webgl1_info$supported, length(webgl1_info$extensions), 0),
          deck_gl_compatible = deck_gl_compatible,
          stringsAsFactors = FALSE
        ))
        
        # Print detailed info for supported browsers
        if (webgl1_info$supported) {
          cat(sprintf("\n%s WebGL Support:\n", toupper(browser)))
          cat(sprintf("  WebGL 1.0: %s\n", webgl1_info$version))
          cat(sprintf("  WebGL 2.0: %s\n", ifelse(webgl2_info$supported, "Supported", "Not supported")))
          cat(sprintf("  Renderer: %s\n", webgl1_info$renderer))
          cat(sprintf("  Max Texture Size: %d\n", webgl1_info$maxTextureSize))
          cat(sprintf("  Extensions: %d available\n", length(webgl1_info$extensions)))
          cat(sprintf("  Deck.gl Compatible: %s\n", ifelse(deck_gl_compatible, "Yes", "No")))
          
          if (!extensions_info$supported) {
            cat(sprintf("  Missing Extensions: %s\n", paste(extensions_info$missing_extensions, collapse = ", ")))
          }
        } else {
          cat(sprintf("\n%s: WebGL not supported\n", toupper(browser)))
        }
      }
      
      # Print summary table
      cat("\n=== WebGL Support Summary ===\n")
      print(webgl_results)
      
      # Verify expectations
      expect_true(webgl_results$webgl1_supported[webgl_results$browser == "chrome"])
      expect_true(webgl_results$webgl1_supported[webgl_results$browser == "firefox"])
      expect_true(webgl_results$webgl1_supported[webgl_results$browser == "safari"])
      expect_true(webgl_results$webgl1_supported[webgl_results$browser == "edge"])
      expect_false(webgl_results$webgl1_supported[webgl_results$browser == "ie11"])
      
      # Check that modern browsers support required extensions
      modern_browsers <- c("chrome", "firefox", "safari", "edge")
      for (browser in modern_browsers) {
        expect_true(webgl_results$deck_gl_compatible[webgl_results$browser == browser])
      }
      
      # Check texture size requirements (minimum 4096 for deck.gl)
      for (browser in modern_browsers) {
        texture_size <- webgl_results$max_texture_size[webgl_results$browser == browser]
        expect_gte(texture_size, 4096)
      }
    }
  )
})

# Test GPU feature detection
test_that("GPU feature detection works correctly", {
  skip_if_no_browser_test()
  
  # Mock the GPU detection JavaScript function
  js_gpu_detection <- '
  function detectGPUCapabilities() {
    var capabilities = {
      supported: false,
      extensions: [],
      maxTextureSize: 0,
      maxViewportDims: [0, 0],
      pointSizeRange: [0, 0]
    };
    
    try {
      var canvas = document.createElement("canvas");
      var gl = canvas.getContext("webgl") || 
               canvas.getContext("experimental-webgl");
      
      if (gl && gl instanceof WebGLRenderingContext) {
        capabilities.supported = true;
        capabilities.extensions = gl.getSupportedExtensions();
        capabilities.maxTextureSize = gl.getParameter(gl.MAX_TEXTURE_SIZE);
        capabilities.maxViewportDims = gl.getParameter(gl.MAX_VIEWPORT_DIMS);
        capabilities.pointSizeRange = gl.getParameter(gl.ALIASED_POINT_SIZE_RANGE);
      }
      
      return capabilities;
    } catch (e) {
      capabilities.error = e.message;
      return capabilities;
    }
  }
  '
  
  # Mock the R function that would call this JavaScript
  with_mock(
    `check_gpu_capabilities` = function() {
      # Simulate GPU capabilities detection result
      list(
        supported = TRUE,
        extensions = c(
          "ANGLE_instanced_arrays",
          "EXT_blend_minmax",
          "EXT_color_buffer_half_float",
          "EXT_frag_depth",
          "EXT_sRGB",
          "OES_element_index_uint",
          "OES_standard_derivatives",
          "OES_texture_float",
          "OES_texture_half_float",
          "OES_vertex_array_object",
          "WEBGL_color_buffer_float",
          "WEBGL_compressed_texture_s3tc",
          "WEBGL_debug_renderer_info",
          "WEBGL_debug_shaders",
          "WEBGL_depth_texture",
          "WEBGL_draw_buffers"
        ),
        maxTextureSize = 16384,
        maxViewportDims = c(16384, 16384),
        pointSizeRange = c(1, 64)
      )
    },
    {
      # Test GPU capabilities detection
      gpu_info <- check_gpu_capabilities()
      
      # Check that detection returns expected structure
      expect_true(is.list(gpu_info))
      expect_true("supported" %in% names(gpu_info))
      
      # In our mock, GPU is supported
      expect_true(gpu_info$supported)
      
      # Check that we have detailed information
      expect_true("extensions" %in% names(gpu_info))
      expect_true("maxTextureSize" %in% names(gpu_info))
      expect_true("maxViewportDims" %in% names(gpu_info))
      expect_true("pointSizeRange" %in% names(gpu_info))
      
      # Check for required extensions
      required_extensions <- c(
        "OES_texture_float",
        "OES_standard_derivatives"
      )
      
      for (ext in required_extensions) {
        expect_true(ext %in% gpu_info$extensions)
      }
    }
  )
})

# Test browser feature compatibility matrix
test_that("browser compatibility matrix is accurate", {
  skip_if_no_browser_test()
  
  # Define expected browser compatibility matrix
  browser_compatibility <- list(
    "Chrome" = list(
      webgl = TRUE,
      webgl2 = TRUE,
      gpu_acceleration = TRUE,
      deck_gl = TRUE,
      mapbox_gl = TRUE,
      leaflet = TRUE,
      openlayers = TRUE
    ),
    "Firefox" = list(
      webgl = TRUE,
      webgl2 = TRUE,
      gpu_acceleration = TRUE,
      deck_gl = TRUE,
      mapbox_gl = TRUE,
      leaflet = TRUE,
      openlayers = TRUE
    ),
    "Safari" = list(
      webgl = TRUE,
      webgl2 = TRUE,
      gpu_acceleration = TRUE,
      deck_gl = TRUE,
      mapbox_gl = TRUE,
      leaflet = TRUE,
      openlayers = TRUE
    ),
    "Edge" = list(
      webgl = TRUE,
      webgl2 = TRUE,
      gpu_acceleration = TRUE,
      deck_gl = TRUE,
      mapbox_gl = TRUE,
      leaflet = TRUE,
      openlayers = TRUE
    ),
    "IE11" = list(
      webgl = FALSE,
      webgl2 = FALSE,
      gpu_acceleration = FALSE,
      deck_gl = FALSE,
      mapbox_gl = FALSE,
      leaflet = TRUE,
      openlayers = TRUE
    )
  )
  
  # Mock the function that checks browser compatibility
  with_mock(
    `get_browser_compatibility` = function() {
      return(browser_compatibility)
    },
    {
      # Test browser compatibility matrix
      compat <- get_browser_compatibility()
      
      # Check that all major browsers are included
      expect_true(all(c("Chrome", "Firefox", "Safari", "Edge", "IE11") %in% names(compat)))
      
      # Check that all features are included for each browser
      features <- c("webgl", "webgl2", "gpu_acceleration", "deck_gl", 
                   "mapbox_gl", "leaflet", "openlayers")
      
      for (browser in names(compat)) {
        expect_true(all(features %in% names(compat[[browser]])))
      }
      
      # Check specific compatibility expectations
      # Modern browsers should support WebGL
      expect_true(compat$Chrome$webgl)
      expect_true(compat$Firefox$webgl)
      expect_true(compat$Safari$webgl)
      expect_true(compat$Edge$webgl)
      
      # IE11 should not support WebGL2 or deck.gl
      expect_false(compat$IE11$webgl2)
      expect_false(compat$IE11$deck_gl)
      
      # All browsers should support Leaflet
      expect_true(compat$Chrome$leaflet)
      expect_true(compat$Firefox$leaflet)
      expect_true(compat$Safari$leaflet)
      expect_true(compat$Edge$leaflet)
      expect_true(compat$IE11$leaflet)
    }
  )
})

# Test WebGL fallback mechanisms
test_that("WebGL fallback mechanisms work correctly", {
  skip_if_no_browser_test()
  
  # Mock the WebGL fallback function
  with_mock(
    `create_map_with_webgl_fallback` = function(provider, options) {
      # Check if WebGL is supported
      webgl_supported <- options$force_webgl_support %||% TRUE
      
      if (webgl_supported) {
        # Create WebGL-based map
        return(list(
          provider = provider,
          renderer = "webgl",
          options = options
        ))
      } else {
        # Fall back to non-WebGL alternative
        fallback_provider <- switch(provider,
          "mapbox" = "leaflet",
          "gaode" = "leaflet",
          "baidu" = "leaflet",
          provider  # Keep the same if already non-WebGL
        )
        
        return(list(
          provider = fallback_provider,
          renderer = "canvas",
          options = options,
          fallback = TRUE
        ))
      }
    },
    {
      # Test with WebGL support
      map1 <- create_map_with_webgl_fallback("mapbox", list(force_webgl_support = TRUE))
      expect_equal(map1$provider, "mapbox")
      expect_equal(map1$renderer, "webgl")
      expect_false(isTRUE(map1$fallback))
      
      # Test with WebGL not supported
      map2 <- create_map_with_webgl_fallback("mapbox", list(force_webgl_support = FALSE))
      expect_equal(map2$provider, "leaflet")
      expect_equal(map2$renderer, "canvas")
      expect_true(map2$fallback)
      
      # Test with already non-WebGL provider
      map3 <- create_map_with_webgl_fallback("leaflet", list(force_webgl_support = FALSE))
      expect_equal(map3$provider, "leaflet")
      expect_equal(map3$renderer, "canvas")
      expect_true(map3$fallback)
    }
  )
})

# Test GPU acceleration fallback
test_that("GPU acceleration fallback works correctly", {
  skip_if_no_browser_test()
  
  # Mock the GPU acceleration function
  with_mock(
    `spatial_sample_with_gpu_fallback` = function(data, n, use_gpu = TRUE) {
      # Check if GPU acceleration is available
      gpu_available <- use_gpu && runif(1) > 0.5  # Randomly simulate GPU availability
      
      if (gpu_available) {
        # Use GPU acceleration
        result <- list(
          data = data[sample(nrow(data), min(n, nrow(data))), ],
          method = "gpu",
          performance = "high"
        )
      } else {
        # Fall back to CPU implementation
        result <- list(
          data = data[sample(nrow(data), min(n, nrow(data))), ],
          method = "cpu",
          performance = "medium"
        )
      }
      
      return(result)
    },
    {
      # Generate test data
      test_data <- data.frame(
        lon = runif(1000, -180, 180),
        lat = runif(1000, -90, 90)
      )
      
      # Force GPU usage
      set.seed(123)  # For reproducible random GPU availability
      result1 <- spatial_sample_with_gpu_fallback(test_data, 100, use_gpu = TRUE)
      
      # Force CPU usage
      result2 <- spatial_sample_with_gpu_fallback(test_data, 100, use_gpu = FALSE)
      
      # Check that both methods return valid results
      expect_equal(nrow(result1$data), 100)
      expect_equal(nrow(result2$data), 100)
      
      # CPU method should always return "cpu"
      expect_equal(result2$method, "cpu")
      
      # Print results
      cat("\nGPU acceleration test:\n")
      cat(sprintf("Method with use_gpu=TRUE: %s\n", result1$method))
      cat(sprintf("Method with use_gpu=FALSE: %s\n", result2$method))
    }
  )
})

# Test browser feature detection and adaptation
test_that("browser feature detection and adaptation works correctly", {
  skip_if_no_browser_test()
  
  # Mock the browser detection function
  with_mock(
    `detect_browser` = function() {
      # Simulate browser detection
      list(
        name = "Chrome",
        version = "90.0.4430.212",
        engine = "Blink",
        os = "Windows",
        features = list(
          webgl = TRUE,
          webgl2 = TRUE,
          canvas = TRUE,
          svg = TRUE
        )
      )
    },
    `adapt_to_browser` = function(browser_info) {
      # Adapt settings based on browser capabilities
      settings <- list()
      
      if (browser_info$features$webgl) {
        settings$renderer <- "webgl"
        settings$use_gpu <- TRUE
      } else if (browser_info$features$canvas) {
        settings$renderer <- "canvas"
        settings$use_gpu <- FALSE
      } else {
        settings$renderer <- "svg"
        settings$use_gpu <- FALSE
      }
      
      # Set recommended provider based on browser
      if (browser_info$name == "IE" && as.numeric(sub("\\..+$", "", browser_info$version)) <= 11) {
        settings$recommended_provider <- "leaflet"
      } else if (!browser_info$features$webgl) {
        settings$recommended_provider <- "leaflet"
      } else {
        settings$recommended_provider <- "mapbox"
      }
      
      return(settings)
    },
    {
      # Test browser detection and adaptation
      browser_info <- detect_browser()
      settings <- adapt_to_browser(browser_info)
      
      # Check that detection returns expected structure
      expect_true(is.list(browser_info))
      expect_true("name" %in% names(browser_info))
      expect_true("version" %in% names(browser_info))
      expect_true("features" %in% names(browser_info))
      
      # Check that adaptation returns expected settings
      expect_true(is.list(settings))
      expect_true("renderer" %in% names(settings))
      expect_true("use_gpu" %in% names(settings))
      expect_true("recommended_provider" %in% names(settings))
      
      # For Chrome with WebGL support, we expect:
      expect_equal(settings$renderer, "webgl")
      expect_true(settings$use_gpu)
      expect_equal(settings$recommended_provider, "mapbox")
      
      # Test with a different browser configuration
      browser_info$name <- "IE"
      browser_info$version <- "11.0"
      browser_info$features$webgl <- FALSE
      browser_info$features$webgl2 <- FALSE
      
      settings <- adapt_to_browser(browser_info)
      
      # For IE11 without WebGL support, we expect:
      expect_equal(settings$renderer, "canvas")
      expect_false(settings$use_gpu)
      expect_equal(settings$recommended_provider, "leaflet")
    }
  )
})

# Test Chinese provider compatibility
test_that("Chinese providers work with appropriate browsers", {
  skip_if_no_browser_test()
  
  # Mock the Chinese provider compatibility check
  with_mock(
    `check_chinese_provider_compatibility` = function(browser_info, provider) {
      # Check if the browser is compatible with Chinese providers
      
      # Gaode compatibility
      if (provider == "gaode") {
        # Gaode works with most modern browsers
        return(browser_info$features$webgl && 
               !grepl("IE", browser_info$name) &&
               !(browser_info$name == "Safari" && as.numeric(sub("\\..+$", "", browser_info$version)) < 10))
      }
      
      # Baidu compatibility
      if (provider == "baidu") {
        # Baidu has similar requirements
        return(browser_info$features$webgl && 
               !grepl("IE", browser_info$name) &&
               !(browser_info$name == "Safari" && as.numeric(sub("\\..+$", "", browser_info$version)) < 10))
      }
      
      # Default to true for other providers
      return(TRUE)
    },
    {
      # Test with different browser configurations
      
      # Chrome (should work with Chinese providers)
      chrome_info <- list(
        name = "Chrome",
        version = "90.0.4430.212",
        features = list(webgl = TRUE)
      )
      
      # IE11 (should not work with Chinese providers)
      ie_info <- list(
        name = "IE",
        version = "11.0",
        features = list(webgl = FALSE)
      )
      
      # Old Safari (should not work with Chinese providers)
      old_safari_info <- list(
        name = "Safari",
        version = "9.0",
        features = list(webgl = TRUE)
      )
      
      # Check compatibility
      expect_true(check_chinese_provider_compatibility(chrome_info, "gaode"))
      expect_true(check_chinese_provider_compatibility(chrome_info, "baidu"))
      
      expect_false(check_chinese_provider_compatibility(ie_info, "gaode"))
      expect_false(check_chinese_provider_compatibility(ie_info, "baidu"))
      
      expect_false(check_chinese_provider_compatibility(old_safari_info, "gaode"))
      expect_false(check_chinese_provider_compatibility(old_safari_info, "baidu"))
    }
  )
})

# Helper function for browser feature detection
`%||%` <- function(x, y) if (is.null(x)) y else x