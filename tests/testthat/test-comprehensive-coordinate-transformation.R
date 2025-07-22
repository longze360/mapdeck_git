# Comprehensive Coordinate Transformation Tests
# Tests for coordinate transformation accuracy with 1-meter tolerance requirement

test_that("CoordinateTransformer initialization is complete", {
  skip_if_not_installed("R6")
  
  transformer <- CoordinateTransformer$new()
  
  expect_s3_class(transformer, "CoordinateTransformer")
  expect_s3_class(transformer, "R6")
  
  # Test supported CRS
  supported_crs <- transformer$get_supported_crs()
  expect_equal(supported_crs, c("WGS84", "GCJ02", "BD09"))
  expect_true(is.character(supported_crs))
  expect_equal(length(supported_crs), 3)
})

test_that("CoordinateTransformer CRS validation is comprehensive", {
  skip_if_not_installed("R6")
  
  transformer <- CoordinateTransformer$new()
  
  # Test valid CRS
  expect_true(transformer$is_supported_crs("WGS84"))
  expect_true(transformer$is_supported_crs("GCJ02"))
  expect_true(transformer$is_supported_crs("BD09"))
  
  # Test case sensitivity
  expect_false(transformer$is_supported_crs("wgs84"))
  expect_false(transformer$is_supported_crs("gcj02"))
  expect_false(transformer$is_supported_crs("bd09"))
  
  # Test invalid CRS
  expect_false(transformer$is_supported_crs("INVALID"))
  expect_false(transformer$is_supported_crs("EPSG:4326"))
  expect_false(transformer$is_supported_crs("UTM"))
  expect_false(transformer$is_supported_crs(""))
  expect_false(transformer$is_supported_crs(NULL))
  expect_false(transformer$is_supported_crs(123))
  expect_false(transformer$is_supported_crs(c("WGS84", "GCJ02")))
})

test_that("CoordinateTransformer handles same CRS transformation", {
  skip_if_not_installed("R6")
  
  transformer <- CoordinateTransformer$new()
  
  # Test coordinates
  test_coords <- list(
    c(116.3974, 39.9093),  # Beijing
    c(121.4737, 31.2304),  # Shanghai
    c(113.2644, 23.1291),  # Guangzhou
    c(-74.0060, 40.7128),  # New York
    c(2.3522, 48.8566)     # Paris
  )
  
  # Test same CRS transformations
  for (coords in test_coords) {
    for (crs in c("WGS84", "GCJ02", "BD09")) {
      result <- transformer$transform(coords, crs, crs)
      expect_equal(result, coords, 
                   info = paste("Same CRS transformation failed for", crs))
    }
  }
})

test_that("CoordinateTransformer validates coordinate inputs comprehensively", {
  skip_if_not_installed("R6")
  
  transformer <- CoordinateTransformer$new()
  
  # Test invalid coordinate formats
  expect_error(transformer$transform(c(116.3974), "WGS84", "GCJ02"),
               "Coordinates must be a numeric vector of length 2")
  expect_error(transformer$transform(c(116.3974, 39.9093, 10), "WGS84", "GCJ02"),
               "Coordinates must be a numeric vector of length 2")
  expect_error(transformer$transform("invalid", "WGS84", "GCJ02"),
               "Coordinates must be numeric")
  expect_error(transformer$transform(NULL, "WGS84", "GCJ02"),
               "Coordinates cannot be NULL")
  expect_error(transformer$transform(list(116.3974, 39.9093), "WGS84", "GCJ02"),
               "Coordinates must be a numeric vector")
  
  # Test invalid coordinate values
  expect_error(transformer$transform(c(200, 39.9093), "WGS84", "GCJ02"),
               "Longitude must be between -180 and 180")
  expect_error(transformer$transform(c(-200, 39.9093), "WGS84", "GCJ02"),
               "Longitude must be between -180 and 180")
  expect_error(transformer$transform(c(116.3974, 100), "WGS84", "GCJ02"),
               "Latitude must be between -90 and 90")
  expect_error(transformer$transform(c(116.3974, -100), "WGS84", "GCJ02"),
               "Latitude must be between -90 and 90")
  
  # Test invalid CRS
  expect_error(transformer$transform(c(116.3974, 39.9093), "INVALID", "GCJ02"),
               "Unsupported source CRS")
  expect_error(transformer$transform(c(116.3974, 39.9093), "WGS84", "INVALID"),
               "Unsupported target CRS")
  expect_error(transformer$transform(c(116.3974, 39.9093), NULL, "GCJ02"),
               "CRS cannot be NULL")
  expect_error(transformer$transform(c(116.3974, 39.9093), "WGS84", NULL),
               "CRS cannot be NULL")
})

test_that("WGS84 to GCJ02 transformation accuracy meets 1-meter tolerance", {
  skip_if_not_installed("R6")
  
  transformer <- CoordinateTransformer$new()
  
  # Test cities across China with known coordinates
  test_cities <- list(
    list(name = "Beijing", wgs84 = c(116.3974, 39.9093)),
    list(name = "Shanghai", wgs84 = c(121.4737, 31.2304)),
    list(name = "Guangzhou", wgs84 = c(113.2644, 23.1291)),
    list(name = "Shenzhen", wgs84 = c(114.0579, 22.5431)),
    list(name = "Chengdu", wgs84 = c(104.0668, 30.5728)),
    list(name = "Hangzhou", wgs84 = c(120.1551, 30.2741)),
    list(name = "Wuhan", wgs84 = c(114.3054, 30.5928)),
    list(name = "Xi'an", wgs84 = c(108.9480, 34.2588)),
    list(name = "Nanjing", wgs84 = c(118.7969, 32.0603)),
    list(name = "Tianjin", wgs84 = c(117.2008, 39.0842))
  )
  
  for (city in test_cities) {
    # Transform WGS84 to GCJ02
    gcj02_coords <- transformer$transform(city$wgs84, "WGS84", "GCJ02")
    
    # GCJ02 coordinates should be different from WGS84
    expect_false(identical(city$wgs84, gcj02_coords),
                 info = paste("GCJ02 transformation should change coordinates for", city$name))
    
    # Check that transformation is within reasonable bounds (< 1km difference)
    distance_diff <- sqrt((gcj02_coords[1] - city$wgs84[1])^2 + 
                         (gcj02_coords[2] - city$wgs84[2])^2) * 111000
    expect_true(distance_diff < 1000,
                info = paste("Transformation difference too large for", city$name, ":", distance_diff, "meters"))
    
    # Test round-trip accuracy (1-meter tolerance)
    back_to_wgs84 <- transformer$transform(gcj02_coords, "GCJ02", "WGS84")
    expect_true(transformer$validate_accuracy(city$wgs84, back_to_wgs84, "WGS84", "WGS84", 1.0),
                info = paste("Round-trip accuracy failed for", city$name))
  }
})

test_that("GCJ02 to BD09 transformation accuracy meets 1-meter tolerance", {
  skip_if_not_installed("R6")
  
  transformer <- CoordinateTransformer$new()
  
  # Test with GCJ02 coordinates (approximate)
  test_cities_gcj02 <- list(
    list(name = "Beijing", gcj02 = c(116.404, 39.915)),
    list(name = "Shanghai", gcj02 = c(121.480, 31.236)),
    list(name = "Guangzhou", gcj02 = c(113.270, 23.135)),
    list(name = "Shenzhen", gcj02 = c(114.064, 22.549)),
    list(name = "Chengdu", gcj02 = c(104.073, 30.579))
  )
  
  for (city in test_cities_gcj02) {
    # Transform GCJ02 to BD09
    bd09_coords <- transformer$transform(city$gcj02, "GCJ02", "BD09")
    
    # BD09 coordinates should be different from GCJ02
    expect_false(identical(city$gcj02, bd09_coords),
                 info = paste("BD09 transformation should change coordinates for", city$name))
    
    # Test round-trip accuracy (1-meter tolerance)
    back_to_gcj02 <- transformer$transform(bd09_coords, "BD09", "GCJ02")
    expect_true(transformer$validate_accuracy(city$gcj02, back_to_gcj02, "GCJ02", "GCJ02", 1.0),
                info = paste("GCJ02<->BD09 round-trip accuracy failed for", city$name))
  }
})

test_that("WGS84 to BD09 full chain transformation accuracy", {
  skip_if_not_installed("R6")
  
  transformer <- CoordinateTransformer$new()
  
  # Test full transformation chain: WGS84 -> BD09 -> WGS84
  test_coordinates <- list(
    c(116.3974, 39.9093),  # Beijing
    c(121.4737, 31.2304),  # Shanghai
    c(113.2644, 23.1291),  # Guangzhou
    c(114.0579, 22.5431),  # Shenzhen
    c(108.9480, 34.2588)   # Xi'an
  )
  
  for (i in seq_along(test_coordinates)) {
    original_wgs84 <- test_coordinates[[i]]
    
    # Transform WGS84 -> BD09
    bd09_coords <- transformer$transform(original_wgs84, "WGS84", "BD09")
    expect_false(identical(original_wgs84, bd09_coords),
                 info = paste("WGS84->BD09 should change coordinates for test", i))
    
    # Transform BD09 -> WGS84
    back_to_wgs84 <- transformer$transform(bd09_coords, "BD09", "WGS84")
    
    # Test 1-meter accuracy
    expect_true(transformer$validate_accuracy(original_wgs84, back_to_wgs84, "WGS84", "WGS84", 1.0),
                info = paste("WGS84<->BD09 round-trip accuracy failed for test", i))
  }
})

test_that("Matrix and batch transformation works correctly", {
  skip_if_not_installed("R6")
  
  transformer <- CoordinateTransformer$new()
  
  # Test matrix transformation
  coords_matrix <- matrix(c(
    116.3974, 39.9093,  # Beijing
    121.4737, 31.2304,  # Shanghai
    113.2644, 23.1291,  # Guangzhou
    114.0579, 22.5431,  # Shenzhen
    108.9480, 34.2588   # Xi'an
  ), ncol = 2, byrow = TRUE)
  
  # Test WGS84 to GCJ02 matrix transformation
  gcj02_matrix <- transformer$transform(coords_matrix, "WGS84", "GCJ02")
  
  expect_true(is.matrix(gcj02_matrix))
  expect_equal(dim(gcj02_matrix), dim(coords_matrix))
  expect_false(identical(coords_matrix, gcj02_matrix))
  
  # Test that each row is transformed correctly
  for (i in seq_len(nrow(coords_matrix))) {
    single_transform <- transformer$transform(coords_matrix[i, ], "WGS84", "GCJ02")
    expect_equal(gcj02_matrix[i, ], single_transform,
                 info = paste("Matrix transformation row", i, "doesn't match single transformation"))
  }
  
  # Test batch transformation method
  batch_result <- transformer$transform_batch(coords_matrix, "WGS84", "GCJ02")
  expect_equal(batch_result, gcj02_matrix)
  
  # Test round-trip accuracy for matrix
  back_to_wgs84 <- transformer$transform(gcj02_matrix, "GCJ02", "WGS84")
  for (i in seq_len(nrow(coords_matrix))) {
    expect_true(transformer$validate_accuracy(coords_matrix[i, ], back_to_wgs84[i, ], 
                                              "WGS84", "WGS84", 1.0),
                info = paste("Matrix round-trip accuracy failed for row", i))
  }
})

test_that("Accuracy validation works comprehensively", {
  skip_if_not_installed("R6")
  
  transformer <- CoordinateTransformer$new()
  
  # Test with identical coordinates
  coords <- c(116.3974, 39.9093)
  expect_true(transformer$validate_accuracy(coords, coords, "WGS84", "WGS84", 1.0))
  
  # Test with coordinates within tolerance
  coords1 <- c(116.3974, 39.9093)
  coords2 <- c(116.3974001, 39.9093001)  # ~0.1 meter difference
  expect_true(transformer$validate_accuracy(coords1, coords2, "WGS84", "WGS84", 1.0))
  
  # Test with coordinates outside tolerance
  coords3 <- c(116.3984, 39.9103)  # ~1.5 km difference
  expect_false(transformer$validate_accuracy(coords1, coords3, "WGS84", "WGS84", 1.0))
  
  # Test with different tolerances
  expect_true(transformer$validate_accuracy(coords1, coords3, "WGS84", "WGS84", 2000.0))
  expect_false(transformer$validate_accuracy(coords1, coords3, "WGS84", "WGS84", 0.1))
  
  # Test parameter validation
  expect_error(transformer$validate_accuracy(NULL, coords2, "WGS84", "WGS84", 1.0),
               "Coordinates cannot be NULL")
  expect_error(transformer$validate_accuracy(coords1, NULL, "WGS84", "WGS84", 1.0),
               "Coordinates cannot be NULL")
  expect_error(transformer$validate_accuracy(coords1, coords2, "INVALID", "WGS84", 1.0),
               "Invalid CRS")
  expect_error(transformer$validate_accuracy(coords1, coords2, "WGS84", "WGS84", -1.0),
               "Tolerance must be positive")
})

test_that("Coordinates outside China are handled correctly", {
  skip_if_not_installed("R6")
  
  transformer <- CoordinateTransformer$new()
  
  # Test coordinates outside China
  outside_china_coords <- list(
    c(-74.0060, 40.7128),   # New York
    c(2.3522, 48.8566),     # Paris
    c(139.6917, 35.6895),   # Tokyo
    c(-0.1276, 51.5074),    # London
    c(151.2093, -33.8688),  # Sydney
    c(-43.1729, -22.9068),  # Rio de Janeiro
    c(55.2708, 25.2048)     # Dubai
  )
  
  for (coords in outside_china_coords) {
    # WGS84 to GCJ02 should not transform coordinates outside China
    gcj02_coords <- transformer$transform(coords, "WGS84", "GCJ02")
    expect_equal(coords, gcj02_coords,
                 info = paste("Coordinates outside China should not be transformed:", 
                             paste(coords, collapse = ", ")))
    
    # GCJ02 to WGS84 should also not transform
    back_coords <- transformer$transform(gcj02_coords, "GCJ02", "WGS84")
    expect_equal(gcj02_coords, back_coords)
    
    # BD09 transformations might still work (Baidu's algorithm)
    bd09_coords <- transformer$transform(coords, "WGS84", "BD09")
    expect_true(is.numeric(bd09_coords))
    expect_equal(length(bd09_coords), 2)
  }
})

test_that("Convenience function transform_coordinates works", {
  skip_if_not_installed("R6")
  
  # Test the standalone transform_coordinates function
  coords <- c(116.3974, 39.9093)
  
  # Test basic transformation
  result <- transform_coordinates(coords, "WGS84", "GCJ02")
  expect_true(is.numeric(result))
  expect_equal(length(result), 2)
  expect_false(identical(coords, result))
  
  # Test round-trip
  back <- transform_coordinates(result, "GCJ02", "WGS84")
  expect_true(all(abs(coords - back) < 1e-6))
  
  # Test parameter validation
  expect_error(transform_coordinates(NULL, "WGS84", "GCJ02"))
  expect_error(transform_coordinates(coords, "INVALID", "GCJ02"))
  expect_error(transform_coordinates(coords, "WGS84", "INVALID"))
})

test_that("Transformation accuracy meets requirements for all combinations", {
  skip_if_not_installed("R6")
  
  transformer <- CoordinateTransformer$new()
  
  # Test comprehensive accuracy for all CRS combinations
  test_cities <- list(
    list(name = "Beijing", coords = c(116.3974, 39.9093)),
    list(name = "Shanghai", coords = c(121.4737, 31.2304)),
    list(name = "Guangzhou", coords = c(113.2644, 23.1291)),
    list(name = "Shenzhen", coords = c(114.0579, 22.5431)),
    list(name = "Chengdu", coords = c(104.0668, 30.5728)),
    list(name = "Hangzhou", coords = c(120.1551, 30.2741)),
    list(name = "Wuhan", coords = c(114.3054, 30.5928)),
    list(name = "Xi'an", coords = c(108.9480, 34.2588))
  )
  
  crs_combinations <- list(
    list(from = "WGS84", to = "GCJ02"),
    list(from = "GCJ02", to = "WGS84"),
    list(from = "GCJ02", to = "BD09"),
    list(from = "BD09", to = "GCJ02"),
    list(from = "WGS84", to = "BD09"),
    list(from = "BD09", to = "WGS84")
  )
  
  for (city in test_cities) {
    for (combo in crs_combinations) {
      # Transform coordinates
      transformed <- transformer$transform(city$coords, combo$from, combo$to)
      
      # Transform back
      back_transformed <- transformer$transform(transformed, combo$to, combo$from)
      
      # Check 1-meter accuracy
      expect_true(
        transformer$validate_accuracy(city$coords, back_transformed, combo$from, combo$from, 1.0),
        info = sprintf("1-meter accuracy failed for %s: %s -> %s -> %s", 
                      city$name, combo$from, combo$to, combo$from)
      )
    }
  }
})

test_that("Edge cases and error conditions are handled", {
  skip_if_not_installed("R6")
  
  transformer <- CoordinateTransformer$new()
  
  # Test with extreme coordinates (but still valid)
  extreme_coords <- list(
    c(-179.9, -89.9),   # Near south-west extreme
    c(179.9, 89.9),     # Near north-east extreme
    c(0, 0),            # Origin
    c(-180, -90),       # Exact south-west extreme
    c(180, 90)          # Exact north-east extreme
  )
  
  for (coords in extreme_coords) {
    # Should not error for valid extreme coordinates
    expect_silent(transformer$transform(coords, "WGS84", "GCJ02"))
    expect_silent(transformer$transform(coords, "WGS84", "BD09"))
  }
  
  # Test with coordinates at China borders
  china_border_coords <- list(
    c(73.5, 53.5),      # Northwest China
    c(134.8, 53.3),     # Northeast China
    c(73.5, 18.2),      # Southwest China
    c(134.8, 18.2)      # Southeast China
  )
  
  for (coords in china_border_coords) {
    result <- transformer$transform(coords, "WGS84", "GCJ02")
    expect_true(is.numeric(result))
    expect_equal(length(result), 2)
  }
  
  # Test with NaN and Inf (should error)
  expect_error(transformer$transform(c(NaN, 39.9093), "WGS84", "GCJ02"))
  expect_error(transformer$transform(c(116.3974, NaN), "WGS84", "GCJ02"))
  expect_error(transformer$transform(c(Inf, 39.9093), "WGS84", "GCJ02"))
  expect_error(transformer$transform(c(116.3974, -Inf), "WGS84", "GCJ02"))
})

test_that("Performance requirements are met", {
  skip_if_not_installed("R6")
  
  transformer <- CoordinateTransformer$new()
  
  # Test single coordinate transformation performance
  coords <- c(116.3974, 39.9093)
  
  single_time <- system.time({
    for (i in 1:1000) {
      transformer$transform(coords, "WGS84", "GCJ02")
    }
  })
  
  # Should complete 1000 transformations in reasonable time (< 1 second)
  expect_lt(single_time[["elapsed"]], 1.0)
  
  # Test batch transformation performance
  large_matrix <- matrix(runif(2000, 70, 140), ncol = 2)  # 1000 coordinate pairs
  large_matrix[, 2] <- runif(1000, 10, 60)  # Adjust latitudes
  
  batch_time <- system.time({
    transformer$transform_batch(large_matrix, "WGS84", "GCJ02")
  })
  
  # Batch processing should be efficient
  expect_lt(batch_time[["elapsed"]], 2.0)
})