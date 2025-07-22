test_that("CoordinateTransformer initialization works correctly", {
  transformer <- CoordinateTransformer$new()
  
  expect_s3_class(transformer, "CoordinateTransformer")
  expect_equal(transformer$get_supported_crs(), c("WGS84", "GCJ02", "BD09"))
})

test_that("CoordinateTransformer validates CRS correctly", {
  transformer <- CoordinateTransformer$new()
  
  expect_true(transformer$is_supported_crs("WGS84"))
  expect_true(transformer$is_supported_crs("GCJ02"))
  expect_true(transformer$is_supported_crs("BD09"))
  expect_false(transformer$is_supported_crs("INVALID"))
})

test_that("CoordinateTransformer handles same CRS transformation", {
  transformer <- CoordinateTransformer$new()
  coords <- c(116.3974, 39.9093)
  
  result <- transformer$transform(coords, "WGS84", "WGS84")
  expect_equal(result, coords)
})

test_that("CoordinateTransformer validates coordinate inputs", {
  transformer <- CoordinateTransformer$new()
  
  # Invalid coordinate formats
  expect_error(transformer$transform(c(116.3974), "WGS84", "GCJ02"))
  expect_error(transformer$transform(c(116.3974, 39.9093, 10), "WGS84", "GCJ02"))
  expect_error(transformer$transform("invalid", "WGS84", "GCJ02"))
  
  # Invalid CRS
  expect_error(transformer$transform(c(116.3974, 39.9093), "INVALID", "GCJ02"))
  expect_error(transformer$transform(c(116.3974, 39.9093), "WGS84", "INVALID"))
})

test_that("WGS84 to GCJ02 transformation works correctly", {
  transformer <- CoordinateTransformer$new()
  
  # Beijing coordinates (known test case)
  beijing_wgs84 <- c(116.3974, 39.9093)
  beijing_gcj02 <- transformer$transform(beijing_wgs84, "WGS84", "GCJ02")
  
  # GCJ02 coordinates should be slightly different from WGS84
  expect_false(identical(beijing_wgs84, beijing_gcj02))
  expect_true(abs(beijing_gcj02[1] - beijing_wgs84[1]) > 0)
  expect_true(abs(beijing_gcj02[2] - beijing_wgs84[2]) > 0)
  
  # Transformation should be within reasonable bounds (< 1km difference)
  distance_diff <- sqrt((beijing_gcj02[1] - beijing_wgs84[1])^2 + (beijing_gcj02[2] - beijing_wgs84[2])^2) * 111000
  expect_true(distance_diff < 1000)  # Less than 1km
})

test_that("GCJ02 to WGS84 transformation works correctly", {
  transformer <- CoordinateTransformer$new()
  
  # Start with WGS84, transform to GCJ02, then back to WGS84
  original_wgs84 <- c(116.3974, 39.9093)
  gcj02_coords <- transformer$transform(original_wgs84, "WGS84", "GCJ02")
  back_to_wgs84 <- transformer$transform(gcj02_coords, "GCJ02", "WGS84")
  
  # Should be very close to original (within 1 meter)
  expect_true(transformer$validate_accuracy(original_wgs84, back_to_wgs84, "WGS84", "WGS84", 1.0))
})

test_that("GCJ02 to BD09 transformation works correctly", {
  transformer <- CoordinateTransformer$new()
  
  # Beijing coordinates in GCJ02
  beijing_gcj02 <- c(116.404, 39.915)  # Approximate GCJ02 coordinates
  beijing_bd09 <- transformer$transform(beijing_gcj02, "GCJ02", "BD09")
  
  # BD09 coordinates should be different from GCJ02
  expect_false(identical(beijing_gcj02, beijing_bd09))
  expect_true(abs(beijing_bd09[1] - beijing_gcj02[1]) > 0)
  expect_true(abs(beijing_bd09[2] - beijing_gcj02[2]) > 0)
})

test_that("BD09 to GCJ02 transformation works correctly", {
  transformer <- CoordinateTransformer$new()
  
  # Round trip test: GCJ02 -> BD09 -> GCJ02
  original_gcj02 <- c(116.404, 39.915)
  bd09_coords <- transformer$transform(original_gcj02, "GCJ02", "BD09")
  back_to_gcj02 <- transformer$transform(bd09_coords, "BD09", "GCJ02")
  
  # Should be very close to original
  expect_true(transformer$validate_accuracy(original_gcj02, back_to_gcj02, "GCJ02", "GCJ02", 1.0))
})

test_that("WGS84 to BD09 transformation works correctly", {
  transformer <- CoordinateTransformer$new()
  
  # Beijing coordinates
  beijing_wgs84 <- c(116.3974, 39.9093)
  beijing_bd09 <- transformer$transform(beijing_wgs84, "WGS84", "BD09")
  
  # BD09 coordinates should be different from WGS84
  expect_false(identical(beijing_wgs84, beijing_bd09))
  
  # Round trip test
  back_to_wgs84 <- transformer$transform(beijing_bd09, "BD09", "WGS84")
  expect_true(transformer$validate_accuracy(beijing_wgs84, back_to_wgs84, "WGS84", "WGS84", 1.0))
})

test_that("BD09 to WGS84 transformation works correctly", {
  transformer <- CoordinateTransformer$new()
  
  # Start with WGS84, go to BD09, then back
  original_wgs84 <- c(116.3974, 39.9093)
  bd09_coords <- transformer$transform(original_wgs84, "WGS84", "BD09")
  back_to_wgs84 <- transformer$transform(bd09_coords, "BD09", "WGS84")
  
  # Should be very close to original
  expect_true(transformer$validate_accuracy(original_wgs84, back_to_wgs84, "WGS84", "WGS84", 1.0))
})

test_that("Matrix transformation works correctly", {
  transformer <- CoordinateTransformer$new()
  
  # Multiple points
  coords_matrix <- matrix(c(
    116.3974, 39.9093,  # Beijing
    121.4737, 31.2304   # Shanghai
  ), ncol = 2, byrow = TRUE)
  
  gcj02_matrix <- transformer$transform(coords_matrix, "WGS84", "GCJ02")
  
  expect_true(is.matrix(gcj02_matrix))
  expect_equal(dim(gcj02_matrix), dim(coords_matrix))
  expect_false(identical(coords_matrix, gcj02_matrix))
})

test_that("Batch transformation works correctly", {
  transformer <- CoordinateTransformer$new()
  
  coords_matrix <- matrix(c(
    116.3974, 39.9093,
    121.4737, 31.2304,
    113.2644, 23.1291
  ), ncol = 2, byrow = TRUE)
  
  result <- transformer$transform_batch(coords_matrix, "WGS84", "GCJ02")
  
  expect_true(is.matrix(result))
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 2)
})

test_that("Accuracy validation works correctly", {
  transformer <- CoordinateTransformer$new()
  
  original <- c(116.3974, 39.9093)
  
  # Transform and back - should be accurate
  gcj02 <- transformer$transform(original, "WGS84", "GCJ02")
  back <- transformer$transform(gcj02, "GCJ02", "WGS84")
  
  expect_true(transformer$validate_accuracy(original, back, "WGS84", "WGS84", 1.0))
  
  # Test with artificially inaccurate coordinates
  inaccurate <- c(original[1] + 0.01, original[2] + 0.01)  # ~1km off
  expect_false(transformer$validate_accuracy(original, inaccurate, "WGS84", "WGS84", 1.0))
})

test_that("Coordinates outside China are handled correctly", {
  transformer <- CoordinateTransformer$new()
  
  # New York coordinates (outside China)
  ny_coords <- c(-74.0060, 40.7128)
  
  # WGS84 to GCJ02 should not transform coordinates outside China
  gcj02_coords <- transformer$transform(ny_coords, "WGS84", "GCJ02")
  expect_equal(ny_coords, gcj02_coords)
  
  # GCJ02 to WGS84 should also not transform
  back_coords <- transformer$transform(gcj02_coords, "GCJ02", "WGS84")
  expect_equal(gcj02_coords, back_coords)
})

test_that("Convenience function works correctly", {
  # Test the standalone transform_coordinates function
  coords <- c(116.3974, 39.9093)
  result <- transform_coordinates(coords, "WGS84", "GCJ02")
  
  expect_true(is.numeric(result))
  expect_equal(length(result), 2)
  expect_false(identical(coords, result))
})

test_that("Transformation accuracy meets 1-meter tolerance requirement", {
  transformer <- CoordinateTransformer$new()
  
  # Test multiple cities in China
  test_cities <- matrix(c(
    116.3974, 39.9093,  # Beijing
    121.4737, 31.2304,  # Shanghai
    113.2644, 23.1291,  # Guangzhou
    114.0579, 22.5431,  # Shenzhen
    108.9480, 34.2588   # Xi'an
  ), ncol = 2, byrow = TRUE)
  
  for (i in seq_len(nrow(test_cities))) {
    original <- test_cities[i, ]
    
    # Test WGS84 <-> GCJ02 accuracy
    gcj02 <- transformer$transform(original, "WGS84", "GCJ02")
    back_wgs84 <- transformer$transform(gcj02, "GCJ02", "WGS84")
    expect_true(transformer$validate_accuracy(original, back_wgs84, "WGS84", "WGS84", 1.0),
                info = sprintf("WGS84<->GCJ02 accuracy failed for city %d", i))
    
    # Test GCJ02 <-> BD09 accuracy
    bd09 <- transformer$transform(gcj02, "GCJ02", "BD09")
    back_gcj02 <- transformer$transform(bd09, "BD09", "GCJ02")
    expect_true(transformer$validate_accuracy(gcj02, back_gcj02, "GCJ02", "GCJ02", 1.0),
                info = sprintf("GCJ02<->BD09 accuracy failed for city %d", i))
    
    # Test WGS84 <-> BD09 accuracy (full chain)
    bd09_direct <- transformer$transform(original, "WGS84", "BD09")
    back_wgs84_direct <- transformer$transform(bd09_direct, "BD09", "WGS84")
    expect_true(transformer$validate_accuracy(original, back_wgs84_direct, "WGS84", "WGS84", 1.0),
                info = sprintf("WGS84<->BD09 accuracy failed for city %d", i))
  }
})