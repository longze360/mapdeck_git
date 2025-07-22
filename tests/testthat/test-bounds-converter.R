test_that("BoundsConverter initialization works correctly", {
  bounds_converter <- BoundsConverter$new()
  
  expect_s3_class(bounds_converter, "BoundsConverter")
})

test_that("BoundsConverter validates bounds input", {
  bounds_converter <- BoundsConverter$new()
  
  # Invalid bounds formats
  expect_error(bounds_converter$transform_bounds(c(1, 2, 3), "WGS84", "GCJ02"))  # Too few elements
  expect_error(bounds_converter$transform_bounds(c(1, 2, 3, 4, 5), "WGS84", "GCJ02"))  # Too many elements
  expect_error(bounds_converter$transform_bounds(c("a", "b", "c", "d"), "WGS84", "GCJ02"))  # Non-numeric
  
  # Invalid bounds geometry (west >= east, south >= north)
  expect_error(bounds_converter$transform_bounds(c(117, 39, 116, 40), "WGS84", "GCJ02"))  # west >= east
  expect_error(bounds_converter$transform_bounds(c(116, 40, 117, 39), "WGS84", "GCJ02"))  # south >= north
})

test_that("BoundsConverter handles same CRS transformation", {
  bounds_converter <- BoundsConverter$new()
  
  bounds <- c(116.3, 39.8, 116.5, 40.0)
  result <- bounds_converter$transform_bounds(bounds, "WGS84", "WGS84")
  
  expect_equal(result, bounds)
})

test_that("BoundsConverter transforms bounds between coordinate systems", {
  bounds_converter <- BoundsConverter$new()
  
  # Beijing area bounds in WGS84
  wgs84_bounds <- c(116.3, 39.8, 116.5, 40.0)
  
  # Transform to GCJ02
  gcj02_bounds <- bounds_converter$transform_bounds(wgs84_bounds, "WGS84", "GCJ02")
  
  expect_equal(length(gcj02_bounds), 4)
  expect_false(identical(wgs84_bounds, gcj02_bounds))
  
  # Bounds should still be valid (west < east, south < north)
  expect_true(gcj02_bounds[1] < gcj02_bounds[3])  # west < east
  expect_true(gcj02_bounds[2] < gcj02_bounds[4])  # south < north
})

test_that("BoundsConverter transforms bounds to BD09", {
  bounds_converter <- BoundsConverter$new()
  
  wgs84_bounds <- c(116.3, 39.8, 116.5, 40.0)
  
  # Transform WGS84 to BD09
  bd09_bounds <- bounds_converter$transform_bounds(wgs84_bounds, "WGS84", "BD09")
  
  expect_equal(length(bd09_bounds), 4)
  expect_false(identical(wgs84_bounds, bd09_bounds))
  expect_true(bd09_bounds[1] < bd09_bounds[3])  # west < east
  expect_true(bd09_bounds[2] < bd09_bounds[4])  # south < north
})

test_that("BoundsConverter handles different sample point counts", {
  bounds_converter <- BoundsConverter$new()
  
  bounds <- c(116.3, 39.8, 116.5, 40.0)
  
  # Test with different sample point counts
  result_5 <- bounds_converter$transform_bounds(bounds, "WGS84", "GCJ02", sample_points = 5)
  result_20 <- bounds_converter$transform_bounds(bounds, "WGS84", "GCJ02", sample_points = 20)
  
  expect_equal(length(result_5), 4)
  expect_equal(length(result_20), 4)
  
  # Results should be similar but may differ slightly due to sampling
  expect_true(abs(result_5[1] - result_20[1]) < 0.001)  # Small difference expected
})

test_that("BoundsConverter transforms viewport correctly", {
  bounds_converter <- BoundsConverter$new()
  
  # Test viewport
  viewport <- list(
    longitude = 116.3974,
    latitude = 39.9093,
    zoom = 10,
    pitch = 0,
    bearing = 0
  )
  
  # Transform to GCJ02
  gcj02_viewport <- bounds_converter$transform_viewport(viewport, "WGS84", "GCJ02")
  
  expect_true(is.list(gcj02_viewport))
  expect_true("longitude" %in% names(gcj02_viewport))
  expect_true("latitude" %in% names(gcj02_viewport))
  expect_equal(gcj02_viewport$zoom, viewport$zoom)  # Non-coordinate fields preserved
  expect_equal(gcj02_viewport$pitch, viewport$pitch)
  expect_equal(gcj02_viewport$bearing, viewport$bearing)
  
  # Coordinates should be transformed
  expect_false(gcj02_viewport$longitude == viewport$longitude)
  expect_false(gcj02_viewport$latitude == viewport$latitude)
})

test_that("BoundsConverter validates viewport structure", {
  bounds_converter <- BoundsConverter$new()
  
  # Invalid viewport (missing required fields)
  invalid_viewport <- list(zoom = 10)
  expect_error(bounds_converter$transform_viewport(invalid_viewport, "WGS84", "GCJ02"))
  
  # Valid minimal viewport
  minimal_viewport <- list(longitude = 116.3974, latitude = 39.9093)
  result <- bounds_converter$transform_viewport(minimal_viewport, "WGS84", "GCJ02")
  expect_true(is.list(result))
  expect_true("longitude" %in% names(result))
  expect_true("latitude" %in% names(result))
})

test_that("BoundsConverter handles same CRS viewport transformation", {
  bounds_converter <- BoundsConverter$new()
  
  viewport <- list(longitude = 116.3974, latitude = 39.9093, zoom = 10)
  result <- bounds_converter$transform_viewport(viewport, "WGS84", "WGS84")
  
  expect_equal(result, viewport)
})

test_that("BoundsConverter transforms multiple bounds efficiently", {
  bounds_converter <- BoundsConverter$new()
  
  bounds_list <- list(
    c(116.3, 39.8, 116.5, 40.0),  # Beijing
    c(121.4, 31.1, 121.6, 31.3),  # Shanghai
    c(113.2, 23.0, 113.4, 23.2)   # Guangzhou
  )
  
  transformed_list <- bounds_converter$transform_bounds_batch(bounds_list, "WGS84", "GCJ02")
  
  expect_true(is.list(transformed_list))
  expect_equal(length(transformed_list), 3)
  
  for (i in seq_along(transformed_list)) {
    expect_equal(length(transformed_list[[i]]), 4)
    expect_false(identical(bounds_list[[i]], transformed_list[[i]]))
  }
})

test_that("BoundsConverter calculates bounds area correctly", {
  bounds_converter <- BoundsConverter$new()
  
  # Small area in Beijing (approximately 1km x 1km)
  small_bounds <- c(116.39, 39.90, 116.40, 39.91)
  area <- bounds_converter$calculate_bounds_area(small_bounds, "WGS84")
  
  expect_true(is.numeric(area))
  expect_true(area > 0)
  # Should be roughly 1 square kilometer (allowing for latitude effects)
  expect_true(area > 500000 && area < 2000000)  # 0.5 to 2 sq km
})

test_that("BoundsConverter calculates area for different CRS", {
  bounds_converter <- BoundsConverter$new()
  
  bounds <- c(116.39, 39.90, 116.40, 39.91)
  
  # Calculate area in different coordinate systems
  wgs84_area <- bounds_converter$calculate_bounds_area(bounds, "WGS84")
  gcj02_area <- bounds_converter$calculate_bounds_area(bounds, "GCJ02")
  bd09_area <- bounds_converter$calculate_bounds_area(bounds, "BD09")
  
  # Areas should be similar (within 10% due to coordinate system differences)
  expect_true(abs(wgs84_area - gcj02_area) / wgs84_area < 0.1)
  expect_true(abs(wgs84_area - bd09_area) / wgs84_area < 0.1)
})

test_that("BoundsConverter checks point in bounds correctly", {
  bounds_converter <- BoundsConverter$new()
  
  bounds <- c(116.3, 39.8, 116.5, 40.0)
  
  # Point inside bounds
  inside_point <- c(116.4, 39.9)
  expect_true(bounds_converter$point_in_bounds(inside_point, bounds, "WGS84"))
  
  # Point outside bounds
  outside_point <- c(116.2, 39.9)  # West of bounds
  expect_false(bounds_converter$point_in_bounds(outside_point, bounds, "WGS84"))
  
  # Point on boundary
  boundary_point <- c(116.3, 39.9)  # On west edge
  expect_true(bounds_converter$point_in_bounds(boundary_point, bounds, "WGS84"))
})

test_that("BoundsConverter expands bounds correctly", {
  bounds_converter <- BoundsConverter$new()
  
  original_bounds <- c(116.3, 39.8, 116.5, 40.0)
  margin <- 0.1
  
  expanded_bounds <- bounds_converter$expand_bounds(original_bounds, margin)
  
  expect_equal(length(expanded_bounds), 4)
  expect_equal(expanded_bounds[1], original_bounds[1] - margin)  # west
  expect_equal(expanded_bounds[2], original_bounds[2] - margin)  # south
  expect_equal(expanded_bounds[3], original_bounds[3] + margin)  # east
  expect_equal(expanded_bounds[4], original_bounds[4] + margin)  # north
})

test_that("BoundsConverter gets bounds center correctly", {
  bounds_converter <- BoundsConverter$new()
  
  bounds <- c(116.3, 39.8, 116.5, 40.0)
  center <- bounds_converter$get_bounds_center(bounds)
  
  expect_equal(length(center), 2)
  expect_equal(center[1], (116.3 + 116.5) / 2)  # longitude
  expect_equal(center[2], (39.8 + 40.0) / 2)    # latitude
})

test_that("BoundsConverter validates input formats", {
  bounds_converter <- BoundsConverter$new()
  
  # Invalid bounds for area calculation
  expect_error(bounds_converter$calculate_bounds_area(c(1, 2, 3), "WGS84"))
  
  # Invalid point for point_in_bounds
  expect_error(bounds_converter$point_in_bounds(c(1), c(1, 2, 3, 4), "WGS84"))
  expect_error(bounds_converter$point_in_bounds(c(1, 2), c(1, 2, 3), "WGS84"))
  
  # Invalid bounds for expand_bounds
  expect_error(bounds_converter$expand_bounds(c(1, 2, 3), 0.1))
  
  # Invalid bounds for get_bounds_center
  expect_error(bounds_converter$get_bounds_center(c(1, 2, 3)))
})

test_that("Convenience functions work correctly", {
  # Test transform_bounds function
  bounds <- c(116.3, 39.8, 116.5, 40.0)
  result <- transform_bounds(bounds, "WGS84", "GCJ02")
  
  expect_equal(length(result), 4)
  expect_false(identical(bounds, result))
  
  # Test transform_viewport function
  viewport <- list(longitude = 116.3974, latitude = 39.9093, zoom = 10)
  result_viewport <- transform_viewport(viewport, "WGS84", "GCJ02")
  
  expect_true(is.list(result_viewport))
  expect_false(result_viewport$longitude == viewport$longitude)
})

test_that("BoundsConverter handles edge cases", {
  bounds_converter <- BoundsConverter$new()
  
  # Very small bounds
  tiny_bounds <- c(116.3974, 39.9093, 116.3975, 39.9094)
  result <- bounds_converter$transform_bounds(tiny_bounds, "WGS84", "GCJ02")
  expect_equal(length(result), 4)
  expect_true(result[1] < result[3])  # Still valid bounds
  expect_true(result[2] < result[4])
  
  # Large bounds (whole China)
  large_bounds <- c(73, 18, 135, 54)
  result_large <- bounds_converter$transform_bounds(large_bounds, "WGS84", "GCJ02")
  expect_equal(length(result_large), 4)
  expect_true(result_large[1] < result_large[3])
  expect_true(result_large[2] < result_large[4])
})

test_that("BoundsConverter transformation accuracy", {
  bounds_converter <- BoundsConverter$new()
  
  # Test round-trip transformation accuracy
  original_bounds <- c(116.3, 39.8, 116.5, 40.0)
  
  # WGS84 -> GCJ02 -> WGS84
  gcj02_bounds <- bounds_converter$transform_bounds(original_bounds, "WGS84", "GCJ02")
  back_to_wgs84 <- bounds_converter$transform_bounds(gcj02_bounds, "GCJ02", "WGS84")
  
  # Should be close to original (within reasonable tolerance)
  for (i in 1:4) {
    expect_true(abs(original_bounds[i] - back_to_wgs84[i]) < 0.001,
                info = sprintf("Round-trip accuracy failed for bounds element %d", i))
  }
  
  # Test viewport round-trip
  original_viewport <- list(longitude = 116.3974, latitude = 39.9093, zoom = 10)
  gcj02_viewport <- bounds_converter$transform_viewport(original_viewport, "WGS84", "GCJ02")
  back_viewport <- bounds_converter$transform_viewport(gcj02_viewport, "GCJ02", "WGS84")
  
  expect_true(abs(original_viewport$longitude - back_viewport$longitude) < 0.001)
  expect_true(abs(original_viewport$latitude - back_viewport$latitude) < 0.001)
})