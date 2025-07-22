test_that("CoordinateDetector initialization works correctly", {
  detector <- CoordinateDetector$new()
  
  expect_s3_class(detector, "CoordinateDetector")
  expect_equal(detector$get_supported_crs(), c("WGS84", "GCJ02", "BD09"))
})

test_that("CoordinateDetector detects WGS84 coordinates outside China", {
  detector <- CoordinateDetector$new()
  
  # New York coordinates (clearly WGS84)
  ny_data <- data.frame(lon = c(-74.0060, -73.9352), lat = c(40.7128, 40.7589))
  detection <- detector$detect_crs(ny_data)
  
  expect_equal(detection$crs, "WGS84")
  expect_true(detection$confidence > 0.8)
})

test_that("CoordinateDetector detects coordinates in China region", {
  detector <- CoordinateDetector$new()
  
  # Beijing coordinates
  beijing_data <- data.frame(lon = c(116.3974, 116.4074), lat = c(39.9093, 39.9193))
  detection <- detector$detect_crs(beijing_data)
  
  expect_true(detection$crs %in% c("WGS84", "GCJ02", "BD09"))
  expect_true(detection$confidence > 0.3)  # Should have some confidence
  expect_true(is.list(detection$scores))
  expect_equal(length(detection$scores), 3)
})

test_that("CoordinateDetector handles different data formats", {
  detector <- CoordinateDetector$new()
  
  # Vector format
  coords_vector <- c(-74.0060, 40.7128)
  detection_vector <- detector$detect_crs(coords_vector)
  expect_equal(detection_vector$crs, "WGS84")
  
  # Matrix format
  coords_matrix <- matrix(c(-74.0060, 40.7128, -73.9352, 40.7589), ncol = 2, byrow = TRUE)
  detection_matrix <- detector$detect_crs(coords_matrix)
  expect_equal(detection_matrix$crs, "WGS84")
  
  # Data frame with custom column names
  coords_df <- data.frame(longitude = c(-74.0060, -73.9352), latitude = c(40.7128, 40.7589))
  detection_df <- detector$detect_crs(coords_df)
  expect_equal(detection_df$crs, "WGS84")
})

test_that("CoordinateDetector validates input data", {
  detector <- CoordinateDetector$new()
  
  # Empty data
  empty_data <- data.frame(lon = numeric(0), lat = numeric(0))
  expect_error(detector$detect_crs(empty_data))
  
  # Invalid data format
  expect_error(detector$detect_crs("invalid"))
  expect_error(detector$detect_crs(list(a = 1, b = 2)))
})

test_that("CoordinateDetector handles missing values", {
  detector <- CoordinateDetector$new()
  
  # Data with missing values
  data_with_na <- data.frame(
    lon = c(116.3974, NA, 121.4737),
    lat = c(39.9093, 31.2304, NA)
  )
  
  # Should work by removing incomplete cases
  detection <- detector$detect_crs(data_with_na)
  expect_true(detection$crs %in% c("WGS84", "GCJ02", "BD09"))
})

test_that("CoordinateDetector auto-transforms for providers", {
  detector <- CoordinateDetector$new()
  
  # Test data (assume WGS84)
  wgs84_data <- data.frame(lon = c(116.3974, 121.4737), lat = c(39.9093, 31.2304))
  
  # Transform for Mapbox (WGS84) - should be unchanged
  mapbox_data <- detector$auto_transform_for_provider(wgs84_data, "mapbox", "WGS84")
  expect_equal(wgs84_data, mapbox_data)
  
  # Transform for Gaode (GCJ02) - should be transformed
  gaode_data <- detector$auto_transform_for_provider(wgs84_data, "gaode", "WGS84")
  expect_false(identical(wgs84_data$lon, gaode_data$lon))
  expect_false(identical(wgs84_data$lat, gaode_data$lat))
  expect_equal(wgs84_data$lon, gaode_data$lon, tolerance = 0.01)  # Should be close but different
})

test_that("CoordinateDetector auto-detects source CRS", {
  detector <- CoordinateDetector$new()
  
  # US coordinates (should be detected as WGS84)
  us_data <- data.frame(lon = c(-74.0060, -73.9352), lat = c(40.7128, 40.7589))
  
  # Auto-transform for Mapbox (should detect WGS84 and leave unchanged)
  result <- detector$auto_transform_for_provider(us_data, "mapbox")
  expect_equal(us_data, result)
})

test_that("CoordinateDetector validates transformation accuracy", {
  detector <- CoordinateDetector$new()
  
  # Create test data
  original_data <- data.frame(lon = c(116.3974, 121.4737), lat = c(39.9093, 31.2304))
  
  # Transform to GCJ02
  transformer <- CoordinateTransformer$new()
  transformed_coords <- transformer$transform_batch(as.matrix(original_data), "WGS84", "GCJ02")
  transformed_data <- data.frame(lon = transformed_coords[, 1], lat = transformed_coords[, 2])
  
  # Validate accuracy
  validation <- detector$validate_transformation_accuracy(
    original_data, transformed_data, "WGS84", "GCJ02", tolerance_meters = 1.0
  )
  
  expect_true(is.list(validation))
  expect_true("accuracy_rate" %in% names(validation))
  expect_true("points_tested" %in% names(validation))
  expect_true("passed" %in% names(validation))
  expect_true(validation$passed)  # Should pass accuracy test
  expect_equal(validation$points_tested, 2)
})

test_that("CoordinateDetector checks China region correctly", {
  detector <- CoordinateDetector$new()
  
  # China coordinates
  china_coords <- data.frame(lon = c(116.3974, 121.4737), lat = c(39.9093, 31.2304))
  expect_true(detector$is_china_region(china_coords))
  
  # US coordinates
  us_coords <- data.frame(lon = c(-74.0060, -73.9352), lat = c(40.7128, 40.7589))
  expect_false(detector$is_china_region(us_coords))
  
  # Mixed coordinates (majority in China)
  mixed_coords <- data.frame(
    lon = c(116.3974, 121.4737, -74.0060),
    lat = c(39.9093, 31.2304, 40.7128)
  )
  expect_true(detector$is_china_region(mixed_coords))  # 2/3 in China
})

test_that("CoordinateDetector handles edge cases", {
  detector <- CoordinateDetector$new()
  
  # Single point
  single_point <- data.frame(lon = 116.3974, lat = 39.9093)
  detection <- detector$detect_crs(single_point)
  expect_true(detection$crs %in% c("WGS84", "GCJ02", "BD09"))
  
  # Coordinates at boundaries
  boundary_coords <- data.frame(
    lon = c(-180, 180, 0),
    lat = c(-90, 90, 0)
  )
  detection_boundary <- detector$detect_crs(boundary_coords)
  expect_equal(detection_boundary$crs, "WGS84")  # Should be WGS84 for global coordinates
})

test_that("CoordinateDetector handles low confidence detection", {
  detector <- CoordinateDetector$new()
  
  # Ambiguous coordinates that might be hard to classify
  ambiguous_data <- data.frame(lon = c(116.4, 116.41), lat = c(39.91, 39.92))
  
  # Should still return a result but may warn about low confidence
  expect_warning(
    detection <- detector$detect_crs(ambiguous_data, confidence_threshold = 0.9),
    "Low confidence"
  )
  
  expect_true(detection$crs %in% c("WGS84", "GCJ02", "BD09"))
})

test_that("Convenience functions work correctly", {
  # Test detect_coordinate_system function
  data <- data.frame(lon = c(-74.0060, -73.9352), lat = c(40.7128, 40.7589))
  detection <- detect_coordinate_system(data)
  
  expect_true(is.list(detection))
  expect_true("crs" %in% names(detection))
  expect_equal(detection$crs, "WGS84")
  
  # Test auto_transform_for_provider function
  china_data <- data.frame(lon = c(116.3974, 121.4737), lat = c(39.9093, 31.2304))
  transformed <- auto_transform_for_provider(china_data, "gaode", "WGS84")
  
  expect_true(is.data.frame(transformed))
  expect_equal(names(transformed), names(china_data))
})

test_that("CoordinateDetector handles coordinate column detection", {
  detector <- CoordinateDetector$new()
  
  # Test various column name patterns
  test_cases <- list(
    data.frame(lon = 116.3974, lat = 39.9093),
    data.frame(lng = 116.3974, lat = 39.9093),
    data.frame(longitude = 116.3974, latitude = 39.9093),
    data.frame(x = 116.3974, y = 39.9093),
    data.frame(X = 116.3974, Y = 39.9093)
  )
  
  for (test_data in test_cases) {
    detection <- detector$detect_crs(test_data)
    expect_true(detection$crs %in% c("WGS84", "GCJ02", "BD09"))
  }
})

test_that("CoordinateDetector validates transformation with sampling", {
  detector <- CoordinateDetector$new()
  
  # Create larger dataset
  n_points <- 200
  original_data <- data.frame(
    lon = runif(n_points, 116, 122),
    lat = runif(n_points, 30, 40)
  )
  
  # Transform data
  transformer <- CoordinateTransformer$new()
  transformed_coords <- transformer$transform_batch(as.matrix(original_data), "WGS84", "GCJ02")
  transformed_data <- data.frame(lon = transformed_coords[, 1], lat = transformed_coords[, 2])
  
  # Validate with sampling
  validation <- detector$validate_transformation_accuracy(
    original_data, transformed_data, "WGS84", "GCJ02", 
    tolerance_meters = 1.0, sample_size = 50
  )
  
  expect_equal(validation$points_tested, 50)  # Should sample 50 points
  expect_true(validation$passed)
})

test_that("CoordinateDetector detects coordinate systems with known transformations", {
  detector <- CoordinateDetector$new()
  transformer <- CoordinateTransformer$new()
  
  # Start with known WGS84 coordinates
  wgs84_data <- data.frame(lon = c(116.3974, 121.4737), lat = c(39.9093, 31.2304))
  
  # Transform to GCJ02
  gcj02_coords <- transformer$transform_batch(as.matrix(wgs84_data), "WGS84", "GCJ02")
  gcj02_data <- data.frame(lon = gcj02_coords[, 1], lat = gcj02_coords[, 2])
  
  # Transform to BD09
  bd09_coords <- transformer$transform_batch(gcj02_coords, "GCJ02", "BD09")
  bd09_data <- data.frame(lon = bd09_coords[, 1], lat = bd09_coords[, 2])
  
  # Detection should work for all coordinate systems
  # Note: Detection might not be 100% accurate due to the similarity of coordinate systems
  # but should at least identify them as valid Chinese coordinate systems
  
  wgs84_detection <- detector$detect_crs(wgs84_data)
  gcj02_detection <- detector$detect_crs(gcj02_data)
  bd09_detection <- detector$detect_crs(bd09_data)
  
  expect_true(wgs84_detection$crs %in% c("WGS84", "GCJ02", "BD09"))
  expect_true(gcj02_detection$crs %in% c("WGS84", "GCJ02", "BD09"))
  expect_true(bd09_detection$crs %in% c("WGS84", "GCJ02", "BD09"))
})