test_that("Chinese coordinate system transformations are accurate", {
  skip_if_not_installed("R6")
  
  # Test coordinate transformation accuracy between all Chinese coordinate systems
  transformer <- CoordinateTransformer$new()
  
  # Test coordinates: Beijing Tiananmen Square
  wgs84_beijing <- c(116.3974, 39.9093)
  
  # Test WGS84 -> GCJ02 -> WGS84 round trip
  gcj02_beijing <- transformer$transform(wgs84_beijing, "WGS84", "GCJ02")
  back_to_wgs84_from_gcj02 <- transformer$transform(gcj02_beijing, "GCJ02", "WGS84")
  
  # Test WGS84 -> BD09 -> WGS84 round trip
  bd09_beijing <- transformer$transform(wgs84_beijing, "WGS84", "BD09")
  back_to_wgs84_from_bd09 <- transformer$transform(bd09_beijing, "BD09", "WGS84")
  
  # Test GCJ02 -> BD09 -> GCJ02 round trip
  bd09_from_gcj02 <- transformer$transform(gcj02_beijing, "GCJ02", "BD09")
  back_to_gcj02 <- transformer$transform(bd09_from_gcj02, "BD09", "GCJ02")
  
  # Validate accuracy within 1 meter tolerance (approximately 1e-5 degrees)
  tolerance <- 1e-5
  
  expect_lt(abs(wgs84_beijing[1] - back_to_wgs84_from_gcj02[1]), tolerance)
  expect_lt(abs(wgs84_beijing[2] - back_to_wgs84_from_gcj02[2]), tolerance)
  
  expect_lt(abs(wgs84_beijing[1] - back_to_wgs84_from_bd09[1]), tolerance)
  expect_lt(abs(wgs84_beijing[2] - back_to_wgs84_from_bd09[2]), tolerance)
  
  expect_lt(abs(gcj02_beijing[1] - back_to_gcj02[1]), tolerance)
  expect_lt(abs(gcj02_beijing[2] - back_to_gcj02[2]), tolerance)
})

test_that("Coordinate transformations work with multiple points", {
  skip_if_not_installed("R6")
  
  transformer <- CoordinateTransformer$new()
  
  # Test with multiple Chinese cities
  chinese_cities <- matrix(c(
    116.3974, 39.9093,  # Beijing
    121.4737, 31.2304,  # Shanghai
    113.2644, 23.1291,  # Guangzhou
    114.0579, 22.5431   # Shenzhen
  ), ncol = 2, byrow = TRUE)
  
  # Transform WGS84 -> GCJ02
  gcj02_cities <- transformer$transform_batch(chinese_cities, "WGS84", "GCJ02")
  expect_equal(dim(gcj02_cities), dim(chinese_cities))
  
  # Transform WGS84 -> BD09
  bd09_cities <- transformer$transform_batch(chinese_cities, "WGS84", "BD09")
  expect_equal(dim(bd09_cities), dim(chinese_cities))
  
  # Verify that transformations are different
  expect_false(identical(chinese_cities, gcj02_cities))
  expect_false(identical(chinese_cities, bd09_cities))
  expect_false(identical(gcj02_cities, bd09_cities))
})

test_that("Coordinate transformations handle edge cases", {
  skip_if_not_installed("R6")
  
  transformer <- CoordinateTransformer$new()
  
  # Test coordinates outside China (should remain unchanged for GCJ02)
  outside_china <- c(-74.006, 40.7128) # New York
  gcj02_outside <- transformer$transform(outside_china, "WGS84", "GCJ02")
  expect_equal(gcj02_outside, outside_china)
  
  # Test coordinates at China boundary
  china_boundary <- c(72.004, 0.8293) # Southwest corner of China
  gcj02_boundary <- transformer$transform(china_boundary, "WGS84", "GCJ02")
  # Should be transformed since it's at the boundary
  expect_false(identical(china_boundary, gcj02_boundary))
  
  # Test with extreme coordinates
  extreme_coords <- c(180, 90)
  gcj02_extreme <- transformer$transform(extreme_coords, "WGS84", "GCJ02")
  expect_equal(gcj02_extreme, extreme_coords) # Outside China, unchanged
})

test_that("Provider coordinate system integration works", {
  skip_if_not_installed("R6")
  
  # Create both Chinese providers
  gaode_provider <- GaodeProvider$new()
  gaode_provider$initialize_provider(list(api_key = "test_gaode_key"))
  
  baidu_provider <- BaiduProvider$new()
  baidu_provider$initialize_provider(list(api_key = "test_baidu_key"))
  
  # Test data with WGS84 coordinates
  test_data <- data.frame(
    longitude = c(116.3974, 121.4737),
    latitude = c(39.9093, 31.2304),
    value = c(10, 20)
  )
  
  # Prepare data for each provider
  suppressMessages({
    gaode_data <- gaode_provider$prepare_data(test_data)
    baidu_data <- baidu_provider$prepare_data(test_data)
  })
  
  # Both should have transformed the coordinates
  expect_false(identical(test_data$longitude, gaode_data$longitude))
  expect_false(identical(test_data$latitude, gaode_data$latitude))
  expect_false(identical(test_data$longitude, baidu_data$longitude))
  expect_false(identical(test_data$latitude, baidu_data$latitude))
  
  # Gaode (GCJ02) and Baidu (BD09) should have different transformations
  expect_false(identical(gaode_data$longitude, baidu_data$longitude))
  expect_false(identical(gaode_data$latitude, baidu_data$latitude))
})

test_that("Coordinate system detection works correctly", {
  skip_if_not_installed("R6")
  
  gaode_provider <- GaodeProvider$new()
  gaode_provider$initialize_provider(list(api_key = "test_key"))
  
  baidu_provider <- BaiduProvider$new()
  baidu_provider$initialize_provider(list(api_key = "test_key"))
  
  # Test with coordinates clearly in China
  china_data <- data.frame(
    longitude = c(116.3974, 121.4737),
    latitude = c(39.9093, 31.2304)
  )
  
  gaode_detection <- gaode_provider$detect_coordinate_system(china_data)
  baidu_detection <- baidu_provider$detect_coordinate_system(china_data)
  
  expect_true(gaode_detection %in% c("WGS84", "GCJ02", "BD09"))
  expect_true(baidu_detection %in% c("WGS84", "GCJ02", "BD09"))
  
  # Test with coordinates outside China
  outside_data <- data.frame(
    longitude = c(-74.006, -73.935),
    latitude = c(40.7128, 40.7306)
  )
  
  gaode_detection_outside <- gaode_provider$detect_coordinate_system(outside_data)
  baidu_detection_outside <- baidu_provider$detect_coordinate_system(outside_data)
  
  expect_equal(gaode_detection_outside, "WGS84")
  expect_equal(baidu_detection_outside, "WGS84")
})

test_that("Cross-provider coordinate consistency", {
  skip_if_not_installed("R6")
  
  # Test that both providers handle the same WGS84 input consistently
  wgs84_coords <- c(116.3974, 39.9093)
  
  gaode_provider <- GaodeProvider$new()
  gaode_provider$initialize_provider(list(api_key = "test_key"))
  
  baidu_provider <- BaiduProvider$new()
  baidu_provider$initialize_provider(list(api_key = "test_key"))
  
  # Transform to each provider's native coordinate system
  gcj02_coords <- gaode_provider$transform_coordinates(wgs84_coords, "WGS84", "GCJ02")
  bd09_coords <- baidu_provider$transform_coordinates(wgs84_coords, "WGS84", "BD09")
  
  # Transform back to WGS84
  back_from_gcj02 <- gaode_provider$transform_coordinates(gcj02_coords, "GCJ02", "WGS84")
  back_from_bd09 <- baidu_provider$transform_coordinates(bd09_coords, "BD09", "WGS84")
  
  # Both should return to approximately the same WGS84 coordinates
  tolerance <- 1e-6
  expect_lt(abs(wgs84_coords[1] - back_from_gcj02[1]), tolerance)
  expect_lt(abs(wgs84_coords[2] - back_from_gcj02[2]), tolerance)
  expect_lt(abs(wgs84_coords[1] - back_from_bd09[1]), tolerance)
  expect_lt(abs(wgs84_coords[2] - back_from_bd09[2]), tolerance)
})

test_that("Coordinate transformation performance is acceptable", {
  skip_if_not_installed("R6")
  
  transformer <- CoordinateTransformer$new()
  
  # Generate large dataset for performance testing
  n_points <- 1000
  large_dataset <- matrix(
    runif(n_points * 2, min = c(110, 30), max = c(125, 45)),
    ncol = 2
  )
  
  # Time the transformation
  start_time <- Sys.time()
  transformed <- transformer$transform_batch(large_dataset, "WGS84", "GCJ02")
  end_time <- Sys.time()
  
  transformation_time <- as.numeric(end_time - start_time)
  
  # Should complete within reasonable time (less than 1 second for 1000 points)
  expect_lt(transformation_time, 1.0)
  
  # Verify transformation worked
  expect_equal(dim(transformed), dim(large_dataset))
  expect_false(identical(large_dataset, transformed))
})

test_that("Coordinate validation works correctly", {
  skip_if_not_installed("R6")
  
  transformer <- CoordinateTransformer$new()
  
  # Test accuracy validation
  original <- c(116.3974, 39.9093)
  transformed <- transformer$transform(original, "WGS84", "GCJ02")
  
  # Should validate as accurate within 1 meter
  is_accurate <- transformer$validate_accuracy(original, transformed, "WGS84", "GCJ02", 1.0)
  expect_true(is_accurate)
  
  # Should not validate as accurate within very tight tolerance
  is_very_accurate <- transformer$validate_accuracy(original, transformed, "WGS84", "GCJ02", 1e-10)
  expect_false(is_very_accurate)
  
  # Test with deliberately inaccurate transformation
  inaccurate_transformed <- c(transformed[1] + 0.01, transformed[2] + 0.01) # ~1km offset
  is_inaccurate <- transformer$validate_accuracy(original, inaccurate_transformed, "WGS84", "GCJ02", 1.0)
  expect_false(is_inaccurate)
})