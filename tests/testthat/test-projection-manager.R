test_that("ProjectionManager initialization works correctly", {
  proj_manager <- ProjectionManager$new()
  
  expect_s3_class(proj_manager, "ProjectionManager")
  expect_true(length(proj_manager$get_supported_providers()) > 0)
})

test_that("ProjectionManager returns correct provider projections", {
  proj_manager <- ProjectionManager$new()
  
  expect_equal(proj_manager$get_provider_projection("mapbox"), "WGS84")
  expect_equal(proj_manager$get_provider_projection("leaflet"), "WGS84")
  expect_equal(proj_manager$get_provider_projection("openlayers"), "WGS84")
  expect_equal(proj_manager$get_provider_projection("gaode"), "GCJ02")
  expect_equal(proj_manager$get_provider_projection("baidu"), "BD09")
})

test_that("ProjectionManager handles case insensitive provider names", {
  proj_manager <- ProjectionManager$new()
  
  expect_equal(proj_manager$get_provider_projection("MAPBOX"), "WGS84")
  expect_equal(proj_manager$get_provider_projection("Gaode"), "GCJ02")
  expect_equal(proj_manager$get_provider_projection("BAIDU"), "BD09")
})

test_that("ProjectionManager validates unknown providers", {
  proj_manager <- ProjectionManager$new()
  
  expect_error(proj_manager$get_provider_projection("unknown_provider"))
})

test_that("ProjectionManager gets all provider projections", {
  proj_manager <- ProjectionManager$new()
  
  all_projections <- proj_manager$get_all_provider_projections()
  
  expect_true(is.list(all_projections))
  expect_true("mapbox" %in% names(all_projections))
  expect_true("gaode" %in% names(all_projections))
  expect_true("baidu" %in% names(all_projections))
  expect_equal(all_projections$mapbox, "WGS84")
  expect_equal(all_projections$gaode, "GCJ02")
  expect_equal(all_projections$baidu, "BD09")
})

test_that("ProjectionManager prepares data for WGS84 providers correctly", {
  proj_manager <- ProjectionManager$new()
  
  # Test data in WGS84
  data <- data.frame(
    lon = c(116.3974, 121.4737),
    lat = c(39.9093, 31.2304),
    value = c(1, 2)
  )
  
  # Prepare for Mapbox (WGS84) - should be unchanged
  mapbox_data <- proj_manager$prepare_data_for_provider(data, "mapbox")
  expect_equal(data, mapbox_data)
  
  # Prepare for Leaflet (WGS84) - should be unchanged
  leaflet_data <- proj_manager$prepare_data_for_provider(data, "leaflet")
  expect_equal(data, leaflet_data)
})

test_that("ProjectionManager prepares data for Chinese providers correctly", {
  proj_manager <- ProjectionManager$new()
  
  # Test data in WGS84
  data <- data.frame(
    lon = c(116.3974, 121.4737),
    lat = c(39.9093, 31.2304),
    value = c(1, 2)
  )
  
  # Prepare for Gaode (GCJ02) - coordinates should be transformed
  gaode_data <- proj_manager$prepare_data_for_provider(data, "gaode")
  expect_false(identical(data$lon, gaode_data$lon))
  expect_false(identical(data$lat, gaode_data$lat))
  expect_equal(data$value, gaode_data$value)  # Other columns unchanged
  
  # Prepare for Baidu (BD09) - coordinates should be transformed
  baidu_data <- proj_manager$prepare_data_for_provider(data, "baidu")
  expect_false(identical(data$lon, baidu_data$lon))
  expect_false(identical(data$lat, baidu_data$lat))
  expect_equal(data$value, baidu_data$value)  # Other columns unchanged
})

test_that("ProjectionManager handles custom column names", {
  proj_manager <- ProjectionManager$new()
  
  # Test data with custom column names
  data <- data.frame(
    longitude = c(116.3974, 121.4737),
    latitude = c(39.9093, 31.2304),
    value = c(1, 2)
  )
  
  # Should auto-detect longitude/latitude columns
  gaode_data <- proj_manager$prepare_data_for_provider(data, "gaode")
  expect_false(identical(data$longitude, gaode_data$longitude))
  expect_false(identical(data$latitude, gaode_data$latitude))
  
  # Test with explicit column specification
  gaode_data_explicit <- proj_manager$prepare_data_for_provider(
    data, "gaode", 
    lon_col = "longitude", 
    lat_col = "latitude"
  )
  expect_equal(gaode_data, gaode_data_explicit)
})

test_that("ProjectionManager handles matrix data", {
  proj_manager <- ProjectionManager$new()
  
  # Test matrix data
  data_matrix <- matrix(c(
    116.3974, 39.9093,
    121.4737, 31.2304
  ), ncol = 2, byrow = TRUE)
  
  # Prepare for Gaode
  gaode_matrix <- proj_manager$prepare_data_for_provider(data_matrix, "gaode")
  expect_true(is.matrix(gaode_matrix))
  expect_equal(dim(gaode_matrix), dim(data_matrix))
  expect_false(identical(data_matrix, gaode_matrix))
})

test_that("ProjectionManager transforms between providers correctly", {
  proj_manager <- ProjectionManager$new()
  
  coords <- c(116.3974, 39.9093)
  
  # Transform from Mapbox (WGS84) to Gaode (GCJ02)
  gaode_coords <- proj_manager$transform_between_providers(coords, "mapbox", "gaode")
  expect_false(identical(coords, gaode_coords))
  
  # Transform from Gaode (GCJ02) to Baidu (BD09)
  baidu_coords <- proj_manager$transform_between_providers(gaode_coords, "gaode", "baidu")
  expect_false(identical(gaode_coords, baidu_coords))
  
  # Transform back from Baidu to Mapbox
  back_to_mapbox <- proj_manager$transform_between_providers(baidu_coords, "baidu", "mapbox")
  
  # Should be close to original (within 1 meter tolerance)
  transformer <- CoordinateTransformer$new()
  expect_true(transformer$validate_accuracy(coords, back_to_mapbox, "WGS84", "WGS84", 1.0))
})

test_that("ProjectionManager checks transformation requirements correctly", {
  proj_manager <- ProjectionManager$new()
  
  expect_false(proj_manager$requires_transformation("mapbox"))
  expect_false(proj_manager$requires_transformation("leaflet"))
  expect_false(proj_manager$requires_transformation("openlayers"))
  expect_true(proj_manager$requires_transformation("gaode"))
  expect_true(proj_manager$requires_transformation("baidu"))
})

test_that("ProjectionManager validates coordinates correctly", {
  proj_manager <- ProjectionManager$new()
  
  # Valid WGS84 coordinates
  valid_wgs84 <- c(116.3974, 39.9093)
  expect_true(proj_manager$validate_coordinates_for_provider(valid_wgs84, "mapbox"))
  
  # Invalid WGS84 coordinates (out of bounds)
  invalid_wgs84 <- c(200, 100)  # Invalid longitude/latitude
  expect_false(proj_manager$validate_coordinates_for_provider(invalid_wgs84, "mapbox"))
  
  # Test with Chinese coordinate systems
  # Transform valid WGS84 to GCJ02 and validate
  transformer <- CoordinateTransformer$new()
  gcj02_coords <- transformer$transform(valid_wgs84, "WGS84", "GCJ02")
  expect_true(proj_manager$validate_coordinates_for_provider(gcj02_coords, "gaode"))
})

test_that("ProjectionManager handles missing coordinate columns gracefully", {
  proj_manager <- ProjectionManager$new()
  
  # Data without recognizable coordinate columns
  data <- data.frame(
    x = c(1, 2),
    y = c(3, 4),
    value = c(5, 6)
  )
  
  # Should fall back to first two numeric columns
  result <- proj_manager$prepare_data_for_provider(data, "gaode")
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 3)
})

test_that("ProjectionManager validates data format", {
  proj_manager <- ProjectionManager$new()
  
  # Invalid data types
  expect_error(proj_manager$prepare_data_for_provider("invalid", "mapbox"))
  expect_error(proj_manager$prepare_data_for_provider(list(a = 1), "mapbox"))
  
  # Data with insufficient columns
  insufficient_data <- data.frame(x = c(1, 2))
  expect_error(proj_manager$prepare_data_for_provider(insufficient_data, "mapbox"))
})

test_that("Convenience functions work correctly", {
  # Test get_provider_crs function
  expect_equal(get_provider_crs("mapbox"), "WGS84")
  expect_equal(get_provider_crs("gaode"), "GCJ02")
  expect_equal(get_provider_crs("baidu"), "BD09")
  
  # Test prepare_data_for_provider function
  data <- data.frame(lon = 116.3974, lat = 39.9093)
  result <- prepare_data_for_provider(data, "gaode")
  expect_true(is.data.frame(result))
  expect_false(identical(data$lon, result$lon))
})

test_that("ProjectionManager handles edge cases", {
  proj_manager <- ProjectionManager$new()
  
  # Empty data frame
  empty_data <- data.frame(lon = numeric(0), lat = numeric(0))
  result <- proj_manager$prepare_data_for_provider(empty_data, "gaode")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), names(empty_data))
  
  # Single row data
  single_row <- data.frame(lon = 116.3974, lat = 39.9093)
  result_single <- proj_manager$prepare_data_for_provider(single_row, "gaode")
  expect_equal(nrow(result_single), 1)
  expect_false(identical(single_row$lon, result_single$lon))
})

test_that("ProjectionManager preserves data structure", {
  proj_manager <- ProjectionManager$new()
  
  # Data with various column types
  complex_data <- data.frame(
    lon = c(116.3974, 121.4737),
    lat = c(39.9093, 31.2304),
    name = c("Beijing", "Shanghai"),
    value = c(100, 200),
    flag = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  
  result <- proj_manager$prepare_data_for_provider(complex_data, "gaode")
  
  # Check that non-coordinate columns are preserved
  expect_equal(result$name, complex_data$name)
  expect_equal(result$value, complex_data$value)
  expect_equal(result$flag, complex_data$flag)
  
  # Check that coordinate columns are transformed
  expect_false(identical(result$lon, complex_data$lon))
  expect_false(identical(result$lat, complex_data$lat))
})