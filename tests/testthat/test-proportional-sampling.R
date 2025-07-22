test_that("ProportionalRegionalSampler can be created and initialized", {
  sampler <- ProportionalRegionalSampler$new()
  expect_s3_class(sampler, "ProportionalRegionalSampler")
  
  sampler$initialize()
  expect_s3_class(sampler$base_sampler, "AdministrativeSampler")
  expect_equal(sampler$sampling_ratio, 1.0)
})

test_that("numeric ratio validation works correctly", {
  sampler <- ProportionalRegionalSampler$new()
  sampler$initialize()
  admin_data <- create_test_admin_polygons_with_variables()
  
  # Test valid numeric ratios
  expect_no_error(
    sampler$sample_by_ratio(
      admin_data$polygons,
      "population",
      ratio = 1.0
    )
  )
  
  expect_no_error(
    sampler$sample_by_ratio(
      admin_data$polygons,
      "population", 
      ratio = 0.1
    )
  )
  
  expect_no_error(
    sampler$sample_by_ratio(
      admin_data$polygons,
      "population",
      ratio = 2.0
    )
  )
  
  # Test invalid ratios
  expect_error(
    sampler$sample_by_ratio(
      admin_data$polygons,
      "population",
      ratio = -0.1
    ),
    "ratio must be a positive numeric value"
  )
  
  expect_error(
    sampler$sample_by_ratio(
      admin_data$polygons,
      "population",
      ratio = 0
    ),
    "ratio must be a positive numeric value"
  )
})

test_that("spatial_sample_proportional works with different ratios", {
  # Create test data
  admin_data <- create_test_admin_polygons_with_variables()
  
  # Test 0.1 ratio (1:10)
  samples_0_1 <- spatial_sample_proportional(
    admin_data$polygons,
    variable_column = "population",
    ratio = 0.1,
    seed = 123
  )
  
  expect_s3_class(samples_0_1, "sf")
  expect_true("admin_id" %in% names(samples_0_1))
  expect_true("sampling_ratio" %in% names(samples_0_1))
  expect_equal(unique(samples_0_1$sampling_ratio), 0.1)
  
  # Test 0.01 ratio (1:100)
  samples_0_01 <- spatial_sample_proportional(
    admin_data$polygons,
    variable_column = "population",
    ratio = 0.01,
    seed = 456
  )
  
  expect_s3_class(samples_0_01, "sf")
  expect_lt(nrow(samples_0_01), nrow(samples_0_1))  # Should have fewer samples
})

test_that("population proportional sampling works correctly", {
  admin_data <- create_test_admin_polygons_with_variables()
  
  # Test population sampling
  pop_samples <- spatial_sample_population(
    admin_data$polygons,
    population_column = "population",
    ratio = 0.001,
    seed = 789
  )
  
  expect_s3_class(pop_samples, "sf")
  expect_true("sampling_type" %in% names(pop_samples))
  expect_equal(unique(pop_samples$sampling_type), "population_proportional")
  
  # Check sample counts are proportional to population
  sample_counts <- table(pop_samples$admin_id)
  populations <- setNames(admin_data$polygons$population, admin_data$polygons$admin_id)
  
  # Should be roughly proportional (allowing for rounding)
  for (admin_id in names(sample_counts)) {
    expected <- round(populations[[admin_id]] / 1000)
    actual <- sample_counts[[admin_id]]
    expect_equal(actual, max(1, expected))  # At least 1 sample (min_samples default)
  }
})

test_that("case proportional sampling works correctly", {
  admin_data <- create_test_admin_polygons_with_variables()
  
  # Test case sampling
  case_samples <- spatial_sample_cases(
    admin_data$polygons,
    case_column = "cases",
    ratio = 0.2,
    seed = 999
  )
  
  expect_s3_class(case_samples, "sf")
  expect_true("sampling_type" %in% names(case_samples))
  expect_equal(unique(case_samples$sampling_type), "case_proportional")
  
  # Check sample counts are proportional to cases
  sample_counts <- table(case_samples$admin_id)
  cases <- setNames(admin_data$polygons$cases, admin_data$polygons$admin_id)
  
  # Should be roughly proportional (allowing for rounding)
  for (admin_id in names(sample_counts)) {
    expected <- round(cases[[admin_id]] / 5)
    actual <- sample_counts[[admin_id]]
    expect_equal(actual, max(1, expected))  # At least 1 sample (min_samples default)
  }
})

test_that("min and max sample constraints work correctly", {
  admin_data <- create_test_admin_polygons_with_variables()
  
  # Test with constraints
  constrained_samples <- spatial_sample_proportional(
    admin_data$polygons,
    variable_column = "cases",
    ratio = 1.0,  # Would normally give many samples
    min_samples = 2,
    max_samples = 5,
    seed = 111
  )
  
  expect_s3_class(constrained_samples, "sf")
  
  # Check constraints are respected
  sample_counts <- table(constrained_samples$admin_id)
  expect_true(all(sample_counts >= 2))  # Min constraint
  expect_true(all(sample_counts <= 5))  # Max constraint
})

test_that("sampling summary statistics work correctly", {
  admin_data <- create_test_admin_polygons_with_variables()
  
  samples <- spatial_sample_proportional(
    admin_data$polygons,
    variable_column = "population",
    ratio = 0.01,
    seed = 222
  )
  
  sampler <- ProportionalRegionalSampler$new()
  sampler$initialize()
  
  summary_stats <- sampler$get_sampling_summary(samples)
  
  expect_true(is.list(summary_stats))
  expect_true("total_samples" %in% names(summary_stats))
  expect_true("regions_sampled" %in% names(summary_stats))
  expect_true("sampling_ratio" %in% names(summary_stats))
  
  expect_equal(summary_stats$total_samples, nrow(samples))
  expect_equal(summary_stats$regions_sampled, length(unique(samples$admin_id)))
  expect_equal(summary_stats$sampling_ratio, 0.01)
})

test_that("error handling works correctly for proportional sampling", {
  admin_data <- create_test_admin_polygons_with_variables()
  
  # Test invalid variable column
  expect_error(
    spatial_sample_proportional(
      admin_data$polygons,
      variable_column = "nonexistent",
      ratio = 0.1
    ),
    "variable_column must be a valid column"
  )
  
  # Test negative sampling ratio
  expect_error(
    spatial_sample_proportional(
      admin_data$polygons,
      variable_column = "population",
      ratio = -0.1
    ),
    "ratio must be a positive numeric value"
  )
  
  # Test zero ratio
  expect_error(
    spatial_sample_proportional(
      admin_data$polygons,
      variable_column = "population",
      ratio = 0
    ),
    "ratio must be a positive numeric value"
  )
  
  # Test invalid min/max samples
  expect_error(
    spatial_sample_proportional(
      admin_data$polygons,
      variable_column = "population",
      ratio = 0.1,
      min_samples = -1
    ),
    "min_samples must be a non-negative numeric value"
  )
  
  expect_error(
    spatial_sample_proportional(
      admin_data$polygons,
      variable_column = "population", 
      ratio = 0.1,
      min_samples = 10,
      max_samples = 5
    ),
    "max_samples must be numeric and >= min_samples"
  )
})

test_that("proportional sampling maintains boundary accuracy", {
  admin_data <- create_test_admin_polygons_with_variables()
  
  samples <- spatial_sample_proportional(
    admin_data$polygons,
    variable_column = "population",
    ratio = 0.02,
    seed = 333
  )
  
  # Check that all points are within their respective polygons
  boundary_violations <- 0
  for (i in seq_len(nrow(samples))) {
    point <- samples[i, ]
    admin_id <- point$admin_id
    
    # Find corresponding polygon
    polygon <- admin_data$polygons[admin_data$polygons$admin_id == admin_id, ]
    
    # Check if point is within polygon
    within_result <- sf::st_within(point, polygon, sparse = FALSE)
    if (!any(within_result)) {
      boundary_violations <- boundary_violations + 1
    }
  }
  
  expect_equal(boundary_violations, 0)
})

test_that("proportional sampling is reproducible with seeds", {
  admin_data <- create_test_admin_polygons_with_variables()
  
  # Generate samples with same seed
  samples1 <- spatial_sample_proportional(
    admin_data$polygons,
    variable_column = "population",
    ratio = 0.01,
    seed = 444
  )
  
  samples2 <- spatial_sample_proportional(
    admin_data$polygons,
    variable_column = "population",
    ratio = 0.01,
    seed = 444
  )
  
  # Should have same number of samples per unit
  counts1 <- table(samples1$admin_id)
  counts2 <- table(samples2$admin_id)
  expect_equal(counts1, counts2)
})

test_that("proportional sampling handles zero values correctly", {
  # Create test data with some zero values
  coords_list <- list(
    matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE),
    matrix(c(1, 0, 3, 0, 3, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE)
  )
  
  polygons <- lapply(coords_list, function(x) sf::st_polygon(list(x)))
  
  admin_data <- data.frame(
    admin_id = c("A", "B"),
    population = c(0, 1000),  # One region has zero population
    cases = c(5, 0)           # One region has zero cases
  )
  
  admin_sf <- sf::st_sf(admin_data, geometry = sf::st_sfc(polygons), crs = 4326)
  
  # Test with zero population
  pop_samples <- spatial_sample_proportional(
    admin_sf,
    variable_column = "population",
    ratio = 0.01,
    min_samples = 1,
    seed = 555
  )
  
  expect_s3_class(pop_samples, "sf")
  sample_counts <- table(pop_samples$admin_id)
  
  # Region A should get min_samples (1) despite zero population
  expect_equal(sample_counts[["A"]], 1)
  # Region B should get samples based on population
  expect_equal(sample_counts[["B"]], round(1000 / 100))
})

# Helper function to create test administrative polygons with variables
create_test_admin_polygons_with_variables <- function() {
  # Create three simple rectangular polygons
  coords_list <- list(
    # Polygon A: 1x1 square
    matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE),
    # Polygon B: 2x1 rectangle (larger area)
    matrix(c(1, 0, 3, 0, 3, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE),
    # Polygon C: 1x1.5 rectangle
    matrix(c(0, 1, 2, 1, 2, 2.5, 0, 2.5, 0, 1), ncol = 2, byrow = TRUE)
  )

  polygons <- lapply(coords_list, function(x) sf::st_polygon(list(x)))

  admin_data <- data.frame(
    admin_id = c("A", "B", "C"),
    population = c(1000, 5000, 2000),  # Population data
    cases = c(10, 50, 20),             # Case data
    area = c(1, 2, 1.5)                # Areas for reference
  )

  admin_sf <- sf::st_sf(admin_data, geometry = sf::st_sfc(polygons), crs = 4326)

  list(
    polygons = admin_sf,
    coords_list = coords_list
  )
}