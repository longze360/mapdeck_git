# Comprehensive Style Management Tests
# Tests for style management system with full coverage

test_that("StyleResolver initialization and basic functionality", {
  skip_if_not_installed("R6")
  skip_if_not(exists("StyleResolver"), "StyleResolver not available")
  
  resolver <- StyleResolver$new()
  expect_s3_class(resolver, "StyleResolver")
  expect_