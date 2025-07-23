# Test provider dependencies

test_that("provider dependency functions exist", {
  # Test that all provider dependency functions are defined
  expect_true(exists("provider_dependencies_js"))
  expect_true(exists("leaflet_js"))
  expect_true(exists("leaflet_deckgl_adapter"))
  expect_true(exists("openlayers_js"))
  expect_true(exists("openlayers_deckgl_adapter"))
  expect_true(exists("gaode_adapter"))
  expect_true(exists("baidu_adapter"))
  
  # Test that they return html_dependency objects
  expect_true(inherits(provider_dependencies_js()[[1]], "html_dependency"))
  expect_true(inherits(leaflet_js()[[1]], "html_dependency"))
  expect_true(inherits(leaflet_deckgl_adapter()[[1]], "html_dependency"))
  expect_true(inherits(openlayers_js()[[1]], "html_dependency"))
  expect_true(inherits(openlayers_deckgl_adapter()[[1]], "html_dependency"))
  expect_true(inherits(gaode_adapter()[[1]], "html_dependency"))
  expect_true(inherits(baidu_adapter()[[1]], "html_dependency"))
})

test_that("provider dependencies are included in mapdeck_dependencies", {
  deps <- mapdeck_dependencies()
  
  # Extract dependency names
  dep_names <- sapply(deps, function(x) x$name)
  
  # Check that provider-dependencies is included
  expect_true("provider-dependencies" %in% dep_names)
})

test_that("provider dependency files exist", {
  # Check that the provider-dependencies.js file exists
  expect_true(file.exists(system.file("htmlwidgets/lib/provider-dependencies.js", package = "mapdeck")))
  
  # Check that adapter files exist
  expect_true(file.exists(system.file("htmlwidgets/lib/leaflet-deckgl-adapter.js", package = "mapdeck")))
  expect_true(file.exists(system.file("htmlwidgets/lib/openlayers/openlayers-adapter.js", package = "mapdeck")))
  expect_true(file.exists(system.file("htmlwidgets/lib/gaode/gaode-adapter.js", package = "mapdeck")))
  expect_true(file.exists(system.file("htmlwidgets/lib/baidu/baidu-adapter.js", package = "mapdeck")))
})