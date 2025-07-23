# Test Provider System
# This script tests the basic provider functionality

library(mapdeck)

cat("Testing Provider System\n")
cat("=======================\n\n")

# Test 1: Basic mapdeck function
cat("Test 1: Basic mapdeck function\n")
map1 <- mapdeck()
cat("✓ Basic mapdeck() works\n\n")

# Test 2: Provider parameter
cat("Test 2: Provider parameter\n")
map2 <- mapdeck(provider = "mapbox")
cat("✓ mapdeck(provider = 'mapbox') works\n")

map3 <- mapdeck(provider = "leaflet")
cat("✓ mapdeck(provider = 'leaflet') works\n\n")

# Test 3: Different providers
cat("Test 3: Different providers\n")
providers <- c("mapbox", "leaflet", "openlayers", "gaode", "baidu")

for (provider in providers) {
  tryCatch({
    map <- mapdeck(provider = provider)
    cat("✓", provider, "provider works\n")
  }, error = function(e) {
    cat("✗", provider, "provider failed:", e$message, "\n")
  })
}

cat("\nProvider system test completed!\n")