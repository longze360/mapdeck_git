# Simple test for Chinese providers
library(mapdeck)

cat("Testing Chinese Providers (Simple)\n")
cat("===================================\n\n")

# Test basic provider creation
cat("Test 1: Basic Gaode provider\n")
gaode_map <- mapdeck(provider = "gaode")
cat("✓ Gaode provider works\n\n")

cat("Test 2: Basic Baidu provider\n")
baidu_map <- mapdeck(provider = "baidu")
cat("✓ Baidu provider works\n\n")

# Test with styles
cat("Test 3: Gaode with styles\n")
gaode_styles <- get_available_styles("gaode")
cat("Available Gaode styles:", paste(gaode_styles, collapse = ", "), "\n")

cat("Test 4: Baidu with styles\n")
baidu_styles <- get_available_styles("baidu")
cat("Available Baidu styles:", paste(baidu_styles, collapse = ", "), "\n")

cat("\nSimple Chinese providers test completed!\n")