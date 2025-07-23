# Chinese Map Providers Examples (Gaode and Baidu)
# 中国地图提供商示例（高德地图和百度地图）
#
# This file demonstrates how to use Chinese mapping providers with mapdeck,
# including authentication, coordinate system handling, and Chinese features.
# 本文件演示如何在mapdeck中使用中国地图提供商，包括身份验证、坐标系统处理和中文特性。

library(mapdeck)
library(sf)

# ============================================================================
# GAODE MAPS SETUP (高德地图设置)
# ============================================================================

# Authentication / 身份验证
# Get API key from: https://console.amap.com/
# 从以下网址获取API密钥：https://console.amap.com/

# Method 1: Set token globally / 方法1：全局设置令牌
set_token("97caa3ca165b813f72393e4a461b3016", provider = "gaode")

# Method 2: Environment variable / 方法2：环境变量
# Sys.setenv(GAODE_API_KEY = "your_gaode_api_key_here")

# Method 3: Direct parameter / 方法3：直接参数
# map <- mapdeck(provider = "gaode", token = "your_gaode_api_key_here")

# Verify token is set / 验证令牌已设置
mapdeck_tokens()

# ============================================================================
# BASIC GAODE MAPS (基础高德地图)
# ============================================================================

# Create basic Gaode map / 创建基础高德地图
gaode_basic <- mapdeck(
    provider = "gaode",
    location = c(116.3974, 39.9093), # Beijing / 北京
    zoom = 12,
    style = "normal"
)
gaode_basic

# Different Gaode styles / 不同的高德地图样式
gaode_satellite <- mapdeck(
    provider = "gaode",
    location = c(121.4737, 31.2304), # Shanghai / 上海
    zoom = 12,
    style = "satellite"
)

gaode_dark <- mapdeck(
    provider = "gaode",
    location = c(113.2644, 23.1291), # Guangzhou / 广州
    zoom = 12,
    style = "dark"
)

# Get available Gaode styles / 获取可用的高德地图样式
gaode_styles <- get_available_styles("gaode")
print(gaode_styles)

# ============================================================================
# COORDINATE SYSTEM HANDLING (坐标系统处理)
# ============================================================================

# Gaode uses GCJ02 coordinate system (China's encrypted coordinate system)
# 高德地图使用GCJ02坐标系（中国的加密坐标系）
# mapdeck automatically transforms WGS84 to GCJ02
# mapdeck自动将WGS84转换为GCJ02

# Sample Chinese cities data (WGS84 coordinates)
# 中国城市样本数据（WGS84坐标）
chinese_cities <- data.frame(
    city_en = c("Beijing", "Shanghai", "Guangzhou", "Shenzhen", "Chengdu"),
    city_cn = c("北京", "上海", "广州", "深圳", "成都"),
    lon = c(116.3974, 121.4737, 113.2644, 114.0579, 104.0668),
    lat = c(39.9093, 31.2304, 23.1291, 22.5431, 30.5728),
    population = c(21540000, 24870000, 15300000, 12530000, 16330000),
    stringsAsFactors = FALSE
)

# Coordinates are automatically transformed to GCJ02
# 坐标自动转换为GCJ02
# Note: Layer examples require compiled package
if (FALSE) {
  gaode_cities <- mapdeck(
      provider = "gaode",
      location = c(116.3974, 39.9093),
      zoom = 5
  ) %>%
      add_scatterplot(
          data = chinese_cities,
          lon = "lon",
          lat = "lat",
          radius = "population",
          fill_colour = "city_cn",
          tooltip = c("city_cn", "city_en", "population"),
          radius_min_pixels = 10,
          radius_max_pixels = 100
      )
  gaode_cities
}

# ============================================================================
# BAIDU MAPS SETUP (百度地图设置)
# ============================================================================

# Authentication / 身份验证
# Get API key from: https://lbsyun.baidu.com/
# 从以下网址获取API密钥：https://lbsyun.baidu.com/

# Set Baidu API key / 设置百度API密钥
set_token("37492c0ee6f924cb5e934fa08c6b1676", provider = "baidu")

# Or use environment variable / 或使用环境变量
# Sys.setenv(BAIDU_API_KEY = "your_baidu_api_key_here")

# ============================================================================
# BASIC BAIDU MAPS (基础百度地图)
# ============================================================================

# Create basic Baidu map / 创建基础百度地图
baidu_basic <- mapdeck(
    provider = "baidu",
    location = c(116.3974, 39.9093), # Beijing / 北京
    zoom = 12,
    style = "normal"
)
baidu_basic

# Different Baidu styles / 不同的百度地图样式
baidu_satellite <- mapdeck(
    provider = "baidu",
    location = c(121.4737, 31.2304), # Shanghai / 上海
    zoom = 12,
    style = "satellite"
)

baidu_dark <- mapdeck(
    provider = "baidu",
    location = c(113.2644, 23.1291), # Guangzhou / 广州
    zoom = 12,
    style = "dark"
)

# Get available Baidu styles / 获取可用的百度地图样式
baidu_styles <- get_available_styles("baidu")
print(baidu_styles)

# ============================================================================
# BAIDU COORDINATE SYSTEM (百度坐标系统)
# ============================================================================

# Baidu uses BD09 coordinate system (Baidu's proprietary system)
# 百度地图使用BD09坐标系（百度专有系统）
# mapdeck automatically transforms WGS84 to BD09
# mapdeck自动将WGS84转换为BD09

# Same cities data, automatically transformed to BD09
# 相同的城市数据，自动转换为BD09
# Note: Layer examples require compiled package
if (FALSE) {
  baidu_cities <- mapdeck(
      provider = "baidu",
      location = c(116.3974, 39.9093),
      zoom = 5
  ) %>%
      add_scatterplot(
          data = chinese_cities,
          lon = "lon",
          lat = "lat",
          radius = "population",
          fill_colour = "city_cn",
          tooltip = c("city_cn", "city_en", "population"),
          radius_min_pixels = 10,
          radius_max_pixels = 100
      )
  baidu_cities
}

# ============================================================================
# ADVANCED CHINESE MAPPING FEATURES (高级中文地图功能)
# ============================================================================

# Chinese administrative boundaries example / 中国行政边界示例
# (This would use actual Chinese administrative data)
# （这将使用实际的中国行政数据）

if (FALSE) { # Example with hypothetical data / 假设数据示例

    # Chinese provinces data / 中国省份数据
    chinese_provinces <- data.frame(
        province_en = c("Beijing", "Shanghai", "Guangdong", "Sichuan"),
        province_cn = c("北京市", "上海市", "广东省", "四川省"),
        capital_lon = c(116.3974, 121.4737, 113.2644, 104.0668),
        capital_lat = c(39.9093, 31.2304, 23.1291, 30.5728),
        gdp = c(3610, 3870, 10770, 4680), # GDP in billion RMB / GDP（十亿人民币）
        stringsAsFactors = FALSE
    )

    # Gaode map with Chinese provinces / 带有中国省份的高德地图
    gaode_provinces <- mapdeck(
        provider = "gaode",
        location = c(104, 35), # Center of China / 中国中心
        zoom = 4
    ) %>%
        add_scatterplot(
            data = chinese_provinces,
            lon = "capital_lon",
            lat = "capital_lat",
            radius = "gdp",
            fill_colour = "province_cn",
            tooltip = c("province_cn", "province_en", "gdp"),
            radius_min_pixels = 20,
            radius_max_pixels = 80
        ) %>%
        add_text(
            data = chinese_provinces,
            lon = "capital_lon",
            lat = "capital_lat",
            text = "province_cn",
            size = 14,
            colour = "#000000"
        )

    # Baidu version of the same map / 相同地图的百度版本
    baidu_provinces <- mapdeck(
        provider = "baidu",
        location = c(104, 35),
        zoom = 4
    ) %>%
        add_scatterplot(
            data = chinese_provinces,
            lon = "capital_lon",
            lat = "capital_lat",
            radius = "gdp",
            fill_colour = "province_cn",
            tooltip = c("province_cn", "province_en", "gdp"),
            radius_min_pixels = 20,
            radius_max_pixels = 80
        )
}

# ============================================================================
# COORDINATE TRANSFORMATION EXAMPLES (坐标转换示例)
# ============================================================================

# Manual coordinate transformation / 手动坐标转换
wgs84_coords <- data.frame(
    lon = c(116.3974, 121.4737),
    lat = c(39.9093, 31.2304)
)

# Transform to GCJ02 (for Gaode) / 转换为GCJ02（用于高德）
# Note: Coordinate transformation requires compiled package
if (FALSE) {
  gcj02_coords <- transform_coordinates(wgs84_coords, "WGS84", "GCJ02")
  print("GCJ02 coordinates for Gaode:")
  print(gcj02_coords)

  # Transform to BD09 (for Baidu) / 转换为BD09（用于百度）
  bd09_coords <- transform_coordinates(wgs84_coords, "WGS84", "BD09")
  print("BD09 coordinates for Baidu:")
  print(bd09_coords)

  # Detect coordinate system / 检测坐标系统
  detected_crs <- detect_coordinate_system(wgs84_coords)
  print(paste("Detected coordinate system:", detected_crs))
}

# ============================================================================
# PROVIDER COMPARISON (提供商比较)
# ============================================================================

# Create the same visualization with both providers
# 使用两个提供商创建相同的可视化
create_comparison <- function(data, title) {
    # Gaode version / 高德版本
    gaode_map <- mapdeck(
        provider = "gaode",
        location = c(116.3974, 39.9093),
        zoom = 5,
        style = "normal"
    ) %>%
        add_scatterplot(
            data = data,
            lon = "lon",
            lat = "lat",
            radius = "population",
            fill_colour = "city_cn",
            tooltip = c("city_cn", "population")
        )

    # Baidu version / 百度版本
    baidu_map <- mapdeck(
        provider = "baidu",
        location = c(116.3974, 39.9093),
        zoom = 5,
        style = "normal"
    ) %>%
        add_scatterplot(
            data = data,
            lon = "lon",
            lat = "lat",
            radius = "population",
            fill_colour = "city_cn",
            tooltip = c("city_cn", "population")
        )

    return(list(gaode = gaode_map, baidu = baidu_map))
}

# Create comparison maps / 创建比较地图
# Note: Layer examples require compiled package
if (FALSE) {
  comparison_maps <- create_comparison(chinese_cities, "Chinese Cities")
}

# ============================================================================
# CHINESE PROVIDER SWITCHING (中国提供商切换)
# ============================================================================

# Start with Gaode map / 从高德地图开始
# Note: Layer examples require compiled package
if (FALSE) {
  original_gaode <- mapdeck(provider = "gaode") %>%
      add_scatterplot(
          data = chinese_cities,
          lon = "lon",
          lat = "lat",
          radius = "population",
          fill_colour = "city_cn"
      )

  # Switch to Baidu (preserves layers, transforms coordinates)
  # 切换到百度（保留图层，转换坐标）
  switched_baidu <- update_provider(original_gaode, "baidu")

  # Check coordinate transformation compatibility
  # 检查坐标转换兼容性
  compatibility <- check_coordinate_system_compatibility("gaode", "baidu")
  print("Coordinate system compatibility:")
  print(compatibility)
}

# ============================================================================
# CHINESE TEXT AND STYLING (中文文本和样式)
# ============================================================================

# Map with Chinese labels / 带有中文标签的地图
# Note: Layer examples require compiled package
if (FALSE) {
  chinese_labels_map <- mapdeck(
      provider = "gaode",
      location = c(116.3974, 39.9093),
      zoom = 5
  ) %>%
      add_scatterplot(
          data = chinese_cities,
          lon = "lon",
          lat = "lat",
          radius = 80000,
          fill_colour = "#FF6B6B",
          layer_id = "cities"
      ) %>%
      add_text(
          data = chinese_cities,
          lon = "lon",
          lat = "lat",
          text = "city_cn", # Chinese city names / 中文城市名称
          size = 16,
          colour = "#2C3E50",
          layer_id = "labels"
      )
  chinese_labels_map
}

# Custom styling for Chinese maps / 中国地图的自定义样式
# Note: Layer examples require compiled package
if (FALSE) {
  chinese_styled_map <- mapdeck(
      provider = "baidu",
      location = c(104, 35),
      zoom = 4,
      style = "dark"
  ) %>%
      add_polygon(
          data = chinese_cities, # Would use actual polygon data / 将使用实际多边形数据
          fill_colour = "city_cn",
          fill_opacity = 0.6,
          stroke_colour = "#FFD700", # Gold color / 金色
          stroke_width = 2
      )
}

# ============================================================================
# PERFORMANCE OPTIMIZATION FOR CHINESE DATA (中国数据的性能优化)
# ============================================================================

# Large Chinese dataset example / 大型中国数据集示例
large_chinese_data <- data.frame(
    lon = runif(5000, 73, 135), # China longitude range / 中国经度范围
    lat = runif(5000, 18, 54), # China latitude range / 中国纬度范围
    value = rnorm(5000, 100, 20),
    category = sample(c("一类", "二类", "三类"), 5000, replace = TRUE)
)

# Use aggregation for performance / 使用聚合提高性能
# Note: Layer examples require compiled package
if (FALSE) {
  aggregated_gaode <- mapdeck(
      provider = "gaode",
      location = c(104, 35),
      zoom = 4
  ) %>%
      add_screengrid(
          data = large_chinese_data,
          lon = "lon",
          lat = "lat",
          weight = "value",
          cell_size = 50,
          opacity = 0.8
      )
}

# Hexagon aggregation with Baidu / 百度的六边形聚合
# Note: Layer examples require compiled package
if (FALSE) {
  hexagon_baidu <- mapdeck(
      provider = "baidu",
      location = c(104, 35),
      zoom = 4
  ) %>%
      add_hexagon(
          data = large_chinese_data,
          lon = "lon",
          lat = "lat",
          colour_range = c(
              "#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272",
              "#FB6A4A", "#EF3B2C", "#CB181D", "#99000D"
          ),
          elevation_scale = 100
      )
}

# ============================================================================
# TROUBLESHOOTING CHINESE PROVIDERS (中国提供商故障排除)
# ============================================================================

# Check provider availability / 检查提供商可用性
available_providers <- list_available_providers()
cat("Available providers / 可用提供商:\n")
print(available_providers)

# Check if Chinese providers are available / 检查中国提供商是否可用
gaode_available <- "gaode" %in% available_providers
baidu_available <- "baidu" %in% available_providers

cat("Gaode available / 高德可用:", gaode_available, "\n")
cat("Baidu available / 百度可用:", baidu_available, "\n")

# Validate API keys / 验证API密钥
if (gaode_available) {
    gaode_token_valid <- get_token_store()$validate_token("gaode")
    cat("Gaode token valid / 高德令牌有效:", gaode_token_valid, "\n")
}

if (baidu_available) {
    baidu_token_valid <- get_token_store()$validate_token("baidu")
    cat("Baidu token valid / 百度令牌有效:", baidu_token_valid, "\n")
}

# Get provider capabilities / 获取提供商功能
if (gaode_available) {
    gaode_capabilities <- get_provider_capabilities("gaode")
    cat("Gaode capabilities / 高德功能:\n")
    print(gaode_capabilities)
}

if (baidu_available) {
    baidu_capabilities <- get_provider_capabilities("baidu")
    cat("Baidu capabilities / 百度功能:\n")
    print(baidu_capabilities)
}

# Test coordinate transformation accuracy / 测试坐标转换精度
# Note: Coordinate transformation requires compiled package
if (FALSE) {
  test_coords <- data.frame(lon = 116.3974, lat = 39.9093) # Beijing / 北京

  # Transform and back-transform to test accuracy / 转换和反向转换以测试精度
  gcj02_test <- transform_coordinates(test_coords, "WGS84", "GCJ02")
  back_to_wgs84 <- transform_coordinates(gcj02_test, "GCJ02", "WGS84")

  cat("Original WGS84 / 原始WGS84:", test_coords$lon, test_coords$lat, "\n")
  cat("Transformed GCJ02 / 转换后GCJ02:", gcj02_test$lon, gcj02_test$lat, "\n")
  cat("Back to WGS84 / 回到WGS84:", back_to_wgs84$lon, back_to_wgs84$lat, "\n")

  # Calculate transformation accuracy / 计算转换精度
  accuracy <- sqrt((test_coords$lon - back_to_wgs84$lon)^2 +
      (test_coords$lat - back_to_wgs84$lat)^2) * 111000 # meters
  cat("Transformation accuracy / 转换精度:", round(accuracy, 2), "meters / 米\n")
}

# ============================================================================
# CHINESE PROVIDER BEST PRACTICES (中国提供商最佳实践)
# ============================================================================

cat("\nBest Practices for Chinese Providers / 中国提供商最佳实践:\n")
cat("================================================\n")
cat("1. Always use valid API keys / 始终使用有效的API密钥\n")
cat("2. Let mapdeck handle coordinate transformations / 让mapdeck处理坐标转换\n")
cat("3. Use appropriate aggregation for large datasets / 对大数据集使用适当的聚合\n")
cat("4. Consider network connectivity in China / 考虑中国的网络连接\n")
cat("5. Test both providers for your use case / 为您的用例测试两个提供商\n")
cat("6. Use Chinese text encoding properly / 正确使用中文文本编码\n")
cat("7. Respect API rate limits / 尊重API速率限制\n")

# Note: Some advanced examples are commented out due to compilation requirements
# 注意：一些高级示例由于编译要求而被注释掉

print("Chinese providers examples completed! / 中国提供商示例完成！")
