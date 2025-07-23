# Chinese Map Providers Examples (Gaode and Baidu) - Simple Version
# 中国地图提供商示例（高德地图和百度地图）- 简化版本
#
# This file demonstrates basic usage of Chinese mapping providers with mapdeck.
# 本文件演示如何在mapdeck中使用中国地图提供商的基本用法。

library(mapdeck)

cat("Chinese Map Providers Examples / 中国地图提供商示例\n")
cat("================================================\n\n")

# ============================================================================
# GAODE MAPS SETUP (高德地图设置)
# ============================================================================

cat("1. Gaode Maps Setup / 高德地图设置\n")
cat("--------------------------------\n")

# Authentication / 身份验证
# Get API key from: https://console.amap.com/
# 从以下网址获取API密钥：https://console.amap.com/

# Method 1: Set token globally / 方法1：全局设置令牌
# set_token("your_gaode_api_key_here", provider = "gaode")

# Method 1: Set token globally / 方法1：全局设置令牌
set_token("97caa3ca165b813f72393e4a461b3016", provider = "gaode")
# Verify token is set / 验证令牌已设置
mapdeck_tokens()


# Create basic Gaode map / 创建基础高德地图
gaode_basic <- mapdeck(
  provider = "gaode",
  location = c(116.3974, 39.9093),  # Beijing / 北京
  zoom = 12,
  style = "normal"
)
cat("✓ Basic Gaode map created / 基础高德地图已创建\n")

# Get available Gaode styles / 获取可用的高德地图样式
gaode_styles <- get_available_styles("gaode")
cat("Available Gaode styles / 可用的高德地图样式:", paste(gaode_styles, collapse = ", "), "\n\n")

# ============================================================================
# BAIDU MAPS SETUP (百度地图设置)
# ============================================================================

cat("2. Baidu Maps Setup / 百度地图设置\n")
cat("--------------------------------\n")

# Authentication / 身份验证
# Get API key from: https://lbsyun.baidu.com/
# 从以下网址获取API密钥：https://lbsyun.baidu.com/

# Set Baidu API key / 设置百度API密钥
# set_token("your_baidu_api_key_here", provider = "baidu")

# Create basic Baidu map / 创建基础百度地图
baidu_basic <- mapdeck(
  provider = "baidu",
  location = c(116.3974, 39.9093),  # Beijing / 北京
  zoom = 12,
  style = "normal"
)
cat("✓ Basic Baidu map created / 基础百度地图已创建\n")

# Get available Baidu styles / 获取可用的百度地图样式
baidu_styles <- get_available_styles("baidu")
cat("Available Baidu styles / 可用的百度地图样式:", paste(baidu_styles, collapse = ", "), "\n\n")

# ============================================================================
# COORDINATE SYSTEM INFORMATION (坐标系统信息)
# ============================================================================

cat("3. Coordinate System Information / 坐标系统信息\n")
cat("--------------------------------------------\n")

cat("Gaode uses GCJ02 coordinate system / 高德地图使用GCJ02坐标系\n")
cat("Baidu uses BD09 coordinate system / 百度地图使用BD09坐标系\n")
cat("mapdeck automatically transforms WGS84 to appropriate system / mapdeck自动将WGS84转换为适当的坐标系\n\n")

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

cat("Sample data created with", nrow(chinese_cities), "Chinese cities / 已创建包含", nrow(chinese_cities), "个中国城市的样本数据\n\n")

# ============================================================================
# DIFFERENT STYLES DEMONSTRATION (不同样式演示)
# ============================================================================

cat("4. Different Styles Demonstration / 不同样式演示\n")
cat("----------------------------------------------\n")

# Gaode with different styles / 不同样式的高德地图
gaode_satellite <- mapdeck(
  provider = "gaode",
  location = c(121.4737, 31.2304),  # Shanghai / 上海
  zoom = 12,
  style = "satellite"
)
cat("✓ Gaode satellite map created / 高德卫星地图已创建\n")

# Baidu with different styles / 不同样式的百度地图
baidu_dark <- mapdeck(
  provider = "baidu",
  location = c(113.2644, 23.1291),  # Guangzhou / 广州
  zoom = 12,
  style = "dark"
)
cat("✓ Baidu dark map created / 百度深色地图已创建\n\n")

# ============================================================================
# PROVIDER COMPARISON (提供商比较)
# ============================================================================

cat("5. Provider Comparison / 提供商比较\n")
cat("----------------------------------\n")

# Create maps with both providers for comparison
# 使用两个提供商创建地图进行比较
gaode_comparison <- mapdeck(
  provider = "gaode",
  location = c(116.3974, 39.9093),
  zoom = 5,
  style = "normal"
)

baidu_comparison <- mapdeck(
  provider = "baidu",
  location = c(116.3974, 39.9093),
  zoom = 5,
  style = "normal"
)

cat("✓ Comparison maps created for both providers / 已为两个提供商创建比较地图\n\n")

# ============================================================================
# PROVIDER AVAILABILITY CHECK (提供商可用性检查)
# ============================================================================

cat("6. Provider Availability Check / 提供商可用性检查\n")
cat("-----------------------------------------------\n")

# Check provider availability / 检查提供商可用性
available_providers <- list_available_providers()
cat("Available providers / 可用提供商:", paste(available_providers, collapse = ", "), "\n")

# Check if Chinese providers are available / 检查中国提供商是否可用
gaode_available <- "gaode" %in% available_providers
baidu_available <- "baidu" %in% available_providers

cat("Gaode available / 高德可用:", gaode_available, "\n")
cat("Baidu available / 百度可用:", baidu_available, "\n\n")

# ============================================================================
# BEST PRACTICES (最佳实践)
# ============================================================================

cat("7. Best Practices for Chinese Providers / 中国提供商最佳实践\n")
cat("======================================================\n")
cat("1. Always use valid API keys / 始终使用有效的API密钥\n")
cat("2. Let mapdeck handle coordinate transformations / 让mapdeck处理坐标转换\n")
cat("3. Use appropriate aggregation for large datasets / 对大数据集使用适当的聚合\n")
cat("4. Consider network connectivity in China / 考虑中国的网络连接\n")
cat("5. Test both providers for your use case / 为您的用例测试两个提供商\n")
cat("6. Use Chinese text encoding properly / 正确使用中文文本编码\n")
cat("7. Respect API rate limits / 尊重API速率限制\n\n")

# ============================================================================
# SUMMARY (总结)
# ============================================================================

cat("Summary / 总结\n")
cat("=============\n")
cat("✓ Gaode Maps (高德地图): Uses GCJ02 coordinate system / 使用GCJ02坐标系\n")
cat("✓ Baidu Maps (百度地图): Uses BD09 coordinate system / 使用BD09坐标系\n")
cat("✓ Both providers work with mapdeck's multi-provider system / 两个提供商都可以与mapdeck的多提供商系统配合使用\n")
cat("✓ Automatic coordinate transformation is handled by mapdeck / mapdeck自动处理坐标转换\n")
cat("✓ Multiple styles available for both providers / 两个提供商都有多种样式可用\n\n")

cat("Chinese providers examples completed! / 中国提供商示例完成！\n")
cat("For more advanced examples with data layers, ensure the package is properly compiled.\n")
cat("要获得更多带有数据图层的高级示例，请确保包已正确编译。\n")