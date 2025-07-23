# 按比例抽样示例
# Proportional Sampling Examples
#
# 本文件展示了如何使用按比例抽样功能进行常见的区域空间抽样任务
# This file demonstrates how to use proportional sampling for common
# regional spatial sampling tasks

library(sf)
library(R6)

# 加载必要的抽样功能
# Load required sampling functions
source('R/concurrent-processor.R')
source('R/spatial-sampling-engine.R')
source('R/administrative-sampler.R')
source('R/proportional-sampler.R')

# ============================================================================
# 示例1: 人口比例抽样 (Population Proportional Sampling)
# ============================================================================

cat("=== 示例1: 人口比例抽样 ===\n")

# 创建模拟的行政区域数据
# Create simulated administrative region data
create_example_regions <- function() {
  # 定义5个行政区域的边界坐标
  # Define boundary coordinates for 5 administrative regions
  regions <- list(
    # 城市中心区 (Urban center)
    list(
      name = "城市中心区",
      coords = matrix(c(0, 0, 2, 0, 2, 2, 0, 2, 0, 0), ncol = 2, byrow = TRUE),
      population = 50000,
      cases = 120
    ),
    # 工业区 (Industrial zone)
    list(
      name = "工业区",
      coords = matrix(c(2, 0, 4, 0, 4, 2, 2, 2, 2, 0), ncol = 2, byrow = TRUE),
      population = 30000,
      cases = 80
    ),
    # 住宅区 (Residential area)
    list(
      name = "住宅区",
      coords = matrix(c(0, 2, 2, 2, 2, 4, 0, 4, 0, 2), ncol = 2, byrow = TRUE),
      population = 80000,
      cases = 200
    ),
    # 商业区 (Commercial district)
    list(
      name = "商业区",
      coords = matrix(c(2, 2, 4, 2, 4, 4, 2, 4, 2, 2), ncol = 2, byrow = TRUE),
      population = 25000,
      cases = 60
    ),
    # 郊区 (Suburban area)
    list(
      name = "郊区",
      coords = matrix(c(4, 0, 6, 0, 6, 4, 4, 4, 4, 0), ncol = 2, byrow = TRUE),
      population = 15000,
      cases = 30
    )
  )

  # 转换为sf对象
  # Convert to sf object
  polygons <- lapply(regions, function(r) sf::st_polygon(list(r$coords)))

  admin_data <- data.frame(
    region_name = sapply(regions, function(r) r$name),
    population = sapply(regions, function(r) r$population),
    disease_cases = sapply(regions, function(r) r$cases),
    stringsAsFactors = FALSE
  )

  admin_sf <- sf::st_sf(admin_data, geometry = sf::st_sfc(polygons), crs = 4326)
  return(admin_sf)
}

# 创建示例数据
example_regions <- create_example_regions()

cat("行政区域基本信息:\n")
print(example_regions[, c("region_name", "population", "disease_cases")])

# 1.1 人口比例抽样 - 每1000人抽取1个样本 (ratio = 0.001)
cat("\n1.1 人口比例抽样 (ratio = 0.001, 即 1:1000):\n")
population_samples <- spatial_sample_population(
  example_regions,
  population_column = "population",
  ratio = 0.001,
  min_samples = 1,
  max_samples = 100,
  seed = 2024
)

# 显示结果
pop_counts <- table(population_samples$region_name)
cat("各区域样本数:\n")
for (region in names(pop_counts)) {
  pop <- example_regions$population[example_regions$region_name == region]
  expected <- round(pop * 0.001)
  actual <- pop_counts[[region]]
  cat(sprintf("  %s: %d个样本 (人口: %d, 预期: %d)\n",
              region, actual, pop, expected))
}

# ============================================================================
# 示例2: 病例比例抽样 (Case Proportional Sampling)
# ============================================================================

cat("\n=== 示例2: 病例比例抽样 ===\n")

# 2.1 疾病病例抽样 - 每10个病例抽取1个样本 (ratio = 0.1)
cat("\n2.1 疾病病例抽样 (ratio = 0.1, 即 1:10):\n")
case_samples <- spatial_sample_cases(
  example_regions,
  case_column = "disease_cases",
  ratio = 0.1,
  min_samples = 1,
  max_samples = 50,
  seed = 2024
)

# 显示结果
case_counts <- table(case_samples$region_name)
cat("各区域样本数:\n")
for (region in names(case_counts)) {
  cases <- example_regions$disease_cases[example_regions$region_name == region]
  expected <- round(cases * 0.1)
  actual <- case_counts[[region]]
  cat(sprintf("  %s: %d个样本 (病例: %d, 预期: %d)\n",
              region, actual, cases, expected))
}

# ============================================================================
# 示例3: 不同比例的对比 (Comparison of Different Ratios)
# ============================================================================

cat("\n=== 示例3: 不同比例的对比 ===\n")

ratios <- c(0.002, 0.001, 0.0005, 0.0002)  # 对应 1:500, 1:1000, 1:2000, 1:5000
ratio_labels <- c("1:500", "1:1000", "1:2000", "1:5000")
cat("人口抽样不同比例对比:\n")
cat(sprintf("%-12s", "区域名称"))
for (label in ratio_labels) {
  cat(sprintf("%-8s", label))
}
cat("\n")

for (i in seq_len(nrow(example_regions))) {
  region <- example_regions$region_name[i]
  population <- example_regions$population[i]

  cat(sprintf("%-12s", substr(region, 1, 10)))

  for (j in seq_along(ratios)) {
    samples <- spatial_sample_population(
      example_regions[i, ],
      population_column = "population",
      ratio = ratios[j],
      seed = 2024
    )
    cat(sprintf("%-8d", nrow(samples)))
  }
  cat(sprintf(" (人口: %d)\n", population))
}

# ============================================================================
# 示例4: 约束条件的使用 (Using Constraints)
# ============================================================================

cat("\n=== 示例4: 约束条件的使用 ===\n")

# 4.1 设置最小最大样本数约束
cat("\n4.1 带约束的人口抽样:\n")
constrained_samples <- spatial_sample_population(
  example_regions,
  population_column = "population",
  ratio = 0.0001,  # 很小的比例 (1:10000)
  min_samples = 3,           # 每个区域至少3个样本
  max_samples = 20,          # 每个区域最多20个样本
  seed = 2024
)

constrained_counts <- table(constrained_samples$region_name)
cat("约束抽样结果 (min=3, max=20):\n")
for (region in names(constrained_counts)) {
  population <- example_regions$population[example_regions$region_name == region]
  raw_expected <- population * 0.0001
  actual <- constrained_counts[[region]]
  cat(sprintf("  %s: %d个样本 (原始期望: %.1f, 约束后: %d)\n",
              region, actual, raw_expected, actual))
}

# ============================================================================
# 示例5: 抽样质量评估 (Sampling Quality Assessment)
# ============================================================================

cat("\n=== 示例5: 抽样质量评估 ===\n")

# 5.1 获取抽样摘要统计
sampler <- ProportionalRegionalSampler$new()
sampler$initialize()

summary_stats <- sampler$get_sampling_summary(population_samples)

cat("人口抽样摘要统计:\n")
cat(sprintf("  总样本数: %d\n", summary_stats$total_samples))
cat(sprintf("  抽样区域数: %d\n", summary_stats$regions_sampled))
cat(sprintf("  抽样比例: %.4f\n", summary_stats$sampling_ratio))

if (!is.null(summary_stats$efficiency)) {
  cat("抽样效率:\n")
  cat(sprintf("  总人口: %d\n", summary_stats$efficiency$total_variable_value))
  cat(sprintf("  预期样本数: %d\n", summary_stats$efficiency$total_expected_samples))
  cat(sprintf("  实际样本数: %d\n", summary_stats$efficiency$actual_samples))
  cat(sprintf("  抽样效率: %.2f%%\n",
              summary_stats$efficiency$sampling_efficiency * 100))
}

# 5.2 边界准确性检查
cat("\n5.2 边界准确性检查:\n")
boundary_violations <- 0
total_points <- nrow(population_samples)

for (i in seq_len(nrow(population_samples))) {
  point <- population_samples[i, ]
  region_name <- point$region_name

  # 找到对应的区域多边形
  region_polygon <- example_regions[example_regions$region_name == region_name, ]

  # 检查点是否在多边形内
  within_result <- sf::st_within(point, region_polygon, sparse = FALSE)
  if (!any(within_result)) {
    boundary_violations <- boundary_violations + 1
  }
}

cat(sprintf("  总测试点数: %d\n", total_points))
cat(sprintf("  边界违规数: %d\n", boundary_violations))
cat(sprintf("  边界准确率: %.1f%%\n",
            (total_points - boundary_violations) / total_points * 100))

# ============================================================================
# 示例6: 实际应用场景 (Real-world Application Scenarios)
# ============================================================================

cat("\n=== 示例6: 实际应用场景 ===\n")

# 6.1 流行病学调查抽样
cat("\n6.1 流行病学调查抽样场景:\n")
cat("场景: 某地区爆发传染病，需要按病例数比例进行流行病学调查\n")

epi_samples <- spatial_sample_cases(
  example_regions,
  case_column = "disease_cases",
  ratio = 0.2,  # 每5个病例调查1个 (1:5)
  min_samples = 2,       # 每个区域至少调查2个
  seed = 2024
)

epi_counts <- table(epi_samples$region_name)
cat("流行病学调查抽样结果:\n")
total_cases <- sum(example_regions$disease_cases)
total_samples <- sum(epi_counts)
cat(sprintf("  总病例数: %d\n", total_cases))
cat(sprintf("  总调查样本: %d\n", total_samples))
cat(sprintf("  调查覆盖率: %.1f%%\n", total_samples / total_cases * 5 * 100))

for (region in names(epi_counts)) {
  cases <- example_regions$disease_cases[example_regions$region_name == region]
  samples <- epi_counts[[region]]
  coverage <- samples / cases * 5 * 100
  cat(sprintf("  %s: %d样本/%d病例 (覆盖率: %.1f%%)\n",
              region, samples, cases, coverage))
}

# 6.2 人口普查抽样
cat("\n6.2 人口普查抽样场景:\n")
cat("场景: 进行人口普查前的预调查，按人口比例抽样\n")

census_samples <- spatial_sample_population(
  example_regions,
  population_column = "population",
  ratio = 0.0005,  # 每2000人抽查1个 (1:2000)
  min_samples = 2,
  max_samples = 50,
  seed = 2024
)

census_counts <- table(census_samples$region_name)
cat("人口普查预调查抽样结果:\n")
total_population <- sum(example_regions$population)
total_census_samples <- sum(census_counts)
cat(sprintf("  总人口: %d\n", total_population))
cat(sprintf("  总调查样本: %d\n", total_census_samples))
cat(sprintf("  抽样比例: 1:%.0f\n", total_population / total_census_samples))

for (region in names(census_counts)) {
  population <- example_regions$population[example_regions$region_name == region]
  samples <- census_counts[[region]]
  ratio_actual <- population / samples
  cat(sprintf("  %s: %d样本/%d人口 (比例: 1:%.0f)\n",
              region, samples, population, ratio_actual))
}

# ============================================================================
# 示例7: 高级比例抽样 (Advanced Ratio Sampling)
# ============================================================================

cat("\n=== 示例7: 高级比例抽样 ===\n")

# 7.1 高比例抽样 (2:1 比例)
cat("\n7.1 高比例抽样 (ratio = 2.0, 即 2:1):\n")
high_ratio_samples <- spatial_sample_proportional(
  example_regions,
  variable_column = "disease_cases",
  ratio = 2.0,  # 每个病例抽取2个样本
  max_samples = 50,  # 限制最大样本数
  seed = 2024
)

high_ratio_counts <- table(high_ratio_samples$region_name)
cat("高比例抽样结果:\n")
for (region in names(high_ratio_counts)) {
  cases <- example_regions$disease_cases[example_regions$region_name == region]
  samples <- high_ratio_counts[[region]]
  expected <- min(50, cases * 2)  # 考虑最大样本数限制
  cat(sprintf("  %s: %d样本/%d病例 (预期: %d)\n",
              region, samples, cases, expected))
}

# 7.2 精确小比例抽样
cat("\n7.2 精确小比例抽样 (ratio = 0.005, 即 1:200):\n")
precise_samples <- spatial_sample_proportional(
  example_regions,
  variable_column = "population",
  ratio = 0.005,  # 每200人抽取1个样本
  min_samples = 1,
  seed = 2024
)

precise_counts <- table(precise_samples$region_name)
cat("精确小比例抽样结果:\n")
for (region in names(precise_counts)) {
  population <- example_regions$population[example_regions$region_name == region]
  samples <- precise_counts[[region]]
  expected <- round(population * 0.005)
  actual_ratio <- population / samples
  cat(sprintf("  %s: %d样本/%d人口 (预期: %d, 实际比例: 1:%.0f)\n",
              region, samples, population, expected, actual_ratio))
}

cat("\n=== 所有示例完成 ===\n")
cat("提示: 这些示例展示了使用数值比例参数进行按比例抽样的灵活性和准确性\n")
cat("Tip: These examples demonstrate the flexibility and accuracy of proportional sampling using numeric ratio parameters\n")

# ============================================================================
# 比例参数说明 (Ratio Parameter Explanation)
# ============================================================================

cat("\n=== 比例参数说明 ===\n")
cat("数值比例参数对应关系:\n")
cat("  ratio = 1.0   → 1:1   (每1个单位抽取1个样本)\n")
cat("  ratio = 0.5   → 1:2   (每2个单位抽取1个样本)\n")
cat("  ratio = 0.1   → 1:10  (每10个单位抽取1个样本)\n")
cat("  ratio = 0.01  → 1:100 (每100个单位抽取1个样本)\n")
cat("  ratio = 0.001 → 1:1000(每1000个单位抽取1个样本)\n")
cat("  ratio = 2.0   → 2:1   (每1个单位抽取2个样本)\n")
cat("  ratio = 0.2   → 1:5   (每5个单位抽取1个样本)\n")
cat("\n使用数值比例参数更直观，便于计算和理解！\n")
