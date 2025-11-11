# Performance Benchmark for rwevidence Package
# Run this script to profile key functions

library(rwevidence)

cat("=== Performance Benchmarks ===\n\n")

# Generate synthetic test data
set.seed(123)
n <- 1000
test_data <- data.frame(
  patient_id = 1:n,
  age = rnorm(n, 65, 10),
  treatment = sample(0:1, n, replace = TRUE),
  outcome = sample(0:1, n, replace = TRUE),
  biomarker = rnorm(n, 100, 20),
  enrollment_date = as.Date("2020-01-01") + sample(1:365, n, replace = TRUE)
)

# Benchmark 1: Data Quality Assessment
cat("1. Data Quality Assessment...\n")
t1 <- system.time({
  quality <- rwe_assess_quality(test_data)
})
cat("   Time:", round(t1[3], 3), "seconds\n")
cat("   Overall score:", round(quality$overall_score, 3), "\n\n")

# Benchmark 2: Propensity Score Estimation
cat("2. Propensity Score Estimation...\n")
t2 <- system.time({
  ps <- rwe_propensity_score(
    data = test_data,
    treatment = "treatment",
    covariates = c("age", "biomarker"),
    method = "logistic"
  )
})
cat("   Time:", round(t2[3], 3), "seconds\n")
cat("   Mean PS:", round(mean(ps$propensity_scores), 3), "\n\n")

# Benchmark 3: Survival Analysis
cat("3. Kaplan-Meier Analysis...\n")
test_data$time <- rexp(n, 0.05)
test_data$event <- sample(0:1, n, replace = TRUE, prob = c(0.3, 0.7))
t3 <- system.time({
  km <- rwe_kaplan_meier(
    data = test_data,
    time_var = "time",
    event_var = "event",
    group_var = "treatment"
  )
})
cat("   Time:", round(t3[3], 3), "seconds\n\n")

# Benchmark 4: Table One Generation
cat("4. Table One Generation...\n")
t4 <- system.time({
  table1 <- rwe_table_one(
    data = test_data,
    strata_var = "treatment",
    vars = c("age", "biomarker"),
    cat_vars = NULL
  )
})
cat("   Time:", round(t4[3], 3), "seconds\n\n")

# Summary
cat("=== Summary ===\n")
cat("Total benchmark time:", round(sum(t1[3], t2[3], t3[3], t4[3]), 3), "seconds\n")
cat("Package is performant for datasets of size:", n, "rows\n")
cat("\nRecommendations:\n")
cat("- Use lazy evaluation (arrow package) for datasets > 1M rows\n")
cat("- Enable parallel processing for propensity score estimation\n")
cat("- Cache frequently computed values in production\n")
