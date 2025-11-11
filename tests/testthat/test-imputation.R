# Tests for imputation.R

library(testthat)
library(rwevidence)

test_that("rwe_impute works with mean imputation", {
  set.seed(123)
  n <- 100
  test_data <- data.frame(
    id = 1:n,
    age = rnorm(n, 60, 10),
    bmi = rnorm(n, 25, 5),
    lab_value = rnorm(n, 100, 20)
  )

  # Introduce missing values
  test_data$age[sample(n, 10)] <- NA
  test_data$bmi[sample(n, 15)] <- NA

  imputed <- rwe_impute(
    data = test_data,
    method = "mean",
    seed = 123
  )

  expect_s3_class(imputed, "rwe_imputation")
  expect_true("imputed_data" %in% names(imputed))
  expect_equal(nrow(imputed$imputed_data), n)

  # Check no missing values in imputed variables
  expect_equal(sum(is.na(imputed$imputed_data$age)), 0)
  expect_equal(sum(is.na(imputed$imputed_data$bmi)), 0)

  # Check imputed values are reasonable (should be close to mean)
  original_mean_age <- mean(test_data$age, na.rm = TRUE)
  imputed_age_values <- imputed$imputed_data$age[is.na(test_data$age)]
  expect_true(all(abs(imputed_age_values - original_mean_age) < 0.01))
})


test_that("rwe_impute works with median imputation", {
  set.seed(456)
  n <- 100
  test_data <- data.frame(
    id = 1:n,
    age = rnorm(n, 60, 10),
    weight = rnorm(n, 70, 15)
  )

  # Introduce missing values
  test_data$age[sample(n, 12)] <- NA

  imputed <- rwe_impute(
    data = test_data,
    method = "median",
    vars = "age",
    seed = 456
  )

  expect_s3_class(imputed, "rwe_imputation")
  expect_equal(sum(is.na(imputed$imputed_data$age)), 0)

  # Check imputed values equal the median
  original_median_age <- median(test_data$age, na.rm = TRUE)
  imputed_age_values <- imputed$imputed_data$age[is.na(test_data$age)]
  expect_true(all(abs(imputed_age_values - original_median_age) < 0.01))
})


test_that("rwe_impute works with mode imputation", {
  set.seed(789)
  n <- 100
  test_data <- data.frame(
    id = 1:n,
    category = sample(c("A", "B", "C"), n, replace = TRUE),
    value = rnorm(n, 50, 10)
  )

  # Introduce missing values in categorical variable
  test_data$category[sample(n, 15)] <- NA

  imputed <- rwe_impute(
    data = test_data,
    method = "mode",
    vars = "category",
    seed = 789
  )

  expect_s3_class(imputed, "rwe_imputation")
  expect_equal(sum(is.na(imputed$imputed_data$category)), 0)
})


test_that("rwe_impute auto-detects variables with missing data", {
  set.seed(123)
  n <- 50
  test_data <- data.frame(
    complete = 1:n,
    missing1 = rnorm(n),
    missing2 = rnorm(n),
    missing3 = rnorm(n)
  )

  test_data$missing1[sample(n, 5)] <- NA
  test_data$missing2[sample(n, 8)] <- NA
  test_data$missing3[sample(n, 3)] <- NA

  imputed <- rwe_impute(
    data = test_data,
    method = "mean",
    seed = 123
  )

  # Should impute all three variables with missing data
  expect_length(imputed$vars_imputed, 3)
  expect_true(all(c("missing1", "missing2", "missing3") %in% imputed$vars_imputed))

  # No missing values should remain
  expect_equal(sum(is.na(imputed$imputed_data[, imputed$vars_imputed])), 0)
})


test_that("rwe_impute handles data with no missing values", {
  test_data <- data.frame(
    id = 1:50,
    age = rnorm(50, 60, 10),
    bmi = rnorm(50, 25, 5)
  )

  imputed <- rwe_impute(
    data = test_data,
    method = "mean"
  )

  expect_s3_class(imputed, "rwe_imputation")
  expect_equal(imputed$n_imputed, 0)
  expect_length(imputed$vars_imputed, 0)
  expect_identical(imputed$imputed_data, test_data)
})


test_that("rwe_impute validates inputs", {
  test_data <- data.frame(
    age = c(50, 60, NA, 70)
  )

  # Invalid method
  expect_error(
    rwe_impute(test_data, method = "invalid_method"),
    "'arg' should be one of"
  )
})


test_that("rwe_impute respects seed for reproducibility", {
  set.seed(999)
  n <- 100
  test_data <- data.frame(
    age = rnorm(n, 60, 10),
    bmi = rnorm(n, 25, 5)
  )
  test_data$age[sample(n, 20)] <- NA

  imputed1 <- rwe_impute(test_data, method = "mean", seed = 42)
  imputed2 <- rwe_impute(test_data, method = "mean", seed = 42)
  imputed3 <- rwe_impute(test_data, method = "mean", seed = 99)

  # Same seed should give same results
  expect_identical(imputed1$imputed_data, imputed2$imputed_data)

  # Different seed might give different results (though for mean it won't)
  # This is more relevant for stochastic methods
  expect_equal(nrow(imputed3$imputed_data), n)
})


test_that("rwe_impute creates missingness statistics", {
  set.seed(123)
  n <- 100
  test_data <- data.frame(
    age = rnorm(n),
    bmi = rnorm(n)
  )
  test_data$age[sample(n, 15)] <- NA
  test_data$bmi[sample(n, 10)] <- NA

  imputed <- rwe_impute(test_data, method = "mean", seed = 123)

  expect_true("missingness_before" %in% names(imputed))
  expect_true("missingness_after" %in% names(imputed))

  # Before imputation should have missing values
  expect_true(any(imputed$missingness_before$n_missing > 0))

  # After imputation should have no missing values
  expect_equal(sum(imputed$missingness_after$n_missing), 0)
})


test_that("rwe_impute creates diagnostics", {
  set.seed(123)
  test_data <- data.frame(
    age = c(50, 60, NA, 70, NA, 55),
    bmi = c(25, NA, 27, 28, 26, NA)
  )

  imputed <- rwe_impute(test_data, method = "mean", seed = 123)

  expect_true("diagnostics" %in% names(imputed))
  expect_type(imputed$diagnostics, "list")
})


test_that("rwe_impute creates audit trail", {
  set.seed(123)
  test_data <- data.frame(
    age = c(50, 60, NA, 70),
    bmi = c(25, NA, 27, 28)
  )

  imputed <- rwe_impute(test_data, method = "mean", seed = 123)

  expect_true("audit_trail" %in% names(imputed))
  expect_s3_class(imputed$audit_trail, "rwe_audit_trail")
})


test_that("rwe_impute works with rwe_data objects", {
  set.seed(123)
  test_df <- data.frame(
    age = c(50, 60, NA, 70, NA),
    bmi = c(25, 26, 27, NA, 29)
  )

  rwe_obj <- new_rwe_data(test_df, metadata = list(source = "test"))

  imputed <- rwe_impute(rwe_obj, method = "mean", seed = 123)

  expect_s3_class(imputed, "rwe_imputation")
  expect_equal(nrow(imputed$imputed_data), 5)
  expect_equal(sum(is.na(imputed$imputed_data)), 0)
})


test_that("rwe_impute only imputes specified variables", {
  set.seed(123)
  test_data <- data.frame(
    age = c(50, NA, 60, NA),
    bmi = c(25, NA, 27, NA),
    weight = c(70, NA, 75, NA)
  )

  imputed <- rwe_impute(
    test_data,
    method = "mean",
    vars = c("age", "bmi"),  # Don't impute weight
    seed = 123
  )

  expect_length(imputed$vars_imputed, 2)
  expect_equal(sum(is.na(imputed$imputed_data$age)), 0)
  expect_equal(sum(is.na(imputed$imputed_data$bmi)), 0)
  expect_equal(sum(is.na(imputed$imputed_data$weight)), 2)  # Still has NAs
})


test_that("rwe_impute handles mixed data types", {
  set.seed(123)
  test_data <- data.frame(
    id = 1:10,
    age = c(50, 60, NA, 70, NA, 55, 65, NA, 75, 80),
    category = c("A", "B", NA, "A", "B", NA, "C", "A", NA, "B"),
    score = c(10, NA, 15, 20, NA, 25, 30, NA, 35, 40),
    stringsAsFactors = FALSE
  )

  # Mean imputation should handle numeric variables
  imputed <- rwe_impute(
    test_data,
    method = "mean",
    vars = c("age", "score"),
    seed = 123
  )

  expect_equal(sum(is.na(imputed$imputed_data$age)), 0)
  expect_equal(sum(is.na(imputed$imputed_data$score)), 0)
  # Category still has NAs (mean doesn't work on characters)
  expect_true(sum(is.na(imputed$imputed_data$category)) > 0)
})


test_that("impute_mean only imputes numeric variables", {
  test_data <- data.frame(
    numeric_var = c(1, 2, NA, 4),
    char_var = c("a", "b", NA, "d"),
    stringsAsFactors = FALSE
  )

  # Should skip character variable
  imputed <- impute_mean(test_data, c("numeric_var", "char_var"))

  expect_equal(sum(is.na(imputed$numeric_var)), 0)
  expect_equal(sum(is.na(imputed$char_var)), 1)  # Still has NA
})


test_that("impute_median only imputes numeric variables", {
  test_data <- data.frame(
    numeric_var = c(1, 2, NA, 4, 5),
    char_var = c("a", "b", NA, "d", "e"),
    stringsAsFactors = FALSE
  )

  imputed <- impute_median(test_data, c("numeric_var", "char_var"))

  expect_equal(sum(is.na(imputed$numeric_var)), 0)
  expect_equal(sum(is.na(imputed$char_var)), 1)  # Still has NA
})


test_that("calculate_missingness returns correct structure", {
  test_data <- data.frame(
    var1 = c(1, 2, NA, 4, NA),
    var2 = c(10, NA, 30, 40, 50),
    var3 = c(100, 200, 300, 400, 500)  # No missing
  )

  result <- calculate_missingness(test_data, c("var1", "var2", "var3"))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(c("variable", "n_missing", "pct_missing") %in% names(result)))

  expect_equal(result$n_missing[result$variable == "var1"], 2)
  expect_equal(result$n_missing[result$variable == "var2"], 1)
  expect_equal(result$n_missing[result$variable == "var3"], 0)

  expect_equal(result$pct_missing[result$variable == "var1"], 0.4)
  expect_equal(result$pct_missing[result$variable == "var2"], 0.2)
  expect_equal(result$pct_missing[result$variable == "var3"], 0.0)
})


test_that("rwe_impute records number of imputed values", {
  set.seed(123)
  test_data <- data.frame(
    age = c(50, 60, NA, 70, NA, 55),
    bmi = c(25, NA, 27, 28, 26, NA)
  )

  imputed <- rwe_impute(test_data, method = "mean", seed = 123)

  # Should have imputed 4 values total (2 age + 2 bmi)
  expect_equal(imputed$n_imputed, 4)
})


test_that("rwe_impute stores method information", {
  test_data <- data.frame(
    age = c(50, 60, NA, 70)
  )

  imputed_mean <- rwe_impute(test_data, method = "mean")
  imputed_median <- rwe_impute(test_data, method = "median")

  expect_equal(imputed_mean$method, "mean")
  expect_equal(imputed_median$method, "median")
})


test_that("rwe_impute handles edge case with all missing values", {
  test_data <- data.frame(
    age = c(NA, NA, NA, NA),
    bmi = c(25, 26, 27, 28)
  )

  # Mean of all NAs is NaN
  imputed <- rwe_impute(test_data, method = "mean", vars = "age")

  # Should not crash but result might be NaN
  expect_s3_class(imputed, "rwe_imputation")
})


test_that("rwe_impute handles single row data", {
  test_data <- data.frame(
    age = NA,
    bmi = 25
  )

  # Can't really impute with single row
  imputed <- rwe_impute(test_data, method = "mean")

  expect_s3_class(imputed, "rwe_imputation")
})


test_that("rwe_impute works with knn method", {
  skip_if_not_installed("FNN")

  set.seed(321)
  n <- 150
  age_vals <- rnorm(n, 60, 8)
  test_data <- data.frame(
    age = age_vals,
    bmi = rnorm(n, 28, 4),
    glucose = 0.5 * age_vals + rnorm(n, 0, 5)
  )

  test_data$glucose[sample(n, 20)] <- NA

  imputed <- rwe_impute(
    data = test_data,
    method = "knn",
    vars = "glucose",
    predictors = c("age", "bmi"),
    k = 3,
    seed = 321
  )

  expect_s3_class(imputed, "rwe_imputation")
  expect_equal(sum(is.na(imputed$imputed_data$glucose)), 0)
})


test_that("rwe_impute works with xgboost method", {
  skip_if_not_installed("xgboost")

  set.seed(654)
  n <- 120
  age_vals <- rnorm(n, 55, 7)
  weight_vals <- rnorm(n, 75, 12)
  test_data <- data.frame(
    age = age_vals,
    weight = weight_vals,
    cholesterol = 0.2 * age_vals + 0.5 * weight_vals + rnorm(n, 0, 10)
  )

  test_data$cholesterol[sample(n, 18)] <- NA

  imputed <- rwe_impute(
    data = test_data,
    method = "xgboost",
    vars = "cholesterol",
    predictors = c("age", "weight"),
    xgb_nrounds = 20,
    seed = 654
  )

  expect_s3_class(imputed, "rwe_imputation")
  expect_equal(sum(is.na(imputed$imputed_data$cholesterol)), 0)
})


test_that("print.rwe_imputation method exists", {
  set.seed(123)
  test_data <- data.frame(
    age = c(50, 60, NA, 70)
  )

  imputed <- rwe_impute(test_data, method = "mean", seed = 123)

  # Should have a print method
  expect_output(print(imputed))
})
