# Tests for propensity_score.R

library(testthat)
library(rwevidence)

test_that("rwe_propensity_score works with logistic regression", {
  # Create test data
  set.seed(123)
  n <- 200
  test_data <- data.frame(
    patient_id = 1:n,
    treatment = rbinom(n, 1, 0.5),
    age = rnorm(n, 60, 10),
    sex = sample(c("M", "F"), n, replace = TRUE),
    bmi = rnorm(n, 25, 5),
    comorbidity_score = rpois(n, 2)
  )

  # Convert sex to numeric for modeling
  test_data$sex_numeric <- ifelse(test_data$sex == "M", 1, 0)

  ps_result <- rwe_propensity_score(
    data = test_data,
    treatment = "treatment",
    covariates = c("age", "sex_numeric", "bmi", "comorbidity_score"),
    method = "logistic"
  )

  expect_s3_class(ps_result, "rwe_propensity_score")
  expect_true("propensity_score" %in% names(ps_result$data))
  expect_true(all(ps_result$data$propensity_score >= 0 & ps_result$data$propensity_score <= 1))
  expect_equal(nrow(ps_result$data), n)
  expect_true("balance_before" %in% names(ps_result))
  expect_equal(ps_result$method, "logistic")
  expect_true("metrics" %in% names(ps_result))
  expect_gte(ps_result$metrics$auc, 0)
  expect_lte(ps_result$metrics$auc, 1)
})


test_that("rwe_propensity_score works with xgboost", {
  skip_if_not_installed("xgboost")

  set.seed(321)
  n <- 150
  test_data <- data.frame(
    treatment = rbinom(n, 1, 0.5),
    age = rnorm(n, 60, 8),
    bmi = rnorm(n, 26, 4),
    comorbidity = rpois(n, 2)
  )

  ps_result <- rwe_propensity_score(
    data = test_data,
    treatment = "treatment",
    covariates = c("age", "bmi", "comorbidity"),
    method = "xgboost",
    learner_control = list(xgb = list(nrounds = 25, seed = 321))
  )

  expect_s3_class(ps_result, "rwe_propensity_score")
  expect_equal(ps_result$method, "xgboost")
  expect_true(all(ps_result$data$propensity_score >= 0 & ps_result$data$propensity_score <= 1))
})


test_that("rwe_propensity_score works with super learner", {
  skip_if_not_installed("caret")

  set.seed(987)
  n <- 160
  test_data <- data.frame(
    treatment = rbinom(n, 1, 0.5),
    age = rnorm(n, 58, 9),
    bmi = rnorm(n, 27, 4),
    systolic = rnorm(n, 120, 12)
  )

  ps_result <- rwe_propensity_score(
    data = test_data,
    treatment = "treatment",
    covariates = c("age", "bmi", "systolic"),
    method = "super_learner",
    learner_control = list(super_learner = list(learners = c("glm"), method = "none", seed = 987))
  )

  expect_s3_class(ps_result, "rwe_propensity_score")
  expect_equal(ps_result$method, "super_learner")
  expect_true(all(ps_result$data$propensity_score >= 0 & ps_result$data$propensity_score <= 1))
})


test_that("rwe_propensity_score validates inputs", {
  test_data <- data.frame(
    patient_id = 1:10,
    treatment = rep(c(0, 1), 5),
    age = rnorm(10, 60, 10)
  )

  # Missing treatment column
  expect_error(
    rwe_propensity_score(test_data, treatment = "trt", covariates = "age"),
    "Treatment variable 'trt' not found"
  )

  # Missing covariate
  expect_error(
    rwe_propensity_score(test_data, treatment = "treatment", covariates = "height"),
    "Covariate 'height' not found"
  )

  # Non-binary treatment
  bad_data <- test_data
  bad_data$treatment <- c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1)
  expect_error(
    rwe_propensity_score(bad_data, treatment = "treatment", covariates = "age"),
    "Treatment variable must be binary"
  )
})


test_that("rwe_propensity_score calculates balance metrics", {
  set.seed(456)
  n <- 150
  test_data <- data.frame(
    treatment = rbinom(n, 1, 0.5),
    age = rnorm(n, 60, 10),
    bmi = rnorm(n, 25, 5)
  )

  ps_result <- rwe_propensity_score(
    data = test_data,
    treatment = "treatment",
    covariates = c("age", "bmi"),
    method = "logistic"
  )

  expect_true("balance_before" %in% names(ps_result))
  expect_true("variable" %in% names(ps_result$balance_before))
  expect_true("smd" %in% names(ps_result$balance_before))
  expect_equal(nrow(ps_result$balance_before), 2)  # Two covariates
})


test_that("rwe_propensity_score accepts custom formula", {
  set.seed(789)
  n <- 100
  test_data <- data.frame(
    treatment = rbinom(n, 1, 0.5),
    age = rnorm(n, 60, 10),
    bmi = rnorm(n, 25, 5)
  )

  custom_formula <- as.formula("treatment ~ age + I(age^2) + bmi")

  ps_result <- rwe_propensity_score(
    data = test_data,
    treatment = "treatment",
    covariates = c("age", "bmi"),
    method = "logistic",
    formula = custom_formula
  )

  expect_s3_class(ps_result, "rwe_propensity_score")
  expect_equal(nrow(ps_result$data), n)
})


test_that("rwe_match performs nearest neighbor matching", {
  set.seed(123)
  n <- 200
  test_data <- data.frame(
    treatment = rbinom(n, 1, 0.5),
    age = rnorm(n, 60, 10),
    bmi = rnorm(n, 25, 5)
  )

  ps_result <- rwe_propensity_score(
    data = test_data,
    treatment = "treatment",
    covariates = c("age", "bmi"),
    method = "logistic"
  )

  matched <- rwe_match(ps_result, method = "nearest", ratio = 1)

  expect_s3_class(matched, "rwe_matched_cohort")
  expect_true("matched" %in% names(matched$data))
  expect_true(all(matched$data$matched %in% c(0, 1)))
  expect_true("balance_after" %in% names(matched))
})


test_that("rwe_match validates inputs", {
  set.seed(456)
  test_data <- data.frame(
    treatment = rbinom(100, 1, 0.5),
    age = rnorm(100, 60, 10)
  )

  ps_result <- rwe_propensity_score(
    data = test_data,
    treatment = "treatment",
    covariates = "age",
    method = "logistic"
  )

  # Invalid method
  expect_error(
    rwe_match(ps_result, method = "invalid"),
    "'arg' should be one of"
  )

  # Invalid ratio
  expect_error(
    rwe_match(ps_result, ratio = 0),
    "ratio must be positive"
  )

  # Invalid caliper
  expect_error(
    rwe_match(ps_result, caliper = -0.1),
    "caliper must be positive"
  )
})


test_that("rwe_match improves balance", {
  set.seed(789)
  n <- 200

  # Create data with imbalance
  treatment <- rbinom(n, 1, 0.5)
  age <- rnorm(n, 60 + 5 * treatment, 10)  # Age is related to treatment
  bmi <- rnorm(n, 25 + 2 * treatment, 5)   # BMI is related to treatment

  test_data <- data.frame(
    treatment = treatment,
    age = age,
    bmi = bmi
  )

  ps_result <- rwe_propensity_score(
    data = test_data,
    treatment = "treatment",
    covariates = c("age", "bmi"),
    method = "logistic"
  )

  matched <- rwe_match(ps_result, method = "nearest", ratio = 1)

  # Check that balance improved
  expect_true("balance_improvement" %in% names(matched))

  # Get max absolute SMD before and after
  max_smd_before <- max(abs(ps_result$balance_before$smd))
  max_smd_after <- max(abs(matched$balance_after$smd))

  # Balance should improve (or at least not get worse)
  expect_true(max_smd_after <= max_smd_before + 0.1)  # Allow small tolerance
})


test_that("assess_covariate_balance calculates SMD correctly", {
  set.seed(123)
  n <- 100

  test_data <- data.frame(
    treatment = rep(c(0, 1), each = n/2),
    age = c(rnorm(n/2, 55, 10), rnorm(n/2, 65, 10)),
    bmi = c(rnorm(n/2, 24, 5), rnorm(n/2, 26, 5))
  )

  balance <- assess_covariate_balance(
    data = test_data,
    treatment = "treatment",
    covariates = c("age", "bmi")
  )

  expect_s3_class(balance, "data.frame")
  expect_equal(nrow(balance), 2)
  expect_true(all(c("variable", "smd") %in% names(balance)))

  # SMD for age should be substantial (means differ by ~10, sd ~10)
  age_smd <- balance$smd[balance$variable == "age"]
  expect_true(abs(age_smd) > 0.5)
})


test_that("print.rwe_propensity_score produces output", {
  set.seed(123)
  test_data <- data.frame(
    treatment = rbinom(100, 1, 0.5),
    age = rnorm(100, 60, 10)
  )

  ps_result <- rwe_propensity_score(
    data = test_data,
    treatment = "treatment",
    covariates = "age",
    method = "logistic"
  )

  expect_output(print(ps_result), "Propensity Score Analysis")
  expect_output(print(ps_result), "Method:")
  expect_output(print(ps_result), "Treatment variable:")
})


test_that("print.rwe_matched_cohort produces output", {
  set.seed(123)
  test_data <- data.frame(
    treatment = rbinom(100, 1, 0.5),
    age = rnorm(100, 60, 10)
  )

  ps_result <- rwe_propensity_score(
    data = test_data,
    treatment = "treatment",
    covariates = "age",
    method = "logistic"
  )

  matched <- rwe_match(ps_result, method = "nearest")

  expect_output(print(matched), "Matched Cohort Analysis")
  expect_output(print(matched), "Matching method:")
  expect_output(print(matched), "Matched observations:")
})


test_that("rwe_propensity_score handles missing data", {
  set.seed(123)
  test_data <- data.frame(
    treatment = rbinom(100, 1, 0.5),
    age = rnorm(100, 60, 10),
    bmi = rnorm(100, 25, 5)
  )

  # Introduce some missing values
  test_data$age[c(1, 5, 10)] <- NA
  test_data$bmi[c(2, 7, 15)] <- NA

  # Should remove rows with missing data
  ps_result <- rwe_propensity_score(
    data = test_data,
    treatment = "treatment",
    covariates = c("age", "bmi"),
    method = "logistic"
  )

  # Should have fewer rows due to missing data removal
  expect_true(nrow(ps_result$data) < 100)
  expect_true(nrow(ps_result$data) >= 90)  # At least 90 complete cases
})


test_that("summarize_ps_distribution returns correct structure", {
  set.seed(123)
  ps_data <- data.frame(
    treatment = rep(c(0, 1), each = 50),
    propensity_score = c(runif(50, 0.2, 0.6), runif(50, 0.4, 0.8))
  )

  summary <- summarize_ps_distribution(ps_data, "treatment", ps_data$propensity_score)

  expect_type(summary, "list")
  expect_true(all(c("mean_treated", "sd_treated", "mean_control", "sd_control") %in% names(summary)))
  expect_true(summary$mean_treated > 0)
  expect_true(summary$mean_control > 0)
})


test_that("perform_simple_matching creates matched pairs", {
  set.seed(123)
  ps_data <- data.frame(
    patient_id = 1:100,
    treatment = rep(c(0, 1), each = 50),
    propensity_score = c(runif(50, 0.2, 0.6), runif(50, 0.4, 0.8))
  )

  matched_data <- perform_simple_matching(
    data = ps_data,
    treatment = "treatment",
    ps_var = "propensity_score",
    ratio = 1,
    caliper = 0.2,
    replace = FALSE
  )

  expect_true("matched" %in% names(matched_data))
  expect_true(all(matched_data$matched %in% c(0, 1)))

  # Check that some observations were matched
  n_matched <- sum(matched_data$matched)
  expect_true(n_matched > 0)
  expect_true(n_matched <= 100)
})


test_that("rwe_propensity_score stores metadata", {
  set.seed(123)
  test_data <- data.frame(
    treatment = rbinom(100, 1, 0.5),
    age = rnorm(100, 60, 10),
    bmi = rnorm(100, 25, 5)
  )

  ps_result <- rwe_propensity_score(
    data = test_data,
    treatment = "treatment",
    covariates = c("age", "bmi"),
    method = "logistic"
  )

  expect_true("treatment" %in% names(ps_result))
  expect_true("covariates" %in% names(ps_result))
  expect_true("method" %in% names(ps_result))
  expect_equal(ps_result$treatment, "treatment")
  expect_equal(ps_result$covariates, c("age", "bmi"))
  expect_equal(ps_result$method, "logistic")
})
