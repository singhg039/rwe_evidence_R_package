# Tests for survival_analysis.R

library(testthat)
library(rwevidence)

# Skip all tests if survival package not installed
skip_if_not_installed("survival")

test_that("rwe_cox_regression works with basic formula", {
  skip_if_not_installed("survival")

  set.seed(123)
  n <- 200

  # Create survival data
  age <- rnorm(n, 60, 10)
  treatment <- rbinom(n, 1, 0.5)

  # Simulate survival times with treatment effect
  lambda <- exp(-2 + 0.5 * treatment + 0.02 * age)
  time <- rexp(n, rate = lambda)
  event <- rbinom(n, 1, 0.7)  # 70% event rate

  test_data <- data.frame(
    time = time,
    event = event,
    treatment = treatment,
    age = age
  )

  cox_result <- rwe_cox_regression(
    data = test_data,
    time_var = "time",
    event_var = "event",
    treatment = "treatment",
    covariates = "age"
  )

  expect_s3_class(cox_result, "rwe_survival")
  expect_true("model" %in% names(cox_result))
  expect_true("coefficients" %in% names(cox_result))
  expect_equal(cox_result$n, n)
})


test_that("rwe_cox_regression validates inputs", {
  skip_if_not_installed("survival")

  test_data <- data.frame(
    time = c(10, 20, 30),
    event = c(1, 0, 1),
    treatment = c(0, 1, 0)
  )

  # Missing time variable
  expect_error(
    rwe_cox_regression(
      data = test_data,
      time_var = "missing_time",
      event_var = "event",
      treatment = "treatment"
    ),
    "Time variable 'missing_time' not found"
  )

  # Missing event variable
  expect_error(
    rwe_cox_regression(
      data = test_data,
      time_var = "time",
      event_var = "missing_event",
      treatment = "treatment"
    ),
    "Event variable 'missing_event' not found"
  )

  # Missing both treatment and covariates
  expect_error(
    rwe_cox_regression(
      data = test_data,
      time_var = "time",
      event_var = "event"
    ),
    "Must specify either treatment or covariates"
  )
})


test_that("rwe_cox_regression works with IPTW weights", {
  skip_if_not_installed("survival")

  set.seed(456)
  n <- 150

  # Create data
  age <- rnorm(n, 60, 10)
  treatment <- rbinom(n, 1, plogis(-1 + 0.02 * age))
  lambda <- exp(-2 + 0.5 * treatment + 0.02 * age)
  time <- rexp(n, rate = lambda)
  event <- rbinom(n, 1, 0.65)

  test_data <- data.frame(
    time = time,
    event = event,
    treatment = treatment,
    age = age
  )

  # Create IPTW weights
  iptw_result <- rwe_iptw(
    data = test_data,
    treatment = "treatment",
    covariates = "age"
  )

  # Cox regression with weights
  cox_weighted <- rwe_cox_regression(
    data = test_data,
    time_var = "time",
    event_var = "event",
    treatment = "treatment",
    weights = iptw_result
  )

  expect_s3_class(cox_weighted, "rwe_survival")
  expect_true(cox_weighted$weighted)
})


test_that("rwe_cox_regression works with numeric weights", {
  skip_if_not_installed("survival")

  set.seed(789)
  n <- 100

  test_data <- data.frame(
    time = rexp(n, 0.1),
    event = rbinom(n, 1, 0.6),
    treatment = rbinom(n, 1, 0.5),
    age = rnorm(n, 60, 10)
  )

  weights <- runif(n, 0.5, 2)

  cox_result <- rwe_cox_regression(
    data = test_data,
    time_var = "time",
    event_var = "event",
    treatment = "treatment",
    weights = weights
  )

  expect_s3_class(cox_result, "rwe_survival")
  expect_true(cox_result$weighted)
})


test_that("rwe_cox_regression handles multiple covariates", {
  skip_if_not_installed("survival")

  set.seed(123)
  n <- 150

  test_data <- data.frame(
    time = rexp(n, 0.1),
    event = rbinom(n, 1, 0.65),
    treatment = rbinom(n, 1, 0.5),
    age = rnorm(n, 60, 10),
    sex = sample(c(0, 1), n, replace = TRUE),
    bmi = rnorm(n, 25, 5)
  )

  cox_result <- rwe_cox_regression(
    data = test_data,
    time_var = "time",
    event_var = "event",
    treatment = "treatment",
    covariates = c("age", "sex", "bmi")
  )

  expect_s3_class(cox_result, "rwe_survival")
  expect_equal(length(cox_result$covariates), 3)
})


test_that("rwe_cox_regression creates audit trail", {
  skip_if_not_installed("survival")

  set.seed(123)
  test_data <- data.frame(
    time = rexp(50, 0.1),
    event = rbinom(50, 1, 0.6),
    treatment = rbinom(50, 1, 0.5),
    age = rnorm(50, 60, 10)
  )

  cox_result <- rwe_cox_regression(
    data = test_data,
    time_var = "time",
    event_var = "event",
    treatment = "treatment",
    covariates = "age"
  )

  expect_true("audit_trail" %in% names(cox_result))
  expect_s3_class(cox_result$audit_trail, "rwe_audit_trail")
})


test_that("rwe_kaplan_meier estimates survival curves", {
  skip_if_not_installed("survival")

  set.seed(123)
  n <- 200

  test_data <- data.frame(
    time = rexp(n, 0.1),
    event = rbinom(n, 1, 0.7),
    treatment = rbinom(n, 1, 0.5)
  )

  km_result <- rwe_kaplan_meier(
    data = test_data,
    time_var = "time",
    event_var = "event",
    group_var = "treatment"
  )

  expect_s3_class(km_result, "rwe_km")
  expect_true("survfit" %in% names(km_result))
  expect_true("logrank_test" %in% names(km_result))
})


test_that("rwe_kaplan_meier works without grouping", {
  skip_if_not_installed("survival")

  set.seed(456)
  n <- 100

  test_data <- data.frame(
    time = rexp(n, 0.1),
    event = rbinom(n, 1, 0.65)
  )

  km_result <- rwe_kaplan_meier(
    data = test_data,
    time_var = "time",
    event_var = "event"
  )

  expect_s3_class(km_result, "rwe_km")
  expect_null(km_result$group_var)
  expect_null(km_result$logrank_test)
})


test_that("rwe_kaplan_meier validates inputs", {
  skip_if_not_installed("survival")

  test_data <- data.frame(
    time = c(10, 20, 30),
    event = c(1, 0, 1)
  )

  # Missing time variable
  expect_error(
    rwe_kaplan_meier(
      data = test_data,
      time_var = "missing_time",
      event_var = "event"
    ),
    "Time variable 'missing_time' not found"
  )

  # Missing event variable
  expect_error(
    rwe_kaplan_meier(
      data = test_data,
      time_var = "time",
      event_var = "missing_event"
    ),
    "Event variable 'missing_event' not found"
  )
})


test_that("rwe_kaplan_meier works with IPTW weights", {
  skip_if_not_installed("survival")

  set.seed(789)
  n <- 150

  age <- rnorm(n, 60, 10)
  treatment <- rbinom(n, 1, plogis(-1 + 0.02 * age))
  time <- rexp(n, 0.1 * (1 + 0.5 * treatment))
  event <- rbinom(n, 1, 0.65)

  test_data <- data.frame(
    time = time,
    event = event,
    treatment = treatment,
    age = age
  )

  # Create weights
  iptw_result <- rwe_iptw(
    data = test_data,
    treatment = "treatment",
    covariates = "age"
  )

  km_weighted <- rwe_kaplan_meier(
    data = test_data,
    time_var = "time",
    event_var = "event",
    group_var = "treatment",
    weights = iptw_result
  )

  expect_s3_class(km_weighted, "rwe_km")
  expect_true(km_weighted$weighted)
})


test_that("rwe_kaplan_meier creates audit trail", {
  skip_if_not_installed("survival")

  set.seed(123)
  test_data <- data.frame(
    time = rexp(50, 0.1),
    event = rbinom(50, 1, 0.6),
    treatment = rbinom(50, 1, 0.5)
  )

  km_result <- rwe_kaplan_meier(
    data = test_data,
    time_var = "time",
    event_var = "event",
    group_var = "treatment"
  )

  expect_true("audit_trail" %in% names(km_result))
  expect_s3_class(km_result$audit_trail, "rwe_audit_trail")
})


test_that("print.rwe_survival produces output", {
  skip_if_not_installed("survival")

  set.seed(123)
  test_data <- data.frame(
    time = rexp(100, 0.1),
    event = rbinom(100, 1, 0.6),
    treatment = rbinom(100, 1, 0.5),
    age = rnorm(100, 60, 10)
  )

  cox_result <- rwe_cox_regression(
    data = test_data,
    time_var = "time",
    event_var = "event",
    treatment = "treatment",
    covariates = "age"
  )

  expect_output(print(cox_result), "Cox Proportional Hazards")
  expect_output(print(cox_result), "Sample size")
  expect_output(print(cox_result), "Concordance")
})


test_that("print.rwe_km produces output", {
  skip_if_not_installed("survival")

  set.seed(123)
  test_data <- data.frame(
    time = rexp(100, 0.1),
    event = rbinom(100, 1, 0.6),
    treatment = rbinom(100, 1, 0.5)
  )

  km_result <- rwe_kaplan_meier(
    data = test_data,
    time_var = "time",
    event_var = "event",
    group_var = "treatment"
  )

  expect_output(print(km_result), "Kaplan-Meier")
  expect_output(print(km_result), "Log-Rank Test")
})


test_that("rwe_cox_regression works with rwe_data objects", {
  skip_if_not_installed("survival")

  set.seed(123)
  test_df <- data.frame(
    time = rexp(50, 0.1),
    event = rbinom(50, 1, 0.6),
    treatment = rbinom(50, 1, 0.5),
    age = rnorm(50, 60, 10)
  )

  rwe_obj <- new_rwe_data(test_df, metadata = list(source = "test"))

  cox_result <- rwe_cox_regression(
    data = rwe_obj,
    time_var = "time",
    event_var = "event",
    treatment = "treatment",
    covariates = "age"
  )

  expect_s3_class(cox_result, "rwe_survival")
})


test_that("rwe_kaplan_meier works with rwe_data objects", {
  skip_if_not_installed("survival")

  set.seed(456)
  test_df <- data.frame(
    time = rexp(50, 0.1),
    event = rbinom(50, 1, 0.6),
    treatment = rbinom(50, 1, 0.5)
  )

  rwe_obj <- new_rwe_data(test_df, metadata = list(source = "test"))

  km_result <- rwe_kaplan_meier(
    data = rwe_obj,
    time_var = "time",
    event_var = "event",
    group_var = "treatment"
  )

  expect_s3_class(km_result, "rwe_km")
})


test_that("rwe_cox_regression stores key statistics", {
  skip_if_not_installed("survival")

  set.seed(789)
  n <- 100

  test_data <- data.frame(
    time = rexp(n, 0.1),
    event = rbinom(n, 1, 0.6),
    treatment = rbinom(n, 1, 0.5),
    age = rnorm(n, 60, 10)
  )

  cox_result <- rwe_cox_regression(
    data = test_data,
    time_var = "time",
    event_var = "event",
    treatment = "treatment",
    covariates = "age"
  )

  expect_true("n" %in% names(cox_result))
  expect_true("n_events" %in% names(cox_result))
  expect_true("concordance" %in% names(cox_result))

  expect_equal(cox_result$n, n)
  expect_true(cox_result$n_events <= n)
  expect_true(cox_result$concordance >= 0 && cox_result$concordance <= 1)
})


test_that("rwe_cox_regression handles censored data correctly", {
  skip_if_not_installed("survival")

  set.seed(123)
  n <- 100

  # Mix of events and censored observations
  test_data <- data.frame(
    time = rexp(n, 0.1),
    event = rbinom(n, 1, 0.5),  # 50% event rate
    treatment = rbinom(n, 1, 0.5),
    age = rnorm(n, 60, 10)
  )

  cox_result <- rwe_cox_regression(
    data = test_data,
    time_var = "time",
    event_var = "event",
    treatment = "treatment",
    covariates = "age"
  )

  expect_s3_class(cox_result, "rwe_survival")
  expect_true(cox_result$n_events < cox_result$n)
})


test_that("plot.rwe_survival runs without error", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")

  set.seed(123)
  test_data <- data.frame(
    time = rexp(100, 0.1),
    event = rbinom(100, 1, 0.6),
    treatment = rbinom(100, 1, 0.5),
    age = rnorm(100, 60, 10)
  )

  cox_result <- rwe_cox_regression(
    data = test_data,
    time_var = "time",
    event_var = "event",
    treatment = "treatment",
    covariates = "age"
  )

  expect_silent(plot(cox_result))
})


test_that("plot.rwe_km runs without error", {
  skip_if_not_installed("survival")

  set.seed(123)
  test_data <- data.frame(
    time = rexp(100, 0.1),
    event = rbinom(100, 1, 0.6),
    treatment = rbinom(100, 1, 0.5)
  )

  km_result <- rwe_kaplan_meier(
    data = test_data,
    time_var = "time",
    event_var = "event",
    group_var = "treatment"
  )

  expect_silent(plot(km_result))
})
