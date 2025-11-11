# Tests for effectiveness_analysis.R

library(testthat)
library(rwevidence)

set.seed(123)

test_that("rwe_relative_risk computes expected crude estimates", {
  n <- 400
  treatment <- rep(c(0, 1), each = n / 2)
  outcome <- c(rbinom(n / 2, 1, 0.25), rbinom(n / 2, 1, 0.12))
  data <- data.frame(
    treatment = treatment,
    outcome = outcome
  )

  result <- rwe_relative_risk(
    data = data,
    outcome = "outcome",
    treatment = "treatment",
    quiet = TRUE
  )

  expect_s3_class(result, "rwe_relative_risk")

  expected_risk_treated <- mean(data$outcome[data$treatment == 1])
  expected_risk_control <- mean(data$outcome[data$treatment == 0])
  expected_rr <- expected_risk_treated / expected_risk_control

  expect_equal(result$risk_treated, expected_risk_treated, tolerance = 1e-6)
  expect_equal(result$risk_control, expected_risk_control, tolerance = 1e-6)
  expect_equal(result$risk_ratio, expected_rr, tolerance = 1e-6)
})


test_that("rwe_odds_ratio computes expected crude estimates", {
  n <- 400
  treatment <- rep(c(0, 1), each = n / 2)
  outcome <- c(rbinom(n / 2, 1, 0.30), rbinom(n / 2, 1, 0.15))
  age <- rnorm(n, 65, 5)
  data <- data.frame(
    treatment = treatment,
    outcome = outcome,
    age = age
  )

  result <- rwe_odds_ratio(
    data = data,
    outcome = "outcome",
    treatment = "treatment",
    quiet = TRUE
  )

  expect_s3_class(result, "rwe_odds_ratio")

  odds_treated <- sum(data$outcome[data$treatment == 1]) /
    sum(1 - data$outcome[data$treatment == 1])
  odds_control <- sum(data$outcome[data$treatment == 0]) /
    sum(1 - data$outcome[data$treatment == 0])

  expect_equal(result$odds_ratio, odds_treated / odds_control, tolerance = 1e-6)
})


test_that("rwe_nnt returns consistent values", {
  n <- 400
  treatment <- rep(c(0, 1), each = n / 2)
  outcome <- c(rbinom(n / 2, 1, 0.18), rbinom(n / 2, 1, 0.05))
  data <- data.frame(
    treatment = treatment,
    outcome = outcome
  )

  result <- rwe_nnt(
    data = data,
    outcome = "outcome",
    treatment = "treatment",
    quiet = TRUE
  )

  expect_s3_class(result, "rwe_nnt")

  risk_treated <- mean(data$outcome[data$treatment == 1])
  risk_control <- mean(data$outcome[data$treatment == 0])
  risk_diff <- risk_treated - risk_control
  expected_nnt <- 1 / abs(risk_diff)

  expect_equal(result$risk_difference, risk_diff, tolerance = 1e-6)
  expect_equal(result$nnt, expected_nnt, tolerance = 1e-6)
})


test_that("rwe_subgroup_analysis produces forest plot", {
  skip_if_not_installed("ggplot2")

  n <- 500
  treatment <- rbinom(n, 1, 0.5)
  sex <- sample(c("Male", "Female"), n, replace = TRUE)
  age_group <- sample(c("<65", "65-75", "75+"), n, replace = TRUE)
  outcome <- rbinom(n, 1, plogis(-1 + 0.4 * treatment + 0.2 * (sex == "Male")))
  data <- data.frame(
    treatment = treatment,
    outcome = outcome,
    sex = sex,
    age_group = age_group
  )

  subgroup <- rwe_subgroup_analysis(
    data = data,
    outcome = "outcome",
    treatment = "treatment",
    subgroup_vars = c("sex", "age_group"),
    measure = "risk_difference",
    quiet = TRUE
  )

  expect_s3_class(subgroup, "rwe_subgroup_analysis")
  expect_gt(nrow(subgroup$results), 0)
  expect_s3_class(subgroup$plot, "ggplot")
})


test_that("rwe_sensitivity_analysis returns requested analyses", {
  skip_if_not_installed("ggplot2")

  n <- 300
  treatment <- rbinom(n, 1, 0.5)
  outcome <- rbinom(n, 1, plogis(-0.8 + 0.5 * treatment))
  outcome[sample(seq_len(n), 15)] <- NA
  bmi <- rnorm(n, 27, 4)
  weights <- runif(n, 0.3, 2)

  data <- data.frame(
    treatment = treatment,
    outcome = outcome,
    bmi = bmi
  )

  sensitivity <- rwe_sensitivity_analysis(
    data = data,
    outcome = "outcome",
    treatment = "treatment",
    confounders = "bmi",
    weights = weights,
    analyses = c("trim_weights", "missing_outcomes", "permutation"),
    n_permutations = 30,
    seed = 42,
    quiet = TRUE
  )

  expect_s3_class(sensitivity, "rwe_sensitivity_analysis")
  expect_true("trim_weights" %in% names(sensitivity$analyses))
  expect_true("missing_outcomes" %in% names(sensitivity$analyses))
  expect_true("permutation" %in% names(sensitivity$analyses))
  expect_length(sensitivity$analyses$permutation$estimates, 30)
})
