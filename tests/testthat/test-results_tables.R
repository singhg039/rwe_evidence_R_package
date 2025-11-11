library(testthat)
library(rwevidence)

set.seed(1234)

build_demo_table_one <- function() {
  data <- data.frame(
    treatment = rep(c("A", "B"), each = 100),
    age = rnorm(200, mean = 60, sd = 8),
    bmi = rnorm(200, mean = 28, sd = 5),
    sex = sample(c("Male", "Female"), 200, replace = TRUE),
    smoker = sample(c("Yes", "No"), 200, replace = TRUE)
  )

  rwe_table_one(
    data = data,
    group_var = "treatment",
    variables = c("age", "bmi", "sex", "smoker"),
    test = TRUE,
    smd = TRUE,
    digits = 1
  )
}

test_that("rwe_results_table handles demographics via rwe_table_one", {
  table_one <- build_demo_table_one()

  results_table <- rwe_results_table(
    results = table_one,
    table_type = "demographics",
    format = "data",
    quiet = TRUE
  )

  expect_s3_class(results_table, "rwe_results_table")
  expect_true("variable" %in% names(results_table$data))
  expect_gt(nrow(results_table$data), 0)
  expect_equal(results_table$table_type, "demographics")
})

test_that("rwe_results_table builds effectiveness table from risk measures", {
  data <- data.frame(
    treatment = rep(c(0, 1), each = 200),
    outcome = c(rbinom(200, 1, 0.25), rbinom(200, 1, 0.12))
  )

  rr <- rwe_relative_risk(
    data = data,
    outcome = "outcome",
    treatment = "treatment",
    quiet = TRUE
  )

  results_table <- rwe_results_table(
    results = rr,
    table_type = "effectiveness",
    format = "data",
    quiet = TRUE
  )

  expect_s3_class(results_table, "rwe_results_table")
  expect_true(all(c("measure", "estimate", "ci_lower", "ci_upper") %in%
                    names(results_table$data)))
})

test_that("rwe_results_table summarises subgroup analysis", {
  skip_if_not_installed("ggplot2")

  data <- data.frame(
    treatment = rbinom(300, 1, 0.5),
    outcome = rbinom(300, 1, 0.3),
    sex = sample(c("Male", "Female"), 300, replace = TRUE),
    age_group = sample(c("<60", "60-70", ">70"), 300, replace = TRUE)
  )

  subgroup <- rwe_subgroup_analysis(
    data = data,
    outcome = "outcome",
    treatment = "treatment",
    subgroup_vars = c("sex", "age_group"),
    quiet = TRUE
  )

  results_table <- rwe_results_table(
    results = subgroup,
    table_type = "subgroup",
    format = "data",
    quiet = TRUE
  )

  expect_s3_class(results_table, "rwe_results_table")
  expect_true(all(c("subgroup", "level", "estimate") %in%
                    names(results_table$data)))
})

test_that("rwe_results_table summarises safety analysis", {
  skip_if_not_installed("zoo")

  control_df <- data.frame(
    treatment = 0,
    events = rpois(120, 1.5),
    person_time = runif(120, 0.8, 1.2),
    day = sample(1:120, 120, replace = TRUE)
  )

  treated_df <- data.frame(
    treatment = 1,
    events = rpois(120, 2.5),
    person_time = runif(120, 0.8, 1.2),
    day = sample(1:120, 120, replace = TRUE)
  )

  safety_df <- rbind(control_df, treated_df)

  safety <- rwe_analyze_safety(
    data = safety_df,
    events = "events",
    person_time = "person_time",
    treatment = "treatment",
    time_var = "day",
    alpha = 0.05,
    quiet = TRUE
  )

  results_table <- rwe_results_table(
    results = safety,
    table_type = "safety",
    format = "data",
    quiet = TRUE
  )

  expect_s3_class(results_table, "rwe_results_table")
  expect_true("measure" %in% names(results_table$data))
})
