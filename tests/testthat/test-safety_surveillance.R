library(testthat)
library(rwevidence)

set.seed(321)

test_that("rwe_incidence_rate calculates expected rates", {
  safety_df <- data.frame(
    treatment = rep(c(0, 1), each = 50),
    events = rpois(100, lambda = c(rep(3, 50), rep(6, 50))),
    person_time = runif(100, 0.8, 1.2)
  )

  rates <- rwe_incidence_rate(
    data = safety_df,
    events = "events",
    person_time = "person_time",
    group = "treatment",
    scale = 100,
    quiet = TRUE
  )

  expect_s3_class(rates, "rwe_incidence_rate")
  expect_equal(nrow(rates$estimates), 2)

  manual_rate0 <- sum(safety_df$events[safety_df$treatment == 0]) /
    sum(safety_df$person_time[safety_df$treatment == 0]) * 100
  expect_equal(
    rates$estimates$incidence_rate[rates$estimates$group == 0],
    manual_rate0,
    tolerance = 1e-6
  )
})


test_that("rwe_rate_ratio matches Poisson model expectations", {
  safety_df <- data.frame(
    treatment = rep(c(0, 1), each = 100),
    events = c(rpois(100, 2), rpois(100, 4)),
    person_time = 1
  )

  rr <- rwe_rate_ratio(
    data = safety_df,
    events = "events",
    person_time = "person_time",
    treatment = "treatment",
    quiet = TRUE
  )

  expect_s3_class(rr, "rwe_rate_ratio")
  expect_true(rr$rate_ratio > 0)

  manual_rate_ratio <- (sum(safety_df$events[safety_df$treatment == 1]) /
                          sum(safety_df$person_time[safety_df$treatment == 1])) /
    (sum(safety_df$events[safety_df$treatment == 0]) /
       sum(safety_df$person_time[safety_df$treatment == 0]))

  expect_equal(rr$rate_ratio, manual_rate_ratio, tolerance = 0.05)
})


test_that("rwe_safety_signal detects elevated rates", {
  skip_if_not_installed("zoo")

  control_df <- data.frame(
    treatment = 0,
    events = rpois(200, 1),
    person_time = rep(1, 200),
    day = 1:200
  )

  treated_df <- data.frame(
    treatment = 1,
    events = rpois(200, 3),
    person_time = rep(1, 200),
    day = 1:200
  )

  safety_df <- rbind(control_df, treated_df)

  signal <- rwe_safety_signal(
    data = safety_df,
    events = "events",
    person_time = "person_time",
    treatment = "treatment",
    time_var = "day",
    alpha = 0.05,
    method = "cumulative_poisson",
    quiet = TRUE
  )

  expect_s3_class(signal, "rwe_safety_signal")
  expect_true(signal$detected)
  expect_true(signal$first_signal_index >= 1)
})


test_that("rwe_analyze_safety returns comprehensive object", {
  skip_if_not_installed("zoo")

  control_df <- data.frame(
    treatment = 0,
    bmi = rnorm(150, 25, 3),
    events = rpois(150, 1.5),
    person_time = runif(150, 0.8, 1.2),
    day = sample(1:150, 150, replace = TRUE)
  )

  treated_df <- data.frame(
    treatment = 1,
    bmi = rnorm(150, 26, 3),
    events = rpois(150, 3),
    person_time = runif(150, 0.8, 1.2),
    day = sample(1:150, 150, replace = TRUE)
  )

  safety_df <- rbind(control_df, treated_df)

  analysis <- rwe_analyze_safety(
    data = safety_df,
    events = "events",
    person_time = "person_time",
    treatment = "treatment",
    confounders = "bmi",
    time_var = "day",
    alpha = 0.05,
    quiet = TRUE
  )

  expect_s3_class(analysis, "rwe_safety_analysis")
  expect_s3_class(analysis$incidence, "rwe_incidence_rate")
  expect_s3_class(analysis$rate_ratio, "rwe_rate_ratio")
  expect_s3_class(analysis$safety_signal, "rwe_safety_signal")
})
