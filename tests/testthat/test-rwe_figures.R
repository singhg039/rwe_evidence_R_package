library(testthat)
library(rwevidence)

set.seed(2024)

test_that("rwe_figure_forest returns ggplot object", {
  skip_if_not_installed("ggplot2")

  data <- data.frame(
    treatment = rbinom(300, 1, 0.5),
    outcome = rbinom(300, 1, 0.3),
    sex = sample(c("Male", "Female"), 300, replace = TRUE),
    region = sample(c("North", "South", "East", "West"), 300, replace = TRUE)
  )

  subgroup <- rwe_subgroup_analysis(
    data = data,
    outcome = "outcome",
    treatment = "treatment",
    subgroup_vars = c("sex", "region"),
    quiet = TRUE
  )

  forest_plot <- rwe_figure_forest(subgroup)

  expect_s3_class(forest_plot, "ggplot")
})


test_that("rwe_figure_survival handles rwe_km objects", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")

  n <- 200
  sample_data <- data.frame(
    time = rexp(n, rate = 0.1),
    event = rbinom(n, 1, 0.5),
    treatment = sample(c("A", "B"), n, replace = TRUE)
  )

  km <- rwe_kaplan_meier(
    data = sample_data,
    time_var = "time",
    event_var = "event",
    group_var = "treatment"
  )

  km_plot <- rwe_figure_survival(km)
  expect_s3_class(km_plot, "ggplot")
})


test_that("rwe_figure_top_ae works with safety report", {
  skip_if_not_installed("ggplot2")

  ae_data <- data.frame(
    patient_id = rep(1:80, each = 2),
    treatment = sample(c("Drug", "Control"), 160, replace = TRUE),
    adverse_event = sample(
      c("Nausea", "Headache", "Fatigue", "Rash", "Dizziness"),
      160,
      replace = TRUE,
      prob = c(0.3, 0.25, 0.2, 0.15, 0.1)
    ),
    severity = sample(c("Mild", "Moderate", "Severe"), 160, replace = TRUE),
    serious = sample(c(TRUE, FALSE), 160, replace = TRUE, prob = c(0.1, 0.9))
  )

  n_per_group <- table(ae_data$treatment)

  safety_report <- rwe_safety_report(
    data = ae_data,
    group_var = "treatment",
    ae_var = "adverse_event",
    severity_var = "severity",
    serious_var = "serious",
    n_per_group = n_per_group,
    top_n = 5
  )

  ae_plot <- rwe_figure_top_ae(safety_report, top_n = 5)
  expect_s3_class(ae_plot, "ggplot")
})


test_that("rwe_figure_consort generates DiagrammeR graph", {
  skip_if_not_installed("DiagrammeR")

  consort <- rwe_consort_diagram(
    screened = 1200,
    enrolled = 900,
    excluded = list(
      "Did not meet inclusion criteria" = 200,
      "Declined to participate" = 50
    ),
    allocated = list(
      "Treatment A" = 450,
      "Treatment B" = 450
    ),
    lost_to_followup = list(
      "Treatment A" = 30,
      "Treatment B" = 25
    ),
    discontinued = list(
      "Treatment A" = list("Adverse events" = 15, "Protocol deviation" = 10),
      "Treatment B" = list("Adverse events" = 12)
    ),
    analyzed = list(
      "Treatment A" = 395,
      "Treatment B" = 413
    )
  )

  consort_fig <- rwe_figure_consort(consort)
  expect_s3_class(consort_fig, "grViz")
})
