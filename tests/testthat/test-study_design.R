# Tests for Study Design Module

# Setup test data ---------------------------------------------------------

setup_test_data <- function() {
  set.seed(123)
  n_external <- 200
  n_treatment <- 50

  # External pool
  external_data <- data.frame(
    patient_id = 1:n_external,
    age = rnorm(n_external, 60, 10),
    sex = sample(c("M", "F"), n_external, replace = TRUE),
    disease_stage = sample(c("I", "II", "III", "IV"), n_external, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)),
    comorbidities = rpois(n_external, 2),
    prior_treatment = sample(c(TRUE, FALSE), n_external, replace = TRUE, prob = c(0.3, 0.7)),
    baseline_score = rnorm(n_external, 50, 15),
    index_date = as.Date("2020-01-01") + sample(0:730, n_external, replace = TRUE)
  )

  # Treatment arm
  treatment_data <- data.frame(
    patient_id = (n_external + 1):(n_external + n_treatment),
    age = rnorm(n_treatment, 58, 12),
    sex = sample(c("M", "F"), n_treatment, replace = TRUE),
    disease_stage = sample(c("I", "II", "III", "IV"), n_treatment, replace = TRUE, prob = c(0.15, 0.35, 0.35, 0.15)),
    comorbidities = rpois(n_treatment, 2),
    prior_treatment = rep(FALSE, n_treatment),
    baseline_score = rnorm(n_treatment, 52, 14),
    index_date = as.Date("2022-01-01") + sample(0:180, n_treatment, replace = TRUE)
  )

  list(external = external_data, treatment = treatment_data)
}


# External Control Tests --------------------------------------------------

test_that("rwe_external_control works with propensity score matching", {
  test_data <- setup_test_data()

  result <- rwe_external_control(
    external_data = test_data$external,
    treatment_data = test_data$treatment,
    matching_vars = c("age", "sex", "disease_stage"),
    method = "propensity",
    ratio = 1,
    seed = 123
  )

  expect_s3_class(result, "rwe_external_control")
  expect_s3_class(result, "rwe_analysis")
  expect_true(nrow(result$matched_controls) > 0)
  expect_equal(result$method, "propensity")
  expect_true("balance_assessment" %in% names(result))
})


test_that("rwe_external_control works with exact matching", {
  test_data <- setup_test_data()

  result <- rwe_external_control(
    external_data = test_data$external,
    treatment_data = test_data$treatment,
    matching_vars = c("sex", "disease_stage"),
    method = "exact",
    ratio = 2,
    seed = 123
  )

  expect_s3_class(result, "rwe_external_control")
  expect_equal(result$method, "exact")
  expect_true(nrow(result$matched_controls) > 0)
})


test_that("rwe_external_control works with nearest neighbor matching", {
  test_data <- setup_test_data()

  result <- rwe_external_control(
    external_data = test_data$external,
    treatment_data = test_data$treatment,
    matching_vars = c("age", "baseline_score"),
    method = "nearest",
    ratio = 1,
    seed = 123
  )

  expect_s3_class(result, "rwe_external_control")
  expect_equal(result$method, "nearest")
  expect_true(nrow(result$matched_controls) > 0)
})


test_that("rwe_external_control applies inclusion criteria", {
  test_data <- setup_test_data()

  result <- rwe_external_control(
    external_data = test_data$external,
    treatment_data = test_data$treatment,
    matching_vars = c("age", "sex"),
    method = "propensity",
    inclusion_criteria = list(
      age = c(40, 75),
      disease_stage = c("I", "II", "III")
    ),
    seed = 123
  )

  expect_s3_class(result, "rwe_external_control")
  expect_true(length(result$inclusion_summary) > 0)
  expect_true(all(result$matched_controls$age >= 40 & result$matched_controls$age <= 75, na.rm = TRUE))
})


test_that("rwe_external_control applies exclusion criteria", {
  test_data <- setup_test_data()

  result <- rwe_external_control(
    external_data = test_data$external,
    treatment_data = test_data$treatment,
    matching_vars = c("age", "sex"),
    method = "propensity",
    exclusion_criteria = list(
      prior_treatment = TRUE
    ),
    seed = 123
  )

  expect_s3_class(result, "rwe_external_control")
  expect_true(length(result$exclusion_summary) > 0)
  expect_true(all(result$matched_controls$prior_treatment == FALSE, na.rm = TRUE))
})


test_that("rwe_external_control works with both inclusion and exclusion", {
  test_data <- setup_test_data()

  result <- rwe_external_control(
    external_data = test_data$external,
    treatment_data = test_data$treatment,
    matching_vars = c("age", "sex"),
    method = "propensity",
    inclusion_criteria = list(age = c(40, 75)),
    exclusion_criteria = list(prior_treatment = TRUE),
    ratio = 1,
    seed = 123
  )

  expect_s3_class(result, "rwe_external_control")
  expect_true(result$n_eligible < result$n_external_original)
  expect_true(nrow(result$matched_controls) > 0)
})


test_that("rwe_external_control respects matching ratio", {
  test_data <- setup_test_data()

  result <- rwe_external_control(
    external_data = test_data$external,
    treatment_data = test_data$treatment,
    matching_vars = c("age", "sex"),
    method = "propensity",
    ratio = 2,
    seed = 123
  )

  expect_true(result$matching_ratio >= 1.5)  # Should be close to 2
  expect_true(result$n_controls <= result$n_treatment * 2 + 5)  # Allow some slack
})


test_that("rwe_external_control calculates balance improvement", {
  test_data <- setup_test_data()

  result <- rwe_external_control(
    external_data = test_data$external,
    treatment_data = test_data$treatment,
    matching_vars = c("age", "baseline_score"),
    method = "propensity",
    seed = 123
  )

  expect_true("balance_assessment" %in% names(result))
  expect_true("before" %in% names(result$balance_assessment))
  expect_true("after" %in% names(result$balance_assessment))
  expect_true("improvement" %in% names(result$balance_assessment))
})


test_that("rwe_external_control handles rwe_data objects", {
  test_data <- setup_test_data()

  external_rwe <- structure(
    list(data = test_data$external),
    class = c("rwe_data", "rwe_ehr")
  )

  treatment_rwe <- structure(
    list(data = test_data$treatment),
    class = c("rwe_data", "rwe_ehr")
  )

  result <- rwe_external_control(
    external_data = external_rwe,
    treatment_data = treatment_rwe,
    matching_vars = c("age", "sex"),
    method = "propensity",
    seed = 123
  )

  expect_s3_class(result, "rwe_external_control")
  expect_true(nrow(result$matched_controls) > 0)
})


test_that("rwe_external_control validates inputs", {
  test_data <- setup_test_data()

  expect_error(
    rwe_external_control(
      external_data = test_data$external,
      treatment_data = test_data$treatment,
      matching_vars = c("nonexistent_var"),
      method = "propensity"
    )
  )
})


test_that("rwe_external_control creates audit trail", {
  test_data <- setup_test_data()

  result <- rwe_external_control(
    external_data = test_data$external,
    treatment_data = test_data$treatment,
    matching_vars = c("age", "sex"),
    method = "propensity",
    seed = 123
  )

  expect_true("audit_trail" %in% names(result))
  expect_s3_class(result$audit_trail, "rwe_audit_trail")
})


test_that("print.rwe_external_control works", {
  test_data <- setup_test_data()

  result <- rwe_external_control(
    external_data = test_data$external,
    treatment_data = test_data$treatment,
    matching_vars = c("age", "sex"),
    method = "propensity",
    seed = 123
  )

  expect_output(print(result), "External Control Arm")
  expect_output(print(result), "Matching Summary")
})


test_that("summary.rwe_external_control works", {
  test_data <- setup_test_data()

  result <- rwe_external_control(
    external_data = test_data$external,
    treatment_data = test_data$treatment,
    matching_vars = c("age", "sex"),
    method = "propensity",
    seed = 123
  )

  expect_output(summary(result), "Detailed Summary")
  expect_output(summary(result), "Covariate Balance")
})


# Study Design Tests ------------------------------------------------------

test_that("rwe_design_study creates study design template", {
  design <- rwe_design_study(
    study_type = "external_control",
    study_title = "Test RWE Study",
    population_definition = list(
      disease = "Cancer",
      age_range = c(18, 85)
    ),
    primary_endpoint = list(
      type = "time_to_event",
      variable = "overall_survival",
      timepoint = 365
    ),
    follow_up_duration = 730,
    index_date_definition = "First treatment date"
  )

  expect_s3_class(design, "rwe_study_design")
  expect_s3_class(design, "rwe_analysis")
  expect_equal(design$study_info$type, "external_control")
  expect_equal(design$study_info$title, "Test RWE Study")
})


test_that("rwe_design_study requires mandatory parameters", {
  expect_error(
    rwe_design_study(
      study_type = "cohort",
      study_title = "Test"
    ),
    "population_definition is required"
  )

  expect_error(
    rwe_design_study(
      study_type = "cohort",
      study_title = "Test",
      population_definition = list(disease = "Cancer")
    ),
    "primary_endpoint is required"
  )
})


test_that("rwe_design_study handles inclusion criteria", {
  design <- rwe_design_study(
    study_type = "cohort",
    study_title = "Test Study",
    population_definition = list(disease = "Cancer"),
    primary_endpoint = list(type = "continuous", variable = "response"),
    follow_up_duration = 365,
    index_date_definition = "Diagnosis date",
    inclusion_criteria = list(
      confirmed_diagnosis = TRUE,
      age = c(18, 75)
    )
  )

  expect_equal(design$population$n_inclusion_criteria, 2)
  expect_true("confirmed_diagnosis" %in% names(design$population$inclusion_criteria))
})


test_that("rwe_design_study handles exclusion criteria", {
  design <- rwe_design_study(
    study_type = "cohort",
    study_title = "Test Study",
    population_definition = list(disease = "Cancer"),
    primary_endpoint = list(type = "binary", variable = "response"),
    follow_up_duration = 365,
    index_date_definition = "Diagnosis date",
    exclusion_criteria = list(
      prior_treatment = TRUE,
      missing_data = TRUE
    )
  )

  expect_equal(design$population$n_exclusion_criteria, 2)
  expect_true("prior_treatment" %in% names(design$population$exclusion_criteria))
})


test_that("rwe_design_study handles treatment definition", {
  design <- rwe_design_study(
    study_type = "external_control",
    study_title = "Test Study",
    population_definition = list(disease = "Cancer"),
    treatment_definition = list(
      drug = "Drug X",
      route = "oral",
      min_exposure = 30
    ),
    primary_endpoint = list(type = "time_to_event", variable = "survival"),
    follow_up_duration = 730,
    index_date_definition = "First dose"
  )

  expect_true(!is.null(design$treatment))
  expect_equal(design$treatment$drug, "Drug X")
  expect_true(design$design_summary$has_treatment_definition)
})


test_that("rwe_design_study handles secondary endpoints", {
  design <- rwe_design_study(
    study_type = "cohort",
    study_title = "Test Study",
    population_definition = list(disease = "Cancer"),
    primary_endpoint = list(type = "time_to_event", variable = "survival"),
    secondary_endpoints = list(
      list(type = "continuous", variable = "quality_of_life"),
      list(type = "binary", variable = "adverse_events")
    ),
    follow_up_duration = 365,
    index_date_definition = "Enrollment"
  )

  expect_equal(design$endpoints$n_secondary, 2)
  expect_equal(length(design$endpoints$secondary), 2)
})


test_that("rwe_design_study handles propensity score variables", {
  design <- rwe_design_study(
    study_type = "external_control",
    study_title = "Test Study",
    population_definition = list(disease = "Cancer"),
    primary_endpoint = list(type = "time_to_event", variable = "survival"),
    follow_up_duration = 365,
    index_date_definition = "Treatment start",
    propensity_score_vars = c("age", "sex", "disease_stage", "comorbidities")
  )

  expect_true(design$design_summary$has_propensity_score)
  expect_equal(design$design_summary$n_ps_variables, 4)
  expect_equal(length(design$analysis_plan$propensity_score_vars), 4)
})


test_that("rwe_design_study handles sensitivity analyses", {
  design <- rwe_design_study(
    study_type = "cohort",
    study_title = "Test Study",
    population_definition = list(disease = "Cancer"),
    primary_endpoint = list(type = "time_to_event", variable = "survival"),
    follow_up_duration = 365,
    index_date_definition = "Enrollment",
    sensitivity_analyses = list(
      "Restrict to complete cases",
      "Alternative imputation method",
      "Different follow-up cutoff"
    )
  )

  expect_equal(design$analysis_plan$n_sensitivity, 3)
  expect_equal(length(design$analysis_plan$sensitivity_analyses), 3)
})


test_that("rwe_design_study handles subgroup analyses", {
  design <- rwe_design_study(
    study_type = "cohort",
    study_title = "Test Study",
    population_definition = list(disease = "Cancer"),
    primary_endpoint = list(type = "time_to_event", variable = "survival"),
    follow_up_duration = 365,
    index_date_definition = "Enrollment",
    subgroup_analyses = list(
      "By age group (<65 vs >=65)",
      "By disease stage",
      "By sex"
    )
  )

  expect_equal(design$analysis_plan$n_subgroups, 3)
  expect_equal(length(design$analysis_plan$subgroup_analyses), 3)
})


test_that("rwe_design_study sets protocol version", {
  design <- rwe_design_study(
    study_type = "cohort",
    study_title = "Test Study",
    population_definition = list(disease = "Cancer"),
    primary_endpoint = list(type = "binary", variable = "response"),
    follow_up_duration = 365,
    index_date_definition = "Enrollment",
    protocol_version = "2.1"
  )

  expect_equal(design$study_info$protocol_version, "2.1")
})


test_that("rwe_design_study creates audit trail", {
  design <- rwe_design_study(
    study_type = "cohort",
    study_title = "Test Study",
    population_definition = list(disease = "Cancer"),
    primary_endpoint = list(type = "binary", variable = "response"),
    follow_up_duration = 365,
    index_date_definition = "Enrollment"
  )

  expect_true("audit_trail" %in% names(design))
  expect_s3_class(design$audit_trail, "rwe_audit_trail")
})


test_that("print.rwe_study_design works", {
  design <- rwe_design_study(
    study_type = "cohort",
    study_title = "Test Study",
    population_definition = list(disease = "Cancer"),
    primary_endpoint = list(type = "binary", variable = "response"),
    follow_up_duration = 365,
    index_date_definition = "Enrollment"
  )

  expect_output(print(design), "RWE Study Design")
  expect_output(print(design), "Test Study")
})


test_that("summary.rwe_study_design works", {
  design <- rwe_design_study(
    study_type = "external_control",
    study_title = "Test Study",
    population_definition = list(disease = "Cancer"),
    treatment_definition = list(drug = "Drug X"),
    primary_endpoint = list(type = "time_to_event", variable = "survival"),
    follow_up_duration = 365,
    index_date_definition = "Treatment start",
    inclusion_criteria = list(confirmed = TRUE),
    exclusion_criteria = list(prior_treatment = TRUE)
  )

  expect_output(summary(design), "Detailed Summary")
  expect_output(summary(design), "POPULATION DEFINITION")
  expect_output(summary(design), "ENDPOINTS")
})


test_that("rwe_design_study handles all study types", {
  study_types <- c("cohort", "case_control", "external_control", "registry")

  for (type in study_types) {
    design <- rwe_design_study(
      study_type = type,
      study_title = paste("Test", type, "Study"),
      population_definition = list(disease = "Cancer"),
      primary_endpoint = list(type = "binary", variable = "response"),
      follow_up_duration = 365,
      index_date_definition = "Enrollment"
    )

    expect_equal(design$study_info$type, type)
  }
})


test_that("rwe_design_study stores design summary", {
  design <- rwe_design_study(
    study_type = "cohort",
    study_title = "Test Study",
    population_definition = list(disease = "Cancer"),
    primary_endpoint = list(type = "time_to_event", variable = "survival"),
    secondary_endpoints = list(
      list(type = "binary", variable = "response")
    ),
    follow_up_duration = 365,
    index_date_definition = "Enrollment",
    inclusion_criteria = list(age = c(18, 75)),
    exclusion_criteria = list(prior_treatment = TRUE),
    propensity_score_vars = c("age", "sex")
  )

  expect_true("design_summary" %in% names(design))
  expect_equal(design$design_summary$study_type, "cohort")
  expect_equal(design$design_summary$endpoint_type, "time_to_event")
  expect_equal(design$design_summary$n_inclusion_criteria, 1)
  expect_equal(design$design_summary$n_exclusion_criteria, 1)
  expect_equal(design$design_summary$n_secondary_endpoints, 1)
  expect_true(design$design_summary$has_propensity_score)
})


test_that("rwe_design_study handles sample size target", {
  design <- rwe_design_study(
    study_type = "cohort",
    study_title = "Test Study",
    population_definition = list(disease = "Cancer"),
    primary_endpoint = list(type = "binary", variable = "response"),
    follow_up_duration = 365,
    index_date_definition = "Enrollment",
    sample_size_target = 500
  )

  expect_equal(design$study_info$sample_size_target, 500)
})


test_that("rwe_design_study sets creation date", {
  design <- rwe_design_study(
    study_type = "cohort",
    study_title = "Test Study",
    population_definition = list(disease = "Cancer"),
    primary_endpoint = list(type = "binary", variable = "response"),
    follow_up_duration = 365,
    index_date_definition = "Enrollment"
  )

  expect_true("creation_date" %in% names(design$study_info))
  expect_s3_class(design$study_info$creation_date, "Date")
  expect_equal(design$study_info$creation_date, Sys.Date())
})


# Export function tests ---------------------------------------------------

test_that("export_study_design validates input", {
  expect_error(
    export_study_design("not a design", "output.txt"),
    "design must be an rwe_study_design object"
  )
})


test_that("export_study_design detects format from extension", {
  skip_if_not_installed("jsonlite")

  design <- rwe_design_study(
    study_type = "cohort",
    study_title = "Test Study",
    population_definition = list(disease = "Cancer"),
    primary_endpoint = list(type = "binary", variable = "response"),
    follow_up_duration = 365,
    index_date_definition = "Enrollment"
  )

  temp_file <- tempfile(fileext = ".txt")
  expect_output(export_study_design(design, temp_file), "exported to")
  expect_true(file.exists(temp_file))
  unlink(temp_file)
})
