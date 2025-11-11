# Tests for Regulatory Reporting Module

# Setup test data ---------------------------------------------------------

setup_test_data <- function() {
  set.seed(456)
  n <- 200

  data.frame(
    patient_id = 1:n,
    treatment = sample(c("Treatment", "Control"), n, replace = TRUE),
    age = rnorm(n, 60, 12),
    sex = sample(c("M", "F"), n, replace = TRUE),
    bmi = rnorm(n, 27, 5),
    disease_stage = sample(c("I", "II", "III", "IV"), n, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)),
    comorbidities = rpois(n, 2),
    smoker = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.3, 0.7)),
    baseline_score = rnorm(n, 50, 15)
  )
}


# Table One Tests ---------------------------------------------------------

test_that("rwe_table_one creates baseline characteristics table", {
  test_data <- setup_test_data()

  table1 <- rwe_table_one(
    data = test_data,
    group_var = "treatment",
    variables = c("age", "sex", "bmi", "disease_stage"),
    test = TRUE,
    smd = TRUE
  )

  expect_s3_class(table1, "rwe_table_one")
  expect_s3_class(table1, "rwe_analysis")
  expect_true("summary" %in% names(table1))
  expect_true("tests" %in% names(table1))
  expect_true("smd" %in% names(table1))
})


test_that("rwe_table_one auto-detects variable types", {
  test_data <- setup_test_data()

  table1 <- rwe_table_one(
    data = test_data,
    group_var = "treatment",
    variables = c("age", "sex", "bmi", "smoker")
  )

  expect_true(length(table1$continuous_vars) > 0)
  expect_true(length(table1$categorical_vars) > 0)
})


test_that("rwe_table_one calculates summary statistics", {
  test_data <- setup_test_data()

  table1 <- rwe_table_one(
    data = test_data,
    group_var = "treatment",
    variables = c("age", "sex")
  )

  expect_true("age" %in% names(table1$summary))
  expect_true("sex" %in% names(table1$summary))

  # Check continuous variable stats
  age_stats <- table1$summary$age
  expect_true(all(c("mean", "sd") %in% names(age_stats[[1]])))
})


test_that("rwe_table_one performs statistical tests", {
  test_data <- setup_test_data()

  table1 <- rwe_table_one(
    data = test_data,
    group_var = "treatment",
    variables = c("age", "sex"),
    test = TRUE
  )

  expect_true(length(table1$tests) > 0)
  expect_true("age" %in% names(table1$tests))
  expect_true("p_value" %in% names(table1$tests$age))
})


test_that("rwe_table_one calculates SMD", {
  test_data <- setup_test_data()

  table1 <- rwe_table_one(
    data = test_data,
    group_var = "treatment",
    variables = c("age", "bmi"),
    smd = TRUE
  )

  expect_true(length(table1$smd) > 0)
  expect_true(is.numeric(table1$smd$age))
})


test_that("rwe_table_one handles categorical variables", {
  test_data <- setup_test_data()

  table1 <- rwe_table_one(
    data = test_data,
    group_var = "treatment",
    variables = c("sex", "disease_stage"),
    categorical_vars = c("sex", "disease_stage")
  )

  expect_true("sex" %in% names(table1$summary))
  sex_stats <- table1$summary$sex
  expect_true("counts" %in% names(sex_stats[[1]]))
  expect_true("proportions" %in% names(sex_stats[[1]]))
})


test_that("rwe_table_one handles rwe_data objects", {
  test_data <- setup_test_data()

  rwe_data_obj <- structure(
    list(data = test_data),
    class = c("rwe_data", "rwe_ehr")
  )

  table1 <- rwe_table_one(
    data = rwe_data_obj,
    group_var = "treatment",
    variables = c("age", "sex")
  )

  expect_s3_class(table1, "rwe_table_one")
})


test_that("rwe_table_one validates inputs", {
  test_data <- setup_test_data()

  expect_error(
    rwe_table_one(
      data = test_data,
      group_var = "nonexistent",
      variables = c("age")
    ),
    "not found in data"
  )

  expect_error(
    rwe_table_one(
      data = test_data,
      group_var = "treatment",
      variables = c("nonexistent")
    ),
    "not found in data"
  )
})


test_that("rwe_table_one creates audit trail", {
  test_data <- setup_test_data()

  table1 <- rwe_table_one(
    data = test_data,
    group_var = "treatment",
    variables = c("age", "sex")
  )

  expect_true("audit_trail" %in% names(table1))
  expect_s3_class(table1$audit_trail, "rwe_audit_trail")
})


test_that("print.rwe_table_one works", {
  test_data <- setup_test_data()

  table1 <- rwe_table_one(
    data = test_data,
    group_var = "treatment",
    variables = c("age", "sex")
  )

  expect_output(print(table1), "Table 1")
  expect_output(print(table1), "Baseline Characteristics")
})


test_that("summary.rwe_table_one works", {
  test_data <- setup_test_data()

  table1 <- rwe_table_one(
    data = test_data,
    group_var = "treatment",
    variables = c("age", "sex")
  )

  expect_output(summary(table1), "Detailed Summary")
  expect_output(summary(table1), "Variable")
})


test_that("export_table_one validates input", {
  expect_error(
    export_table_one("not a table", "output.html"),
    "must be an rwe_table_one object"
  )
})


test_that("export_table_one to HTML works", {
  test_data <- setup_test_data()

  table1 <- rwe_table_one(
    data = test_data,
    group_var = "treatment",
    variables = c("age", "sex")
  )

  temp_file <- tempfile(fileext = ".html")
  export_table_one(table1, temp_file)

  expect_true(file.exists(temp_file))
  expect_true(file.size(temp_file) > 0)

  # Check HTML content
  content <- readLines(temp_file)
  expect_true(any(grepl("<table>", content)))
  expect_true(any(grepl("Table 1", content)))

  unlink(temp_file)
})


test_that("export_table_one to CSV works", {
  test_data <- setup_test_data()

  table1 <- rwe_table_one(
    data = test_data,
    group_var = "treatment",
    variables = c("age", "sex")
  )

  temp_file <- tempfile(fileext = ".csv")
  export_table_one(table1, temp_file)

  expect_true(file.exists(temp_file))
  expect_true(file.size(temp_file) > 0)

  unlink(temp_file)
})


# CONSORT Diagram Tests ---------------------------------------------------

test_that("rwe_consort_diagram creates flow diagram", {
  consort <- rwe_consort_diagram(
    screened = 500,
    enrolled = 350,
    allocated = list("Treatment" = 175, "Control" = 175),
    analyzed = list("Treatment" = 165, "Control" = 160)
  )

  expect_s3_class(consort, "rwe_consort")
  expect_s3_class(consort, "rwe_analysis")
  expect_equal(consort$screened, 500)
  expect_equal(consort$enrolled, 350)
})


test_that("rwe_consort_diagram handles exclusions", {
  consort <- rwe_consort_diagram(
    screened = 500,
    enrolled = 350,
    excluded = list(
      "Did not meet criteria" = 100,
      "Declined" = 50
    ),
    allocated = list("Treatment" = 175, "Control" = 175),
    analyzed = list("Treatment" = 165, "Control" = 160)
  )

  expect_true(!is.null(consort$excluded))
  expect_equal(sum(unlist(consort$excluded)), 150)
})


test_that("rwe_consort_diagram handles lost to follow-up", {
  consort <- rwe_consort_diagram(
    screened = 500,
    enrolled = 350,
    allocated = list("Treatment" = 175, "Control" = 175),
    lost_to_followup = list("Treatment" = 5, "Control" = 10),
    analyzed = list("Treatment" = 165, "Control" = 160)
  )

  expect_true(!is.null(consort$lost_to_followup))
  expect_equal(consort$lost_to_followup$Treatment, 5)
  expect_equal(consort$lost_to_followup$Control, 10)
})


test_that("rwe_consort_diagram handles discontinued patients", {
  consort <- rwe_consort_diagram(
    screened = 500,
    enrolled = 350,
    allocated = list("Treatment" = 175, "Control" = 175),
    discontinued = list(
      "Treatment" = list("Adverse events" = 3, "Withdrew" = 2),
      "Control" = list("Adverse events" = 1, "Withdrew" = 4)
    ),
    analyzed = list("Treatment" = 165, "Control" = 160)
  )

  expect_true(!is.null(consort$discontinued))
  expect_equal(sum(unlist(consort$discontinued$Treatment)), 5)
  expect_equal(sum(unlist(consort$discontinued$Control)), 5)
})


test_that("rwe_consort_diagram calculates retention rates", {
  consort <- rwe_consort_diagram(
    screened = 500,
    enrolled = 400,
    allocated = list("Treatment" = 200, "Control" = 200),
    analyzed = list("Treatment" = 180, "Control" = 190)
  )

  expect_true(!is.null(consort$retention_rates))
  expect_equal(consort$retention_rates$Treatment, 90)
  expect_equal(consort$retention_rates$Control, 95)
})


test_that("rwe_consort_diagram calculates overall attrition", {
  consort <- rwe_consort_diagram(
    screened = 500,
    enrolled = 400,
    allocated = list("Treatment" = 200, "Control" = 200),
    analyzed = list("Treatment" = 180, "Control" = 160)
  )

  expected_attrition <- ((400 - 340) / 400) * 100
  expect_equal(consort$overall_attrition, expected_attrition)
})


test_that("rwe_consort_diagram validates inputs", {
  expect_error(
    rwe_consort_diagram(
      screened = 100,
      enrolled = 200,
      allocated = list("Treatment" = 100, "Control" = 100),
      analyzed = list("Treatment" = 90, "Control" = 90)
    ),
    "Enrolled cannot exceed screened"
  )
})


test_that("rwe_consort_diagram handles multiple arms", {
  consort <- rwe_consort_diagram(
    screened = 600,
    enrolled = 450,
    allocated = list("Treatment A" = 150, "Treatment B" = 150, "Control" = 150),
    analyzed = list("Treatment A" = 140, "Treatment B" = 135, "Control" = 145)
  )

  expect_equal(consort$n_arms, 3)
  expect_equal(length(consort$retention_rates), 3)
})


test_that("rwe_consort_diagram creates audit trail", {
  consort <- rwe_consort_diagram(
    screened = 500,
    enrolled = 350,
    allocated = list("Treatment" = 175, "Control" = 175),
    analyzed = list("Treatment" = 165, "Control" = 160)
  )

  expect_true("audit_trail" %in% names(consort))
  expect_s3_class(consort$audit_trail, "rwe_audit_trail")
})


test_that("print.rwe_consort works", {
  consort <- rwe_consort_diagram(
    screened = 500,
    enrolled = 350,
    allocated = list("Treatment" = 175, "Control" = 175),
    analyzed = list("Treatment" = 165, "Control" = 160)
  )

  expect_output(print(consort), "CONSORT Flow Diagram")
  expect_output(print(consort), "Screened")
})


test_that("summary.rwe_consort works", {
  consort <- rwe_consort_diagram(
    screened = 500,
    enrolled = 350,
    excluded = list("Did not meet criteria" = 150),
    allocated = list("Treatment" = 175, "Control" = 175),
    lost_to_followup = list("Treatment" = 5, "Control" = 10),
    analyzed = list("Treatment" = 165, "Control" = 160)
  )

  expect_output(summary(consort), "Detailed Summary")
  expect_output(summary(consort), "ENROLLMENT")
  expect_output(summary(consort), "ALLOCATION")
  expect_output(summary(consort), "ANALYSIS")
})


test_that("rwe_consort_diagram handles different study types", {
  study_types <- c("rct", "cohort", "external_control")

  for (type in study_types) {
    consort <- rwe_consort_diagram(
      screened = 500,
      enrolled = 350,
      allocated = list("Treatment" = 175, "Control" = 175),
      analyzed = list("Treatment" = 165, "Control" = 160),
      study_type = type
    )

    expect_equal(consort$study_type, type)
  }
})


test_that("rwe_table_one handles missing values", {
  test_data <- setup_test_data()
  test_data$age[1:10] <- NA
  test_data$sex[5:15] <- NA

  table1 <- rwe_table_one(
    data = test_data,
    group_var = "treatment",
    variables = c("age", "sex"),
    missing = TRUE
  )

  expect_s3_class(table1, "rwe_table_one")
  expect_true("summary" %in% names(table1))
})


test_that("rwe_table_one works with more than 2 groups", {
  test_data <- setup_test_data()
  test_data$treatment <- sample(c("A", "B", "C"), nrow(test_data), replace = TRUE)

  table1 <- rwe_table_one(
    data = test_data,
    group_var = "treatment",
    variables = c("age", "bmi"),
    test = TRUE
  )

  expect_equal(table1$n_groups, 3)
  expect_equal(length(table1$groups), 3)
})


# Safety Report Tests -----------------------------------------------------

setup_safety_data <- function() {
  set.seed(789)
  n <- 300

  data.frame(
    patient_id = rep(1:100, each = 3),
    treatment = rep(sample(c("Treatment", "Control"), 100, replace = TRUE), each = 3),
    ae_term = sample(
      c("Nausea", "Headache", "Fatigue", "Dizziness", "Rash", "Insomnia",
        "Diarrhea", "Pain", "Fever", "Cough"),
      n, replace = TRUE
    ),
    severity = sample(c("Mild", "Moderate", "Severe"), n, replace = TRUE,
                      prob = c(0.6, 0.3, 0.1)),
    relatedness = sample(c("Related", "Possibly Related", "Unrelated"), n,
                        replace = TRUE, prob = c(0.3, 0.4, 0.3)),
    serious = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.1, 0.9))
  )
}


test_that("rwe_safety_report creates basic safety report", {
  safety_data <- setup_safety_data()

  report <- rwe_safety_report(
    data = safety_data,
    group_var = "treatment",
    ae_var = "ae_term"
  )

  expect_s3_class(report, "rwe_safety_report")
  expect_s3_class(report, "rwe_analysis")
  expect_true("summary" %in% names(report))
  expect_true("ae_counts" %in% names(report))
  expect_true("groups" %in% names(report))
})


test_that("rwe_safety_report tracks serious adverse events", {
  safety_data <- setup_safety_data()

  report <- rwe_safety_report(
    data = safety_data,
    group_var = "treatment",
    ae_var = "ae_term",
    serious_var = "serious"
  )

  expect_true("serious_ae" %in% names(report))
  expect_true(!is.null(report$serious_ae))
  expect_true(is.list(report$serious_ae))
})


test_that("rwe_safety_report summarizes by severity", {
  safety_data <- setup_safety_data()

  report <- rwe_safety_report(
    data = safety_data,
    group_var = "treatment",
    ae_var = "ae_term",
    severity_var = "severity"
  )

  expect_true("severity_summary" %in% names(report))
  expect_true(!is.null(report$severity_summary))
  expect_true(all(c("Mild", "Moderate", "Severe") %in% names(report$severity_summary[[1]])))
})


test_that("rwe_safety_report calculates AE rates per patient", {
  safety_data <- setup_safety_data()

  n_per_group <- list("Treatment" = 50, "Control" = 50)

  report <- rwe_safety_report(
    data = safety_data,
    group_var = "treatment",
    ae_var = "ae_term",
    n_per_group = n_per_group
  )

  expect_true("ae_rates" %in% names(report))
  expect_true(!is.null(report$ae_rates))
  expect_true(all(report$ae_rates$Treatment >= 0))
})


test_that("rwe_safety_report sorts by frequency", {
  safety_data <- setup_safety_data()

  report <- rwe_safety_report(
    data = safety_data,
    group_var = "treatment",
    ae_var = "ae_term",
    sort_by = "frequency"
  )

  expect_s3_class(report, "rwe_safety_report")
  expect_equal(report$sort_by, "frequency")
})


test_that("rwe_safety_report limits top N AEs", {
  safety_data <- setup_safety_data()

  report <- rwe_safety_report(
    data = safety_data,
    group_var = "treatment",
    ae_var = "ae_term",
    top_n = 5
  )

  expect_true(!is.null(report$top_n))
  expect_equal(report$top_n, 5)
})


test_that("rwe_safety_report validates inputs", {
  safety_data <- setup_safety_data()

  expect_error(
    rwe_safety_report(
      data = safety_data,
      group_var = "nonexistent",
      ae_var = "ae_term"
    ),
    "not found in data"
  )

  expect_error(
    rwe_safety_report(
      data = safety_data,
      group_var = "treatment",
      ae_var = "nonexistent"
    ),
    "not found in data"
  )
})


test_that("rwe_safety_report handles rwe_data objects", {
  safety_data <- setup_safety_data()

  rwe_data_obj <- structure(
    list(data = safety_data),
    class = c("rwe_data", "rwe_ehr")
  )

  report <- rwe_safety_report(
    data = rwe_data_obj,
    group_var = "treatment",
    ae_var = "ae_term"
  )

  expect_s3_class(report, "rwe_safety_report")
})


test_that("rwe_safety_report creates audit trail", {
  safety_data <- setup_safety_data()

  report <- rwe_safety_report(
    data = safety_data,
    group_var = "treatment",
    ae_var = "ae_term"
  )

  expect_true("audit_trail" %in% names(report))
  expect_s3_class(report$audit_trail, "rwe_audit_trail")
})


test_that("print.rwe_safety_report works", {
  safety_data <- setup_safety_data()

  report <- rwe_safety_report(
    data = safety_data,
    group_var = "treatment",
    ae_var = "ae_term"
  )

  expect_output(print(report), "Safety Report")
  expect_output(print(report), "Treatment Groups")
})


test_that("summary.rwe_safety_report works", {
  safety_data <- setup_safety_data()

  n_per_group <- list("Treatment" = 50, "Control" = 50)

  report <- rwe_safety_report(
    data = safety_data,
    group_var = "treatment",
    ae_var = "ae_term",
    severity_var = "severity",
    serious_var = "serious",
    n_per_group = n_per_group
  )

  expect_output(summary(report), "Safety Report - Detailed Summary")
  expect_output(summary(report), "OVERALL ADVERSE EVENT RATES")
  expect_output(summary(report), "SERIOUS ADVERSE EVENTS")
  expect_output(summary(report), "ADVERSE EVENTS BY SEVERITY")
})


test_that("rwe_safety_report handles multiple treatment groups", {
  safety_data <- setup_safety_data()
  safety_data$treatment <- sample(c("A", "B", "C"), nrow(safety_data), replace = TRUE)

  report <- rwe_safety_report(
    data = safety_data,
    group_var = "treatment",
    ae_var = "ae_term"
  )

  expect_equal(length(report$groups), 3)
})


# Regulatory Report Tests -------------------------------------------------

test_that("rwe_regulatory_report creates comprehensive report", {
  test_data <- setup_test_data()
  safety_data <- setup_safety_data()

  table1 <- rwe_table_one(
    data = test_data,
    group_var = "treatment",
    variables = c("age", "sex")
  )

  consort <- rwe_consort_diagram(
    screened = 500,
    enrolled = 350,
    allocated = list("Treatment" = 175, "Control" = 175),
    analyzed = list("Treatment" = 165, "Control" = 160)
  )

  safety <- rwe_safety_report(
    data = safety_data,
    group_var = "treatment",
    ae_var = "ae_term"
  )

  report <- rwe_regulatory_report(
    study_title = "Phase III Clinical Trial",
    table_one = table1,
    consort = consort,
    safety = safety
  )

  expect_s3_class(report, "rwe_regulatory_report")
  expect_s3_class(report, "rwe_analysis")
  expect_equal(report$study_title, "Phase III Clinical Trial")
  expect_true(!is.null(report$table_one))
  expect_true(!is.null(report$consort))
  expect_true(!is.null(report$safety))
})


test_that("rwe_regulatory_report validates component classes", {
  expect_error(
    rwe_regulatory_report(
      study_title = "Test Study",
      table_one = "not a table"
    ),
    "table_one must be an rwe_table_one object"
  )

  expect_error(
    rwe_regulatory_report(
      study_title = "Test Study",
      consort = "not a consort"
    ),
    "consort must be an rwe_consort object"
  )

  expect_error(
    rwe_regulatory_report(
      study_title = "Test Study",
      safety = "not a safety report"
    ),
    "safety must be an rwe_safety_report object"
  )
})


test_that("rwe_regulatory_report handles NULL components", {
  report <- rwe_regulatory_report(
    study_title = "Test Study"
  )

  expect_s3_class(report, "rwe_regulatory_report")
  expect_true(is.null(report$table_one))
  expect_true(is.null(report$consort))
  expect_true(is.null(report$safety))
})


test_that("rwe_regulatory_report includes study info", {
  report <- rwe_regulatory_report(
    study_title = "Test Study",
    study_info = list(
      protocol = "PROTO-001",
      sponsor = "Test Pharma",
      indication = "Test Indication"
    )
  )

  expect_true(!is.null(report$study_info))
  expect_equal(report$study_info$protocol, "PROTO-001")
  expect_equal(report$study_info$sponsor, "Test Pharma")
})


test_that("rwe_regulatory_report includes efficacy results", {
  efficacy <- list(
    primary_endpoint = "Overall Survival",
    hr = 0.75,
    ci = c(0.60, 0.95),
    p_value = 0.015
  )

  report <- rwe_regulatory_report(
    study_title = "Test Study",
    efficacy_results = efficacy
  )

  expect_true(!is.null(report$efficacy_results))
  expect_equal(report$efficacy_results$primary_endpoint, "Overall Survival")
})


test_that("rwe_regulatory_report creates audit trail", {
  report <- rwe_regulatory_report(
    study_title = "Test Study"
  )

  expect_true("audit_trail" %in% names(report))
  expect_s3_class(report$audit_trail, "rwe_audit_trail")
})


test_that("rwe_regulatory_report records generation timestamp", {
  report <- rwe_regulatory_report(
    study_title = "Test Study"
  )

  expect_true("generated_at" %in% names(report))
  expect_true(inherits(report$generated_at, "POSIXct"))
})


test_that("print.rwe_regulatory_report works", {
  test_data <- setup_test_data()

  table1 <- rwe_table_one(
    data = test_data,
    group_var = "treatment",
    variables = c("age", "sex")
  )

  report <- rwe_regulatory_report(
    study_title = "Phase III Clinical Trial",
    table_one = table1
  )

  expect_output(print(report), "Regulatory Report")
  expect_output(print(report), "Phase III Clinical Trial")
  expect_output(print(report), "\\[x\\]")
})


test_that("export_regulatory_report validates input", {
  expect_error(
    export_regulatory_report("not a report", "output.html"),
    "report must be an rwe_regulatory_report object"
  )
})


test_that("export_regulatory_report writes html output", {
  skip_if_not_installed("htmltools")
  test_data <- setup_test_data()
  table1 <- rwe_table_one(
    data = test_data,
    group_var = "treatment",
    variables = c("age", "sex")
  )

  report <- rwe_regulatory_report(
    study_title = "Export Test",
    table_one = table1
  )

  tmp <- tempfile(fileext = ".html")
  export_regulatory_report(report, tmp, include_tables = TRUE, include_figures = FALSE)
  expect_true(file.exists(tmp))
  contents <- readLines(tmp, warn = FALSE)
  expect_true(any(grepl("Export Test", contents, fixed = TRUE)))
})


test_that("rwe_regulatory_report handles all components together", {
  test_data <- setup_test_data()
  safety_data <- setup_safety_data()

  table1 <- rwe_table_one(
    data = test_data,
    group_var = "treatment",
    variables = c("age", "sex", "bmi")
  )

  consort <- rwe_consort_diagram(
    screened = 500,
    enrolled = 400,
    allocated = list("Treatment" = 200, "Control" = 200),
    analyzed = list("Treatment" = 185, "Control" = 180)
  )

  safety <- rwe_safety_report(
    data = safety_data,
    group_var = "treatment",
    ae_var = "ae_term",
    severity_var = "severity",
    serious_var = "serious"
  )

  efficacy <- list(
    primary_endpoint = "Progression-Free Survival",
    hr = 0.68,
    ci = c(0.52, 0.89),
    p_value = 0.004
  )

  study_info <- list(
    protocol = "STUDY-2024-001",
    sponsor = "Test Pharmaceutical Inc.",
    indication = "Advanced Cancer",
    study_phase = "Phase III"
  )

  report <- rwe_regulatory_report(
    study_title = "Efficacy and Safety Study",
    table_one = table1,
    consort = consort,
    safety = safety,
    efficacy_results = efficacy,
    study_info = study_info
  )

  expect_s3_class(report, "rwe_regulatory_report")
  expect_equal(report$study_title, "Efficacy and Safety Study")
  expect_s3_class(report$table_one, "rwe_table_one")
  expect_s3_class(report$consort, "rwe_consort")
  expect_s3_class(report$safety, "rwe_safety_report")
  expect_equal(report$efficacy_results$hr, 0.68)
  expect_equal(report$study_info$protocol, "STUDY-2024-001")
})


test_that("rwe_generate_regulatory_report assembles components", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("survival")
  skip_if_not_installed("zoo")

  set.seed(42)
  cohort <- data.frame(
    arm = sample(c("Drug", "Control"), 250, replace = TRUE),
    age = rnorm(250, 65, 8),
    bmi = rnorm(250, 28, 4),
    sex = sample(c("Male", "Female"), 250, replace = TRUE),
    smoker = sample(c("Yes", "No"), 250, replace = TRUE),
    time = rexp(250, rate = 0.05),
    event = rbinom(250, 1, 0.4)
  )

  cohort$treatment <- ifelse(cohort$arm == "Drug", 1, 0)

  effectiveness <- rwe_relative_risk(
    data = cohort,
    outcome = "event",
    treatment = "treatment",
    quiet = TRUE
  )

  km <- rwe_kaplan_meier(
    data = cohort,
    time_var = "time",
    event_var = "event",
    group_var = "arm"
  )

  subgroup <- rwe_subgroup_analysis(
    data = cohort,
    outcome = "event",
    treatment = "treatment",
    subgroup_vars = c("sex", "smoker"),
    quiet = TRUE
  )

  safety_df <- data.frame(
    treatment_flag = cohort$treatment,
    events = rpois(250, lambda = ifelse(cohort$treatment == 1, 3, 2)),
    person_time = runif(250, 0.8, 1.2),
    day = sample(1:250, 250, replace = TRUE)
  )

  safety_analysis <- rwe_analyze_safety(
    data = safety_df,
    events = "events",
    person_time = "person_time",
    treatment = "treatment_flag",
    time_var = "day",
    quiet = TRUE
  )

  ae_records <- data.frame(
    subject = rep(1:150, each = 2),
    treatment = sample(c("Drug", "Control"), 300, replace = TRUE),
    adverse_event = sample(c("Nausea", "Fatigue", "Headache", "Rash"), 300, replace = TRUE),
    severity = sample(c("Mild", "Moderate", "Severe"), 300, replace = TRUE),
    serious = sample(c(TRUE, FALSE), 300, replace = TRUE, prob = c(0.1, 0.9))
  )

  safety_report <- rwe_safety_report(
    data = ae_records,
    group_var = "treatment",
    ae_var = "adverse_event",
    severity_var = "severity",
    serious_var = "serious",
    n_per_group = table(ae_records$treatment),
    top_n = 5
  )

  consort <- rwe_consort_diagram(
    screened = 500,
    enrolled = 400,
    excluded = list("Not eligible" = 60, "Declined" = 40),
    allocated = list("Drug" = 200, "Control" = 200),
    lost_to_followup = list("Drug" = 15, "Control" = 12),
    discontinued = list(
      "Drug" = list("Adverse events" = 8),
      "Control" = list("Adverse events" = 5)
    ),
    analyzed = list("Drug" = 177, "Control" = 183)
  )

  report <- rwe_generate_regulatory_report(
    study_title = "Phase III Trial",
    cohort_data = cohort,
    treatment_var = "arm",
    baseline_vars = c("age", "bmi", "sex", "smoker"),
    effectiveness = effectiveness,
    survival = km,
    subgroup = subgroup,
    safety_analysis = safety_analysis,
    safety_report = safety_report,
    consort = consort
  )

  expect_s3_class(report, "rwe_regulatory_report")
  expect_true("baseline_table" %in% report$components)
  expect_true("effectiveness_table" %in% report$components)
  expect_equal(report$study_title, "Phase III Trial")
  expect_true(is.list(report$tables))
  if (!is.null(report$figures$subgroup_forest)) {
    expect_s3_class(report$figures$subgroup_forest, "ggplot")
  }
})
