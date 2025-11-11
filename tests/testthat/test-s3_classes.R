test_that("new_rwe_data creates object with correct structure", {
  test_data <- data.frame(x = 1:5, y = letters[1:5])
  metadata <- list(source = "test")

  obj <- new_rwe_data(test_data, metadata)

  expect_s3_class(obj, "rwe_data")
  expect_true(is.list(obj))
  expect_named(obj, c("data", "metadata"))
  expect_equal(obj$data, test_data)
  expect_true("source" %in% names(obj$metadata))
  expect_true("created_at" %in% names(obj$metadata))
})


test_that("print.rwe_data displays correctly", {
  test_data <- data.frame(x = 1:5, y = letters[1:5])
  obj <- new_rwe_data(test_data, list(source = "test"))

  output <- capture.output(print(obj))

  expect_true(any(grepl("rwe_data", output)))
  expect_true(any(grepl("5 rows", output)))
  expect_true(any(grepl("2 columns", output)))
})


test_that("summary.rwe_data provides detailed information", {
  test_data <- data.frame(
    x = 1:5,
    y = letters[1:5],
    z = factor(c("a", "b", "a", "b", "c"))
  )
  obj <- new_rwe_data(test_data)

  output <- capture.output(summary(obj))

  expect_true(any(grepl("RWE Data Summary", output)))
  expect_true(any(grepl("Rows:", output)))
  expect_true(any(grepl("Columns:", output)))
})


test_that("new_rwe_ehr creates EHR object correctly", {
  test_data <- data.frame(patient_id = 1:5, age = c(25, 30, 45, 60, 55))
  metadata <- list(source = "hospital_ehr", format = "omop_cdm")
  schema <- list(patient_id = "integer", age = "numeric")

  ehr_obj <- new_rwe_ehr(test_data, metadata, schema)

  expect_s3_class(ehr_obj, "rwe_ehr")
  expect_s3_class(ehr_obj, "rwe_data")
  expect_equal(ehr_obj$data, test_data)
  expect_equal(ehr_obj$schema, schema)
  expect_equal(ehr_obj$metadata$format, "omop_cdm")
})


test_that("print.rwe_ehr displays EHR-specific information", {
  test_data <- data.frame(patient_id = 1:5, age = c(25, 30, 45, 60, 55))
  metadata <- list(format = "omop_cdm", version = "5.4")

  ehr_obj <- new_rwe_ehr(test_data, metadata)
  output <- capture.output(print(ehr_obj))

  expect_true(any(grepl("rwe_ehr", output)))
  expect_true(any(grepl("Format:", output)))
  expect_true(any(grepl("omop_cdm", output)))
})


test_that("new_rwe_quality_report creates report object", {
  data_info <- list(n_rows = 100, n_cols = 10)
  dimensions <- list(
    completeness = list(score = 0.85),
    validity = list(score = 0.92)
  )
  overall_score <- 0.885
  issues <- character()
  recommendations <- c("Review missing data patterns")

  report <- new_rwe_quality_report(
    data_info, dimensions, overall_score, issues, recommendations
  )

  expect_s3_class(report, "rwe_quality_report")
  expect_s3_class(report, "rwe_report")
  expect_equal(report$overall_score, 0.885)
  expect_length(report$dimension_scores, 2)
  expect_equal(report$dimension_scores[["completeness"]], 0.85)
})


test_that("print.rwe_quality_report displays quality information", {
  data_info <- list(n_rows = 100, n_cols = 10)
  dimensions <- list(
    completeness = list(score = 0.85),
    validity = list(score = 0.95)
  )

  report <- new_rwe_quality_report(
    data_info, dimensions, 0.90, character(), character()
  )

  output <- capture.output(print(report))

  expect_true(any(grepl("Quality Report", output)))
  expect_true(any(grepl("Overall Quality Score", output)))
  expect_true(any(grepl("0.9", output)))
  expect_true(any(grepl("Excellent|Good", output)))
})


test_that("new_rwe_harmonized creates harmonized object", {
  test_data <- data.frame(patient_id = 1:5, condition = c("A", "B", "C", "D", "E"))
  mapping_config <- list(condition = "condition_code")

  harm_obj <- new_rwe_harmonized(
    test_data,
    source_schema = "custom",
    target_schema = "omop_cdm_5.4",
    mapping_config = mapping_config
  )

  expect_s3_class(harm_obj, "rwe_harmonized")
  expect_s3_class(harm_obj, "rwe_transformed")
  expect_s3_class(harm_obj, "rwe_data")
  expect_equal(harm_obj$metadata$source_schema, "custom")
  expect_equal(harm_obj$metadata$target_schema, "omop_cdm_5.4")
  expect_equal(harm_obj$mapping_config, mapping_config)
})


test_that("print.rwe_harmonized displays harmonization information", {
  test_data <- data.frame(id = 1:3)

  harm_obj <- new_rwe_harmonized(
    test_data,
    source_schema = "custom",
    target_schema = "omop_cdm",
    mapping_config = list()
  )

  output <- capture.output(print(harm_obj))

  expect_true(any(grepl("rwe_harmonized", output)))
  expect_true(any(grepl("Source Schema:", output)))
  expect_true(any(grepl("Target Schema:", output)))
  expect_true(any(grepl("custom", output)))
  expect_true(any(grepl("omop_cdm", output)))
})


test_that("new_rwe_matched_cohort creates matched cohort object", {
  matched_data <- data.frame(
    id = 1:10,
    treatment = rep(c(0, 1), 5),
    age = rnorm(10, 50, 10)
  )

  balance_before <- data.frame(variable = "age", std_diff = 0.5)
  balance_after <- data.frame(variable = "age", std_diff = 0.05)

  cohort <- new_rwe_matched_cohort(
    matched_data,
    match_object = NULL,
    balance_before = balance_before,
    balance_after = balance_after
  )

  expect_s3_class(cohort, "rwe_matched_cohort")
  expect_s3_class(cohort, "rwe_cohort")
  expect_s3_class(cohort, "rwe_data")
  expect_equal(cohort$metadata$n_matched, 10)
})


test_that("print.rwe_matched_cohort displays balance information", {
  matched_data <- data.frame(id = 1:10, treatment = rep(c(0, 1), 5))
  balance_before <- data.frame(variable = "age", std_diff = 0.5)
  balance_after <- data.frame(variable = "age", std_diff = 0.05)

  cohort <- new_rwe_matched_cohort(
    matched_data, NULL, balance_before, balance_after
  )

  output <- capture.output(print(cohort))

  expect_true(any(grepl("Matched Cohort Analysis", output)))
  expect_true(any(grepl("Matched observations:", output)))
  expect_true(any(grepl("Balance Improvement", output)))
})


test_that("new_rwe_analysis creates analysis object", {
  primary <- list(estimate = 0.5, se = 0.1, p_value = 0.001)

  analysis <- new_rwe_analysis(primary, method = "cox")

  expect_s3_class(analysis, "rwe_analysis")
  expect_equal(analysis$method, "cox")
  expect_equal(analysis$primary_analysis, primary)
  expect_true("analyzed_at" %in% names(analysis))
})


test_that("get_quality_rating returns correct ratings", {
  expect_match(get_quality_rating(0.95), "Excellent")
  expect_match(get_quality_rating(0.85), "Good")
  expect_match(get_quality_rating(0.75), "Fair")
  expect_match(get_quality_rating(0.65), "Poor")
  expect_match(get_quality_rating(0.50), "Critical")
})


test_that("is_rwe_data checks class correctly", {
  test_data <- data.frame(x = 1:5)
  obj <- new_rwe_data(test_data)

  expect_true(is_rwe_data(obj))
  expect_false(is_rwe_data(test_data))
  expect_false(is_rwe_data(list()))
})


test_that("is_rwe_ehr checks class correctly", {
  test_data <- data.frame(x = 1:5)
  ehr_obj <- new_rwe_ehr(test_data)
  data_obj <- new_rwe_data(test_data)

  expect_true(is_rwe_ehr(ehr_obj))
  expect_false(is_rwe_ehr(data_obj))
  expect_false(is_rwe_ehr(test_data))
})


test_that("is_rwe_quality_report checks class correctly", {
  report <- new_rwe_quality_report(
    list(n_rows = 10),
    list(completeness = list(score = 0.9)),
    0.9,
    character(),
    character()
  )

  test_data <- data.frame(x = 1:5)

  expect_true(is_rwe_quality_report(report))
  expect_false(is_rwe_quality_report(test_data))
  expect_false(is_rwe_quality_report(list()))
})
