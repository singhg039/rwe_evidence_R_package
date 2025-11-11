library(testthat)
library(rwevidence)

test_that("generate_mapping_config creates standardized targets", {
  source_data <- data.frame(
    PatientID = 1:5,
    diagnosis_code = c("E11.9", "I10", "J44.0", "E11.9", "I10"),
    stringsAsFactors = FALSE
  )

  config <- generate_mapping_config(source_data, "custom", "omop_cdm_5.4")

  expect_true("diagnosis_code" %in% names(config$column_mappings))
  expect_equal(config$column_mappings$diagnosis_code$target, "diagnosis_code")
})

test_that("apply_structural_mapping respects transform instructions", {
  source_data <- data.frame(
    DiagnosisCode = c("e11.9", "i10"),
    Description = c("type 2 diabetes", "hypertension"),
    stringsAsFactors = FALSE
  )

  mapping_config <- list(
    column_mappings = list(
      DiagnosisCode = list(target = "diagnosis_code", transform = "uppercase"),
      Description = list(target = "description", transform = "direct")
    )
  )

  transformed <- apply_structural_mapping(source_data, mapping_config, list())
  expect_equal(transformed$diagnosis_code, toupper(source_data$DiagnosisCode))
  expect_equal(transformed$description, source_data$Description)
})

test_that("apply_single_terminology_map creates mapped columns", {
  data <- data.frame(
    diagnosis_icd10 = c("E11.9", "I10", "UNKNOWN"),
    stringsAsFactors = FALSE
  )

  mapped <- apply_single_terminology_map(data, "icd10cm_to_snomed")

  expect_true("diagnosis_icd10_snomed" %in% names(mapped))
  expect_equal(mapped$diagnosis_icd10_snomed[1], "44054006")
  expect_true(is.na(mapped$diagnosis_icd10_snomed[3]))
})

test_that("apply_single_terminology_map can replace original column", {
  data <- data.frame(
    ndc_code = c("00009328401", "00023396001"),
    stringsAsFactors = FALSE
  )

  mapped <- apply_single_terminology_map(
    data,
    list(
      from = "NDC",
      to = "RxNorm",
      columns = "ndc_code",
      replace = TRUE
    )
  )

  expect_equal(mapped$ndc_code, c("617314", "617320"))
})

test_that("rwe_map_terminology uses standard tables", {
  codes <- c("E11.9", "I10", "UNKNOWN")

  result <- rwe_map_terminology(
    codes = codes,
    from_system = "ICD10CM",
    to_system = "SNOMED"
  )

  expect_equal(result$target_code[1], "44054006")
  expect_equal(result$target_code[2], "38341003")
  expect_true(is.na(result$target_code[3]))
  expect_equal(result$mapping_quality[3], "unmapped")
})

test_that("load_standard_mapping returns built-in tables", {
  table <- load_standard_mapping("ICD10CM", "SNOMED")
  expect_s3_class(table, "data.frame")
  expect_true(all(c("source_code", "target_code") %in% names(table)))
})

test_that("standardize_measurement_units converts known units", {
  data <- data.frame(
    hgb_value = c(13.5, 12.1),
    hgb_unit = c("g/dL", "g/dL"),
    pressure_value = c(120, 135),
    pressure_unit = c("mmHg", "mmHg"),
    stringsAsFactors = FALSE
  )

  standardized <- standardize_measurement_units(data, list())

  expect_equal(standardized$hgb_value, c(135, 121))
  expect_equal(unique(standardized$hgb_unit), "g/L")
  expect_true(all(abs(standardized$pressure_value - c(15.998684, 17.99852)) < 1e-3))
  expect_equal(unique(standardized$pressure_unit), "kPa")
})
