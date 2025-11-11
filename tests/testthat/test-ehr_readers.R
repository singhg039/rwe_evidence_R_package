test_that("rwe_read_ehr reads CSV files correctly", {
  # Create test data
  test_data <- data.frame(
    patient_id = 1:10,
    age = sample(20:80, 10, replace = TRUE),
    diagnosis_date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 10),
    stringsAsFactors = FALSE
  )

  # Write to temp file
  temp_file <- tempfile(fileext = ".csv")
  write.csv(test_data, temp_file, row.names = FALSE)

  # Test reading
  result <- rwe_read_ehr(
    source = temp_file,
    format = "custom",
    validate = FALSE
  )

  # Assertions
  expect_s3_class(result, "rwe_ehr")
  expect_s3_class(result, "rwe_data")
  expect_equal(nrow(result$data), 10)
  expect_named(result, c("data", "metadata", "schema", "audit_trail"))
  expect_equal(result$metadata$format, "custom")
  expect_true(!is.null(result$metadata$loaded_at))

  # Cleanup
  unlink(temp_file)
})


test_that("rwe_read_ehr validates required parameters", {
  expect_error(rwe_read_ehr(), "source must be provided")
})


test_that("rwe_read_ehr handles non-existent files", {
  expect_error(
    rwe_read_ehr(source = "non_existent_file.csv", format = "custom"),
    "File not found"
  )
})


test_that("rwe_read_ehr applies date range filter", {
  # Create test data with dates
  test_data <- data.frame(
    patient_id = 1:100,
    visit_date = seq.Date(as.Date("2020-01-01"), by = "day", length.out = 100)
  )

  temp_file <- tempfile(fileext = ".csv")
  write.csv(test_data, temp_file, row.names = FALSE)

  # Read with date filter
  result <- rwe_read_ehr(
    source = temp_file,
    format = "custom",
    date_range = c("2020-02-01", "2020-02-29"),
    validate = FALSE
  )

  # Should only include February dates
  expect_true(nrow(result$data) < 100)
  expect_true(nrow(result$data) >= 28)  # At least 28 days in Feb

  unlink(temp_file)
})


test_that("rwe_read_ehr creates audit trail", {
  test_data <- data.frame(x = 1:5, y = letters[1:5])
  temp_file <- tempfile(fileext = ".csv")
  write.csv(test_data, temp_file, row.names = FALSE)

  result <- rwe_read_ehr(
    source = temp_file,
    format = "custom",
    validate = FALSE
  )

  expect_true("audit_trail" %in% names(result))
  expect_s3_class(result$audit_trail, "rwe_audit_trail")
  expect_equal(result$audit_trail$operation, "read_ehr")

  unlink(temp_file)
})


test_that("detect_source_type identifies file paths", {
  expect_equal(
    rwevidence:::detect_source_type("data/file.csv"),
    "file"
  )

  expect_equal(
    rwevidence:::detect_source_type("/absolute/path/to/file.parquet"),
    "file"
  )
})


test_that("detect_source_type identifies URLs", {
  expect_equal(
    rwevidence:::detect_source_type("https://api.example.com/data"),
    "api"
  )

  expect_equal(
    rwevidence:::detect_source_type("http://example.com/data.json"),
    "api"
  )
})


test_that("detect_source_type handles invalid input", {
  expect_error(
    rwevidence:::detect_source_type(123),
    "Unable to detect source type"
  )

  expect_error(
    rwevidence:::detect_source_type(list()),
    "Unable to detect source type"
  )
})


test_that("read_csv_file handles different delimiters", {
  # Test CSV
  test_data <- data.frame(a = 1:3, b = c("x", "y", "z"))
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(test_data, temp_csv, row.names = FALSE)

  result <- rwevidence:::read_csv_file(temp_csv)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 2)

  unlink(temp_csv)
})


test_that("filter_by_date works correctly", {
  test_data <- data.frame(
    id = 1:10,
    event_date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 10)
  )

  filtered <- rwevidence:::filter_by_date(
    test_data,
    c("2020-03-01", "2020-06-30")
  )

  expect_true(nrow(filtered) < nrow(test_data))
  expect_true(all(filtered$event_date >= as.Date("2020-03-01")))
  expect_true(all(filtered$event_date <= as.Date("2020-06-30")))
})


test_that("filter_by_date warns when no date columns found", {
  test_data <- data.frame(id = 1:5, value = c("a", "b", "c", "d", "e"))

  expect_warning(
    rwevidence:::filter_by_date(test_data, c("2020-01-01", "2020-12-31")),
    "No date columns found"
  )
})


test_that("validate_ehr_schema performs validation", {
  test_data <- data.frame(
    patient_id = 1:5,
    age = c(25, 30, 45, 60, 55)
  )

  result <- rwevidence:::validate_ehr_schema(test_data, "custom", NULL)

  expect_true(is.list(result))
  expect_true("passed" %in% names(result))
  expect_true("message" %in% names(result))
})


test_that("get_main_table_for_format returns correct table names", {
  expect_equal(
    rwevidence:::get_main_table_for_format("omop_cdm"),
    "person"
  )

  expect_equal(
    rwevidence:::get_main_table_for_format("fhir"),
    "patient"
  )

  expect_equal(
    rwevidence:::get_main_table_for_format("custom"),
    "patient"
  )
})


test_that("rwe_read_claims wraps rwe_read_ehr", {
  test_data <- data.frame(
    claim_id = 1:5,
    amount = c(100, 200, 150, 300, 250)
  )

  temp_file <- tempfile(fileext = ".csv")
  write.csv(test_data, temp_file, row.names = FALSE)

  result <- rwe_read_claims(source = temp_file, format = "custom")

  expect_s3_class(result, "rwe_ehr")
  expect_equal(nrow(result$data), 5)

  unlink(temp_file)
})


test_that("rwe_read_registry adds registry metadata", {
  test_data <- data.frame(
    patient_id = 1:5,
    cancer_type = c("Breast", "Lung", "Colon", "Breast", "Lung")
  )

  temp_file <- tempfile(fileext = ".csv")
  write.csv(test_data, temp_file, row.names = FALSE)

  result <- rwe_read_registry(
    source = temp_file,
    registry_name = "SEER"
  )

  expect_s3_class(result, "rwe_ehr")
  expect_equal(result$metadata$registry_name, "SEER")

  unlink(temp_file)
})


test_that("rwe_read_ehr handles RDS files", {
  test_data <- data.frame(x = 1:5, y = letters[1:5])
  temp_file <- tempfile(fileext = ".rds")
  saveRDS(test_data, temp_file)

  result <- rwe_read_ehr(
    source = temp_file,
    format = "custom",
    validate = FALSE
  )

  expect_s3_class(result, "rwe_ehr")
  expect_equal(nrow(result$data), 5)

  unlink(temp_file)
})


test_that("rwe_read_ehr metadata is complete", {
  test_data <- data.frame(id = 1:10, value = rnorm(10))
  temp_file <- tempfile(fileext = ".csv")
  write.csv(test_data, temp_file, row.names = FALSE)

  result <- rwe_read_ehr(
    source = temp_file,
    format = "custom",
    version = "1.0",
    validate = FALSE
  )

  metadata <- result$metadata

  expect_true("source" %in% names(metadata))
  expect_true("format" %in% names(metadata))
  expect_true("version" %in% names(metadata))
  expect_true("loaded_at" %in% names(metadata))
  expect_true("n_records" %in% names(metadata))

  expect_equal(metadata$format, "custom")
  expect_equal(metadata$version, "1.0")
  expect_equal(metadata$n_records, 10)

  unlink(temp_file)
})


test_that("rwe_read_ehr print method works", {
  test_data <- data.frame(id = 1:3, name = c("A", "B", "C"))
  temp_file <- tempfile(fileext = ".csv")
  write.csv(test_data, temp_file, row.names = FALSE)

  result <- rwe_read_ehr(
    source = temp_file,
    format = "custom",
    validate = FALSE
  )

  output <- capture.output(print(result))

  expect_true(any(grepl("rwe_ehr", output)))
  expect_true(any(grepl("Records:", output)))

  unlink(temp_file)
})
