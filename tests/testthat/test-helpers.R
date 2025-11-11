test_that("validate_inputs validates data frames correctly", {
  # Valid input
  expect_invisible(validate_inputs(mtcars))
  expect_true(validate_inputs(mtcars))

  # Missing data
  expect_error(validate_inputs(), "data must be provided")

  # Non-data frame input
  expect_error(validate_inputs(1:10), "data must be a data frame")
  expect_error(validate_inputs(list(a = 1, b = 2)), "data must be a data frame")

  # Empty data frame
  empty_df <- data.frame()
  expect_error(validate_inputs(empty_df), "data has 0 rows")
  expect_invisible(validate_inputs(empty_df, allow_empty = TRUE))
})


test_that("validate_inputs checks required columns", {
  # All columns present
  expect_invisible(validate_inputs(mtcars, required_cols = c("mpg", "cyl")))

  # Missing columns
  expect_error(
    validate_inputs(mtcars, required_cols = c("mpg", "missing_col")),
    "Missing required columns: missing_col"
  )

  # Multiple missing columns
  expect_error(
    validate_inputs(mtcars, required_cols = c("missing1", "missing2")),
    "Missing required columns: missing1, missing2"
  )

  # Invalid required_cols type
  expect_error(
    validate_inputs(mtcars, required_cols = 123),
    "required_cols must be a character vector"
  )
})


test_that("check_data_structure validates schema correctly", {
  test_data <- data.frame(
    id = 1:5,
    name = letters[1:5],
    value = rnorm(5)
  )

  expected_schema <- list(
    id = "integer",
    name = "character",
    value = "numeric"
  )

  result <- check_data_structure(test_data, expected_schema)

  expect_true(result$passed)
  expect_length(result$missing_columns, 0)
  expect_length(result$type_mismatches, 0)
})


test_that("check_data_structure detects missing columns", {
  test_data <- data.frame(id = 1:5, name = letters[1:5])

  expected_schema <- list(
    id = "integer",
    name = "character",
    missing_col = "numeric"
  )

  result <- check_data_structure(test_data, expected_schema)

  expect_false(result$passed)
  expect_equal(result$missing_columns, "missing_col")
  expect_match(result$message, "Missing columns")
})


test_that("check_data_structure detects type mismatches", {
  test_data <- data.frame(
    id = as.character(1:5),  # Should be integer
    value = as.numeric(1:5)  # Correct type
  )

  expected_schema <- list(
    id = "integer",
    value = "numeric"
  )

  result <- check_data_structure(test_data, expected_schema)

  expect_false(result$passed)
  expect_length(result$type_mismatches, 1)  # Only id is mismatched
  expect_true(grepl("id: expected integer, got character", result$type_mismatches[1]))
})


test_that("standardize_variable_names converts to snake_case", {
  names <- c("Patient ID", "Date.of.Birth", "AGE", "blood_pressure")

  result <- standardize_variable_names(names, style = "snake_case")

  expect_equal(result, c("patient_id", "date_of_birth", "age", "blood_pressure"))
})


test_that("standardize_variable_names handles special characters", {
  names <- c("Patient#ID", "Date@Birth", "Age (years)", "BP-Systolic")

  result <- standardize_variable_names(names)

  expect_true(all(grepl("^[a-z0-9_]+$", result)))
  expect_false(any(grepl("__", result)))  # No double underscores
})


test_that("standardize_variable_names validates input", {
  expect_error(
    standardize_variable_names(123),
    "names must be a character vector"
  )
})


test_that("safe_join performs left join correctly", {
  x <- data.frame(id = 1:3, x_val = c("a", "b", "c"))
  y <- data.frame(id = 2:4, y_val = c("d", "e", "f"))

  result <- safe_join(x, y, by = "id", type = "left", validate = FALSE)

  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 3)
  expect_true("x_val" %in% names(result))
  expect_true("y_val" %in% names(result))
})


test_that("safe_join validates join keys exist", {
  x <- data.frame(id = 1:3, x_val = c("a", "b", "c"))
  y <- data.frame(id = 2:4, y_val = c("d", "e", "f"))

  expect_error(
    safe_join(x, y, by = "missing_key", type = "left", validate = FALSE),
    "Join keys not found in x: missing_key"
  )

  expect_error(
    safe_join(x, y, by = "x_val", type = "left", validate = FALSE),
    "Join keys not found in y: x_val"
  )
})


test_that("safe_join warns about duplicate keys", {
  x <- data.frame(id = c(1, 1, 2), x_val = c("a", "b", "c"))
  y <- data.frame(id = 2:4, y_val = c("d", "e", "f"))

  expect_warning(
    safe_join(x, y, by = "id", type = "left", validate = TRUE),
    "Duplicate keys detected"
  )
})


test_that("extract_data_info provides correct summary", {
  test_data <- data.frame(
    id = 1:10,
    name = letters[1:10],
    value = rnorm(10),
    date = Sys.Date() + 1:10
  )

  info <- extract_data_info(test_data)

  expect_equal(info$n_rows, 10)
  expect_equal(info$n_cols, 4)
  expect_length(info$col_names, 4)
  expect_equal(info$summary$numeric_cols, 2)  # id and value
  expect_equal(info$summary$character_cols, 1)  # name
  expect_equal(info$summary$date_cols, 1)  # date
})


test_that("create_error_message formats correctly", {
  msg <- create_error_message(
    context = "test_function",
    issue = "invalid input",
    suggestion = "check the data"
  )

  expect_match(msg, "Error in test_function")
  expect_match(msg, "invalid input")
  expect_match(msg, "Suggestion: check the data")
})


test_that("create_error_message works without suggestion", {
  msg <- create_error_message(
    context = "test_function",
    issue = "invalid input"
  )

  expect_match(msg, "Error in test_function")
  expect_match(msg, "invalid input")
  expect_false(grepl("Suggestion", msg))
})


test_that("check_required_packages detects missing packages", {
  # Test with a package that definitely doesn't exist
  expect_error(
    check_required_packages("nonexistent_package_xyz123"),
    "Required packages not installed"
  )

  expect_error(
    check_required_packages("nonexistent_package_xyz123"),
    "install.packages"
  )
})


test_that("check_required_packages passes with available packages", {
  # Use base R packages that are always available
  expect_invisible(check_required_packages(c("stats", "utils")))
})
