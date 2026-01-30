# Testing Patterns

**Analysis Date:** 2026-01-30

## Test Framework

**Runner:**
- testthat v3.0.0+ (from DESCRIPTION Suggests)
- Config: `tests/testthat.R` (standard setup - library load wrapper)
- Edition: 3 (from `Config/testthat/edition: 3` in DESCRIPTION)

**Assertion Library:**
- testthat's native expect_* functions (no external assertion library)

**Run Commands:**
```bash
# Run all tests
devtools::test()
# or
testthat::test_local()

# Run tests in watch mode
devtools::load_all()
testthat::test_dir("tests/testthat")

# View coverage
covr::package_coverage()
```

## Test File Organization

**Location:**
- All tests colocated in `tests/testthat/` directory (not co-located with source)
- Pattern: `test-{module_name}.R` matches `R/{module_name}.R`

**File Mapping:**
- `test-helpers.R` tests `R/helpers.R`
- `test-s3_classes.R` tests `R/s3_classes.R`
- `test-effectiveness_analysis.R` tests `R/effectiveness_analysis.R`
- `test-propensity_score.R` tests `R/propensity_score.R`
- `test-causal_inference.R` tests `R/causal_inference.R`
- `test-imputation.R` tests `R/imputation.R`
- `test-quality_assessment.R` tests `R/quality_assessment.R`
- `test-study_design.R` tests `R/study_design.R`
- `test-survival_analysis.R` tests `R/survival_analysis.R`
- (11 total test files, 4893 lines of test code)

**Naming Convention:**
- File: `test-{module}.R`
- Test suite: `test_that("description of what is tested", { ... })`
- Test groups: logical grouping with related tests adjacent

## Test Structure

**Suite Organization:**

Each test file follows pattern (from `tests/testthat/test-helpers.R`):
```r
# Tests for {module_name}.R

library(testthat)
library(rwevidence)

test_that("validate_inputs validates data frames correctly", {
  # Valid input
  expect_invisible(validate_inputs(mtcars))
  expect_true(validate_inputs(mtcars))

  # Missing data
  expect_error(validate_inputs(), "data must be provided")

  # Non-data frame input
  expect_error(validate_inputs(1:10), "data must be a data frame")
})

test_that("validate_inputs checks required columns", {
  # All columns present
  expect_invisible(validate_inputs(mtcars, required_cols = c("mpg", "cyl")))

  # Missing columns
  expect_error(
    validate_inputs(mtcars, required_cols = c("mpg", "missing_col")),
    "Missing required columns: missing_col"
  )
})
```

**Setup Pattern:**
```r
# Standard imports at file start
library(testthat)
library(rwevidence)

# Seed setting for reproducibility
set.seed(123)

# Test data creation inline or in test blocks
test_data <- data.frame(
  id = 1:5,
  name = letters[1:5],
  value = rnorm(5)
)
```

**Teardown Pattern:**
- No explicit teardown functions observed
- Test data cleaned automatically after test
- No file I/O or resource cleanup needed

## Test Structure - Detailed Patterns

**Assertion Patterns:**

From `tests/testthat/test-helpers.R` (lines 1-62):
```r
# Testing return values
expect_invisible(validate_inputs(mtcars))
expect_true(validate_inputs(mtcars))

# Testing error conditions
expect_error(validate_inputs(), "data must be provided")
expect_error(validate_inputs(1:10), "data must be a data frame")

# Testing data structure validation
result <- check_data_structure(test_data, expected_schema)
expect_true(result$passed)
expect_length(result$missing_columns, 0)
expect_length(result$type_mismatches, 0)

# Testing specific field values
expect_equal(result$missing_columns, "missing_col")
expect_match(result$message, "Missing columns")

# Testing column type mismatches
expect_false(result$passed)
expect_length(result$type_mismatches, 1)
expect_true(grepl("id: expected integer, got character", result$type_mismatches[1]))
```

**S3 Object Testing:**

From `tests/testthat/test-s3_classes.R` (lines 1-42):
```r
test_that("new_rwe_data creates object with correct structure", {
  test_data <- data.frame(x = 1:5, y = letters[1:5])
  metadata <- list(source = "test")

  obj <- new_rwe_data(test_data, metadata)

  # Class checking
  expect_s3_class(obj, "rwe_data")

  # Structure validation
  expect_true(is.list(obj))
  expect_named(obj, c("data", "metadata"))

  # Content validation
  expect_equal(obj$data, test_data)
  expect_true("source" %in% names(obj$metadata))
  expect_true("created_at" %in% names(obj$metadata))
})

# Print method testing
test_that("print.rwe_data displays correctly", {
  test_data <- data.frame(x = 1:5, y = letters[1:5])
  obj <- new_rwe_data(test_data, list(source = "test"))

  output <- capture.output(print(obj))

  expect_true(any(grepl("rwe_data", output)))
  expect_true(any(grepl("5 rows", output)))
  expect_true(any(grepl("2 columns", output)))
})
```

**Analysis Function Testing:**

From `tests/testthat/test-effectiveness_analysis.R` (lines 1-33):
```r
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

  # Check return type
  expect_s3_class(result, "rwe_relative_risk")

  # Calculate expected values
  expected_risk_treated <- mean(data$outcome[data$treatment == 1])
  expected_risk_control <- mean(data$outcome[data$treatment == 0])
  expected_rr <- expected_risk_treated / expected_risk_control

  # Verify calculations with tolerance
  expect_equal(result$risk_treated, expected_risk_treated, tolerance = 1e-6)
  expect_equal(result$risk_control, expected_risk_control, tolerance = 1e-6)
  expect_equal(result$risk_ratio, expected_rr, tolerance = 1e-6)
})
```

## Mocking

**Framework:** No explicit mocking framework detected (testthat::with_mock not used)

**Approach:**
- Tests use real data generation (rbinom, rnorm, data.frame)
- No database mocking; tests work with in-memory data
- Optional package testing with `skip_if_not_installed()`

**Pattern from `tests/testthat/test-propensity_score.R` (lines 41-64):**
```r
test_that("rwe_propensity_score works with xgboost", {
  skip_if_not_installed("xgboost")

  set.seed(321)
  n <- 150
  test_data <- data.frame(
    treatment = rbinom(n, 1, 0.5),
    age = rnorm(n, 60, 8),
    bmi = rnorm(n, 26, 4),
    comorbidity = rpois(n, 2)
  )

  ps_result <- rwe_propensity_score(
    data = test_data,
    treatment = "treatment",
    covariates = c("age", "bmi", "comorbidity"),
    method = "xgboost",
    learner_control = list(xgb = list(nrounds = 25, seed = 321))
  )

  expect_s3_class(ps_result, "rwe_propensity_score")
  expect_equal(ps_result$method, "xgboost")
  expect_true(all(ps_result$data$propensity_score >= 0 & ps_result$data$propensity_score <= 1))
})
```

**What to Mock:**
- Use skip_if_not_installed() for optional packages
- No other mocking currently used; all tests use real computation

**What NOT to Mock:**
- Core analysis calculations (expect actual results)
- Data frame operations
- Statistical computations

## Fixtures and Factories

**Test Data Creation:**

Pattern: inline data creation within test blocks

**Simple fixtures from `tests/testthat/test-helpers.R`:**
```r
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
```

**Generated fixtures from `tests/testthat/test-effectiveness_analysis.R` (lines 8-15):**
```r
set.seed(123)
n <- 400
treatment <- rep(c(0, 1), each = n / 2)
outcome <- c(rbinom(n / 2, 1, 0.25), rbinom(n / 2, 1, 0.12))
data <- data.frame(
  treatment = treatment,
  outcome = outcome
)
```

**Location:**
- No dedicated fixture files; all test data created inline
- Seed setting ensures reproducibility: `set.seed(123)`
- Each test creates independent data

## Coverage

**Requirements:** No explicit coverage requirement configured
- covr package in Suggests (DESCRIPTION line 26)
- No coverage.yml or similar configuration

**View Coverage:**
```r
# Run coverage analysis
covr::package_coverage()

# View detailed coverage
report <- covr::package_coverage()
print(report)
```

## Test Types

**Unit Tests:**
- Scope: Individual functions tested in isolation
- Approach: Direct function calls with known inputs
- Example: `test-helpers.R` tests utility functions like `validate_inputs()`

**Integration Tests:**
- Scope: Tests combining multiple functions
- Approach: Testing propensity scores → IPTW → effectiveness analysis pipelines
- Example from `tests/testthat/test-causal_inference.R` (lines 6-36):
```r
test_that("rwe_iptw works with existing PS object", {
  set.seed(123)
  n <- 200
  test_data <- data.frame(
    treatment = rbinom(n, 1, 0.5),
    age = rnorm(n, 60, 10),
    bmi = rnorm(n, 25, 5),
    outcome = rnorm(n, 100, 20)
  )

  # First estimate propensity scores
  ps_result <- rwe_propensity_score(
    data = test_data,
    treatment = "treatment",
    covariates = c("age", "bmi"),
    method = "logistic"
  )

  # Then calculate IPTW using PS result
  iptw_result <- rwe_iptw(
    data = test_data,
    ps_object = ps_result,
    outcome = "outcome"
  )

  expect_s3_class(iptw_result, "rwe_iptw")
  expect_true("weights" %in% names(iptw_result))
})
```

**E2E Tests:**
- Not explicitly implemented
- Package intended for programmatic use via R functions, not CLI

## Common Patterns

**Async Testing:**
Not applicable (R is single-threaded for user code)

**Error Testing:**

From `tests/testthat/test-helpers.R` (lines 6-17):
```r
test_that("validate_inputs validates data frames correctly", {
  # Missing data - expects error
  expect_error(validate_inputs(), "data must be provided")

  # Non-data frame input
  expect_error(validate_inputs(1:10), "data must be a data frame")
  expect_error(validate_inputs(list(a = 1, b = 2)), "data must be a data frame")

  # Empty data frame
  empty_df <- data.frame()
  expect_error(validate_inputs(empty_df), "data has 0 rows")

  # Allow empty with flag
  expect_invisible(validate_inputs(empty_df, allow_empty = TRUE))
})
```

**Tolerance Testing:**

From `tests/testthat/test-effectiveness_analysis.R` (lines 28-32):
```r
# Allow for floating point precision differences
expect_equal(result$risk_treated, expected_risk_treated, tolerance = 1e-6)
expect_equal(result$risk_control, expected_risk_control, tolerance = 1e-6)
expect_equal(result$risk_ratio, expected_rr, tolerance = 1e-6)
```

**Boundary Testing:**

From `tests/testthat/test-propensity_score.R` (lines 30-31):
```r
# Propensity scores must be between 0 and 1
expect_true(all(ps_result$data$propensity_score >= 0 & ps_result$data$propensity_score <= 1))
```

**Output Capture Testing:**

From `tests/testthat/test-s3_classes.R` (lines 16-25):
```r
test_that("print.rwe_data displays correctly", {
  test_data <- data.frame(x = 1:5, y = letters[1:5])
  obj <- new_rwe_data(test_data, list(source = "test"))

  # Capture printed output
  output <- capture.output(print(obj))

  # Verify output contains expected elements
  expect_true(any(grepl("rwe_data", output)))
  expect_true(any(grepl("5 rows", output)))
  expect_true(any(grepl("2 columns", output)))
})
```

## Known Test Issues

**Skipped Tests:**

From `tests/testthat/test-causal_inference.R` (lines 63-70):
```r
test_that("rwe_iptw uses stabilized weights by default", {
  skip("Stabilized weights test temporarily disabled - needs PS trimming fix")
})

test_that("rwe_iptw truncates extreme weights", {
  skip("Weight truncation test temporarily disabled - needs PS trimming fix")
})
```

Note: These tests are temporarily disabled pending fixes to PS trimming algorithm. See CONCERNS.md for details.

## Test Execution Workflow

**Standard workflow:**
1. Set random seed for reproducibility
2. Create test data with known properties
3. Call function under test with quiet=TRUE to suppress output
4. Assert return class, structure, and specific values
5. Test error conditions separately with expect_error()

**Example complete test (from `tests/testthat/test-imputation.R`, lines 6-38):**
```r
test_that("rwe_impute works with mean imputation", {
  # Setup
  set.seed(123)
  n <- 100
  test_data <- data.frame(
    id = 1:n,
    age = rnorm(n, 60, 10),
    bmi = rnorm(n, 25, 5),
    lab_value = rnorm(n, 100, 20)
  )

  # Introduce missing values
  test_data$age[sample(n, 10)] <- NA
  test_data$bmi[sample(n, 15)] <- NA

  # Execute
  imputed <- rwe_impute(
    data = test_data,
    method = "mean",
    seed = 123
  )

  # Assert
  expect_s3_class(imputed, "rwe_imputation")
  expect_true("imputed_data" %in% names(imputed))
  expect_equal(nrow(imputed$imputed_data), n)

  # Verify no missing values
  expect_equal(sum(is.na(imputed$imputed_data$age)), 0)
  expect_equal(sum(is.na(imputed$imputed_data$bmi)), 0)

  # Verify imputation logic (mean values)
  original_mean_age <- mean(test_data$age, na.rm = TRUE)
  imputed_age_values <- imputed$imputed_data$age[is.na(test_data$age)]
  expect_true(all(abs(imputed_age_values - original_mean_age) < 0.01))
})
```

---

*Testing analysis: 2026-01-30*
