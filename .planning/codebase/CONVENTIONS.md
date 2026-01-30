# Coding Conventions

**Analysis Date:** 2026-01-30

## Naming Patterns

**Functions:**
- `snake_case` format for all exported and internal functions
- Prefix with `rwe_` for all package-exported functions (e.g., `rwe_assess_quality`, `rwe_propensity_score`, `rwe_iptw`)
- Internal helper functions prefixed with `.` for clarity (e.g., `.extract_analysis_frame`, `.coerce_binary`, `.resolve_weights`)
- Descriptive names that indicate purpose: `validate_inputs`, `check_data_structure`, `standardize_variable_names`
- Located in: `R/*.R`

**Variables:**
- `snake_case` for all variable names in data frames and function parameters
- Constants use `UPPERCASE_SNAKE_CASE` (e.g., `DEFAULT_QUALITY_THRESHOLDS`, `OMOP_CDM_5.4_SCHEMA`, `SUPPORTED_TERMINOLOGIES`)
- Boolean parameters typically follow pattern: `allow_empty`, `validate`, `console_output`, `suggest_install`
- Located in: `R/constants.R` for global constants

**S3 Classes:**
- Class names use `rwe_` prefix: `rwe_data`, `rwe_ehr`, `rwe_quality_report`, `rwe_harmonized`, `rwe_matched_cohort`, `rwe_analysis`
- Constructor functions use `new_` prefix: `new_rwe_data()`, `new_rwe_ehr()`, `new_rwe_quality_report()`
- Check/is functions use `is_` prefix: `is_rwe_data()`, `is_rwe_ehr()`, `is_rwe_quality_report()`
- Located in: `R/s3_classes.R`

**Data Frame Columns:**
- Use `snake_case` in all contexts
- Standard naming for outcomes/treatments: `treatment`, `outcome`, `age`, `bmi`
- Generated columns like IPTW weights added as: `iptw`, `propensity_score`

## Code Style

**Formatting:**
- No explicit formatter configured (no .prettierrc or styler config detected)
- Use base R conventions from tidyverse style guide
- Indentation: 2 spaces (observed in all files)
- Line breaks: Keep related code together, separate logical sections with comments

**Roxygen Documentation:**
- All exported functions documented with `#' @export` tag
- All internal/helper functions tagged with `#' @keywords internal`
- Use `@description` for function purpose
- Use `@param` for each parameter with type and description
- Use `@return` for return value type and content
- Use `@examples` for usage examples (wrapped in `\dontrun{}` for examples requiring external packages)
- Located in: `R/*.R` (directly above function definition)

**Example pattern from `R/helpers.R` (lines 1-50):**
```r
#' Validate Input Data Structure
#'
#' @description
#' Validates that input data meets basic requirements for processing
#'
#' @param data Input data to validate
#' @param required_cols Optional character vector of required column names
#' @param allow_empty Logical. Allow empty data frames (default: FALSE)
#'
#' @return Invisibly returns TRUE if validation passes, throws error otherwise
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' validate_inputs(mtcars, required_cols = c("mpg", "cyl"))
#' }
validate_inputs <- function(data, required_cols = NULL, allow_empty = FALSE) {
  # Implementation
}
```

## Import Organization

**Order in files:**
1. roxygen directives (`@import`, `@importFrom`)
2. Function definitions organized by purpose

**Path Aliases:**
- No alias system detected; direct relative imports via `R/` directory structure

**Example from `R/rwevidence-package.R`:**
```r
#' @importFrom graphics legend
#' @importFrom stats aggregate aov as.formula binomial chisq.test coef complete.cases ...
#' @importFrom utils head object.size str tail write.csv
"_PACKAGE"
```

## Error Handling

**Pattern:**
- Use `stop()` for fatal errors, always with `call. = FALSE` to avoid printing function call
- Use `warning()` for non-fatal issues with `call. = FALSE`
- Use `message()` for informational output
- Validate inputs at function start

**Examples from `R/helpers.R` (lines 17-50):**
```r
validate_inputs <- function(data, required_cols = NULL, allow_empty = FALSE) {
  # Check data is provided
  if (missing(data)) {
    stop("data must be provided", call. = FALSE)
  }

  # Check data is a data frame
  if (!is.data.frame(data)) {
    stop("data must be a data frame, not ", class(data)[1], call. = FALSE)
  }

  # Informative warning
  if (x_dups > 0 || y_dups > 0) {
    warning("Duplicate keys detected. Join may create unexpected results.",
            call. = FALSE)
  }

  invisible(TRUE)
}
```

**Examples from `R/propensity_score.R` (lines 56-80):**
```r
# Validate treatment variable
if (!treatment %in% names(data_df)) {
  stop("Treatment variable '", treatment, "' not found in data", call. = FALSE)
}

# Check treatment is binary
treatment_vals <- unique(data_df[[treatment]])
if (length(treatment_vals) > 2) {
  stop("Treatment variable must be binary (2 unique values)", call. = FALSE)
}

# Multiple missing covariates with clear message
missing_covs <- setdiff(covariates, names(data_df))
if (length(missing_covs) > 0) {
  if (length(missing_covs) == 1) {
    stop("Covariate '", missing_covs, "' not found in data", call. = FALSE)
  } else {
    stop("Covariates not found: ", paste(missing_covs, collapse = ", "), call. = FALSE)
  }
}
```

**Error message style:**
- Context-specific: mention what failed and why
- Include actual vs expected when comparing values
- Suggest solutions when obvious (e.g., in `check_required_packages`)

## Logging

**Framework:** Custom logging system using environment variables
- Located in: `R/logging.R`

**Patterns:**
- `log_info()` for informational messages (e.g., "Starting data quality assessment")
- `log_debug()` for detailed debug information (e.g., formula specifications)
- Setup with `setup_logger(level = "INFO", file = NULL, console = TRUE)`

**Examples from `R/quality_assessment.R` (lines 62-64):**
```r
log_info("Starting data quality assessment", context = "rwe_assess_quality")
log_debug(paste("Dimensions:", paste(dimensions, collapse = ", ")),
          context = "rwe_assess_quality")
```

**User-facing messages:**
- Use `cat()` for informational output during processing
- Use `message()` for general user notifications

**Example from `R/propensity_score.R` (lines 67-70):**
```r
cat("Estimating propensity scores...\n")
cat("Method:", method, "\n")
cat("Treatment:", treatment, "\n")
cat("Covariates:", length(covariates), "\n\n")
```

## Comments

**When to Comment:**
- Comment section breaks with `# Section Name -----` format
- Explain complex logic or non-obvious algorithm steps
- Never state what code obviously does
- Include purpose of internal helper functions

**Comment Style:**
```r
# Section breaks with dashes
# Data Quality Thresholds -----------------------------------------------

# Inline comments are sparingly used
# Only for non-obvious logic

# Complex helper function with explanation
.extract_analysis_frame <- function(data) {
  # Extract underlying data frame from rwe_data objects
  if (is_rwe_data(data)) {
    data$data
  } else {
    data
  }
}
```

**JSDoc/Roxygen:**
- All exported functions have roxygen documentation
- Internal functions (`@keywords internal`) also fully documented
- No additional code comments needed when roxygen provides full documentation

## Function Design

**Size:** Functions are typically 30-150 lines
- Small pure functions for validation and helpers (15-30 lines)
- Medium functions for main analysis (50-100 lines)
- Larger functions for complex workflows (100-150 lines)

**Parameters:**
- Explicit parameter validation at start of function
- Use `match.arg()` for choosing from limited options
- Default to `NULL` for optional parameters
- Order: data/main input first, then configuration/options

**Return Values:**
- All analysis functions return S3 objects with class `rwe_*`
- Helper functions return simple values (logical, list, data frame)
- Use `invisible()` when returning value primarily for side effects
- Always include meaningful structure in returned objects

**Example from `R/effectiveness_analysis.R` (lines 58-106):**
```r
.prepare_analysis_inputs <- function(data,
                                     outcome,
                                     treatment,
                                     confounders = NULL,
                                     weights = NULL) {
  # Validate inputs first
  validate_inputs(data)
  required_cols <- unique(c(outcome, treatment, confounders))
  validate_inputs(data, required_cols = required_cols)

  # Extract weights if present
  weight_info <- .resolve_weights(data, weights)
  n_original <- nrow(data)

  # Remove missing data
  keep_idx <- stats::complete.cases(data[, required_cols, drop = FALSE])
  if (!is.null(weight_info$values)) {
    keep_idx <- keep_idx & !is.na(weight_info$values)
  }

  # Return structured list
  list(
    data = analysis_df,
    weights = weights_vec,
    weight_name = weight_info$name,
    n_original = n_original,
    n_analyzed = nrow(analysis_df),
    dropped_rows = n_original - nrow(analysis_df)
  )
}
```

## Module Design

**Exports:**
- Explicit `@export` tag in roxygen for public functions
- All other functions are internal (`@keywords internal`)
- No barrel files; each module in separate file

**Module Structure:**
- One primary domain per file (e.g., `quality_assessment.R`, `propensity_score.R`)
- Helper functions (`@keywords internal`) in same file as public functions
- Constants defined in `R/constants.R`

**Example module organization from `R/effectiveness_analysis.R`:**
```r
# Internal helpers at top (lines 4-56)
.extract_analysis_frame <- function(data) { }
.coerce_binary <- function(x, name) { }
.resolve_weights <- function(data, weights) { }
.prepare_analysis_inputs <- function(...) { }

# Public functions below (lines 108+)
#' @export
rwe_relative_risk <- function(...) { }

#' @export
rwe_odds_ratio <- function(...) { }
```

## S3 Classes and Methods

**Class Definition:**
- Constructor functions create S3 objects with `structure()` call
- Always inherit from `rwe_data` base class or create specialized class
- Include metadata list and class vector

**Example from `R/s3_classes.R` (lines 23-35):**
```r
new_rwe_data <- function(data, metadata = list(), class_name = NULL) {
  structure(
    list(
      data = data,
      metadata = c(
        metadata,
        list(created_at = Sys.time())
      )
    ),
    class = c(class_name, "rwe_data")
  )
}
```

**Method Implementation:**
- `print.rwe_*` methods for display (use `cat()` and format output)
- `summary.rwe_*` methods for detailed summaries
- Generic `is_rwe_*()` checker functions

**Example from `R/s3_classes.R` (lines 39-55):**
```r
#' @export
print.rwe_data <- function(x, ...) {
  cat("\n<rwe_data>\n\n")
  cat("Dimensions:", nrow(x$data), "rows x", ncol(x$data), "columns\n")

  if (!is.null(x$metadata$source)) {
    cat("Source:", x$metadata$source, "\n")
  }

  cat("\nData preview:\n")
  print(utils::head(x$data, 5))

  invisible(x)
}
```

---

*Convention analysis: 2026-01-30*
