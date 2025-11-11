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

  # Check data is provided
  if (missing(data)) {
    stop("data must be provided", call. = FALSE)
  }

  # Check data is a data frame
  if (!is.data.frame(data)) {
    stop("data must be a data frame, not ", class(data)[1], call. = FALSE)
  }

  # Check for empty data
  if (!allow_empty && nrow(data) == 0) {
    stop("data has 0 rows. Set allow_empty = TRUE to bypass this check.",
         call. = FALSE)
  }

  # Check required columns
  if (!is.null(required_cols)) {
    if (!is.character(required_cols)) {
      stop("required_cols must be a character vector", call. = FALSE)
    }

    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ",
           paste(missing_cols, collapse = ", "),
           call. = FALSE)
    }
  }

  invisible(TRUE)
}


#' Check Data Structure Against Expected Schema
#'
#' @description
#' Validates data structure matches expected column names and types
#'
#' @param data Input data frame
#' @param expected_schema Named list of expected column types
#'
#' @return List with validation results
#' @keywords internal
check_data_structure <- function(data, expected_schema) {

  validate_inputs(data)

  if (!is.list(expected_schema)) {
    stop("expected_schema must be a named list", call. = FALSE)
  }

  # Check for missing columns
  expected_cols <- names(expected_schema)
  actual_cols <- names(data)
  missing_cols <- setdiff(expected_cols, actual_cols)
  extra_cols <- setdiff(actual_cols, expected_cols)

  # Check column types
  type_mismatches <- character()
  for (col in intersect(expected_cols, actual_cols)) {
    expected_type <- expected_schema[[col]]
    actual_type <- class(data[[col]])[1]

    # Check if types match (allowing some flexibility)
    if (!inherits(data[[col]], expected_type)) {
      type_mismatches <- c(type_mismatches,
                          sprintf("%s: expected %s, got %s",
                                  col, expected_type, actual_type))
    }
  }

  list(
    passed = length(missing_cols) == 0 && length(type_mismatches) == 0,
    missing_columns = missing_cols,
    extra_columns = extra_cols,
    type_mismatches = type_mismatches,
    message = if (length(missing_cols) > 0 || length(type_mismatches) > 0) {
      paste(c(
        if (length(missing_cols) > 0)
          paste("Missing columns:", paste(missing_cols, collapse = ", ")),
        if (length(type_mismatches) > 0)
          paste("Type mismatches:", paste(type_mismatches, collapse = "; "))
      ), collapse = ". ")
    } else {
      "Schema validation passed"
    }
  )
}


#' Standardize Variable Names
#'
#' @description
#' Converts variable names to standard format (lowercase with underscores)
#'
#' @param names Character vector of variable names
#' @param style Style for standardization: "snake_case", "camelCase", "lowercase"
#'
#' @return Character vector of standardized names
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' standardize_variable_names(c("Patient ID", "Date.of.Birth", "AGE"))
#' # Returns: c("patient_id", "date_of_birth", "age")
#' }
standardize_variable_names <- function(names, style = c("snake_case", "camelCase", "lowercase")) {

  style <- match.arg(style)

  if (!is.character(names)) {
    stop("names must be a character vector", call. = FALSE)
  }

  # Clean names
  cleaned <- tolower(names)
  cleaned <- gsub("[^a-z0-9_]", "_", cleaned)  # Replace non-alphanumeric with underscore
  cleaned <- gsub("_+", "_", cleaned)  # Collapse multiple underscores
  cleaned <- gsub("^_|_$", "", cleaned)  # Remove leading/trailing underscores

  if (style == "snake_case") {
    return(cleaned)
  } else if (style == "camelCase") {
    # Convert to camelCase
    parts <- strsplit(cleaned, "_")
    camel <- sapply(parts, function(x) {
      if (length(x) == 0) return("")
      paste0(x[1],
             sapply(x[-1], function(part) {
               paste0(toupper(substring(part, 1, 1)),
                     substring(part, 2))
             }))
    })
    return(camel)
  } else if (style == "lowercase") {
    return(gsub("_", "", cleaned))
  }
}


#' Safe Join with Validation
#'
#' @description
#' Performs a join operation with validation and informative messages
#'
#' @param x First data frame
#' @param y Second data frame
#' @param by Character vector of column names to join by
#' @param type Type of join: "left", "right", "inner", "full"
#' @param validate Validate join results (check for duplicates, missing keys)
#'
#' @return Joined data frame
#' @keywords internal
safe_join <- function(x, y, by,
                     type = c("left", "right", "inner", "full"),
                     validate = TRUE) {

  type <- match.arg(type)

  validate_inputs(x)
  validate_inputs(y)

  # Check join keys exist
  if (!all(by %in% names(x))) {
    stop("Join keys not found in x: ",
         paste(setdiff(by, names(x)), collapse = ", "),
         call. = FALSE)
  }

  if (!all(by %in% names(y))) {
    stop("Join keys not found in y: ",
         paste(setdiff(by, names(y)), collapse = ", "),
         call. = FALSE)
  }

  # Check for duplicate keys if validating
  if (validate) {
    x_dups <- anyDuplicated(x[, by, drop = FALSE])
    y_dups <- anyDuplicated(y[, by, drop = FALSE])

    if (x_dups > 0 || y_dups > 0) {
      warning("Duplicate keys detected. Join may create unexpected results.",
              call. = FALSE)
    }
  }

  # Perform join
  result <- switch(type,
    left = dplyr::left_join(x, y, by = by),
    right = dplyr::right_join(x, y, by = by),
    inner = dplyr::inner_join(x, y, by = by),
    full = dplyr::full_join(x, y, by = by)
  )

  # Validation messages
  if (validate) {
    rows_before <- c(nrow(x), nrow(y))
    rows_after <- nrow(result)

    message(sprintf("Join complete: %d + %d rows -> %d rows (%s join)",
                   rows_before[1], rows_before[2], rows_after, type))
  }

  result
}


#' Extract Data Information
#'
#' @description
#' Extracts summary information about a dataset
#'
#' @param data Input data frame
#'
#' @return List with data information
#' @keywords internal
extract_data_info <- function(data) {

  validate_inputs(data, allow_empty = TRUE)

  list(
    n_rows = nrow(data),
    n_cols = ncol(data),
    col_names = names(data),
    col_types = sapply(data, function(x) class(x)[1]),
    memory_size = format(object.size(data), units = "MB"),
    has_rownames = !identical(rownames(data), as.character(seq_len(nrow(data)))),
    summary = list(
      numeric_cols = sum(sapply(data, is.numeric)),
      character_cols = sum(sapply(data, is.character)),
      factor_cols = sum(sapply(data, is.factor)),
      date_cols = sum(sapply(data, function(x) inherits(x, c("Date", "POSIXct"))))
    )
  )
}


#' Create Informative Error Message
#'
#' @description
#' Creates standardized error messages for the package
#'
#' @param context Function or module context
#' @param issue Description of the issue
#' @param suggestion Optional suggestion for resolution
#'
#' @return Character string with formatted error message
#' @keywords internal
create_error_message <- function(context, issue, suggestion = NULL) {

  msg <- paste0("Error in ", context, ": ", issue)

  if (!is.null(suggestion)) {
    msg <- paste0(msg, "\n\nSuggestion: ", suggestion)
  }

  msg
}


#' Check Required Packages
#'
#' @description
#' Checks if required packages are installed and provides installation instructions
#'
#' @param packages Character vector of package names
#' @param suggest_install Logical. Suggest installation command
#'
#' @return Invisibly returns TRUE if all packages available
#' @keywords internal
check_required_packages <- function(packages, suggest_install = TRUE) {

  missing_pkgs <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    msg <- paste0("Required packages not installed: ",
                 paste(missing_pkgs, collapse = ", "))

    if (suggest_install) {
      msg <- paste0(msg, "\n\nInstall with:\n",
                   'install.packages(c("',
                   paste(missing_pkgs, collapse = '", "'),
                   '"))')
    }

    stop(msg, call. = FALSE)
  }

  invisible(TRUE)
}
