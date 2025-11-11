#' S3 Class Definitions for rwevidence Package
#'
#' @description
#' Defines core S3 classes and their methods
#'
#' @name s3_classes
NULL


# Base Class: rwe_data ----------------------------------------------------

#' Create rwe_data Object
#'
#' @description
#' Base class for all data objects in the package
#'
#' @param data Data frame or tibble
#' @param metadata List of metadata
#' @param class_name Additional class name(s)
#'
#' @return Object of class rwe_data
#' @keywords internal
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


#' @export
print.rwe_data <- function(x, ...) {
  cat("\n<rwe_data>\n\n")
  cat("Dimensions:", nrow(x$data), "rows x", ncol(x$data), "columns\n")

  if (!is.null(x$metadata$source)) {
    cat("Source:", x$metadata$source, "\n")
  }

  if (!is.null(x$metadata$created_at)) {
    cat("Created:", format(x$metadata$created_at, "%Y-%m-%d %H:%M:%S"), "\n")
  }

  cat("\nData preview:\n")
  print(utils::head(x$data, 5))

  invisible(x)
}


#' @export
summary.rwe_data <- function(object, ...) {
  cat("\n=== RWE Data Summary ===\n\n")

  cat("Dimensions:\n")
  cat("  Rows:", nrow(object$data), "\n")
  cat("  Columns:", ncol(object$data), "\n\n")

  cat("Column Types:\n")
  types_summary <- table(sapply(object$data, function(x) class(x)[1]))
  print(types_summary)

  if (length(object$metadata) > 1) {  # More than just created_at
    cat("\nMetadata:\n")
    metadata_display <- object$metadata[names(object$metadata) != "created_at"]
    str(metadata_display, max.level = 1)
  }

  invisible(object)
}


# Class: rwe_ehr ----------------------------------------------------------

#' Create rwe_ehr Object
#'
#' @description
#' Class for EHR data with schema information
#'
#' @param data Data frame containing EHR data
#' @param metadata List of metadata
#' @param schema Schema definition
#'
#' @return Object of class rwe_ehr
#' @export
#'
#' @examples
#' \dontrun{
#' ehr_data <- new_rwe_ehr(
#'   data = patient_data,
#'   metadata = list(source = "hospital_ehr", format = "omop_cdm"),
#'   schema = omop_schema
#' )
#' }
new_rwe_ehr <- function(data, metadata = list(), schema = list()) {

  obj <- new_rwe_data(data, metadata, class_name = "rwe_ehr")
  obj$schema <- schema

  obj
}


#' @export
print.rwe_ehr <- function(x, ...) {
  cat("\n<rwe_ehr>\n\n")

  if (!is.null(x$metadata$format)) {
    cat("Format:", x$metadata$format, "\n")
  }

  if (!is.null(x$metadata$version)) {
    cat("Version:", x$metadata$version, "\n")
  }

  cat("Records:", nrow(x$data), "\n")
  cat("Variables:", ncol(x$data), "\n")

  if (!is.null(x$metadata$validation)) {
    cat("Validation:",
        if (x$metadata$validation$passed) "PASSED" else "FAILED",
        "\n")
  }

  cat("\nData preview:\n")
  print(utils::head(x$data, 3))

  invisible(x)
}


# Class: rwe_quality_report -----------------------------------------------

#' Create rwe_quality_report Object
#'
#' @description
#' Class for data quality assessment reports
#'
#' @param data_info Basic data information
#' @param dimensions Quality dimension results
#' @param overall_score Overall quality score
#' @param issues Detected issues
#' @param recommendations Recommendations list
#'
#' @return Object of class rwe_quality_report
#' @export
new_rwe_quality_report <- function(data_info,
                                   dimensions,
                                   overall_score,
                                   issues,
                                   recommendations) {

  structure(
    list(
      data_info = data_info,
      dimensions = dimensions,
      overall_score = overall_score,
      dimension_scores = sapply(dimensions, function(x) x$score),
      issues = issues,
      recommendations = recommendations,
      timestamp = Sys.time()
    ),
    class = c("rwe_quality_report", "rwe_report")
  )
}


#' @export
print.rwe_quality_report <- function(x, ...) {
  cat("\n=== RWE Data Quality Report ===\n\n")
  cat("Assessment Date:", format(x$timestamp, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Dataset:", x$data_info$n_rows, "rows x", x$data_info$n_cols, "columns\n\n")

  cat("Overall Quality Score:", round(x$overall_score, 3), "\n")
  cat(get_quality_rating(x$overall_score), "\n\n")

  cat("Dimension Scores:\n")
  for (dim in names(x$dimension_scores)) {
    score <- x$dimension_scores[[dim]]
    cat(sprintf("  %-15s: %.3f\n",
                tools::toTitleCase(dim), score))
  }

  if (length(x$issues) > 0) {
    cat("\n", length(x$issues), "Quality Issues Detected\n")
  } else {
    cat("\nNo quality issues detected!\n")
  }

  if (length(x$recommendations) > 0) {
    cat("\nTop Recommendations:\n")
    for (i in seq_len(min(3, length(x$recommendations)))) {
      cat("  ", i, ". ", x$recommendations[i], "\n", sep = "")
    }
  }

  invisible(x)
}


# Class: rwe_harmonized ---------------------------------------------------

#' Create rwe_harmonized Object
#'
#' @description
#' Class for harmonized/transformed data
#'
#' @param data Harmonized data frame
#' @param source_schema Source schema name
#' @param target_schema Target schema name
#' @param mapping_config Mapping configuration used
#' @param validation Validation results
#'
#' @return Object of class rwe_harmonized
#' @export
new_rwe_harmonized <- function(data,
                               source_schema,
                               target_schema,
                               mapping_config,
                               validation = NULL) {

  obj <- new_rwe_data(
    data,
    metadata = list(
      source_schema = source_schema,
      target_schema = target_schema,
      harmonized_at = Sys.time()
    ),
    class_name = c("rwe_harmonized", "rwe_transformed")
  )

  obj$mapping_config <- mapping_config
  obj$validation <- validation

  obj
}


#' @export
print.rwe_harmonized <- function(x, ...) {
  cat("\n<rwe_harmonized>\n\n")
  cat("Source Schema:", x$metadata$source_schema, "\n")
  cat("Target Schema:", x$metadata$target_schema, "\n")
  cat("Harmonized:", format(x$metadata$harmonized_at, "%Y-%m-%d %H:%M:%S"), "\n\n")

  cat("Records:", nrow(x$data), "\n")
  cat("Variables:", ncol(x$data), "\n")

  if (!is.null(x$validation)) {
    cat("\nValidation:",
        if (x$validation$passed) "PASSED" else "FAILED",
        "\n")
  }

  invisible(x)
}


# Class: rwe_matched_cohort -----------------------------------------------

#' Create rwe_matched_cohort Object
#'
#' @description
#' Class for propensity score matched cohorts
#'
#' @param matched_data Matched data frame
#' @param match_object Matching object from MatchIt
#' @param balance_before Balance before matching
#' @param balance_after Balance after matching
#'
#' @return Object of class rwe_matched_cohort
#' @export
new_rwe_matched_cohort <- function(matched_data,
                                   match_object,
                                   balance_before,
                                   balance_after) {

  obj <- new_rwe_data(
    matched_data,
    metadata = list(
      matched_at = Sys.time(),
      n_matched = nrow(matched_data)
    ),
    class_name = c("rwe_matched_cohort", "rwe_cohort")
  )

  obj$match_object <- match_object
  obj$balance_before <- balance_before
  obj$balance_after <- balance_after

  obj
}


#' @export
print.rwe_matched_cohort <- function(x, ...) {
  cat("\nMatched Cohort Analysis\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  if (!is.null(x$match_object)) {
    cat("Matching method:", x$match_object$method, "\n")
    cat("Matching ratio:", paste0(x$match_object$ratio, ":1"), "\n")
  }
  cat("Matched observations:", x$metadata$n_matched, "\n")
  cat("Matched on:", format(x$metadata$matched_at, "%Y-%m-%d %H:%M:%S"), "\n\n")

  cat("Balance Improvement:\n")
  if (!is.null(x$balance_before) && !is.null(x$balance_after)) {
    cat("  Before matching: ",
        round(mean(x$balance_before$std_diff, na.rm = TRUE), 3), "\n")
    cat("  After matching:  ",
        round(mean(x$balance_after$std_diff, na.rm = TRUE), 3), "\n")
  }

  invisible(x)
}


# Class: rwe_analysis -----------------------------------------------------

#' Create rwe_analysis Object
#'
#' @description
#' Base class for analysis results
#'
#' @param primary_analysis Primary analysis results
#' @param method Analysis method used
#' @param class_name Additional class name(s)
#'
#' @return Object of class rwe_analysis
#' @export
new_rwe_analysis <- function(primary_analysis, method, class_name = NULL) {

  structure(
    list(
      primary_analysis = primary_analysis,
      method = method,
      analyzed_at = Sys.time()
    ),
    class = c(class_name, "rwe_analysis")
  )
}


#' @export
print.rwe_analysis <- function(x, ...) {
  cat("\n<rwe_analysis>\n\n")
  cat("Method:", x$method, "\n")
  cat("Analyzed:", format(x$analyzed_at, "%Y-%m-%d %H:%M:%S"), "\n\n")

  if (!is.null(x$primary_analysis)) {
    cat("Primary Results:\n")
    print(x$primary_analysis)
  }

  invisible(x)
}


# Utility Functions -------------------------------------------------------

#' Get Quality Rating Label
#'
#' @param score Quality score (0-1)
#' @return Character string with rating
#' @keywords internal
get_quality_rating <- function(score) {
  if (score >= 0.9) {
    return("***** Excellent")
  } else if (score >= 0.8) {
    return("**** Good")
  } else if (score >= 0.7) {
    return("*** Fair")
  } else if (score >= 0.6) {
    return("** Poor")
  } else {
    return("* Critical")
  }
}


#' Check if Object is rwe_data
#'
#' @param x Object to check
#' @return Logical
#' @export
is_rwe_data <- function(x) {
  inherits(x, "rwe_data")
}


#' Check if Object is rwe_ehr
#'
#' @param x Object to check
#' @return Logical
#' @export
is_rwe_ehr <- function(x) {
  inherits(x, "rwe_ehr")
}


#' Check if Object is rwe_quality_report
#'
#' @param x Object to check
#' @return Logical
#' @export
is_rwe_quality_report <- function(x) {
  inherits(x, "rwe_quality_report")
}
