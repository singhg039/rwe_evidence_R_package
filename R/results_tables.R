# Results Tables and Figures

# Internal helpers ---------------------------------------------------------

.results_validate_object <- function(results, expected_class = NULL) {
  if (!is.null(expected_class) && !inherits(results, expected_class)) {
    stop(
      "results must inherit from ",
      paste(expected_class, collapse = " or "),
      call. = FALSE
    )
  }
}

.results_format_p <- function(p) {
  if (is.na(p)) {
    "NA"
  } else if (p < 0.001) {
    "<0.001"
  } else {
    sprintf("%.3f", p)
  }
}

.results_create_demographics_table <- function(results) {
  if (is.data.frame(results)) {
    return(results)
  }

  .results_validate_object(results, "rwe_table_one")

  digits <- results$settings$digits
  output <- list()

  # Continuous variables
  if (length(results$continuous_vars) > 0) {
    for (var in results$continuous_vars) {
      for (grp in results$groups) {
        stats <- results$summary[[var]][[as.character(grp)]]
        value <- sprintf(
          "%.*f (%.*f)",
          digits, stats$mean,
          digits, stats$sd
        )
        output[[length(output) + 1]] <- data.frame(
          variable = var,
          level = "Mean (SD)",
          group = grp,
          statistic = value,
          stringsAsFactors = FALSE
        )
      }

      if (results$settings$test && var %in% names(results$tests)) {
        output[[length(output) + 1]] <- data.frame(
          variable = var,
          level = "P-value",
          group = "Overall",
          statistic = .results_format_p(results$tests[[var]]$p_value),
          stringsAsFactors = FALSE
        )
      }

      if (results$settings$smd && var %in% names(results$smd)) {
        output[[length(output) + 1]] <- data.frame(
          variable = var,
          level = "SMD",
          group = "Overall",
          statistic = sprintf("%.3f", results$smd[[var]]),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # Categorical variables
  if (length(results$categorical_vars) > 0) {
    for (var in results$categorical_vars) {
      all_levels <- unique(unlist(lapply(results$summary[[var]], function(g) {
        names(g$counts)
      })))

      for (level in all_levels) {
        for (grp in results$groups) {
          stats <- results$summary[[var]][[as.character(grp)]]
          count <- if (level %in% names(stats$counts)) stats$counts[[level]] else 0
          prop <- if (level %in% names(stats$proportions)) stats$proportions[[level]] * 100 else 0
          value <- sprintf("%d (%.1f%%)", count, prop)

          output[[length(output) + 1]] <- data.frame(
            variable = var,
            level = level,
            group = grp,
            statistic = value,
            stringsAsFactors = FALSE
          )
        }
      }

      if (results$settings$test && var %in% names(results$tests)) {
        output[[length(output) + 1]] <- data.frame(
          variable = var,
          level = "P-value",
          group = "Overall",
          statistic = .results_format_p(results$tests[[var]]$p_value),
          stringsAsFactors = FALSE
        )
      }

      if (results$settings$smd && var %in% names(results$smd)) {
        output[[length(output) + 1]] <- data.frame(
          variable = var,
          level = "SMD",
          group = "Overall",
          statistic = sprintf("%.3f", results$smd[[var]]),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  do.call(rbind, output)
}

.results_create_effectiveness_table <- function(results) {

  if (inherits(results, "rwe_relative_risk")) {
    return(data.frame(
      measure = "Relative Risk",
      estimate = results$risk_ratio,
      ci_lower = results$ci_lower,
      ci_upper = results$ci_upper,
      p_value = results$p_value,
      stringsAsFactors = FALSE
    ))
  }

  if (inherits(results, "rwe_odds_ratio")) {
    return(data.frame(
      measure = "Odds Ratio",
      estimate = results$odds_ratio,
      ci_lower = results$ci_lower,
      ci_upper = results$ci_upper,
      p_value = results$p_value,
      stringsAsFactors = FALSE
    ))
  }

  if (inherits(results, "rwe_nnt")) {
    return(data.frame(
      measure = results$interpretation,
      estimate = results$nnt,
      ci_lower = results$ci_lower,
      ci_upper = results$ci_upper,
      p_value = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  if (inherits(results, "rwe_safety_analysis")) {
    # Provide summary of rate ratio within effectiveness table scope
    return(data.frame(
      measure = "Incidence Rate Ratio",
      estimate = results$rate_ratio$rate_ratio,
      ci_lower = results$rate_ratio$ci_lower,
      ci_upper = results$rate_ratio$ci_upper,
      p_value = results$rate_ratio$p_value,
      stringsAsFactors = FALSE
    ))
  }

  if (is.list(results)) {
    tables <- lapply(results, .results_create_effectiveness_table)
    return(do.call(rbind, tables))
  }

  if (is.data.frame(results)) {
    return(results)
  }

  stop("Unsupported results object for effectiveness table.", call. = FALSE)
}

.results_create_safety_table <- function(results) {

  if (inherits(results, "rwe_safety_analysis")) {
    rates <- results$incidence$estimates
    rates$measure <- "Incidence Rate"

    ratio_row <- data.frame(
      measure = "Rate Ratio",
      group = "Overall",
      events = NA,
      person_time = NA,
      incidence_rate = results$rate_ratio$rate_ratio,
      ci_lower = results$rate_ratio$ci_lower,
      ci_upper = results$rate_ratio$ci_upper,
      scale = results$incidence$scale,
      stringsAsFactors = FALSE
    )

    surveillance_row <- if (!is.null(results$safety_signal)) {
      data.frame(
        measure = "Safety Signal Detected",
        group = ifelse(results$safety_signal$detected, "Yes", "No"),
        events = NA,
        person_time = NA,
        incidence_rate = NA,
        ci_lower = NA,
        ci_upper = NA,
        scale = results$incidence$scale,
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }

    return(rbind(
      data.frame(measure = rates$measure, group = rates$group,
                 events = rates$events, person_time = rates$person_time,
                 incidence_rate = rates$incidence_rate,
                 ci_lower = rates$ci_lower, ci_upper = rates$ci_upper,
                 scale = rates$scale, stringsAsFactors = FALSE),
      ratio_row,
      surveillance_row
    ))
  }

  if (inherits(results, "rwe_incidence_rate")) {
    return(results$estimates)
  }

  if (is.data.frame(results)) {
    return(results)
  }

  stop("Unsupported results object for safety table.", call. = FALSE)
}

.results_create_subgroup_table <- function(results) {
  if (inherits(results, "rwe_subgroup_analysis")) {
    return(results$results)
  }

  if (is.data.frame(results)) {
    return(results)
  }

  stop("Unsupported results object for subgroup table.", call. = FALSE)
}

.results_format_gt <- function(data, title = NULL, footnote = NULL) {
  gt_available <- suppressWarnings(
    tryCatch({
      requireNamespace("gt", quietly = TRUE)
      TRUE
    }, warning = function(w) {
      FALSE
    }, error = function(e) {
      FALSE
    })
  )

  if (!gt_available) {
    warning("Package 'gt' not available; returning data frame.", call. = FALSE)
    return(data)
  }

  gt_table <- suppressWarnings(
    tryCatch(
      gt::gt(data),
      warning = function(w) {
        warning("Unable to format table with gt: ", conditionMessage(w),
                " Returning data frame.", call. = FALSE)
        return(data)
      },
      error = function(e) {
        warning("Unable to format table with gt: ", conditionMessage(e),
                " Returning data frame.", call. = FALSE)
        return(data)
      }
    )
  )

  if (is.data.frame(gt_table)) {
    return(gt_table)
  }

  if (!is.null(title)) {
    gt_table <- gt::tab_header(gt_table, title = title)
  }

  if (!is.null(footnote)) {
    gt_table <- gt::tab_footnote(gt_table, gt::md(footnote))
  }

  gt_table
}

.results_format_rtables <- function(data, title = NULL) {
  if (!requireNamespace("rtables", quietly = TRUE)) {
    warning("Package 'rtables' not installed; returning data frame.", call. = FALSE)
    return(data)
  }

  layout <- rtables::basic_table(title = title)
  rtables::build_table(layout, data)
}

.results_format_flextable <- function(data, title = NULL) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    warning("Package 'flextable' not installed; returning data frame.", call. = FALSE)
    return(data)
  }

  ft <- flextable::flextable(data)
  if (!is.null(title)) {
    ft <- flextable::add_header_lines(ft, title)
  }
  ft
}

#' Generate Structured Results Table
#'
#' @description
#' Creates a formatted results table for regulatory reporting using the
#' specified backend (gt, rtables, or flextable). Supports demographic,
#' effectiveness, safety, and subgroup outputs from rwevidence analyses.
#'
#' @param results Analysis results object (e.g., \code{rwe_table_one},
#'   \code{rwe_relative_risk}, \code{rwe_safety_analysis},
#'   \code{rwe_subgroup_analysis}) or a pre-formatted data frame.
#' @param table_type Table type to generate: "demographics", "effectiveness",
#'   "safety", or "subgroup".
#' @param format Output format: "data" (raw data frame), "gt", "rtables", or "flextable".
#' @param style Table style preset: "regulatory", "publication", or "custom"
#'   (currently informational for future extensions).
#' @param title Optional table title.
#' @param footnote Optional footnote text (supports Markdown in gt).
#' @param quiet Suppress console messages (default: FALSE).
#'
#' @return An object of class \code{rwe_results_table} containing the raw data,
#'   formatted table (if backend available), and metadata.
#' @export
rwe_results_table <- function(results,
                              table_type = c("demographics", "effectiveness", "safety", "subgroup"),
                              format = c("data", "gt", "rtables", "flextable"),
                              style = c("regulatory", "publication", "custom"),
                              title = NULL,
                              footnote = NULL,
                              quiet = FALSE) {

  table_type <- match.arg(table_type)
  format <- match.arg(format)
  style <- match.arg(style)

  data <- switch(
    table_type,
    demographics = .results_create_demographics_table(results),
    effectiveness = .results_create_effectiveness_table(results),
    safety = .results_create_safety_table(results),
    subgroup = .results_create_subgroup_table(results)
  )

  formatted <- switch(
    format,
    data = data,
    gt = .results_format_gt(data, title = title, footnote = footnote),
    rtables = .results_format_rtables(data, title = title),
    flextable = .results_format_flextable(data, title = title)
  )

  if (!quiet) {
    cat("Results table generated (type:", table_type, ", format:", format, ")\n")
  }

  audit <- create_audit_trail(
    operation = "results_table",
    input_data = list(
      table_type = table_type,
      format = format
    ),
    output_data = list(
      n_rows = nrow(data),
      n_columns = ncol(data)
    ),
    parameters = list(
      style = style,
      title = title
    )
  )

  structure(
    list(
      data = data,
      table = formatted,
      table_type = table_type,
      format = format,
      style = style,
      title = title,
      footnote = footnote,
      audit_trail = audit
    ),
    class = c("rwe_results_table", "rwe_analysis")
  )
}

#' @export
print.rwe_results_table <- function(x, ...) {
  cat("Results Table (", x$table_type, ")\n", sep = "")
  cat(paste(rep("=", 40), collapse = ""), "\n\n")
  if (is.data.frame(x$table)) {
    print(x$table, row.names = FALSE)
  } else {
    print(x$table)
  }
  invisible(x)
}
