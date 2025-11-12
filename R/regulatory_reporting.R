#' Regulatory Reporting Functions
#'
#' @description
#' Functions for generating regulatory-compliant reports, tables, and diagrams
#' for FDA/EMA submissions and Real-World Evidence studies.
#'
#' @name regulatory_reporting
NULL

#' Generate Table One (Baseline Characteristics)
#'
#' @description
#' Creates a standardized Table 1 showing baseline characteristics by treatment
#' group, commonly required for regulatory submissions. Includes summary statistics,
#' statistical tests, and standardized mean differences.
#'
#' @param data Data frame or rwe_data object containing study data
#' @param group_var Character. Variable name for grouping (e.g., "treatment")
#' @param variables Character vector of variables to include in table
#' @param categorical_vars Character vector of categorical variables (optional, auto-detected if NULL)
#' @param continuous_vars Character vector of continuous variables (optional, auto-detected if NULL)
#' @param test Logical. Include statistical tests (default: TRUE)
#' @param smd Logical. Include standardized mean differences (default: TRUE)
#' @param overall Logical. Include overall column (default: TRUE)
#' @param missing Logical. Report missing values (default: TRUE)
#' @param digits Integer. Number of decimal places (default: 1)
#'
#' @return An object of class \code{rwe_table_one} containing:
#' \item{table}{Data frame with formatted table}
#' \item{summary}{Summary statistics by group}
#' \item{tests}{Statistical test results}
#' \item{smd}{Standardized mean differences}
#' \item{n_groups}{Number of groups}
#' \item{n_total}{Total sample size}
#' \item{audit_trail}{Audit trail}
#'
#' @examples
#' \dontrun{
#' # Create Table 1 for baseline characteristics
#' table1 <- rwe_table_one(
#'   data = study_data,
#'   group_var = "treatment",
#'   variables = c("age", "sex", "bmi", "disease_stage"),
#'   test = TRUE,
#'   smd = TRUE
#' )
#'
#' print(table1)
#' export_table_one(table1, "table1.html")
#' }
#'
#' @export
rwe_table_one <- function(data,
                           group_var,
                           variables,
                           categorical_vars = NULL,
                           continuous_vars = NULL,
                           test = TRUE,
                           smd = TRUE,
                           overall = TRUE,
                           missing = TRUE,
                           digits = 1) {

  log_info("Creating Table 1 (baseline characteristics)", context = "rwe_table_one")

  # Extract data
  if (is_rwe_data(data)) {
    data_df <- data$data
  } else {
    data_df <- data
  }

  validate_inputs(data_df)

  # Validate group variable
  if (!group_var %in% names(data_df)) {
    stop("Group variable '", group_var, "' not found in data", call. = FALSE)
  }

  # Validate variables
  missing_vars <- setdiff(variables, names(data_df))
  if (length(missing_vars) > 0) {
    stop("Variables not found in data: ", paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  cat("Table 1: Baseline Characteristics\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  # Auto-detect variable types if not specified
  if (is.null(categorical_vars) && is.null(continuous_vars)) {
    categorical_vars <- variables[sapply(variables, function(v) {
      is.character(data_df[[v]]) || is.factor(data_df[[v]]) || length(unique(data_df[[v]])) <= 10
    })]
    continuous_vars <- setdiff(variables, categorical_vars)
  }

  # Get group levels
  groups <- unique(data_df[[group_var]])
  n_groups <- length(groups)

  cat("Groups:", paste(groups, collapse = ", "), "\n")
  cat("Variables:", length(variables), "\n")
  cat("  Continuous:", length(continuous_vars), "\n")
  cat("  Categorical:", length(categorical_vars), "\n\n")

  # Initialize results
  table_rows <- list()
  summary_stats <- list()
  test_results <- list()
  smd_results <- list()

  # Overall statistics
  if (overall) {
    n_overall <- nrow(data_df)
  }

  # Group sample sizes
  group_ns <- table(data_df[[group_var]])

  # Process continuous variables
  if (length(continuous_vars) > 0) {
    cat("Processing continuous variables...\n")
    for (var in continuous_vars) {
      var_data <- data_df[[var]]

      # Overall stats
      if (overall) {
        overall_mean <- mean(var_data, na.rm = TRUE)
        overall_sd <- sd(var_data, na.rm = TRUE)
        overall_miss <- sum(is.na(var_data))
      }

      # Group stats
      group_stats <- list()
      for (grp in groups) {
        grp_data <- var_data[data_df[[group_var]] == grp]
        group_stats[[as.character(grp)]] <- list(
          mean = mean(grp_data, na.rm = TRUE),
          sd = sd(grp_data, na.rm = TRUE),
          median = median(grp_data, na.rm = TRUE),
          q25 = quantile(grp_data, 0.25, na.rm = TRUE),
          q75 = quantile(grp_data, 0.75, na.rm = TRUE),
          n_missing = sum(is.na(grp_data))
        )
      }

      summary_stats[[var]] <- group_stats

      # Statistical test
      if (test && n_groups == 2) {
        test_result <- tryCatch({
          t_test <- t.test(var_data ~ data_df[[group_var]])
          list(method = "t-test", p_value = t_test$p.value)
        }, error = function(e) {
          list(method = "t-test", p_value = NA)
        })
        test_results[[var]] <- test_result
      } else if (test && n_groups > 2) {
        test_result <- tryCatch({
          aov_test <- aov(var_data ~ data_df[[group_var]])
          p_val <- summary(aov_test)[[1]]["Pr(>F)"][1, 1]
          list(method = "ANOVA", p_value = p_val)
        }, error = function(e) {
          list(method = "ANOVA", p_value = NA)
        })
        test_results[[var]] <- test_result
      }

      # SMD (for 2 groups)
      if (smd && n_groups == 2) {
        grp1_data <- var_data[data_df[[group_var]] == groups[1]]
        grp2_data <- var_data[data_df[[group_var]] == groups[2]]

        mean1 <- mean(grp1_data, na.rm = TRUE)
        mean2 <- mean(grp2_data, na.rm = TRUE)
        sd1 <- sd(grp1_data, na.rm = TRUE)
        sd2 <- sd(grp2_data, na.rm = TRUE)

        pooled_sd <- sqrt((sd1^2 + sd2^2) / 2)
        smd_val <- if (pooled_sd > 1e-10) {
          (mean1 - mean2) / pooled_sd
        } else {
          0
        }

        smd_results[[var]] <- smd_val
      }
    }
  }

  # Process categorical variables
  if (length(categorical_vars) > 0) {
    cat("Processing categorical variables...\n")
    for (var in categorical_vars) {
      var_data <- data_df[[var]]

      # Overall frequencies
      if (overall) {
        overall_table <- table(var_data, useNA = if (missing) "ifany" else "no")
        overall_prop <- prop.table(overall_table)
      }

      # Group frequencies
      group_stats <- list()
      for (grp in groups) {
        grp_data <- var_data[data_df[[group_var]] == grp]
        grp_table <- table(grp_data, useNA = if (missing) "ifany" else "no")
        grp_prop <- prop.table(grp_table)

        group_stats[[as.character(grp)]] <- list(
          counts = grp_table,
          proportions = grp_prop,
          n_missing = sum(is.na(grp_data))
        )
      }

      summary_stats[[var]] <- group_stats

      # Chi-square test
      if (test) {
        test_result <- tryCatch({
          contingency_table <- table(data_df[[group_var]], var_data)
          chi_test <- chisq.test(contingency_table)
          list(method = "Chi-square", p_value = chi_test$p.value)
        }, error = function(e) {
          # Try Fisher's exact for small samples
          tryCatch({
            contingency_table <- table(data_df[[group_var]], var_data)
            fisher_test <- fisher.test(contingency_table, simulate.p.value = TRUE)
            list(method = "Fisher's exact", p_value = fisher_test$p.value)
          }, error = function(e2) {
            list(method = "Chi-square", p_value = NA)
          })
        })
        test_results[[var]] <- test_result
      }

      # SMD for binary variables (2 groups)
      if (smd && n_groups == 2 && length(unique(na.omit(var_data))) == 2) {
        grp1_data <- var_data[data_df[[group_var]] == groups[1]]
        grp2_data <- var_data[data_df[[group_var]] == groups[2]]

        # Proportion of first level
        levels_var <- unique(na.omit(var_data))
        p1 <- mean(grp1_data == levels_var[1], na.rm = TRUE)
        p2 <- mean(grp2_data == levels_var[1], na.rm = TRUE)

        # SMD for binary variables
        smd_val <- (p1 - p2) / sqrt((p1 * (1 - p1) + p2 * (1 - p2)) / 2)
        smd_results[[var]] <- smd_val
      }
    }
  }

  cat("\nTable 1 creation complete!\n")

  log_info("Table 1 created", context = "rwe_table_one")

  # Create audit trail
  audit <- create_audit_trail(
    operation = "table_one_creation",
    input_data = list(
      n_total = nrow(data_df),
      n_groups = n_groups,
      n_variables = length(variables)
    ),
    output_data = list(
      table_created = TRUE
    ),
    parameters = list(
      group_var = group_var,
      variables = variables,
      test = test,
      smd = smd,
      overall = overall
    )
  )

  # Create result object
  result <- structure(
    list(
      summary = summary_stats,
      tests = test_results,
      smd = smd_results,
      group_var = group_var,
      groups = groups,
      n_groups = n_groups,
      n_total = nrow(data_df),
      group_ns = as.list(group_ns),
      variables = variables,
      continuous_vars = continuous_vars,
      categorical_vars = categorical_vars,
      settings = list(
        test = test,
        smd = smd,
        overall = overall,
        missing = missing,
        digits = digits
      ),
      audit_trail = audit
    ),
    class = c("rwe_table_one", "rwe_analysis")
  )

  return(result)
}


#' Print Method for Table One
#'
#' @param x An object of class \code{rwe_table_one}
#' @param ... Additional arguments (not used)
#'
#' @export
print.rwe_table_one <- function(x, ...) {
  cat("Table 1: Baseline Characteristics\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  cat("Sample Sizes:\n")
  for (grp in names(x$group_ns)) {
    cat("  ", grp, ": n =", x$group_ns[[grp]], "\n")
  }
  cat("  Total: n =", x$n_total, "\n\n")

  cat("Variables Summarized:\n")
  cat("  Continuous:", length(x$continuous_vars), "\n")
  cat("  Categorical:", length(x$categorical_vars), "\n\n")

  if (x$settings$test) {
    cat("Statistical tests included\n")
  }

  if (x$settings$smd) {
    cat("Standardized mean differences included\n")
  }

  cat("\nUse summary() for detailed statistics\n")
  cat("Use export_table_one() to export formatted table\n")

  invisible(x)
}


#' Summary Method for Table One
#'
#' @param object An object of class \code{rwe_table_one}
#' @param ... Additional arguments (not used)
#'
#' @export
summary.rwe_table_one <- function(object, ...) {
  digits <- object$settings$digits

  cat("Table 1: Baseline Characteristics - Detailed Summary\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")

  # Header
  cat(sprintf("%-25s", "Variable"))
  for (grp in object$groups) {
    cat(sprintf("%15s", grp))
  }
  if (object$settings$test) {
    cat(sprintf("%12s", "P-value"))
  }
  if (object$settings$smd && object$n_groups == 2) {
    cat(sprintf("%10s", "SMD"))
  }
  cat("\n")
  cat(paste(rep("-", 70), collapse = ""), "\n")

  # Continuous variables
  if (length(object$continuous_vars) > 0) {
    for (var in object$continuous_vars) {
      cat(sprintf("%-25s", var))

      for (grp in object$groups) {
        stats <- object$summary[[var]][[as.character(grp)]]
        cat(sprintf("%15s", paste0(round(stats$mean, digits), " (", round(stats$sd, digits), ")")))
      }

      if (object$settings$test && var %in% names(object$tests)) {
        p_val <- object$tests[[var]]$p_value
        p_str <- if (is.na(p_val)) {
          "NA"
        } else if (p_val < 0.001) {
          "<0.001"
        } else {
          sprintf("%.3f", p_val)
        }
        cat(sprintf("%12s", p_str))
      }

      if (object$settings$smd && var %in% names(object$smd)) {
        cat(sprintf("%10.3f", object$smd[[var]]))
      }

      cat("\n")
    }
  }

  # Categorical variables
  if (length(object$categorical_vars) > 0) {
    for (var in object$categorical_vars) {
      cat(sprintf("%-25s", var))
      cat("\n")

      # Get all levels
      all_levels <- unique(unlist(lapply(object$summary[[var]], function(g) {
        names(g$counts)
      })))

      for (level in all_levels) {
        cat(sprintf("  %-23s", level))

        for (grp in object$groups) {
          stats <- object$summary[[var]][[as.character(grp)]]
          count <- if (level %in% names(stats$counts)) stats$counts[[level]] else 0
          prop <- if (level %in% names(stats$proportions)) stats$proportions[[level]] * 100 else 0
          cat(sprintf("%15s", paste0(count, " (", round(prop, digits), "%)")))
        }

        cat("\n")
      }

      # P-value on first line only
      if (object$settings$test && var %in% names(object$tests)) {
        cat(sprintf("%-25s", ""))
        for (grp in object$groups) {
          cat(sprintf("%15s", ""))
        }
        p_val <- object$tests[[var]]$p_value
        p_str <- if (is.na(p_val)) {
          "NA"
        } else if (p_val < 0.001) {
          "<0.001"
        } else {
          sprintf("%.3f", p_val)
        }
        cat(sprintf("%12s", p_str))

        if (object$settings$smd && var %in% names(object$smd)) {
          cat(sprintf("%10.3f", object$smd[[var]]))
        }

        cat("\n")
      }
    }
  }

  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("\nContinuous variables: Mean (SD)\n")
  cat("Categorical variables: n (%)\n")
  if (object$settings$test) {
    cat("P-values: t-test/ANOVA for continuous, Chi-square/Fisher's for categorical\n")
  }
  if (object$settings$smd) {
    cat("SMD: Standardized Mean Difference\n")
  }

  invisible(object)
}


#' Export Table One to File
#'
#' @description
#' Exports a Table 1 object to various formats (HTML, CSV, Word-compatible)
#'
#' @param table_one An object of class \code{rwe_table_one}
#' @param output_file Path to output file
#' @param format Output format: "html", "csv", "latex" (auto-detected from extension)
#' @param title Optional title for the table
#'
#' @return Invisible NULL (file written as side effect)
#'
#' @examples
#' \dontrun{
#' export_table_one(table1, "baseline_characteristics.html")
#' export_table_one(table1, "table1.csv")
#' }
#'
#' @export
export_table_one <- function(table_one, output_file, format = NULL, title = "Table 1: Baseline Characteristics") {

  if (!inherits(table_one, "rwe_table_one")) {
    stop("table_one must be an rwe_table_one object", call. = FALSE)
  }

  # Auto-detect format
  if (is.null(format)) {
    ext <- tolower(tools::file_ext(output_file))
    format <- switch(ext,
                    html = "html",
                    csv = "csv",
                    tex = "latex",
                    "csv")
  }

  if (format == "html") {
    export_table_one_html(table_one, output_file, title)
  } else if (format == "csv") {
    export_table_one_csv(table_one, output_file)
  } else if (format == "latex") {
    export_table_one_latex(table_one, output_file, title)
  } else {
    stop("Unsupported format: ", format, call. = FALSE)
  }

  cat("Table 1 exported to:", output_file, "\n")
  invisible(NULL)
}


# Internal export functions -----------------------------------------------

#' Export Table One to HTML
#' @keywords internal
export_table_one_html <- function(table_one, output_file, title) {

  html <- paste0(
    "<!DOCTYPE html>\n",
    "<html>\n",
    "<head>\n",
    "<style>\n",
    "table { border-collapse: collapse; width: 100%; font-family: Arial, sans-serif; }\n",
    "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }\n",
    "th { background-color: #4CAF50; color: white; }\n",
    "tr:nth-child(even) { background-color: #f2f2f2; }\n",
    "h2 { font-family: Arial, sans-serif; }\n",
    "</style>\n",
    "</head>\n",
    "<body>\n",
    "<h2>", title, "</h2>\n",
    "<table>\n"
  )

  # Header row
  html <- paste0(html, "<tr><th>Variable</th>")
  for (grp in table_one$groups) {
    html <- paste0(html, "<th>", grp, "<br>(n=", table_one$group_ns[[as.character(grp)]], ")</th>")
  }
  if (table_one$settings$test) {
    html <- paste0(html, "<th>P-value</th>")
  }
  if (table_one$settings$smd && table_one$n_groups == 2) {
    html <- paste0(html, "<th>SMD</th>")
  }
  html <- paste0(html, "</tr>\n")

  # Continuous variables
  if (length(table_one$continuous_vars) > 0) {
    for (var in table_one$continuous_vars) {
      html <- paste0(html, "<tr><td><b>", var, "</b>, mean (SD)</td>")

      for (grp in table_one$groups) {
        stats <- table_one$summary[[var]][[as.character(grp)]]
        html <- paste0(html, "<td>", round(stats$mean, table_one$settings$digits),
                      " (", round(stats$sd, table_one$settings$digits), ")</td>")
      }

      if (table_one$settings$test && var %in% names(table_one$tests)) {
        p_val <- table_one$tests[[var]]$p_value
        p_str <- if (is.na(p_val)) "NA" else if (p_val < 0.001) "&lt;0.001" else sprintf("%.3f", p_val)
        html <- paste0(html, "<td>", p_str, "</td>")
      }

      if (table_one$settings$smd && var %in% names(table_one$smd)) {
        html <- paste0(html, "<td>", sprintf("%.3f", table_one$smd[[var]]), "</td>")
      }

      html <- paste0(html, "</tr>\n")
    }
  }

  # Categorical variables
  if (length(table_one$categorical_vars) > 0) {
    for (var in table_one$categorical_vars) {
      html <- paste0(html, "<tr><td colspan='", 2 + table_one$n_groups, "'><b>", var, "</b>, n (%)</td></tr>\n")

      all_levels <- unique(unlist(lapply(table_one$summary[[var]], function(g) names(g$counts))))

      for (level in all_levels) {
        html <- paste0(html, "<tr><td>&nbsp;&nbsp;", level, "</td>")

        for (grp in table_one$groups) {
          stats <- table_one$summary[[var]][[as.character(grp)]]
          count <- if (level %in% names(stats$counts)) stats$counts[[level]] else 0
          prop <- if (level %in% names(stats$proportions)) stats$proportions[[level]] * 100 else 0
          html <- paste0(html, "<td>", count, " (", round(prop, table_one$settings$digits), "%)</td>")
        }

        html <- paste0(html, "<td></td></tr>\n")
      }

      # P-value row
      if (table_one$settings$test && var %in% names(table_one$tests)) {
        html <- paste0(html, "<tr><td>&nbsp;&nbsp;<i>P-value</i></td>")
        for (grp in table_one$groups) {
          html <- paste0(html, "<td></td>")
        }
        p_val <- table_one$tests[[var]]$p_value
        p_str <- if (is.na(p_val)) "NA" else if (p_val < 0.001) "&lt;0.001" else sprintf("%.3f", p_val)
        html <- paste0(html, "<td>", p_str, "</td></tr>\n")
      }
    }
  }

  html <- paste0(html, "</table>\n</body>\n</html>")

  writeLines(html, output_file)
}


#' Export Table One to CSV
#' @keywords internal
export_table_one_csv <- function(table_one, output_file) {

  rows <- list()

  # Header
  header <- c("Variable", as.character(table_one$groups))
  if (table_one$settings$test) header <- c(header, "P-value")
  if (table_one$settings$smd && table_one$n_groups == 2) header <- c(header, "SMD")
  rows[[1]] <- header

  # Continuous variables
  if (length(table_one$continuous_vars) > 0) {
    for (var in table_one$continuous_vars) {
      row <- c(paste0(var, ", mean (SD)"))

      for (grp in table_one$groups) {
        stats <- table_one$summary[[var]][[as.character(grp)]]
        row <- c(row, paste0(round(stats$mean, table_one$settings$digits), " (",
                            round(stats$sd, table_one$settings$digits), ")"))
      }

      if (table_one$settings$test && var %in% names(table_one$tests)) {
        p_val <- table_one$tests[[var]]$p_value
        p_str <- if (is.na(p_val)) "NA" else if (p_val < 0.001) "<0.001" else sprintf("%.3f", p_val)
        row <- c(row, p_str)
      }

      if (table_one$settings$smd && var %in% names(table_one$smd)) {
        row <- c(row, sprintf("%.3f", table_one$smd[[var]]))
      }

      rows[[length(rows) + 1]] <- row
    }
  }

  # Write CSV
  df <- do.call(rbind, lapply(rows, function(r) {
    data.frame(t(r), stringsAsFactors = FALSE)
  }))
  names(df) <- NULL

  write.csv(df, output_file, row.names = FALSE, quote = FALSE)
}


#' Export Table One to LaTeX
#'
#' @description
#' Exports Table 1 to LaTeX format suitable for journal submissions
#'
#' @param table_one rwe_table_one object
#' @param output_file Path to output .tex file
#' @param title Table caption
#' @param label LaTeX label for referencing
#' @param booktabs Use booktabs package style (recommended)
#' @param longtable Use longtable for multi-page tables
#'
#' @importFrom utils capture.output
#' @keywords internal
export_table_one_latex <- function(table_one,
                                   output_file,
                                   title,
                                   label = "tab:table1",
                                   booktabs = TRUE,
                                   longtable = FALSE) {

  # Check for kableExtra package (preferred) or fall back to xtable
  has_kableExtra <- requireNamespace("kableExtra", quietly = TRUE)
  has_xtable <- requireNamespace("xtable", quietly = TRUE)

  if (!has_kableExtra && !has_xtable) {
    stop("LaTeX export requires either kableExtra or xtable package.\n",
         "Install with: install.packages('kableExtra') or install.packages('xtable')",
         call. = FALSE)
  }

  # Extract data frame from table_one object
  if (inherits(table_one, "rwe_table_one")) {
    df <- table_one$table
  } else if (is.data.frame(table_one)) {
    df <- table_one
  } else {
    stop("table_one must be a data frame or rwe_table_one object", call. = FALSE)
  }

  # Create LaTeX table
  if (has_kableExtra) {
    # Use kableExtra for better formatting
    latex_table <- knitr::kable(
      df,
      format = "latex",
      booktabs = booktabs,
      longtable = longtable,
      caption = title,
      label = label,
      escape = FALSE,
      linesep = ""
    )

    # Add styling
    latex_table <- kableExtra::kable_styling(
      latex_table,
      latex_options = if (longtable) c("repeat_header") else NULL,
      position = "center"
    )

    latex_output <- as.character(latex_table)

  } else {
    # Fall back to xtable
    xt <- xtable::xtable(
      df,
      caption = title,
      label = label
    )

    # Convert to LaTeX
    latex_output <- capture.output(
      print(
        xt,
        type = "latex",
        include.rownames = FALSE,
        caption.placement = "top",
        booktabs = booktabs,
        sanitize.text.function = function(x) x
      )
    )

    latex_output <- paste(latex_output, collapse = "\n")
  }

  # Add LaTeX preamble comments
  header <- paste0(
    "% Table 1: Baseline Characteristics\n",
    "% Generated by rwevidence package\n",
    "% Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
    "%\n",
    if (booktabs) "% Requires \\usepackage{booktabs}\n" else "",
    if (longtable) "% Requires \\usepackage{longtable}\n" else "",
    "\n"
  )

  # Write to file
  writeLines(paste0(header, latex_output), output_file)

  message("LaTeX table exported to: ", output_file)
  message("Include in your LaTeX document with: \\input{", basename(output_file), "}")

  invisible(latex_output)
}


#' Generate CONSORT Flow Diagram
#'
#' @description
#' Creates a CONSORT (Consolidated Standards of Reporting Trials) flow diagram
#' showing patient flow through a study, including screening, enrollment,
#' allocation, follow-up, and analysis stages.
#'
#' @param screened Integer. Number of patients screened
#' @param enrolled Integer. Number of patients enrolled
#' @param excluded List of exclusion reasons with counts
#' @param allocated List of treatment arm allocations
#' @param lost_to_followup List of lost-to-follow-up counts by arm
#' @param discontinued List of discontinued counts and reasons by arm
#' @param analyzed List of patients analyzed by arm
#' @param study_type Type of study: "rct", "cohort", "external_control"
#' @param title Character. Diagram title
#'
#' @return An object of class \code{rwe_consort} containing:
#' \item{flow}{Data frame with flow statistics}
#' \item{exclusions}{Exclusion summary}
#' \item{retention}{Retention rates by arm}
#' \item{diagram_data}{Data for plotting}
#' \item{audit_trail}{Audit trail}
#'
#' @examples
#' \dontrun{
#' consort <- rwe_consort_diagram(
#'   screened = 500,
#'   enrolled = 350,
#'   excluded = list(
#'     "Did not meet inclusion criteria" = 100,
#'     "Declined to participate" = 30,
#'     "Other reasons" = 20
#'   ),
#'   allocated = list(
#'     "Treatment" = 175,
#'     "Control" = 175
#'   ),
#'   lost_to_followup = list(
#'     "Treatment" = 10,
#'     "Control" = 15
#'   ),
#'   discontinued = list(
#'     "Treatment" = list("Adverse events" = 5, "Withdrew consent" = 3),
#'     "Control" = list("Adverse events" = 2, "Withdrew consent" = 8)
#'   ),
#'   analyzed = list(
#'     "Treatment" = 157,
#'     "Control" = 150
#'   )
#' )
#'
#' print(consort)
#' }
#'
#' @export
rwe_consort_diagram <- function(screened,
                                 enrolled,
                                 excluded = NULL,
                                 allocated,
                                 lost_to_followup = NULL,
                                 discontinued = NULL,
                                 analyzed,
                                 study_type = c("rct", "cohort", "external_control"),
                                 title = "CONSORT Flow Diagram") {

  log_info("Creating CONSORT flow diagram", context = "rwe_consort_diagram")

  study_type <- match.arg(study_type)

  cat("CONSORT Flow Diagram\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  # Validate inputs
  if (screened < enrolled) {
    stop("Enrolled cannot exceed screened", call. = FALSE)
  }

  if (sum(unlist(allocated)) != enrolled) {
    warning("Sum of allocated does not equal enrolled")
  }

  arms <- names(allocated)
  n_arms <- length(arms)

  cat("Study Type:", study_type, "\n")
  cat("Number of Arms:", n_arms, "\n\n")

  # Screening and enrollment
  cat("SCREENING & ENROLLMENT\n")
  cat("  Assessed for eligibility:", screened, "\n")

  if (!is.null(excluded) && length(excluded) > 0) {
    total_excluded <- sum(unlist(excluded))
    cat("  Excluded:", total_excluded, "\n")
    for (reason in names(excluded)) {
      cat("    -", reason, ":", excluded[[reason]], "\n")
    }
  }

  cat("  Enrolled:", enrolled, "\n\n")

  # Allocation
  cat("ALLOCATION\n")
  for (arm in arms) {
    cat("  ", arm, ":", allocated[[arm]], "\n")
  }
  cat("\n")

  # Follow-up
  if (!is.null(lost_to_followup) || !is.null(discontinued)) {
    cat("FOLLOW-UP\n")

    for (arm in arms) {
      n_allocated <- allocated[[arm]]
      n_lost <- if (!is.null(lost_to_followup)) lost_to_followup[[arm]] else 0
      n_discontinued <- if (!is.null(discontinued)) sum(unlist(discontinued[[arm]])) else 0

      cat("  ", arm, ":\n")
      if (n_lost > 0) {
        cat("    Lost to follow-up:", n_lost, "\n")
      }
      if (n_discontinued > 0) {
        cat("    Discontinued:", n_discontinued, "\n")
        if (is.list(discontinued[[arm]])) {
          for (reason in names(discontinued[[arm]])) {
            cat("      -", reason, ":", discontinued[[arm]][[reason]], "\n")
          }
        }
      }

      n_completed <- n_allocated - n_lost - n_discontinued
      cat("    Completed follow-up:", n_completed, "\n")
    }
    cat("\n")
  }

  # Analysis
  cat("ANALYSIS\n")
  total_analyzed <- 0
  for (arm in arms) {
    cat("  ", arm, "analyzed:", analyzed[[arm]], "\n")
    total_analyzed <- total_analyzed + analyzed[[arm]]
  }
  cat("  Total analyzed:", total_analyzed, "\n\n")

  # Calculate retention rates
  retention_rates <- list()
  for (arm in arms) {
    retention_rates[[arm]] <- (analyzed[[arm]] / allocated[[arm]]) * 100
  }

  cat("RETENTION RATES\n")
  for (arm in arms) {
    cat("  ", arm, ":", round(retention_rates[[arm]], 1), "%\n")
  }
  cat("\n")

  # Overall attrition
  overall_attrition <- ((enrolled - total_analyzed) / enrolled) * 100
  cat("Overall Attrition:", round(overall_attrition, 1), "%\n")

  cat("\nCONSORT diagram data created successfully!\n")

  log_info("CONSORT diagram created", context = "rwe_consort_diagram")

  # Create audit trail
  audit <- create_audit_trail(
    operation = "consort_diagram_creation",
    input_data = list(
      screened = screened,
      enrolled = enrolled
    ),
    output_data = list(
      total_analyzed = total_analyzed,
      overall_attrition = overall_attrition
    ),
    parameters = list(
      study_type = study_type,
      n_arms = n_arms,
      arms = arms
    )
  )

  # Create result object
  result <- structure(
    list(
      screened = screened,
      enrolled = enrolled,
      excluded = excluded,
      allocated = allocated,
      lost_to_followup = lost_to_followup,
      discontinued = discontinued,
      analyzed = analyzed,
      retention_rates = retention_rates,
      overall_attrition = overall_attrition,
      study_type = study_type,
      arms = arms,
      n_arms = n_arms,
      title = title,
      audit_trail = audit
    ),
    class = c("rwe_consort", "rwe_analysis")
  )

  return(result)
}


#' Print Method for CONSORT Diagrams
#'
#' @param x An object of class \code{rwe_consort}
#' @param ... Additional arguments (not used)
#'
#' @export
print.rwe_consort <- function(x, ...) {
  cat("CONSORT Flow Diagram\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  cat("Study Type:", x$study_type, "\n")
  cat("Number of Arms:", x$n_arms, "\n\n")

  cat("Flow Summary:\n")
  cat("  Screened:", x$screened, "\n")
  cat("  Enrolled:", x$enrolled, "\n")

  if (!is.null(x$excluded)) {
    cat("  Excluded:", sum(unlist(x$excluded)), "\n")
  }

  cat("  Total Analyzed:", sum(unlist(x$analyzed)), "\n")
  cat("  Overall Attrition:", round(x$overall_attrition, 1), "%\n\n")

  cat("Retention Rates by Arm:\n")
  for (arm in x$arms) {
    cat("  ", arm, ":", round(x$retention_rates[[arm]], 1), "%\n")
  }

  cat("\nUse summary() for detailed flow information\n")

  invisible(x)
}


#' Summary Method for CONSORT Diagrams
#'
#' @param object An object of class \code{rwe_consort}
#' @param ... Additional arguments (not used)
#'
#' @export
summary.rwe_consort <- function(object, ...) {
  cat("CONSORT Flow Diagram - Detailed Summary\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")

  cat("ENROLLMENT\n")
  cat(sprintf("  %-40s %10d\n", "Assessed for eligibility", object$screened))

  if (!is.null(object$excluded) && length(object$excluded) > 0) {
    cat(sprintf("  %-40s %10d\n", "Excluded (total)", sum(unlist(object$excluded))))
    for (reason in names(object$excluded)) {
      cat(sprintf("    %-38s %10d\n", reason, object$excluded[[reason]]))
    }
  }

  cat(sprintf("  %-40s %10d\n", "Enrolled and randomized/allocated", object$enrolled))
  cat("\n")

  cat("ALLOCATION\n")
  for (arm in object$arms) {
    cat(sprintf("  %-40s %10d\n", paste("Allocated to", arm), object$allocated[[arm]]))
  }
  cat("\n")

  if (!is.null(object$lost_to_followup) || !is.null(object$discontinued)) {
    cat("FOLLOW-UP\n")
    for (arm in object$arms) {
      n_lost <- if (!is.null(object$lost_to_followup)) object$lost_to_followup[[arm]] else 0
      n_discontinued <- if (!is.null(object$discontinued)) sum(unlist(object$discontinued[[arm]])) else 0

      cat("  ", arm, ":\n")
      if (n_lost > 0) {
        cat(sprintf("    %-36s %10d\n", "Lost to follow-up", n_lost))
      }
      if (n_discontinued > 0) {
        cat(sprintf("    %-36s %10d\n", "Discontinued intervention", n_discontinued))
        if (is.list(object$discontinued[[arm]])) {
          for (reason in names(object$discontinued[[arm]])) {
            cat(sprintf("      %-34s %10d\n", reason, object$discontinued[[arm]][[reason]]))
          }
        }
      }
    }
    cat("\n")
  }

  cat("ANALYSIS\n")
  for (arm in object$arms) {
    cat(sprintf("  %-40s %10d (%.1f%%)\n",
                paste("Analyzed -", arm),
                object$analyzed[[arm]],
                object$retention_rates[[arm]]))
  }
  cat(sprintf("  %-40s %10d\n", "Total analyzed", sum(unlist(object$analyzed))))
  cat("\n")

  cat("ATTRITION\n")
  cat(sprintf("  %-40s %9.1f%%\n", "Overall attrition rate", object$overall_attrition))
  for (arm in object$arms) {
    attrition <- 100 - object$retention_rates[[arm]]
    cat(sprintf("  %-40s %9.1f%%\n", paste(arm, "attrition"), attrition))
  }

  invisible(object)
}


#' Generate Safety Report (Adverse Events)
#'
#' @description
#' Creates a comprehensive safety report summarizing adverse events by treatment
#' group, including incidence rates, severity, relatedness, and serious adverse events.
#' Suitable for regulatory submissions.
#'
#' @param data Data frame or rwe_data object containing adverse event data
#' @param group_var Character. Treatment group variable
#' @param ae_var Character. Adverse event variable (event type/name)
#' @param severity_var Character. Severity variable (optional)
#' @param relatedness_var Character. Relatedness to treatment variable (optional)
#' @param serious_var Character. Serious AE indicator variable (optional)
#' @param n_per_group Named list of sample sizes per group (for rate calculation)
#' @param sort_by Character. Sort AEs by: "frequency", "severity", "alphabetical" (default: "frequency")
#' @param top_n Integer. Show top N most frequent AEs (NULL for all)
#'
#' @return An object of class \code{rwe_safety_report} containing:
#' \item{ae_summary}{Summary of adverse events by group}
#' \item{overall_rates}{Overall AE rates}
#' \item{serious_ae}{Serious adverse event summary}
#' \item{severity_summary}{Summary by severity level}
#' \item{n_groups}{Number of treatment groups}
#' \item{audit_trail}{Audit trail}
#'
#' @examples
#' \dontrun{
#' safety <- rwe_safety_report(
#'   data = ae_data,
#'   group_var = "treatment",
#'   ae_var = "adverse_event",
#'   severity_var = "severity",
#'   serious_var = "is_serious",
#'   n_per_group = list("Treatment" = 175, "Control" = 175)
#' )
#'
#' print(safety)
#' summary(safety)
#' }
#'
#' @export
rwe_safety_report <- function(data,
                               group_var,
                               ae_var,
                               severity_var = NULL,
                               relatedness_var = NULL,
                               serious_var = NULL,
                               n_per_group = NULL,
                               sort_by = c("frequency", "severity", "alphabetical"),
                               top_n = NULL) {

  log_info("Creating safety report", context = "rwe_safety_report")

  sort_by <- match.arg(sort_by)

  # Extract data
  if (is_rwe_data(data)) {
    data_df <- data$data
  } else {
    data_df <- data
  }

  validate_inputs(data_df)

  # Validate required variables
  if (!group_var %in% names(data_df)) {
    stop("Group variable '", group_var, "' not found in data", call. = FALSE)
  }
  if (!ae_var %in% names(data_df)) {
    stop("AE variable '", ae_var, "' not found in data", call. = FALSE)
  }

  cat("Safety Report: Adverse Events\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  groups <- unique(data_df[[group_var]])
  n_groups <- length(groups)

  cat("Treatment Groups:", paste(groups, collapse = ", "), "\n")
  cat("Total AE Records:", nrow(data_df), "\n\n")

  # Calculate sample sizes
  if (is.null(n_per_group)) {
    # Count unique patients per group if not provided
    n_per_group <- table(data_df[[group_var]])
    cat("Note: Using observed counts as denominators\n\n")
  }

  # Overall AE summary
  ae_summary <- list()
  overall_rates <- list()

  for (grp in groups) {
    grp_data <- data_df[data_df[[group_var]] == grp, ]
    n_patients <- n_per_group[[as.character(grp)]]

    # Count AEs
    ae_counts <- table(grp_data[[ae_var]])
    n_total_ae <- nrow(grp_data)
    n_unique_ae <- length(unique(grp_data[[ae_var]]))

    ae_summary[[as.character(grp)]] <- list(
      n_patients = n_patients,
      n_total_ae = n_total_ae,
      n_unique_ae = n_unique_ae,
      ae_counts = ae_counts,
      ae_rates = (as.numeric(ae_counts) / n_patients) * 100
    )

    overall_rates[[as.character(grp)]] <- list(
      patients_with_ae = length(unique(grp_data[[ae_var]])),
      rate_patients_with_ae = (length(unique(grp_data[[ae_var]])) / n_patients) * 100,
      total_ae_count = n_total_ae,
      ae_per_patient = n_total_ae / n_patients
    )
  }

  cat("OVERALL ADVERSE EVENT RATES\n")
  for (grp in groups) {
    rates <- overall_rates[[as.character(grp)]]
    cat("  ", grp, ":\n")
    cat("    Patients:", n_per_group[[as.character(grp)]], "\n")
    cat("    Total AEs:", rates$total_ae_count, "\n")
    cat("    AEs per patient:", round(rates$ae_per_patient, 2), "\n")
    cat("\n")
  }

  # Serious AE summary
  serious_summary <- NULL
  if (!is.null(serious_var) && serious_var %in% names(data_df)) {
    cat("SERIOUS ADVERSE EVENTS\n")
    serious_summary <- list()

    for (grp in groups) {
      grp_data <- data_df[data_df[[group_var]] == grp, ]
      serious_ae <- grp_data[grp_data[[serious_var]] %in% c(TRUE, 1, "Yes", "yes"), ]

      n_serious <- nrow(serious_ae)
      n_patients <- n_per_group[[as.character(grp)]]
      rate_serious <- (n_serious / n_patients) * 100

      serious_summary[[as.character(grp)]] <- list(
        n_serious = n_serious,
        rate_serious = rate_serious,
        serious_ae_types = table(serious_ae[[ae_var]])
      )

      cat("  ", grp, ":", n_serious, "SAEs (", round(rate_serious, 1), "%)\n")
    }
    cat("\n")
  }

  # Severity summary
  severity_summary <- NULL
  if (!is.null(severity_var) && severity_var %in% names(data_df)) {
    cat("ADVERSE EVENTS BY SEVERITY\n")
    severity_summary <- list()

    for (grp in groups) {
      grp_data <- data_df[data_df[[group_var]] == grp, ]
      severity_counts <- table(grp_data[[severity_var]])

      severity_summary[[as.character(grp)]] <- severity_counts

      cat("  ", grp, ":\n")
      for (sev in names(severity_counts)) {
        cat("    ", sev, ":", severity_counts[[sev]], "\n")
      }
    }
    cat("\n")
  }

  cat("Safety report created successfully!\n")

  log_info("Safety report complete", context = "rwe_safety_report")

  # Create audit trail
  audit <- create_audit_trail(
    operation = "safety_report_creation",
    input_data = list(
      n_records = nrow(data_df),
      n_groups = n_groups
    ),
    output_data = list(
      report_created = TRUE
    ),
    parameters = list(
      group_var = group_var,
      ae_var = ae_var,
      has_severity = !is.null(severity_var),
      has_serious = !is.null(serious_var)
    )
  )

  # Create result object
  result <- structure(
    list(
      summary = ae_summary,
      ae_counts = ae_summary,
      overall_rates = overall_rates,
      ae_rates = overall_rates,
      serious_ae = serious_summary,
      severity_summary = severity_summary,
      groups = groups,
      n_groups = n_groups,
      n_per_group = n_per_group,
      group_var = group_var,
      ae_var = ae_var,
      sort_by = sort_by,
      top_n = top_n,
      audit_trail = audit
    ),
    class = c("rwe_safety_report", "rwe_analysis")
  )

  return(result)
}


#' Print Method for Safety Reports
#'
#' @param x An object of class \code{rwe_safety_report}
#' @param ... Additional arguments (not used)
#'
#' @export
print.rwe_safety_report <- function(x, ...) {
  cat("Safety Report: Adverse Events\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  cat("Treatment Groups:", x$n_groups, "\n")
  cat("Groups:", paste(x$groups, collapse = ", "), "\n\n")

  cat("Overall AE Summary:\n")
  for (grp in x$groups) {
    rates <- x$overall_rates[[as.character(grp)]]
    cat("  ", grp, ":\n")
    cat("    Total AEs:", rates$total_ae_count, "\n")
    cat("    AEs per patient:", round(rates$ae_per_patient, 2), "\n")
  }

  if (!is.null(x$serious_ae)) {
    cat("\nSerious Adverse Events:\n")
    for (grp in x$groups) {
      sae <- x$serious_ae[[as.character(grp)]]
      cat("  ", grp, ":", sae$n_serious, "SAEs (", round(sae$rate_serious, 1), "%)\n")
    }
  }

  cat("\nUse summary() for detailed AE breakdown\n")

  invisible(x)
}


#' Summary Method for Safety Reports
#'
#' @param object An object of class \code{rwe_safety_report}
#' @param ... Additional arguments (not used)
#'
#' @export
summary.rwe_safety_report <- function(object, ...) {
  cat("Safety Report - Detailed Summary\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")

  cat("OVERALL ADVERSE EVENT RATES\n")
  cat(paste(rep("-", 70), collapse = ""), "\n")
  for (grp in object$groups) {
    rates <- object$overall_rates[[as.character(grp)]]
    cat(sprintf("%-20s N=%d  Total AEs=%d  Rate=%.2f AEs/patient\n",
                grp,
                object$n_per_group[[as.character(grp)]],
                rates$total_ae_count,
                rates$ae_per_patient))
  }
  cat("\n")

  if (!is.null(object$serious_ae)) {
    cat("SERIOUS ADVERSE EVENTS\n")
    cat(paste(rep("-", 70), collapse = ""), "\n")
    for (grp in object$groups) {
      sae <- object$serious_ae[[as.character(grp)]]
      cat(sprintf("%-20s %d SAEs (%.1f%%)\n",
                  grp, sae$n_serious, sae$rate_serious))

      if (length(sae$serious_ae_types) > 0) {
        cat("  Top SAEs:\n")
        top_sae <- sort(sae$serious_ae_types, decreasing = TRUE)[1:min(5, length(sae$serious_ae_types))]
        for (ae_name in names(top_sae)) {
          cat(sprintf("    %-30s %d\n", ae_name, top_sae[[ae_name]]))
        }
      }
    }
    cat("\n")
  }

  if (!is.null(object$severity_summary)) {
    cat("ADVERSE EVENTS BY SEVERITY\n")
    cat(paste(rep("-", 70), collapse = ""), "\n")
    for (grp in object$groups) {
      cat(grp, ":\n")
      sev <- object$severity_summary[[as.character(grp)]]
      for (level in names(sev)) {
        cat(sprintf("  %-15s %5d\n", level, sev[[level]]))
      }
    }
    cat("\n")
  }

  cat("MOST FREQUENT ADVERSE EVENTS (Top 10)\n")
  cat(paste(rep("-", 70), collapse = ""), "\n")
  cat(sprintf("%-35s", "Adverse Event"))
  for (grp in object$groups) {
    cat(sprintf("%15s", grp))
  }
  cat("\n")

  # Get all unique AEs
  all_aes <- unique(unlist(lapply(object$summary, function(s) names(s$ae_counts))))

  # Count across all groups
  ae_totals <- sapply(all_aes, function(ae) {
    sum(sapply(object$summary, function(s) {
      if (ae %in% names(s$ae_counts)) s$ae_counts[[ae]] else 0
    }))
  })

  # Sort and get top 10
  top_aes <- names(sort(ae_totals, decreasing = TRUE))[1:min(10, length(ae_totals))]

  for (ae in top_aes) {
    cat(sprintf("%-35s", substr(ae, 1, 35)))
    for (grp in object$groups) {
      summary_grp <- object$summary[[as.character(grp)]]
      count <- if (ae %in% names(summary_grp$ae_counts)) summary_grp$ae_counts[[ae]] else 0
      rate <- if (ae %in% names(summary_grp$ae_counts)) {
        (count / summary_grp$n_patients) * 100
      } else {
        0
      }
      cat(sprintf("%8d (%4.1f%%)", count, rate))
    }
    cat("\n")
  }

  invisible(object)
}


#' Generate Comprehensive Regulatory Report
#'
#' @description
#' Creates a comprehensive regulatory report combining multiple components
#' (Table 1, safety, efficacy, CONSORT) suitable for FDA/EMA submissions.
#'
#' @param study_title Character. Study title
#' @param table_one An rwe_table_one object (optional)
#' @param consort An rwe_consort object (optional)
#' @param safety An rwe_safety_report object (optional)
#' @param efficacy_results List of efficacy results (optional)
#' @param study_info List of study metadata
#' @param output_file Path to output file (HTML, Word-compatible)
#'
#' @return An object of class \code{rwe_regulatory_report}
#'
#' @examples
#' \dontrun{
#' report <- rwe_regulatory_report(
#'   study_title = "Real-World Effectiveness Study",
#'   table_one = baseline_table,
#'   consort = patient_flow,
#'   safety = safety_summary,
#'   study_info = list(
#'     protocol_number = "RWE-2024-001",
#'     sponsor = "ABC Pharma",
#'     indication = "Advanced Cancer"
#'   )
#' )
#' }
#'
#' @export
rwe_regulatory_report <- function(study_title,
                                   table_one = NULL,
                                   consort = NULL,
                                   safety = NULL,
                                   efficacy_results = NULL,
                                   study_info = NULL,
                                   output_file = NULL) {

  log_info("Creating comprehensive regulatory report", context = "rwe_regulatory_report")

  cat("Comprehensive Regulatory Report\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")
  cat("Study:", study_title, "\n\n")

  # Validate components
  components <- list()

  if (!is.null(table_one)) {
    if (!inherits(table_one, "rwe_table_one")) {
      stop("table_one must be an rwe_table_one object")
    }
    components$table_one <- TRUE
    cat("[x] Table 1 (Baseline Characteristics) included\n")
  }

  if (!is.null(consort)) {
    if (!inherits(consort, "rwe_consort")) {
      stop("consort must be an rwe_consort object")
    }
    components$consort <- TRUE
    cat("[x] CONSORT Diagram included\n")
  }

  if (!is.null(safety)) {
    if (!inherits(safety, "rwe_safety_report")) {
      stop("safety must be an rwe_safety_report object")
    }
    components$safety <- TRUE
    cat("[x] Safety Report included\n")
  }

  if (!is.null(efficacy_results)) {
    components$efficacy <- TRUE
    cat("[x] Efficacy Results included\n")
  }

  if (!is.null(study_info)) {
    components$study_info <- TRUE
    cat("[x] Study Information included\n")
  }

  cat("\n")

  if (length(components) == 0) {
    warning("No components provided for regulatory report")
  }

  cat("Report compiled successfully!\n")

  log_info("Regulatory report created", context = "rwe_regulatory_report")

  # Create audit trail
  audit <- create_audit_trail(
    operation = "regulatory_report_creation",
    input_data = list(
      study_title = study_title,
      n_components = length(components)
    ),
    output_data = list(
      report_created = TRUE
    ),
    parameters = list(
      components = names(components)
    )
  )

  # Create result object
  result <- structure(
    list(
      study_title = study_title,
      table_one = table_one,
      consort = consort,
      safety = safety,
      efficacy_results = efficacy_results,
      study_info = study_info,
      components = components,
      generated_at = Sys.time(),
      audit_trail = audit
    ),
    class = c("rwe_regulatory_report", "rwe_analysis")
  )

  # Export if output file specified
  if (!is.null(output_file)) {
    export_regulatory_report(result, output_file)
  }

  return(result)
}

 #' Generate Automated Regulatory Report
 #'
 #' @description
 #' Builds a regulatory report bundle by automatically assembling baseline
 #' tables, effectiveness/safety summaries, and standard figures.
 #'
 #' @param study_title Character title for the study/report.
 #' @param cohort_data Data frame containing study cohort for baseline table.
 #' @param treatment_var Character; treatment/group variable.
 #' @param baseline_vars Character vector of baseline variables to summarise.
 #' @param effectiveness Optional analysis result (e.g., \code{rwe_relative_risk},
 #'   \code{rwe_odds_ratio}, or list of such objects).
 #' @param survival Optional survival analysis result (\code{rwe_km} or
 #'   \code{survfit} object).
 #' @param subgroup Optional \code{rwe_subgroup_analysis} object.
 #' @param safety_analysis Optional \code{rwe_safety_analysis} object.
 #' @param safety_report Optional \code{rwe_safety_report} object.
 #' @param consort Optional \code{rwe_consort} object.
 #' @param include_tables Logical; include generated tables (default TRUE).
 #' @param include_figures Logical; include generated figures (default TRUE).
 #'
 #' @return An object of class \code{rwe_regulatory_report}.
 #' @export
 rwe_generate_regulatory_report <- function(study_title,
                                            cohort_data,
                                            treatment_var,
                                            baseline_vars,
                                            effectiveness = NULL,
                                            survival = NULL,
                                            subgroup = NULL,
                                            safety_analysis = NULL,
                                            safety_report = NULL,
                                            consort = NULL,
                                            include_tables = TRUE,
                                            include_figures = TRUE) {

   if (missing(study_title) || !nzchar(study_title)) {
     stop("study_title must be provided.", call. = FALSE)
   }

   if (missing(cohort_data)) {
     stop("cohort_data must be provided.", call. = FALSE)
   }

   if (is_rwe_data(cohort_data)) {
     cohort_df <- cohort_data$data
   } else {
     cohort_df <- cohort_data
   }

   validate_inputs(cohort_df)

   if (missing(treatment_var) || !treatment_var %in% names(cohort_df)) {
     stop("treatment_var must be provided and exist in cohort_data.", call. = FALSE)
   }

   missing_baseline <- setdiff(baseline_vars, names(cohort_df))
   if (length(missing_baseline) > 0) {
     stop("Baseline variables missing from cohort_data: ",
          paste(missing_baseline, collapse = ", "),
          call. = FALSE)
   }

   tables <- list()
   figures <- list()

   table_one <- NULL

   if (include_tables) {
     table_one <- rwe_table_one(
       data = cohort_df,
       group_var = treatment_var,
       variables = baseline_vars,
       test = TRUE,
       smd = TRUE
     )

     tables$baseline <- rwe_results_table(
       results = table_one,
       table_type = "demographics",
       format = "data",
       quiet = TRUE
     )
   }

   if (!is.null(effectiveness) && include_tables) {
     tables$effectiveness <- rwe_results_table(
       results = effectiveness,
       table_type = "effectiveness",
       format = "data",
       quiet = TRUE
     )
   }

   if (!is.null(safety_analysis) && include_tables) {
     tables$safety <- rwe_results_table(
       results = safety_analysis,
       table_type = "safety",
       format = "data",
       quiet = TRUE
     )
   }

   if (!is.null(subgroup) && include_tables) {
     tables$subgroup <- rwe_results_table(
       results = subgroup,
       table_type = "subgroup",
       format = "data",
       quiet = TRUE
     )
   }

   if (include_figures) {
     if (!is.null(subgroup)) {
       figures$subgroup_forest <- tryCatch(
         rwe_figure_forest(subgroup),
         error = function(e) NULL
       )
     }

     if (!is.null(survival)) {
       figures$survival <- tryCatch(
         rwe_figure_survival(survival),
         error = function(e) NULL
       )
     }

     if (!is.null(safety_report)) {
       figures$top_ae <- tryCatch(
         rwe_figure_top_ae(safety_report),
         error = function(e) NULL
       )
     }

     if (!is.null(consort)) {
       figures$consort <- tryCatch(
         rwe_figure_consort(consort),
         error = function(e) NULL
       )
     }
   }

   components <- list()
   if (!is.null(tables$baseline)) components <- c(components, "baseline_table")
   if (!is.null(tables$effectiveness)) components <- c(components, "effectiveness_table")
   if (!is.null(tables$safety)) components <- c(components, "safety_table")
   if (!is.null(tables$subgroup)) components <- c(components, "subgroup_table")
   if (length(figures) > 0) components <- c(components, names(figures))
   if (!is.null(consort)) components <- c(components, "consort")

   audit <- create_audit_trail(
     operation = "automated_regulatory_report",
     input_data = list(
       study_title = study_title,
       n_subjects = nrow(cohort_df),
       treatment_var = treatment_var
     ),
     output_data = list(
       n_tables = length(tables),
       n_figures = length(figures)
     ),
     parameters = list(
       include_tables = include_tables,
       include_figures = include_figures,
       components = components
     )
   )

   report <- structure(
   list(
      study_title = study_title,
      cohort = cohort_data,
      table_one = table_one,
      tables = tables,
      figures = figures,
      effectiveness = effectiveness,
      survival = survival,
      subgroup = subgroup,
      safety_analysis = safety_analysis,
      safety_report = safety_report,
      safety = safety_report,
      consort = consort,
      components = components,
      generated_at = Sys.time(),
      audit_trail = audit
    ),
     class = c("rwe_regulatory_report", "rwe_analysis")
   )

   report
 }


#' Print Method for Regulatory Reports
#'
#' @param x An object of class \code{rwe_regulatory_report}
#' @param ... Additional arguments (not used)
#'
#' @export
print.rwe_regulatory_report <- function(x, ...) {
  cat("Comprehensive Regulatory Report\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  cat("Study:", x$study_title, "\n\n")

  cat("Included Components:\n")
  if (!is.null(x$table_one)) {
    cat("  [x] Table 1 (Baseline Characteristics)\n")
  }
  if (!is.null(x$consort)) {
    cat("  [x] CONSORT Flow Diagram\n")
  }
  if (!is.null(x$safety)) {
    cat("  [x] Safety Report\n")
  }
  if (!is.null(x$tables$effectiveness)) {
    cat("  [x] Effectiveness Tables\n")
  }
  if (!is.null(x$tables$safety)) {
    cat("  [x] Safety Tables\n")
  }
  if (!is.null(x$tables$subgroup)) {
    cat("  [x] Subgroup Tables\n")
  }
  if (!is.null(x$efficacy_results)) {
    cat("  [x] Efficacy Results\n")
  }
  if (!is.null(x$figures) && length(x$figures) > 0) {
    cat("  [x] Reporting Figures (", length(x$figures), ")\n", sep = "")
  }
  if (!is.null(x$study_info)) {
    cat("  [x] Study Information\n")
  }

  cat("\nUse summary() for detailed report\n")
  cat("Use export_regulatory_report() to save to file\n")

  invisible(x)
}


#' Export Regulatory Report
#'
#' @description
#' Exports a comprehensive regulatory report to HTML format
#'
#' @param report An rwe_regulatory_report object
#' @param output_file Path to output HTML file
#' @param include_tables Include tabular summaries (default TRUE)
#' @param include_figures Include rendered figures (default TRUE)
#'
#' @return Invisible NULL
#'
#' @export
export_regulatory_report <- function(report,
                                     output_file,
                                     include_tables = TRUE,
                                     include_figures = TRUE) {

  if (!inherits(report, "rwe_regulatory_report")) {
    stop("report must be an rwe_regulatory_report object", call. = FALSE)
  }

  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Package 'htmltools' is required to export reports.", call. = FALSE)
  }

  if (!dir.exists(dirname(output_file))) {
    dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  }

  sections <- list(
    htmltools::tags$h1(report$study_title),
    htmltools::tags$p(sprintf("Generated: %s", format(report$generated_at, tz = "UTC")))
  )

  sections <- c(sections, htmltools::tags$h2("Components"))
  sections <- c(sections, htmltools::tags$ul(
    lapply(report$components, function(component) {
      htmltools::tags$li(component)
    })
  ))

  render_table <- function(tbl, heading) {
    if (!requireNamespace("knitr", quietly = TRUE)) {
      return(htmltools::tags$p(sprintf("%s available (knitr not installed for HTML rendering).", heading)))
    }

    htmltools::tagList(
      htmltools::tags$h3(heading),
      htmltools::HTML(knitr::kable(tbl, format = "html", table.attr = 'class="table table-striped"'))
    )
  }

  if (include_tables && length(report$tables) > 0) {
    sections <- c(sections, htmltools::tags$h2("Tables"))
    for (name in names(report$tables)) {
      table_obj <- report$tables[[name]]
      if (!is.null(table_obj$data)) {
        title <- gsub("_", " ", tools::toTitleCase(name))
        sections <- c(sections, render_table(table_obj$data, title))
      }
    }
  }

  encode_plot <- function(plot_obj, name_prefix) {
    if (!requireNamespace("base64enc", quietly = TRUE) ||
        !requireNamespace("ggplot2", quietly = TRUE)) {
      return(NULL)
    }

    tmp <- tempfile(pattern = name_prefix, fileext = ".png")
    try({
      ggplot2::ggsave(filename = tmp, plot = plot_obj, width = 7, height = 5, dpi = 150)
      data_uri <- base64enc::dataURI(file = tmp, mime = "image/png")
      htmltools::tagList(
        htmltools::tags$img(src = data_uri, style = "max-width:100%;height:auto;"),
        htmltools::tags$br()
      )
    }, silent = TRUE)
  }

  render_figure <- function(fig, heading) {
    if (inherits(fig, "ggplot")) {
      plot_tag <- encode_plot(fig, heading)
      if (!is.null(plot_tag)) {
        return(htmltools::tagList(htmltools::tags$h3(heading), plot_tag))
      }
    }

    if (inherits(fig, "grViz")) {
      return(htmltools::tagList(htmltools::tags$h3(heading), fig))
    }

    htmltools::tags$p(sprintf("%s figure available (unable to render automatically).", heading))
  }

  if (include_figures && length(report$figures) > 0) {
    sections <- c(sections, htmltools::tags$h2("Figures"))
    for (name in names(report$figures)) {
      fig_obj <- report$figures[[name]]
      if (!is.null(fig_obj)) {
        title <- gsub("_", " ", tools::toTitleCase(name))
        sections <- c(sections, render_figure(fig_obj, title))
      }
    }
  }

  if (!is.null(report$study_info)) {
    sections <- c(sections, htmltools::tags$h2("Study Information"))
    sections <- c(sections, htmltools::tags$ul(
      lapply(names(report$study_info), function(field) {
        htmltools::tags$li(sprintf("%s: %s", field, as.character(report$study_info[[field]])))
      })
    ))
  }

  htmltools::save_html(htmltools::tagList(sections), file = output_file)
  invisible(NULL)
}
