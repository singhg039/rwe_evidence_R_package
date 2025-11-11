#' Survival Analysis for RWE Studies
#'
#' @description
#' Functions for time-to-event analysis including Cox regression,
#' Kaplan-Meier estimation, and survival curve comparison.
#' Integrates with IPTW for weighted survival analysis.
#'
#' @name survival_analysis
NULL


#' Cox Proportional Hazards Regression
#'
#' @description
#' Fits Cox proportional hazards model for survival analysis.
#' Supports IPTW weighting for causal inference.
#'
#' @param data Study cohort data (data frame or rwe_data object)
#' @param time_var Name of time-to-event variable
#' @param event_var Name of event indicator variable (1 = event, 0 = censored)
#' @param treatment Treatment variable name (for treatment effect estimation)
#' @param covariates Character vector of covariate names for adjustment
#' @param weights Optional IPTW weights (numeric vector or rwe_iptw object)
#' @param strata Stratification variables (optional)
#'
#' @return rwe_survival object with Cox model results
#'
#' @examples
#' \dontrun{
#' # Basic Cox regression
#' cox_result <- rwe_cox_regression(
#'   data = study_data,
#'   time_var = "survival_days",
#'   event_var = "died",
#'   treatment = "treated",
#'   covariates = c("age", "sex", "stage")
#' )
#'
#' # IPTW-weighted Cox regression
#' cox_weighted <- rwe_cox_regression(
#'   data = study_data,
#'   time_var = "survival_days",
#'   event_var = "died",
#'   treatment = "treated",
#'   weights = iptw_result
#' )
#' }
#'
#' @export
rwe_cox_regression <- function(data,
                               time_var,
                               event_var,
                               treatment = NULL,
                               covariates = NULL,
                               weights = NULL,
                               strata = NULL) {

  log_info("Starting Cox regression analysis", context = "rwe_cox_regression")

  # Extract data if rwe_data object
  if (is_rwe_data(data)) {
    data_df <- data$data
  } else {
    data_df <- data
  }

  validate_inputs(data_df)

  # Validate required variables
  if (!time_var %in% names(data_df)) {
    stop("Time variable '", time_var, "' not found in data", call. = FALSE)
  }

  if (!event_var %in% names(data_df)) {
    stop("Event variable '", event_var, "' not found in data", call. = FALSE)
  }

  # Extract weights if rwe_iptw object
  if (inherits(weights, "rwe_iptw")) {
    weights_vec <- weights$weights
    # Keep using the original data_df, don't replace it
  } else if (is.numeric(weights)) {
    weights_vec <- weights
  } else {
    weights_vec <- NULL
  }

  cat("Cox Proportional Hazards Regression\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  cat("Time variable:", time_var, "\n")
  cat("Event variable:", event_var, "\n")
  if (!is.null(treatment)) {
    cat("Treatment variable:", treatment, "\n")
  }
  if (!is.null(weights_vec)) {
    cat("Using IPTW weights\n")
  }
  cat("\n")

  # Check if survival package is available first
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("Package 'survival' is required but not installed.\n",
         "Install it with: install.packages('survival')",
         call. = FALSE)
  }

  # Build formula using survival::Surv
  surv_obj <- survival::Surv(data_df[[time_var]], data_df[[event_var]])

  # Add surv_obj to data_df
  data_df$..surv.. <- surv_obj

  # Build model formula
  if (!is.null(treatment) && !is.null(covariates)) {
    formula_str <- paste("..surv.. ~", treatment, "+", paste(covariates, collapse = " + "))
  } else if (!is.null(treatment)) {
    formula_str <- paste("..surv.. ~", treatment)
  } else if (!is.null(covariates)) {
    formula_str <- paste("..surv.. ~", paste(covariates, collapse = " + "))
  } else {
    stop("Must specify either treatment or covariates", call. = FALSE)
  }

  # Add strata if specified
  if (!is.null(strata)) {
    formula_str <- paste(formula_str, "+ strata(", paste(strata, collapse = ","), ")")
  }

  cox_formula <- as.formula(formula_str)

  cat("Formula:", deparse(cox_formula), "\n\n")

  # Fit Cox model
  cat("Fitting Cox model...\n")

  cox_model <- tryCatch({
    if (!is.null(weights_vec)) {
      survival::coxph(cox_formula, data = data_df, weights = weights_vec)
    } else {
      survival::coxph(cox_formula, data = data_df)
    }
  }, error = function(e) {
    log_error(paste("Cox regression failed:", e$message), context = "rwe_cox_regression")
    stop("Cox regression failed: ", e$message, call. = FALSE)
  })

  # Remove temporary column
  data_df$..surv.. <- NULL

  # Extract key results
  coef_table <- summary(cox_model)$coefficients
  conf_int <- summary(cox_model)$conf.int

  # Focus on treatment effect if specified
  if (!is.null(treatment) && treatment %in% rownames(coef_table)) {
    hr <- exp(coef(cox_model)[treatment])
    hr_ci_lower <- conf_int[treatment, "lower .95"]
    hr_ci_upper <- conf_int[treatment, "upper .95"]
    p_value <- coef_table[treatment, "Pr(>|z|)"]

    cat("\nTreatment Effect:\n")
    cat("  Hazard Ratio:", round(hr, 3), "\n")
    cat("  95% CI: (", round(hr_ci_lower, 3), ",", round(hr_ci_upper, 3), ")\n")
    cat("  P-value:", format.pval(p_value, digits = 4), "\n\n")
  }

  # Model fit statistics
  cat("Model Statistics:\n")
  cat("  N:", cox_model$n, "\n")
  cat("  Events:", cox_model$nevent, "\n")
  cat("  Concordance:", round(summary(cox_model)$concordance[1], 3), "\n")
  cat("  R-squared:", round(summary(cox_model)$rsq["rsq"], 3), "\n\n")

  log_info("Cox regression complete", context = "rwe_cox_regression")

  # Create result object
  cox_obj <- structure(
    list(
      model = cox_model,
      formula = cox_formula,
      time_var = time_var,
      event_var = event_var,
      treatment = treatment,
      covariates = covariates,
      weighted = !is.null(weights_vec),
      n = cox_model$n,
      n_events = cox_model$nevent,
      coefficients = coef_table,
      conf_int = conf_int,
      concordance = summary(cox_model)$concordance[1],
      data = data_df
    ),
    class = c("rwe_survival", "rwe_analysis")
  )

  # Add audit trail
  cox_obj$audit_trail <- create_audit_trail(
    operation = "cox_regression",
    input_data = list(
      n_rows = nrow(data_df),
      n_events = cox_model$nevent
    ),
    output_data = list(
      concordance = cox_obj$concordance
    ),
    parameters = list(
      weighted = cox_obj$weighted,
      n_covariates = length(covariates)
    )
  )

  cox_obj
}


#' Kaplan-Meier Survival Curves
#'
#' @description
#' Estimates Kaplan-Meier survival curves, optionally stratified by treatment.
#' Performs log-rank test to compare survival between groups.
#'
#' @param data Study cohort data
#' @param time_var Name of time-to-event variable
#' @param event_var Name of event indicator variable
#' @param group_var Grouping variable (e.g., treatment) for stratification
#' @param weights Optional IPTW weights
#'
#' @return rwe_km object with survival curves and comparison tests
#'
#' @examples
#' \dontrun{
#' km_result <- rwe_kaplan_meier(
#'   data = study_data,
#'   time_var = "survival_days",
#'   event_var = "died",
#'   group_var = "treatment"
#' )
#'
#' plot(km_result)
#' }
#'
#' @export
rwe_kaplan_meier <- function(data,
                             time_var,
                             event_var,
                             group_var = NULL,
                             weights = NULL) {

  log_info("Starting Kaplan-Meier analysis", context = "rwe_kaplan_meier")

  # Extract data if rwe_data object
  if (is_rwe_data(data)) {
    data_df <- data$data
  } else {
    data_df <- data
  }

  validate_inputs(data_df)

  # Validate variables
  if (!time_var %in% names(data_df)) {
    stop("Time variable '", time_var, "' not found in data", call. = FALSE)
  }

  if (!event_var %in% names(data_df)) {
    stop("Event variable '", event_var, "' not found in data", call. = FALSE)
  }

  # Extract weights if needed
  if (inherits(weights, "rwe_iptw")) {
    weights_vec <- weights$weights
    # Keep using the original data_df, don't replace it
  } else if (is.numeric(weights)) {
    weights_vec <- weights
  } else {
    weights_vec <- NULL
  }

  cat("Kaplan-Meier Survival Analysis\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  # Check survival package
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("Package 'survival' is required but not installed.\n",
         "Install it with: install.packages('survival')",
         call. = FALSE)
  }

  # Create Surv object
  surv_obj <- survival::Surv(data_df[[time_var]], data_df[[event_var]])

  # Add surv_obj to data_df
  data_df$..surv.. <- surv_obj

  # Build formula
  if (!is.null(group_var)) {
    km_formula <- as.formula(paste("..surv.. ~", group_var))
  } else {
    km_formula <- as.formula("..surv.. ~ 1")
  }

  cat("Formula:", deparse(km_formula), "\n\n")

  # Fit Kaplan-Meier
  cat("Estimating survival curves...\n")

  km_fit <- tryCatch({
    if (!is.null(weights_vec)) {
      survival::survfit(km_formula, data = data_df, weights = weights_vec)
    } else {
      survival::survfit(km_formula, data = data_df)
    }
  }, error = function(e) {
    log_error(paste("Kaplan-Meier estimation failed:", e$message),
              context = "rwe_kaplan_meier")
    stop("Kaplan-Meier estimation failed: ", e$message, call. = FALSE)
  })

  # Log-rank test if groups specified
  logrank_test <- NULL
  if (!is.null(group_var)) {
    cat("\nPerforming log-rank test...\n")

    logrank_test <- tryCatch({
      if (!is.null(weights_vec)) {
        survival::survdiff(km_formula, data = data_df, weights = weights_vec)
      } else {
        survival::survdiff(km_formula, data = data_df)
      }
    }, error = function(e) {
      log_warning(paste("Log-rank test failed:", e$message),
                  context = "rwe_kaplan_meier")
      NULL
    })

  # Remove temporary column
  data_df$..surv.. <- NULL

    if (!is.null(logrank_test)) {
      p_value <- 1 - pchisq(logrank_test$chisq, df = length(logrank_test$n) - 1)
      cat("  Chi-square:", round(logrank_test$chisq, 3), "\n")
      cat("  P-value:", format.pval(p_value, digits = 4), "\n\n")
    }
  }

  # Median survival times
  cat("Median Survival Times:\n")
  median_surv <- summary(km_fit)$table
  if (is.matrix(median_surv)) {
    print(median_surv[, c("median", "0.95LCL", "0.95UCL")])
  } else {
    cat("  Median:", median_surv["median"], "\n")
    cat("  95% CI: (", median_surv["0.95LCL"], ",",
        median_surv["0.95UCL"], ")\n")
  }
  cat("\n")

  log_info("Kaplan-Meier analysis complete", context = "rwe_kaplan_meier")

  # Create result object
  km_obj <- structure(
    list(
      survfit = km_fit,
      formula = km_formula,
      time_var = time_var,
      event_var = event_var,
      group_var = group_var,
      logrank_test = logrank_test,
      weighted = !is.null(weights_vec),
      data = data_df
    ),
    class = c("rwe_km", "rwe_analysis")
  )

  # Add audit trail
  km_obj$audit_trail <- create_audit_trail(
    operation = "kaplan_meier",
    input_data = list(
      n_rows = nrow(data_df),
      n_events = sum(data_df[[event_var]])
    ),
    output_data = list(
      n_groups = if (!is.null(group_var)) length(unique(data_df[[group_var]])) else 1
    ),
    parameters = list(
      weighted = km_obj$weighted,
      grouped = !is.null(group_var)
    )
  )

  km_obj
}


#' Print Method for Cox Regression Results
#'
#' @param x rwe_survival object
#' @param ... Additional arguments
#'
#' @export
print.rwe_survival <- function(x, ...) {
  cat("\nCox Proportional Hazards Regression\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  cat("Formula:", deparse(x$formula), "\n")
  cat("Sample size:", x$n, "\n")
  cat("Events:", x$n_events, "\n")

  if (x$weighted) {
    cat("Weighted: Yes (IPTW)\n")
  }

  cat("\nModel Performance:\n")
  cat("  Concordance:", round(x$concordance, 3), "\n\n")

  cat("Coefficients:\n")
  print(round(x$coefficients, 4))

  cat("\n")

  if (!is.null(x$treatment) && x$treatment %in% rownames(x$conf_int)) {
    cat("Treatment Effect (Hazard Ratio):\n")
    hr <- x$conf_int[x$treatment, "exp(coef)"]
    hr_lower <- x$conf_int[x$treatment, "lower .95"]
    hr_upper <- x$conf_int[x$treatment, "upper .95"]
    p_val <- x$coefficients[x$treatment, "Pr(>|z|)"]

    cat("  HR:", round(hr, 3), "\n")
    cat("  95% CI: (", round(hr_lower, 3), ",", round(hr_upper, 3), ")\n")
    cat("  P-value:", format.pval(p_val, digits = 4), "\n")
  }

  invisible(x)
}


#' Print Method for Kaplan-Meier Results
#'
#' @param x rwe_km object
#' @param ... Additional arguments
#'
#' @export
print.rwe_km <- function(x, ...) {
  cat("\nKaplan-Meier Survival Analysis\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  cat("Formula:", deparse(x$formula), "\n")
  cat("Time variable:", x$time_var, "\n")
  cat("Event variable:", x$event_var, "\n")

  if (!is.null(x$group_var)) {
    cat("Stratified by:", x$group_var, "\n")
  }

  if (x$weighted) {
    cat("Weighted: Yes (IPTW)\n")
  }

  cat("\n")

  # Print summary
  print(summary(x$survfit)$table)

  # Log-rank test results
  if (!is.null(x$logrank_test)) {
    cat("\nLog-Rank Test:\n")
    p_value <- 1 - pchisq(x$logrank_test$chisq,
                         df = length(x$logrank_test$n) - 1)
    cat("  Chi-square:", round(x$logrank_test$chisq, 3), "\n")
    cat("  P-value:", format.pval(p_value, digits = 4), "\n")
  }

  invisible(x)
}


#' Plot Method for Cox Regression (Forest Plot)
#'
#' @param x rwe_survival object
#' @param ... Additional arguments
#'
#' @export
plot.rwe_survival <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message("Package 'ggplot2' is required for plotting")
    return(invisible(NULL))
  }

  # Create forest plot data
  coef_data <- data.frame(
    variable = rownames(x$coefficients),
    hr = x$conf_int[, "exp(coef)"],
    lower = x$conf_int[, "lower .95"],
    upper = x$conf_int[, "upper .95"],
    p_value = x$coefficients[, "Pr(>|z|)"],
    stringsAsFactors = FALSE
  )

  # Create forest plot
  p <- ggplot2::ggplot(coef_data, ggplot2::aes(x = variable, y = hr)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 0.2) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Cox Regression: Hazard Ratios",
      x = "Variable",
      y = "Hazard Ratio (95% CI)"
    ) +
    ggplot2::theme_minimal()

  print(p)
  invisible(p)
}


#' Plot Method for Kaplan-Meier (Survival Curves)
#'
#' @param x rwe_km object
#' @param ... Additional arguments
#'
#' @export
plot.rwe_km <- function(x, ...) {
  if (!requireNamespace("survival", quietly = TRUE)) {
    message("Package 'survival' is required for plotting")
    return(invisible(NULL))
  }

  # Use survival package plotting
  plot(x$survfit,
       xlab = "Time",
       ylab = "Survival Probability",
       main = "Kaplan-Meier Survival Curves",
       col = 1:length(x$survfit$strata),
       lwd = 2)

  if (!is.null(x$group_var)) {
    legend("topright",
           legend = names(x$survfit$strata),
           col = 1:length(x$survfit$strata),
           lwd = 2,
           bty = "n")
  }

  invisible(NULL)
}
