# Safety Surveillance Module
# Functions to monitor and evaluate treatment safety signals

## Internal helpers ---------------------------------------------------------

.safety_extract_frame <- function(data) {
  if (is_rwe_data(data)) {
    data$data
  } else {
    data
  }
}

.safety_validate_columns <- function(data, cols) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
}

.safety_prepare_data <- function(data,
                                 events,
                                 person_time,
                                 group = NULL,
                                 treatment = NULL,
                                 time_var = NULL) {

  validate_inputs(data)
  cols <- c(events, person_time, group, treatment, time_var)
  cols <- cols[!is.null(cols)]
  cols <- unique(cols)
  .safety_validate_columns(data, cols)

  if (any(data[[person_time]] <= 0, na.rm = TRUE)) {
    stop("person_time must be positive for all observations.", call. = FALSE)
  }

  data[[events]] <- as.numeric(data[[events]])
  data[[person_time]] <- as.numeric(data[[person_time]])

  list(
    data = data,
    events = events,
    person_time = person_time,
    group = group,
    treatment = treatment,
    time_var = time_var
  )
}

.safety_poisson_ci <- function(events, person_time, confidence_level) {
  alpha <- 1 - confidence_level
  events <- round(events)

  lower_events <- if (events == 0) {
    0
  } else {
    0.5 * stats::qchisq(alpha / 2, 2 * events)
  }

  upper_events <- 0.5 * stats::qchisq(1 - alpha / 2, 2 * (events + 1))

  c(
    lower = lower_events / person_time,
    upper = upper_events / person_time
  )
}

.safety_format_rate <- function(rate, scale) {
  rate * scale
}

## Incidence rate ------------------------------------------------------------

#' Calculate Incidence Rates for Safety Outcomes
#'
#' Computes incidence rates and confidence intervals for adverse events,
#' optionally stratified by treatment or other grouping variables.
#'
#' @param data Data frame or \code{rwe_data} object containing safety data.
#' @param events Event count column name.
#' @param person_time Person-time column name (e.g., patient-years).
#' @param group Optional grouping variable for stratified rates.
#' @param scale Scaling factor for rates (e.g., 100 for per 100 patient-years).
#' @param confidence_level Confidence level for interval estimates (default: 0.95).
#' @param quiet Logical; suppress console output (default: FALSE).
#'
#' @return An object of class \code{rwe_incidence_rate} containing rate estimates.
#' @export
#'
#' @examples
#' \dontrun{
#' safety_rates <- rwe_incidence_rate(
#'   data = safety_data,
#'   events = "ae_count",
#'   person_time = "person_years",
#'   group = "treatment"
#' )
#' }
rwe_incidence_rate <- function(data,
                               events,
                               person_time,
                               group = NULL,
                               scale = 100,
                               confidence_level = 0.95,
                               quiet = FALSE) {

  log_info("Calculating safety incidence rates", context = "rwe_incidence_rate")

  data_df <- .safety_extract_frame(data)
  prepared <- .safety_prepare_data(
    data_df,
    events = events,
    person_time = person_time,
    group = group
  )
  df <- prepared$data

  if (is.null(group)) {
    summaries <- list(
      list(
        group = "overall",
        events = sum(df[[events]], na.rm = TRUE),
        person_time = sum(df[[person_time]], na.rm = TRUE)
      )
    )
  } else {
    summaries <- lapply(split(df, df[[group]]), function(subset_df) {
      list(
        group = unique(subset_df[[group]])[1],
        events = sum(subset_df[[events]], na.rm = TRUE),
        person_time = sum(subset_df[[person_time]], na.rm = TRUE)
      )
    })
  }

  results <- lapply(summaries, function(item) {
    rate <- item$events / item$person_time
    ci <- .safety_poisson_ci(
      events = item$events,
      person_time = item$person_time,
      confidence_level = confidence_level
    )

    data.frame(
      group = item$group,
      events = item$events,
      person_time = item$person_time,
      incidence_rate = .safety_format_rate(rate, scale),
      ci_lower = .safety_format_rate(ci["lower"], scale),
      ci_upper = .safety_format_rate(ci["upper"], scale),
      scale = scale,
      stringsAsFactors = FALSE
    )
  })

  result_df <- do.call(rbind, results)

  if (!quiet) {
    cat("Safety Incidence Rates\n")
    cat(paste(rep("=", 50), collapse = ""), "\n\n")
    cat("Scale: per", scale, "person-time units\n\n")
    print(result_df, row.names = FALSE, digits = 3)
    cat("\n")
  }

  audit <- create_audit_trail(
    operation = "incidence_rate",
    input_data = list(
      n_rows = nrow(df),
      events = events,
      person_time = person_time,
      group = group
    ),
    output_data = list(
      n_groups = nrow(result_df),
      rates = result_df$incidence_rate
    ),
    parameters = list(
      scale = scale,
      confidence_level = confidence_level
    )
  )

  structure(
    list(
      estimates = result_df,
      events = events,
      person_time = person_time,
      group = group,
      scale = scale,
      confidence_level = confidence_level,
      audit_trail = audit
    ),
    class = c("rwe_incidence_rate", "rwe_analysis")
  )
}

## Rate ratio ---------------------------------------------------------------

#' Calculate Incidence Rate Ratios
#'
#' Fits a Poisson regression model to compare adverse event incidence
#' between treatment groups while adjusting for optional confounders.
#'
#' @param data Data frame or \code{rwe_data} object with safety data.
#' @param events Event count column name.
#' @param person_time Person-time column name.
#' @param treatment Treatment variable name (binary 0/1).
#' @param confounders Optional vector of adjustment covariates.
#' @param confidence_level Confidence level for interval estimates.
#' @param quiet Logical; suppress console output.
#'
#' @return An object of class \code{rwe_rate_ratio} containing model results.
#' @export
rwe_rate_ratio <- function(data,
                           events,
                           person_time,
                           treatment,
                           confounders = NULL,
                           confidence_level = 0.95,
                           quiet = FALSE) {

  log_info("Calculating incidence rate ratio", context = "rwe_rate_ratio")

  data_df <- .safety_extract_frame(data)
  prepared <- .safety_prepare_data(
    data_df,
    events = events,
    person_time = person_time,
    treatment = treatment,
    group = treatment
  )
  df <- prepared$data

  treatment_vals <- unique(stats::na.omit(df[[treatment]]))
  if (length(treatment_vals) != 2) {
    stop("treatment must have exactly two levels (0/1).", call. = FALSE)
  }

  if (!all(treatment_vals %in% c(0, 1))) {
    stop("treatment variable must be coded as 0/1.", call. = FALSE)
  }

  if (!is.null(confounders)) {
    .safety_validate_columns(df, confounders)
  }

  model_formula <- if (is.null(confounders)) {
    stats::as.formula(paste(events, "~", treatment))
  } else {
    stats::as.formula(paste(
      events, "~", treatment, "+", paste(confounders, collapse = " + ")
    ))
  }

  df$.__offset__ <- log(df[[person_time]])

  model <- suppressWarnings(stats::glm(
    formula = model_formula,
    data = df,
    family = stats::poisson(),
    offset = df$.__offset__
  ))

  coef_idx <- match(treatment, names(stats::coef(model)))
  if (is.na(coef_idx)) {
    stop("Treatment coefficient not found in model.", call. = FALSE)
  }

  est <- stats::coef(model)[coef_idx]
  se <- sqrt(stats::vcov(model)[coef_idx, coef_idx])
  alpha <- 1 - confidence_level
  z <- stats::qnorm(1 - alpha / 2)

  rate_ratio <- unname(exp(est))
  ci_lower <- exp(est - z * se)
  ci_upper <- exp(est + z * se)
  p_value <- summary(model)$coefficients[coef_idx, "Pr(>|z|)"]

  rate_estimates <- rwe_incidence_rate(
    data = df,
    events = events,
    person_time = person_time,
    group = treatment,
    scale = 1,
    confidence_level = confidence_level,
    quiet = TRUE
  )

  rate_table <- rate_estimates$estimates
  rate_treated <- rate_table$incidence_rate[rate_table$group == 1]
  rate_control <- rate_table$incidence_rate[rate_table$group == 0]

  if (!quiet) {
    cat("Incidence Rate Ratio Analysis\n")
    cat(paste(rep("=", 50), collapse = ""), "\n\n")
    cat("Model: Poisson regression with log person-time offset\n")
    if (!is.null(confounders)) {
      cat("Adjustments:", paste(confounders, collapse = ", "), "\n")
    }
    cat("\n")
    cat("Rate Ratio:", round(rate_ratio, 3), "\n")
    cat("95% CI: (", round(ci_lower, 3), ", ", round(ci_upper, 3), ")\n", sep = "")
    cat("P-value:", format.pval(p_value, digits = 3), "\n\n")
  }

  audit <- create_audit_trail(
    operation = "rate_ratio",
    input_data = list(
      n_rows = nrow(df),
      events = events,
      person_time = person_time,
      treatment = treatment
    ),
    output_data = list(
      rate_ratio = rate_ratio,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      p_value = p_value
    ),
    parameters = list(
      confounders = confounders,
      confidence_level = confidence_level
    )
  )

  structure(
    list(
      rate_ratio = rate_ratio,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      p_value = p_value,
      model = model,
      treatment = treatment,
      events = events,
      person_time = person_time,
      confounders = confounders,
      confidence_level = confidence_level,
      rate_table = rate_table,
      audit_trail = audit
    ),
    class = c("rwe_rate_ratio", "rwe_analysis")
  )
}

## Sequential surveillance --------------------------------------------------

#' Sequential Safety Signal Monitoring
#'
#' Implements sequential monitoring of safety events using cumulative Poisson
#' surveillance against a dynamically estimated baseline rate from the control arm.
#'
#' @param data Data frame or \code{rwe_data} object.
#' @param events Event count column.
#' @param person_time Person-time column.
#' @param treatment Treatment variable (0 = control, 1 = exposed).
#' @param time_var Optional column providing chronological ordering (date or numeric).
#' @param alpha Significance level for signal detection (default: 0.01).
#' @param method Surveillance method: \code{"cumulative_poisson"} or \code{"moving_window"}.
#' @param window_size Window size (in rows) for moving window method.
#' @param quiet Suppress console printing.
#'
#' @return An object of class \code{rwe_safety_signal}.
#' @export
rwe_safety_signal <- function(data,
                              events,
                              person_time,
                              treatment,
                              time_var = NULL,
                              alpha = 0.01,
                              method = c("cumulative_poisson", "moving_window"),
                              window_size = 25,
                              quiet = FALSE) {

  method <- match.arg(method)
  log_info("Running safety signal surveillance", context = "rwe_safety_signal")

  data_df <- .safety_extract_frame(data)
  prepared <- .safety_prepare_data(
    data_df,
    events = events,
    person_time = person_time,
    treatment = treatment,
    time_var = time_var
  )
  df <- prepared$data

  control_mask <- df[[treatment]] == 0
  treated_mask <- df[[treatment]] == 1

  if (!any(control_mask)) {
    stop("No control observations (treatment == 0) found for baseline rate estimation.",
         call. = FALSE)
  }

  if (!any(treated_mask)) {
    stop("No treated observations (treatment == 1) found for surveillance.", call. = FALSE)
  }

  baseline_events <- sum(df[[events]][control_mask], na.rm = TRUE)
  baseline_time <- sum(df[[person_time]][control_mask], na.rm = TRUE)

  if (baseline_time <= 0) {
    stop("Baseline person-time must be positive.", call. = FALSE)
  }

  baseline_rate <- (baseline_events + 0.5) / baseline_time

  treated_df <- df[treated_mask, , drop = FALSE]

  if (!is.null(time_var)) {
    treated_df <- treated_df[order(treated_df[[time_var]]), , drop = FALSE]
  }

  treated_df$events_cum <- cumsum(treated_df[[events]])
  treated_df$person_time_cum <- cumsum(treated_df[[person_time]])
  treated_df$expected_events_cum <- baseline_rate * treated_df$person_time_cum
  treated_df$p_value_cum <- stats::ppois(
    treated_df$events_cum - 1,
    lambda = treated_df$expected_events_cum,
    lower.tail = FALSE
  )
  treated_df$signal_cum <- treated_df$p_value_cum < alpha

  if (method == "moving_window") {
    if (window_size < 2) {
      stop("window_size must be >= 2 for moving window surveillance.", call. = FALSE)
    }

    rolling_events <- zoo::rollapply(
      treated_df[[events]],
      width = window_size,
      FUN = sum,
      align = "right",
      fill = NA
    )

    rolling_time <- zoo::rollapply(
      treated_df[[person_time]],
      width = window_size,
      FUN = sum,
      align = "right",
      fill = NA
    )

    treated_df$expected_events_window <- baseline_rate * rolling_time
    treated_df$p_value_window <- stats::ppois(
      rolling_events - 1,
      lambda = treated_df$expected_events_window,
      lower.tail = FALSE
    )
    treated_df$signal_window <- treated_df$p_value_window < alpha
  } else {
    treated_df$expected_events_window <- NA_real_
    treated_df$p_value_window <- NA_real_
    treated_df$signal_window <- NA
  }

  first_signal <- which(treated_df$signal_cum)[1]
  detected <- if (is.na(first_signal)) FALSE else TRUE

  if (!quiet) {
    cat("Safety Surveillance Results\n")
    cat(paste(rep("=", 50), collapse = ""), "\n\n")
    cat("Baseline rate (control arm):", round(baseline_rate, 4), "\n")
    cat("Significance level (alpha):", alpha, "\n")
    cat("Method:", method, "\n")
    if (detected) {
      signal_time <- if (!is.null(time_var)) {
        treated_df[[time_var]][first_signal]
      } else {
        first_signal
      }
      cat("Signal detected at:", signal_time, "\n")
    } else {
      cat("No safety signal detected.\n")
    }
    cat("\n")
  }

  audit <- create_audit_trail(
    operation = "safety_signal",
    input_data = list(
      n_rows = nrow(df),
      events = events,
      person_time = person_time,
      treatment = treatment
    ),
    output_data = list(
      baseline_rate = baseline_rate,
      signal_detected = detected
    ),
    parameters = list(
      alpha = alpha,
      method = method,
      window_size = if (method == "moving_window") window_size else NULL
    )
  )

  structure(
    list(
      surveillance_table = treated_df,
      baseline_rate = baseline_rate,
      alpha = alpha,
      method = method,
      window_size = if (method == "moving_window") window_size else NULL,
      detected = detected,
      first_signal_index = if (detected) first_signal else NA_integer_,
      treatment = treatment,
      time_var = time_var,
      events = events,
      person_time = person_time,
      audit_trail = audit
    ),
    class = c("rwe_safety_signal", "rwe_analysis")
  )
}

## Comprehensive safety analysis -------------------------------------------

#' Comprehensive Safety Analysis Wrapper
#'
#' Combines incidence rate estimation, rate ratio modeling, and optional
#' sequential surveillance into a single analysis object.
#'
#' @param data Data frame or \code{rwe_data} object.
#' @param events Event count column.
#' @param person_time Person-time column.
#' @param treatment Treatment indicator (0/1).
#' @param confounders Optional adjustment covariates for the rate ratio model.
#' @param time_var Optional chronological variable for surveillance.
#' @param alpha Significance level for safety surveillance.
#' @param surveillance_method Method for \code{rwe_safety_signal()}.
#' @param window_size Window size (rows) for moving window surveillance.
#' @param scale Scaling factor for incidence rates.
#' @param confidence_level Confidence level for interval estimates.
#' @param quiet Suppress console output.
#'
#' @return An object of class \code{rwe_safety_analysis}.
#' @export
rwe_analyze_safety <- function(data,
                               events,
                               person_time,
                               treatment,
                               confounders = NULL,
                               time_var = NULL,
                               alpha = 0.01,
                               surveillance_method = "cumulative_poisson",
                               window_size = 25,
                               scale = 100,
                               confidence_level = 0.95,
                               quiet = FALSE) {

  log_info("Starting comprehensive safety analysis", context = "rwe_analyze_safety")

  data_df <- .safety_extract_frame(data)

  rates <- rwe_incidence_rate(
    data = data_df,
    events = events,
    person_time = person_time,
    group = treatment,
    scale = scale,
    confidence_level = confidence_level,
    quiet = TRUE
  )

  rate_ratio <- rwe_rate_ratio(
    data = data_df,
    events = events,
    person_time = person_time,
    treatment = treatment,
    confounders = confounders,
    confidence_level = confidence_level,
    quiet = TRUE
  )

  signal <- NULL
  if (!is.null(time_var)) {
    signal <- rwe_safety_signal(
      data = data_df,
      events = events,
      person_time = person_time,
      treatment = treatment,
      time_var = time_var,
      alpha = alpha,
      method = surveillance_method,
      window_size = window_size,
      quiet = TRUE
    )
  }

  if (!quiet) {
    cat("Comprehensive Safety Analysis Summary\n")
    cat(paste(rep("=", 50), collapse = ""), "\n\n")
    cat("Incidence Rates (per", scale, "person-time units):\n")
    print(rates$estimates, row.names = FALSE, digits = 3)
    cat("\nRate Ratio:", round(rate_ratio$rate_ratio, 3),
        " (95% CI:", round(rate_ratio$ci_lower, 3), "-",
        round(rate_ratio$ci_upper, 3), ")\n", sep = "")
    cat("P-value:", format.pval(rate_ratio$p_value, digits = 3), "\n")
    if (!is.null(signal)) {
      cat("Safety Signal Detected:", ifelse(signal$detected, "Yes", "No"), "\n")
    }
    cat("\n")
  }

  audit <- create_audit_trail(
    operation = "safety_analysis",
    input_data = list(
      n_rows = nrow(data_df),
      events = events,
      person_time = person_time,
      treatment = treatment
    ),
    output_data = list(
      rate_ratio = rate_ratio$rate_ratio,
      signal_detected = if (!is.null(signal)) signal$detected else NA
    ),
    parameters = list(
      confounders = confounders,
      alpha = alpha,
      surveillance_method = surveillance_method,
      scale = scale,
      confidence_level = confidence_level
    )
  )

  structure(
    list(
      incidence = rates,
      rate_ratio = rate_ratio,
      safety_signal = signal,
      treatment = treatment,
      events = events,
      person_time = person_time,
      confounders = confounders,
      time_var = time_var,
      alpha = alpha,
      scale = scale,
      confidence_level = confidence_level,
      audit_trail = audit
    ),
    class = c("rwe_safety_analysis", "rwe_analysis")
  )
}

## Print methods ------------------------------------------------------------

#' @export
print.rwe_incidence_rate <- function(x, ...) {
  cat("Safety Incidence Rates\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")
  cat("Scale: per", x$scale, "person-time units\n\n")
  print(x$estimates, row.names = FALSE, digits = 3)
  invisible(x)
}

#' @export
print.rwe_rate_ratio <- function(x, ...) {
  cat("Incidence Rate Ratio Analysis\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")
  cat("Rate Ratio:", round(x$rate_ratio, 3), "\n")
  cat("95% CI: (", round(x$ci_lower, 3), ", ", round(x$ci_upper, 3), ")\n", sep = "")
  cat("P-value:", format.pval(x$p_value, digits = 3), "\n")
  invisible(x)
}

#' @export
print.rwe_safety_signal <- function(x, ...) {
  cat("Safety Surveillance Summary\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")
  cat("Baseline rate:", round(x$baseline_rate, 4), "\n")
  cat("Alpha:", x$alpha, "\n")
  cat("Method:", x$method, "\n")
  cat("Signal detected:", ifelse(x$detected, "Yes", "No"), "\n")
  if (!is.na(x$first_signal_index)) {
    cat("First signal index:", x$first_signal_index, "\n")
  }
  invisible(x)
}

#' @export
print.rwe_safety_analysis <- function(x, ...) {
  cat("Comprehensive Safety Analysis\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")
  print(x$incidence)
  cat("\n")
  print(x$rate_ratio)
  if (!is.null(x$safety_signal)) {
    cat("\n")
    print(x$safety_signal)
  }
  invisible(x)
}
