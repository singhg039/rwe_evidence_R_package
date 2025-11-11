# Effectiveness Analysis Module
# Treatment effectiveness measures and supporting utilities

# Internal helpers ---------------------------------------------------------

.extract_analysis_frame <- function(data) {
  if (is_rwe_data(data)) {
    data$data
  } else {
    data
  }
}

.coerce_binary <- function(x, name) {
  if (is.logical(x)) {
    return(as.numeric(x))
  }

  if (is.numeric(x)) {
    unique_vals <- unique(stats::na.omit(x))
    if (!all(unique_vals %in% c(0, 1))) {
      stop("'", name, "' must be coded as 0/1 or logical.", call. = FALSE)
    }
    return(as.numeric(x))
  }

  stop("'", name, "' must be numeric (0/1) or logical.", call. = FALSE)
}

.resolve_weights <- function(data, weights) {
  if (is.null(weights)) {
    return(list(values = NULL, name = NULL))
  }

  if (inherits(weights, "rwe_iptw")) {
    values <- weights$weights
    if (length(values) != nrow(data)) {
      stop("Length of IPTW weights does not match number of rows in data.", call. = FALSE)
    }
    return(list(values = values, name = NULL))
  }

  if (is.character(weights) && length(weights) == 1) {
    if (!weights %in% names(data)) {
      stop("Weights column '", weights, "' not found in data.", call. = FALSE)
    }
    values <- data[[weights]]
    return(list(values = values, name = weights))
  }

  if (is.numeric(weights) && length(weights) == nrow(data)) {
    return(list(values = weights, name = NULL))
  }

  stop("weights must be NULL, a column name, numeric vector, or rwe_iptw object.", call. = FALSE)
}

.prepare_analysis_inputs <- function(data,
                                     outcome,
                                     treatment,
                                     confounders = NULL,
                                     weights = NULL) {

  validate_inputs(data)
  required_cols <- unique(c(outcome, treatment, confounders))
  validate_inputs(data, required_cols = required_cols)

  weight_info <- .resolve_weights(data, weights)
  n_original <- nrow(data)

  if (n_original == 0) {
    stop("Input data has no rows.", call. = FALSE)
  }

  keep_idx <- stats::complete.cases(data[, required_cols, drop = FALSE])
  if (!is.null(weight_info$values)) {
    keep_idx <- keep_idx & !is.na(weight_info$values)
  }

  analysis_df <- data[keep_idx, , drop = FALSE]
  weights_vec <- if (!is.null(weight_info$values)) weight_info$values[keep_idx] else NULL

  if (nrow(analysis_df) == 0) {
    stop("No rows available after removing missing values in required columns.", call. = FALSE)
  }

  analysis_df[[outcome]] <- .coerce_binary(analysis_df[[outcome]], outcome)
  analysis_df[[treatment]] <- .coerce_binary(analysis_df[[treatment]], treatment)

  if (length(unique(analysis_df[[treatment]])) < 2) {
    stop("Treatment variable must contain both 0 and 1 after filtering.", call. = FALSE)
  }

  list(
    data = analysis_df,
    weights = weights_vec,
    weight_name = weight_info$name,
    n_original = n_original,
    n_analyzed = nrow(analysis_df),
    dropped_rows = n_original - nrow(analysis_df)
  )
}

.build_2x2_table <- function(data,
                             outcome,
                             treatment,
                             continuity_correction = 0.5) {

  tbl <- table(
    factor(data[[treatment]], levels = c(0, 1)),
    factor(data[[outcome]], levels = c(0, 1))
  )

  correction_applied <- FALSE
  if (any(tbl == 0)) {
    if (continuity_correction > 0) {
      tbl <- tbl + continuity_correction
      correction_applied <- TRUE
    } else {
      stop("Zero counts detected in contingency table. Set continuity_correction > 0 to adjust.",
           call. = FALSE)
    }
  }

  n_treated <- sum(tbl["1", ])
  n_control <- sum(tbl["0", ])

  if (n_treated == 0 || n_control == 0) {
    stop("Both treatment groups must have observations.", call. = FALSE)
  }

  events_treated <- tbl["1", "1"]
  events_control <- tbl["0", "1"]
  non_events_treated <- tbl["1", "0"]
  non_events_control <- tbl["0", "0"]

  list(
    table = tbl,
    correction_applied = correction_applied,
    n_treated = as.numeric(n_treated),
    n_control = as.numeric(n_control),
    events_treated = as.numeric(events_treated),
    events_control = as.numeric(events_control),
    non_events_treated = as.numeric(non_events_treated),
    non_events_control = as.numeric(non_events_control),
    risk_treated = as.numeric(events_treated / n_treated),
    risk_control = as.numeric(events_control / n_control)
  )
}

.compute_relative_risk <- function(data,
                                   outcome,
                                   treatment,
                                   confounders = NULL,
                                   weights = NULL,
                                   confidence_level = 0.95,
                                   continuity_correction = 0.5) {

  analysis <- .prepare_analysis_inputs(data, outcome, treatment, confounders, weights)
  df <- analysis$data
  weight_vec <- analysis$weights

  table_info <- .build_2x2_table(df, outcome, treatment, continuity_correction)
  alpha <- 1 - confidence_level
  z <- stats::qnorm(1 - alpha / 2)

  if (is.null(confounders) && is.null(weights)) {
    a <- table_info$events_treated
    b <- table_info$non_events_treated
    c <- table_info$events_control
    d <- table_info$non_events_control

    rr <- table_info$risk_treated / table_info$risk_control
    se_log_rr <- sqrt((1 / a) - (1 / (a + b)) + (1 / c) - (1 / (c + d)))
    ci_lower <- exp(log(rr) - z * se_log_rr)
    ci_upper <- exp(log(rr) + z * se_log_rr)

    chi_sq <- suppressWarnings(stats::chisq.test(table_info$table, correct = FALSE))
    p_value <- chi_sq$p.value
    model <- NULL
    method <- "crude"
    risk_treated <- table_info$risk_treated
    risk_control <- table_info$risk_control

  } else {
    formula_terms <- c(treatment, confounders)
    formula_str <- paste(outcome, "~", paste(formula_terms, collapse = " + "))
    formula_obj <- stats::as.formula(formula_str)

    model <- suppressWarnings(stats::glm(
      formula_obj,
      data = df,
      family = stats::binomial(link = "log"),
      weights = weight_vec
    ))

    coef_idx <- match(treatment, names(stats::coef(model)))
    if (is.na(coef_idx)) {
      stop("Treatment coefficient not found in model output.", call. = FALSE)
    }

    rr <- exp(stats::coef(model)[coef_idx])
    se <- sqrt(stats::vcov(model)[coef_idx, coef_idx])
    ci_lower <- exp(log(rr) - z * se)
    ci_upper <- exp(log(rr) + z * se)
    p_value <- summary(model)$coefficients[coef_idx, "Pr(>|z|)"]
    method <- "adjusted"

    df_treated <- df
    df_treated[[treatment]] <- 1
    risk_treated <- mean(stats::predict(model, newdata = df_treated, type = "response"))

    df_control <- df
    df_control[[treatment]] <- 0
    risk_control <- mean(stats::predict(model, newdata = df_control, type = "response"))
  }

  list(
    analysis = analysis,
    table_info = table_info,
    risk_ratio = as.numeric(rr),
    ci_lower = as.numeric(ci_lower),
    ci_upper = as.numeric(ci_upper),
    risk_treated = as.numeric(risk_treated),
    risk_control = as.numeric(risk_control),
    risk_difference = as.numeric(risk_treated - risk_control),
    p_value = as.numeric(p_value),
    method = method,
    model = model,
    confidence_level = confidence_level,
    adjustments = confounders,
    weights_used = !is.null(weights)
  )
}

.compute_odds_ratio <- function(data,
                                outcome,
                                treatment,
                                confounders = NULL,
                                weights = NULL,
                                confidence_level = 0.95,
                                continuity_correction = 0.5) {

  analysis <- .prepare_analysis_inputs(data, outcome, treatment, confounders, weights)
  df <- analysis$data
  weight_vec <- analysis$weights

  table_info <- .build_2x2_table(df, outcome, treatment, continuity_correction)
  alpha <- 1 - confidence_level
  z <- stats::qnorm(1 - alpha / 2)

  if (is.null(confounders) && is.null(weights)) {
    a <- table_info$events_treated
    b <- table_info$non_events_treated
    c <- table_info$events_control
    d <- table_info$non_events_control

    odds_treated <- a / b
    odds_control <- c / d
    or <- odds_treated / odds_control
    se_log_or <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
    ci_lower <- exp(log(or) - z * se_log_or)
    ci_upper <- exp(log(or) + z * se_log_or)
    chi_sq <- suppressWarnings(stats::chisq.test(table_info$table, correct = FALSE))
    p_value <- chi_sq$p.value
    method <- "crude"
    model <- NULL
    odds_treated_est <- odds_treated
    odds_control_est <- odds_control

  } else {
    formula_terms <- c(treatment, confounders)
    formula_str <- paste(outcome, "~", paste(formula_terms, collapse = " + "))
    formula_obj <- stats::as.formula(formula_str)

    model <- suppressWarnings(stats::glm(
      formula_obj,
      data = df,
      family = stats::binomial(link = "logit"),
      weights = weight_vec
    ))

    coef_idx <- match(treatment, names(stats::coef(model)))
    if (is.na(coef_idx)) {
      stop("Treatment coefficient not found in model output.", call. = FALSE)
    }

    or <- exp(stats::coef(model)[coef_idx])
    se <- sqrt(stats::vcov(model)[coef_idx, coef_idx])
    ci_lower <- exp(log(or) - z * se)
    ci_upper <- exp(log(or) + z * se)
    p_value <- summary(model)$coefficients[coef_idx, "Pr(>|z|)"]
    method <- "adjusted"

    df_treated <- df
    df_treated[[treatment]] <- 1
    prob_treated <- mean(stats::predict(model, newdata = df_treated, type = "response"))
    odds_treated_est <- prob_treated / (1 - prob_treated)

    df_control <- df
    df_control[[treatment]] <- 0
    prob_control <- mean(stats::predict(model, newdata = df_control, type = "response"))
    odds_control_est <- prob_control / (1 - prob_control)
  }

  list(
    analysis = analysis,
    table_info = table_info,
    odds_ratio = as.numeric(or),
    ci_lower = as.numeric(ci_lower),
    ci_upper = as.numeric(ci_upper),
    odds_treated = as.numeric(odds_treated_est),
    odds_control = as.numeric(odds_control_est),
    p_value = as.numeric(p_value),
    method = method,
    model = model,
    confidence_level = confidence_level,
    adjustments = confounders,
    weights_used = !is.null(weights)
  )
}

.compute_nnt <- function(data,
                         outcome,
                         treatment,
                         beneficial = FALSE,
                         confidence_level = 0.95,
                         continuity_correction = 0.5) {

  analysis <- .prepare_analysis_inputs(data, outcome, treatment)
  df <- analysis$data
  table_info <- .build_2x2_table(df, outcome, treatment, continuity_correction)
  alpha <- 1 - confidence_level
  z <- stats::qnorm(1 - alpha / 2)

  risk_treated <- table_info$risk_treated
  risk_control <- table_info$risk_control
  ard <- risk_treated - risk_control

  if (ard == 0) {
    nnt <- Inf
  } else {
    nnt <- 1 / abs(ard)
  }

  n_treated <- table_info$n_treated
  n_control <- table_info$n_control
  se_ard <- sqrt(
    (risk_treated * (1 - risk_treated) / n_treated) +
    (risk_control * (1 - risk_control) / n_control)
  )

  ard_ci_lower <- ard - z * se_ard
  ard_ci_upper <- ard + z * se_ard

  nnt_ci_lower <- if (ard_ci_upper == 0) Inf else 1 / abs(ard_ci_upper)
  nnt_ci_upper <- if (ard_ci_lower == 0) Inf else 1 / abs(ard_ci_lower)

  if (beneficial) {
    if (ard > 0) {
      interpretation <- "NNH"
      narrative <- "patients need to be treated to cause one additional harmful outcome"
    } else {
      interpretation <- "NNT"
      narrative <- "patients need to be treated to achieve one additional beneficial outcome"
    }
  } else {
    if (ard > 0) {
      interpretation <- "NNH"
      narrative <- "patients need to be treated to cause one additional adverse event"
    } else {
      interpretation <- "NNT"
      narrative <- "patients need to be treated to prevent one additional adverse event"
    }
  }

  list(
    analysis = analysis,
    table_info = table_info,
    nnt = as.numeric(nnt),
    ci_lower = as.numeric(min(nnt_ci_lower, nnt_ci_upper)),
    ci_upper = as.numeric(max(nnt_ci_lower, nnt_ci_upper)),
    risk_difference = as.numeric(ard),
    risk_treated = as.numeric(risk_treated),
    risk_control = as.numeric(risk_control),
    interpretation = interpretation,
    narrative = narrative,
    confidence_level = confidence_level,
    beneficial = beneficial
  )
}

# Relative risk ------------------------------------------------------------

#' Calculate Relative Risk (Risk Ratio)
#'
#' Computes the relative risk (RR) comparing treatment outcomes between groups.
#' The relative risk is the ratio of the probability of an outcome in the
#' exposed group to the probability in the unexposed group.
#'
#' @param data Data frame or rwe_data object containing the analysis data.
#' @param outcome Binary outcome variable name (1 = event, 0 = no event).
#' @param treatment Treatment variable name (typically binary: 1 = treated, 0 = control).
#' @param confounders Optional vector of confounder variable names for adjusted RR.
#' @param weights Optional weights (column name, numeric vector, or rwe_iptw object).
#' @param confidence_level Confidence level for CI (default: 0.95).
#' @param continuity_correction Value added to 2x2 table cells when zeros are present (default: 0.5).
#' @param quiet Logical; suppress console output (default: FALSE).
#'
#' @return An rwe_relative_risk object with estimates, confidence interval, and audit trail.
#' @export
#'
#' @examples
#' \dontrun{
#' rr <- rwe_relative_risk(
#'   data = trial_data,
#'   outcome = "death",
#'   treatment = "treated"
#' )
#' }
rwe_relative_risk <- function(data,
                              outcome,
                              treatment,
                              confounders = NULL,
                              weights = NULL,
                              confidence_level = 0.95,
                              continuity_correction = 0.5,
                              quiet = FALSE) {

  log_info("Calculating relative risk", context = "rwe_relative_risk")

  data_df <- .extract_analysis_frame(data)

  rr_result <- .compute_relative_risk(
    data = data_df,
    outcome = outcome,
    treatment = treatment,
    confounders = confounders,
    weights = weights,
    confidence_level = confidence_level,
    continuity_correction = continuity_correction
  )

  if (!quiet) {
    cat("Relative Risk Analysis\n")
    cat(paste(rep("=", 50), collapse = ""), "\n\n")
    cat("Method:", rr_result$method, "\n")
    if (!is.null(confounders)) {
      cat("Adjustments:", paste(confounders, collapse = ", "), "\n")
    }
    if (!is.null(weights)) {
      cat("Weights supplied\n")
    }
    cat("Relative Risk:", round(rr_result$risk_ratio, 3), "\n")
    cat("Confidence Interval (", confidence_level * 100, "%): ",
        "(", round(rr_result$ci_lower, 3), ", ",
        round(rr_result$ci_upper, 3), ")\n", sep = "")
    cat("Risk (treated):", round(rr_result$risk_treated, 4), "\n")
    cat("Risk (control):", round(rr_result$risk_control, 4), "\n")
    cat("Risk Difference:", round(rr_result$risk_difference, 4), "\n")
    cat("P-value:", format(rr_result$p_value, digits = 3), "\n\n")
  }

  log_info("Relative risk calculated", context = "rwe_relative_risk")

  audit <- create_audit_trail(
    operation = "relative_risk_calculation",
    input_data = list(
      n_rows = rr_result$analysis$n_original,
      n_analyzed = rr_result$analysis$n_analyzed,
      outcome = outcome,
      treatment = treatment
    ),
    output_data = list(
      relative_risk = rr_result$risk_ratio,
      ci_lower = rr_result$ci_lower,
      ci_upper = rr_result$ci_upper,
      p_value = rr_result$p_value
    ),
    parameters = list(
      method = rr_result$method,
      confounders = confounders,
      confidence_level = confidence_level,
      continuity_correction = continuity_correction
    )
  )

  structure(
    list(
      risk_ratio = rr_result$risk_ratio,
      ci_lower = rr_result$ci_lower,
      ci_upper = rr_result$ci_upper,
      risk_treated = rr_result$risk_treated,
      risk_control = rr_result$risk_control,
      risk_difference = rr_result$risk_difference,
      p_value = rr_result$p_value,
      method = rr_result$method,
      outcome = outcome,
      treatment = treatment,
      confidence_level = confidence_level,
      adjustments = confounders,
      weights_used = rr_result$weights_used,
      contingency_table = rr_result$table_info$table,
      correction_applied = rr_result$table_info$correction_applied,
      n_treated = rr_result$table_info$n_treated,
      n_control = rr_result$table_info$n_control,
      events_treated = rr_result$table_info$events_treated,
      events_control = rr_result$table_info$events_control,
      model = rr_result$model,
      analysis_metadata = rr_result$analysis,
      audit_trail = audit
    ),
    class = c("rwe_relative_risk", "rwe_analysis")
  )
}

# Odds ratio ---------------------------------------------------------------

#' Calculate Odds Ratio
#'
#' Computes the odds ratio (OR) comparing treatment outcomes between groups.
#' The odds ratio is the ratio of the odds of an outcome in the exposed group
#' to the odds in the unexposed group.
#'
#' @param data Data frame or rwe_data object containing the analysis data.
#' @param outcome Binary outcome variable name (1 = event, 0 = no event).
#' @param treatment Treatment variable name (typically binary: 1 = treated, 0 = control).
#' @param confounders Optional vector of confounder variable names for adjusted OR.
#' @param weights Optional weights (column name, numeric vector, or rwe_iptw object).
#' @param confidence_level Confidence level for CI (default: 0.95).
#' @param continuity_correction Value added to 2x2 table cells when zeros are present (default: 0.5).
#' @param quiet Logical; suppress console output (default: FALSE).
#'
#' @return An rwe_odds_ratio object with estimates, confidence interval, and audit trail.
#' @export
#'
#' @examples
#' \dontrun{
#' or <- rwe_odds_ratio(
#'   data = cohort_data,
#'   outcome = "remission",
#'   treatment = "treated"
#' )
#' }
rwe_odds_ratio <- function(data,
                           outcome,
                           treatment,
                           confounders = NULL,
                           weights = NULL,
                           confidence_level = 0.95,
                           continuity_correction = 0.5,
                           quiet = FALSE) {

  log_info("Calculating odds ratio", context = "rwe_odds_ratio")

  data_df <- .extract_analysis_frame(data)

  or_result <- .compute_odds_ratio(
    data = data_df,
    outcome = outcome,
    treatment = treatment,
    confounders = confounders,
    weights = weights,
    confidence_level = confidence_level,
    continuity_correction = continuity_correction
  )

  if (!quiet) {
    cat("Odds Ratio Analysis\n")
    cat(paste(rep("=", 50), collapse = ""), "\n\n")
    cat("Method:", or_result$method, "\n")
    if (!is.null(confounders)) {
      cat("Adjustments:", paste(confounders, collapse = ", "), "\n")
    }
    if (!is.null(weights)) {
      cat("Weights supplied\n")
    }
    cat("Odds Ratio:", round(or_result$odds_ratio, 3), "\n")
    cat("Confidence Interval (", confidence_level * 100, "%): ",
        "(", round(or_result$ci_lower, 3), ", ",
        round(or_result$ci_upper, 3), ")\n", sep = "")
    cat("Odds (treated):", round(or_result$odds_treated, 3), "\n")
    cat("Odds (control):", round(or_result$odds_control, 3), "\n")
    cat("P-value:", format(or_result$p_value, digits = 3), "\n\n")
  }

  log_info("Odds ratio calculated", context = "rwe_odds_ratio")

  audit <- create_audit_trail(
    operation = "odds_ratio_calculation",
    input_data = list(
      n_rows = or_result$analysis$n_original,
      n_analyzed = or_result$analysis$n_analyzed,
      outcome = outcome,
      treatment = treatment
    ),
    output_data = list(
      odds_ratio = or_result$odds_ratio,
      ci_lower = or_result$ci_lower,
      ci_upper = or_result$ci_upper,
      p_value = or_result$p_value
    ),
    parameters = list(
      method = or_result$method,
      confounders = confounders,
      confidence_level = confidence_level,
      continuity_correction = continuity_correction
    )
  )

  structure(
    list(
      odds_ratio = or_result$odds_ratio,
      ci_lower = or_result$ci_lower,
      ci_upper = or_result$ci_upper,
      odds_treated = or_result$odds_treated,
      odds_control = or_result$odds_control,
      p_value = or_result$p_value,
      method = or_result$method,
      outcome = outcome,
      treatment = treatment,
      confidence_level = confidence_level,
      adjustments = confounders,
      weights_used = or_result$weights_used,
      contingency_table = or_result$table_info$table,
      correction_applied = or_result$table_info$correction_applied,
      n_treated = or_result$table_info$n_treated,
      n_control = or_result$table_info$n_control,
      events_treated = or_result$table_info$events_treated,
      events_control = or_result$table_info$events_control,
      model = or_result$model,
      analysis_metadata = or_result$analysis,
      audit_trail = audit
    ),
    class = c("rwe_odds_ratio", "rwe_analysis")
  )
}

# Number needed to treat/harm ---------------------------------------------

#' Calculate Number Needed to Treat (NNT) or Harm (NNH)
#'
#' Computes the number needed to treat (NNT) or number needed to harm (NNH).
#' NNT is the number of patients that need to be treated to prevent one
#' additional bad outcome or produce one additional good outcome.
#'
#' @param data Data frame or rwe_data object containing the analysis data.
#' @param outcome Binary outcome variable name (1 = event, 0 = no event).
#' @param treatment Treatment variable name (typically binary: 1 = treated, 0 = control).
#' @param beneficial Logical; is the outcome beneficial? (default: FALSE for adverse outcomes).
#' @param confidence_level Confidence level for CI (default: 0.95).
#' @param continuity_correction Value added to 2x2 table cells when zeros are present (default: 0.5).
#' @param quiet Logical; suppress console output (default: FALSE).
#'
#' @return An rwe_nnt object containing NNT/NNH estimate, confidence interval, and audit trail.
#' @export
rwe_nnt <- function(data,
                    outcome,
                    treatment,
                    beneficial = FALSE,
                    confidence_level = 0.95,
                    continuity_correction = 0.5,
                    quiet = FALSE) {

  log_info("Calculating NNT/NNH", context = "rwe_nnt")

  data_df <- .extract_analysis_frame(data)

  nnt_result <- .compute_nnt(
    data = data_df,
    outcome = outcome,
    treatment = treatment,
    beneficial = beneficial,
    confidence_level = confidence_level,
    continuity_correction = continuity_correction
  )

  if (!quiet) {
    cat(nnt_result$interpretation, " Analysis\n", sep = "")
    cat(paste(rep("=", 50), collapse = ""), "\n\n")
    cat(nnt_result$interpretation, ":", round(nnt_result$nnt, 2), "\n")
    cat("Confidence Interval (", confidence_level * 100, "%): ",
        "(", round(nnt_result$ci_lower, 2), ", ",
        round(nnt_result$ci_upper, 2), ")\n", sep = "")
    cat("Risk (treated):", round(nnt_result$risk_treated, 4), "\n")
    cat("Risk (control):", round(nnt_result$risk_control, 4), "\n")
    cat("Risk Difference:", round(nnt_result$risk_difference, 4), "\n")
    cat("Interpretation: Approximately",
        round(nnt_result$nnt, 0), nnt_result$narrative, "\n\n")
  }

  log_info("NNT/NNH calculated", context = "rwe_nnt")

  audit <- create_audit_trail(
    operation = "nnt_calculation",
    input_data = list(
      n_rows = nnt_result$analysis$n_original,
      n_analyzed = nnt_result$analysis$n_analyzed,
      outcome = outcome,
      treatment = treatment
    ),
    output_data = list(
      nnt = nnt_result$nnt,
      ci_lower = nnt_result$ci_lower,
      ci_upper = nnt_result$ci_upper,
      risk_difference = nnt_result$risk_difference
    ),
    parameters = list(
      beneficial = beneficial,
      confidence_level = confidence_level,
      continuity_correction = continuity_correction
    )
  )

  structure(
    list(
      nnt = nnt_result$nnt,
      ci_lower = nnt_result$ci_lower,
      ci_upper = nnt_result$ci_upper,
      risk_difference = nnt_result$risk_difference,
      risk_treated = nnt_result$risk_treated,
      risk_control = nnt_result$risk_control,
      interpretation = nnt_result$interpretation,
      narrative = nnt_result$narrative,
      outcome = outcome,
      treatment = treatment,
      confidence_level = confidence_level,
      beneficial = beneficial,
      contingency_table = nnt_result$table_info$table,
      correction_applied = nnt_result$table_info$correction_applied,
      n_treated = nnt_result$table_info$n_treated,
      n_control = nnt_result$table_info$n_control,
      analysis_metadata = nnt_result$analysis,
      audit_trail = audit
    ),
    class = c("rwe_nnt", "rwe_analysis")
  )
}

#' Calculate Number Needed to Harm (NNH)
#'
#' Convenience wrapper for `rwe_nnt()` with `beneficial = FALSE`.
#'
#' @inheritParams rwe_nnt
#' @return An rwe_nnt object (interpretation set to NNH).
#' @export
rwe_nnh <- function(data,
                    outcome,
                    treatment,
                    confidence_level = 0.95,
                    continuity_correction = 0.5,
                    quiet = FALSE) {
  rwe_nnt(
    data = data,
    outcome = outcome,
    treatment = treatment,
    beneficial = FALSE,
    confidence_level = confidence_level,
    continuity_correction = continuity_correction,
    quiet = quiet
  )
}

# Subgroup analysis -------------------------------------------------------

#' Perform Subgroup Effectiveness Analysis
#'
#' Calculates treatment effectiveness measures within predefined subgroups and
#' returns a forest plot summarizing subgroup effects.
#'
#' @param data Data frame or rwe_data object.
#' @param outcome Binary outcome variable name.
#' @param treatment Treatment variable name.
#' @param subgroup_vars Character vector of subgroup variable names.
#' @param measure Effect measure for subgroup estimates: "risk_ratio", "odds_ratio", or "risk_difference".
#' @param confounders Optional adjustment covariates applied within each subgroup.
#' @param weights Optional weights (column name, numeric vector, or rwe_iptw object).
#' @param confidence_level Confidence level for confidence intervals.
#' @param min_cells Minimum number of observations per treatment arm required within a subgroup (default: 10).
#' @param continuity_correction Continuity correction applied to 2x2 tables (default: 0.5).
#' @param quiet Logical; suppress console messages.
#'
#' @return An rwe_subgroup_analysis object containing subgroup estimates, forest plot, and audit trail.
#' @export
rwe_subgroup_analysis <- function(data,
                                  outcome,
                                  treatment,
                                  subgroup_vars,
                                  measure = c("risk_ratio", "odds_ratio", "risk_difference"),
                                  confounders = NULL,
                                  weights = NULL,
                                  confidence_level = 0.95,
                                  min_cells = 10,
                                  continuity_correction = 0.5,
                                  quiet = FALSE) {

  measure <- match.arg(measure)
  if (length(subgroup_vars) == 0) {
    stop("subgroup_vars must contain at least one variable.", call. = FALSE)
  }

  data_df <- .extract_analysis_frame(data)
  validate_inputs(data_df)

  missing_subgroups <- setdiff(subgroup_vars, names(data_df))
  if (length(missing_subgroups) > 0) {
    stop("Subgroup variables not found: ", paste(missing_subgroups, collapse = ", "), call. = FALSE)
  }

  if (!quiet) {
    cat("Subgroup Effectiveness Analysis\n")
    cat(paste(rep("=", 60), collapse = ""), "\n\n")
  }

  results_list <- list()
  weight_info <- .resolve_weights(data_df, weights)

  for (subgroup in subgroup_vars) {
    subgroup_levels <- unique(stats::na.omit(data_df[[subgroup]]))
    if (length(subgroup_levels) <= 1) {
      if (!quiet) {
        cat("Skipping subgroup", subgroup, "- less than two levels.\n")
      }
      next
    }

    for (level in subgroup_levels) {
      subset_idx <- which(!is.na(data_df[[subgroup]]) & data_df[[subgroup]] == level)
      subset_df <- data_df[subset_idx, , drop = FALSE]
      if (nrow(subset_df) == 0) {
        next
      }

      subset_weights <- if (!is.null(weight_info$name)) {
        subset_df[[weight_info$name]]
      } else if (!is.null(weight_info$values)) {
        weight_info$values[subset_idx]
      } else {
        NULL
      }

      analysis_subset <- tryCatch(
        .prepare_analysis_inputs(
          data = subset_df,
          outcome = outcome,
          treatment = treatment,
          confounders = confounders,
          weights = subset_weights
        ),
        error = function(err) {
          if (!quiet) {
            cat("Skipping subgroup level", subgroup, "=", level, "-", err$message, "\n")
          }
          NULL
        }
      )

      if (is.null(analysis_subset)) {
        next
      }

      analysis_data <- analysis_subset$data
      treated_mask <- analysis_data[[treatment]] == 1
      control_mask <- analysis_data[[treatment]] == 0
      n_treated_raw <- sum(treated_mask)
      n_control_raw <- sum(control_mask)

      if (n_treated_raw < min_cells || n_control_raw < min_cells) {
        if (!quiet) {
          cat("Skipping subgroup level", subgroup, "=", level,
              "- fewer than", min_cells, "observations per arm.\n")
        }
        next
      }

      table_info <- tryCatch(
        .build_2x2_table(
          data = analysis_data,
          outcome = outcome,
          treatment = treatment,
          continuity_correction = continuity_correction
        ),
        error = function(err) {
          if (!quiet) {
            cat("Skipping subgroup level", subgroup, "=", level, "-", err$message, "\n")
          }
          NULL
        }
      )

      if (is.null(table_info)) {
        next
      }

      events_treated <- sum(analysis_data[[outcome]][treated_mask])
      events_control <- sum(analysis_data[[outcome]][control_mask])
      subset_weights_arg <- analysis_subset$weights

      estimate <- tryCatch({
        if (measure == "risk_ratio") {
          rr <- .compute_relative_risk(
            data = analysis_data,
            outcome = outcome,
            treatment = treatment,
            confounders = confounders,
            weights = subset_weights_arg,
            confidence_level = confidence_level,
            continuity_correction = continuity_correction
          )
          list(
            estimate = rr$risk_ratio,
            ci_lower = rr$ci_lower,
            ci_upper = rr$ci_upper
          )
        } else if (measure == "odds_ratio") {
          or <- .compute_odds_ratio(
            data = analysis_data,
            outcome = outcome,
            treatment = treatment,
            confounders = confounders,
            weights = subset_weights_arg,
            confidence_level = confidence_level,
            continuity_correction = continuity_correction
          )
          list(
            estimate = or$odds_ratio,
            ci_lower = or$ci_lower,
            ci_upper = or$ci_upper
          )
        } else {
          risk_treated <- mean(analysis_data[[outcome]][treated_mask])
          risk_control <- mean(analysis_data[[outcome]][control_mask])
          rd <- risk_treated - risk_control
          z <- stats::qnorm(1 - (1 - confidence_level) / 2)
          se_rd <- sqrt(
            (risk_treated * (1 - risk_treated) / n_treated_raw) +
              (risk_control * (1 - risk_control) / n_control_raw)
          )
          list(
            estimate = rd,
            ci_lower = rd - z * se_rd,
            ci_upper = rd + z * se_rd
          )
        }
      }, error = function(err) {
        if (!quiet) {
          cat("Skipping subgroup level", subgroup, "=", level, "-", err$message, "\n")
        }
        NULL
      })

      if (is.null(estimate)) {
        next
      }

      if (measure == "risk_difference") {
        estimate$ci_lower <- max(-1, estimate$ci_lower)
        estimate$ci_upper <- min(1, estimate$ci_upper)
      }

      results_list[[length(results_list) + 1]] <- data.frame(
        subgroup = subgroup,
        level = as.character(level),
        estimate = estimate$estimate,
        ci_lower = estimate$ci_lower,
        ci_upper = estimate$ci_upper,
        n_treated = n_treated_raw,
        n_control = n_control_raw,
        events_treated = events_treated,
        events_control = events_control,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(results_list) == 0) {
    stop("No valid subgroup estimates could be calculated.", call. = FALSE)
  }

  results_df <- do.call(rbind, results_list)
  results_df$label <- paste(results_df$subgroup, results_df$level, sep = ": ")
  results_df$label <- factor(results_df$label, levels = rev(unique(results_df$label)))

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for subgroup forest plots. Install it with install.packages('ggplot2').",
         call. = FALSE)
  }

  target_line <- if (measure %in% c("risk_ratio", "odds_ratio")) 1 else 0
  x_scale <- if (measure %in% c("risk_ratio", "odds_ratio")) ggplot2::scale_x_log10() else ggplot2::scale_x_continuous()
  x_label <- switch(
    measure,
    risk_ratio = "Risk Ratio",
    odds_ratio = "Odds Ratio",
    risk_difference = "Risk Difference"
  )

  forest_plot <- ggplot2::ggplot(results_df, ggplot2::aes(x = estimate, y = label)) +
    ggplot2::geom_point(color = "#1f78b4", size = 2.5) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
      height = 0.2,
      color = "#1f78b4"
    ) +
    ggplot2::geom_vline(xintercept = target_line, linetype = "dashed", color = "#6b6b6b") +
    x_scale +
    ggplot2::labs(
      title = "Subgroup Effectiveness Analysis",
      x = x_label,
      y = NULL
    ) +
    ggplot2::theme_minimal()

  audit <- create_audit_trail(
    operation = "subgroup_effectiveness",
    input_data = list(
      outcome = outcome,
      treatment = treatment,
      subgroups = subgroup_vars,
      measure = measure
    ),
    output_data = list(
      n_subgroups = nrow(results_df),
      measure = measure
    ),
    parameters = list(
      confidence_level = confidence_level,
      min_cells = min_cells
    )
  )

  structure(
    list(
      results = results_df,
      plot = forest_plot,
      measure = measure,
      outcome = outcome,
      treatment = treatment,
      subgroups = subgroup_vars,
      confidence_level = confidence_level,
      call = match.call(),
      audit_trail = audit
    ),
    class = c("rwe_subgroup_analysis", "rwe_analysis")
  )
}

# Sensitivity analysis ----------------------------------------------------

#' Conduct Sensitivity Analyses for Treatment Effectiveness
#'
#' Performs a suite of sensitivity analyses to evaluate robustness of treatment
#' effectiveness measures to analytic decisions.
#'
#' @param data Data frame or rwe_data object.
#' @param outcome Binary outcome variable name.
#' @param treatment Treatment variable name.
#' @param confounders Optional adjustment covariates.
#' @param weights Optional weights (column name, numeric vector, or rwe_iptw object).
#' @param analyses Character vector specifying which analyses to run. Supported options:
#'   `"trim_weights"`, `"missing_outcomes"`, `"permutation"`.
#' @param measure Primary effect measure: `"risk_ratio"` or `"odds_ratio"`.
#' @param confidence_level Confidence level for interval estimates.
#' @param trim_quantiles Quantiles used to trim weights for the trimming analysis.
#' @param n_permutations Number of permutations for the permutation analysis.
#' @param seed Optional random seed for reproducibility.
#' @param continuity_correction Continuity correction applied to 2x2 tables (default: 0.5).
#' @param quiet Logical; suppress console output.
#'
#' @return An rwe_sensitivity_analysis object containing results for each sensitivity analysis.
#' @export
rwe_sensitivity_analysis <- function(data,
                                     outcome,
                                     treatment,
                                     confounders = NULL,
                                     weights = NULL,
                                     analyses = c("trim_weights", "missing_outcomes", "permutation"),
                                     measure = c("risk_ratio", "odds_ratio"),
                                     confidence_level = 0.95,
                                     trim_quantiles = c(0.01, 0.99),
                                     n_permutations = 200,
                                     seed = NULL,
                                     continuity_correction = 0.5,
                                     quiet = FALSE) {

  analyses <- match.arg(analyses, several.ok = TRUE)
  measure <- match.arg(measure)

  data_df <- .extract_analysis_frame(data)

  if (!is.null(seed)) {
    seed_exists <- exists(".Random.seed", envir = .GlobalEnv)
    if (seed_exists) {
      old_seed <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
    } else {
      on.exit({
        if (exists(".Random.seed", envir = .GlobalEnv)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      }, add = TRUE)
    }
    set.seed(seed)
  }

  compute_primary <- function(df, custom_weights = weights) {
    if (measure == "risk_ratio") {
      res <- .compute_relative_risk(
        data = df,
        outcome = outcome,
        treatment = treatment,
        confounders = confounders,
        weights = custom_weights,
        confidence_level = confidence_level,
        continuity_correction = continuity_correction
      )
      list(
        estimate = res$risk_ratio,
        ci_lower = res$ci_lower,
        ci_upper = res$ci_upper,
        object = res
      )
    } else {
      res <- .compute_odds_ratio(
        data = df,
        outcome = outcome,
        treatment = treatment,
        confounders = confounders,
        weights = custom_weights,
        confidence_level = confidence_level,
        continuity_correction = continuity_correction
      )
      list(
        estimate = res$odds_ratio,
        ci_lower = res$ci_lower,
        ci_upper = res$ci_upper,
        object = res
      )
    }
  }

  if (!quiet) {
    cat("Sensitivity Analysis\n")
    cat(paste(rep("=", 50), collapse = ""), "\n\n")
  }

  primary <- compute_primary(data_df)
  results <- list(
    primary = list(
      estimate = primary$estimate,
      ci_lower = primary$ci_lower,
      ci_upper = primary$ci_upper
    )
  )

  if ("trim_weights" %in% analyses) {
    weight_info <- .resolve_weights(data_df, weights)
    if (is.null(weight_info$values)) {
      if (!quiet) {
        cat("Skipping weight trimming sensitivity - no weights supplied.\n")
      }
    } else {
      bounds <- stats::quantile(weight_info$values, probs = trim_quantiles, na.rm = TRUE)
      keep_idx <- weight_info$values >= bounds[1] & weight_info$values <= bounds[2]
      trimmed_df <- data_df[keep_idx, , drop = FALSE]
      trimmed_weights <- if (!is.null(weight_info$name)) {
        trimmed_df[[weight_info$name]]
      } else {
        weight_info$values[keep_idx]
      }
      trimmed <- compute_primary(trimmed_df, custom_weights = trimmed_weights)
      results$trim_weights <- list(
        estimate = trimmed$estimate,
        ci_lower = trimmed$ci_lower,
        ci_upper = trimmed$ci_upper,
        retained_fraction = sum(keep_idx) / length(keep_idx),
        bounds = bounds
      )
    }
  }

  if ("missing_outcomes" %in% analyses) {
    outcome_vals <- data_df[[outcome]]
    if (anyNA(outcome_vals)) {
      treated_idx <- data_df[[treatment]] == 1
      control_idx <- data_df[[treatment]] == 0
      missing_idx <- is.na(outcome_vals)

      best_df <- data_df
      best_df[[outcome]][missing_idx & treated_idx] <- 0
      best_df[[outcome]][missing_idx & control_idx] <- 1

      worst_df <- data_df
      worst_df[[outcome]][missing_idx & treated_idx] <- 1
      worst_df[[outcome]][missing_idx & control_idx] <- 0

      best <- compute_primary(best_df)
      worst <- compute_primary(worst_df)

      results$missing_outcomes <- list(
        best_case = list(
          estimate = best$estimate,
          ci_lower = best$ci_lower,
          ci_upper = best$ci_upper
        ),
        worst_case = list(
          estimate = worst$estimate,
          ci_lower = worst$ci_lower,
          ci_upper = worst$ci_upper
        ),
        n_missing = sum(missing_idx)
      )
    } else if (!quiet) {
      cat("Skipping missing outcome sensitivity - no missing outcomes detected.\n")
    }
  }

  if ("permutation" %in% analyses && n_permutations > 0) {
    analysis_inputs <- .prepare_analysis_inputs(data_df, outcome, treatment, confounders, weights)
    perm_df <- analysis_inputs$data
    perm_weights <- analysis_inputs$weights
    perm_estimates <- numeric(n_permutations)

    for (i in seq_len(n_permutations)) {
      perm_df[[treatment]] <- sample(perm_df[[treatment]])
      perm <- compute_primary(perm_df, custom_weights = perm_weights)
      perm_estimates[i] <- perm$estimate
    }

    observed <- primary$estimate
    if (measure %in% c("risk_ratio", "odds_ratio")) {
      p_perm <- mean(abs(log(perm_estimates)) >= abs(log(observed)))
    } else {
      p_perm <- mean(abs(perm_estimates) >= abs(observed))
    }

    results$permutation <- list(
      estimates = perm_estimates,
      observed = observed,
      p_value = p_perm,
      n_permutations = n_permutations
    )
  } else if ("permutation" %in% analyses && n_permutations <= 0 && !quiet) {
    cat("Skipping permutation sensitivity - n_permutations must be > 0.\n")
  }

  if (!quiet) {
    cat("Sensitivity analyses completed.\n\n")
  }

  audit <- create_audit_trail(
    operation = "sensitivity_analysis",
    input_data = list(
      outcome = outcome,
      treatment = treatment,
      analyses = analyses,
      measure = measure
    ),
    output_data = list(
      primary_estimate = results$primary$estimate,
      n_analyses = length(results)
    ),
    parameters = list(
      confidence_level = confidence_level,
      n_permutations = n_permutations
    )
  )

  structure(
    list(
      analyses = results,
      measure = measure,
      outcome = outcome,
      treatment = treatment,
      confounders = confounders,
      confidence_level = confidence_level,
      call = match.call(),
      audit_trail = audit
    ),
    class = c("rwe_sensitivity_analysis", "rwe_analysis")
  )
}

# Print and plot methods --------------------------------------------------

#' @export
print.rwe_relative_risk <- function(x, ...) {
  cat("Relative Risk Analysis\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")
  cat("Method:", x$method, "\n")
  if (!is.null(x$adjustments)) {
    cat("Adjustments:", paste(x$adjustments, collapse = ", "), "\n")
  }
  if (isTRUE(x$weights_used)) {
    cat("Weights: applied\n")
  }
  cat("Relative Risk:", round(x$risk_ratio, 3), "\n")
  cat("Confidence Interval (", x$confidence_level * 100, "%): ",
      "(", round(x$ci_lower, 3), ", ",
      round(x$ci_upper, 3), ")\n", sep = "")
  cat("Risk (treated):", round(x$risk_treated, 4), "\n")
  cat("Risk (control):", round(x$risk_control, 4), "\n")
  cat("Risk Difference:", round(x$risk_difference, 4), "\n")
  cat("P-value:", format(x$p_value, digits = 3), "\n")
  invisible(x)
}

#' @export
print.rwe_odds_ratio <- function(x, ...) {
  cat("Odds Ratio Analysis\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")
  cat("Method:", x$method, "\n")
  if (!is.null(x$adjustments)) {
    cat("Adjustments:", paste(x$adjustments, collapse = ", "), "\n")
  }
  if (isTRUE(x$weights_used)) {
    cat("Weights: applied\n")
  }
  cat("Odds Ratio:", round(x$odds_ratio, 3), "\n")
  cat("Confidence Interval (", x$confidence_level * 100, "%): ",
      "(", round(x$ci_lower, 3), ", ",
      round(x$ci_upper, 3), ")\n", sep = "")
  cat("Odds (treated):", round(x$odds_treated, 3), "\n")
  cat("Odds (control):", round(x$odds_control, 3), "\n")
  cat("P-value:", format(x$p_value, digits = 3), "\n")
  invisible(x)
}

#' @export
print.rwe_nnt <- function(x, ...) {
  cat(x$interpretation, " Analysis\n", sep = "")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")
  cat(x$interpretation, ":", round(x$nnt, 2), "\n")
  cat("Confidence Interval (", x$confidence_level * 100, "%): ",
      "(", round(x$ci_lower, 2), ", ",
      round(x$ci_upper, 2), ")\n", sep = "")
  cat("Risk Difference:", round(x$risk_difference, 4), "\n")
  cat("Risk (treated):", round(x$risk_treated, 4), "\n")
  cat("Risk (control):", round(x$risk_control, 4), "\n")
  invisible(x)
}

#' @export
print.rwe_subgroup_analysis <- function(x, ...) {
  cat("Subgroup Effectiveness Analysis\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")
  cat("Measure:", x$measure, "\n")
  cat("Outcome:", x$outcome, "\n")
  cat("Treatment:", x$treatment, "\n")
  cat("Subgroups analyzed:", paste(x$subgroups, collapse = ", "), "\n\n")
  print(x$results,
                          row.names = FALSE,
                          digits = 3)
  invisible(x)
}

#' @export
plot.rwe_subgroup_analysis <- function(x, ...) {
  x$plot
}

#' @export
print.rwe_sensitivity_analysis <- function(x, ...) {
  cat("Sensitivity Analysis Summary\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")
  cat("Measure:", x$measure, "\n")
  cat("Outcome:", x$outcome, "\n")
  cat("Treatment:", x$treatment, "\n")
  cat("Analyses run:", paste(names(x$analyses), collapse = ", "), "\n\n")
  primary <- x$analyses$primary
  cat("Primary estimate:", round(primary$estimate, 3), "\n")
  cat("CI (", x$confidence_level * 100, "%): (",
      round(primary$ci_lower, 3), ", ",
      round(primary$ci_upper, 3), ")\n\n", sep = "")
  invisible(x)
}
