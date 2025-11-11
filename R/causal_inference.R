#' Causal Inference Utilities
#'
#' @description
#' Functions for causal inference in observational studies including
#' inverse probability weighting, doubly robust estimation, and
#' treatment effect estimation.
#'
#' @name causal_inference
NULL


#' Inverse Probability of Treatment Weighting
#'
#' @description
#' Estimates treatment effects using inverse probability of treatment weighting (IPTW).
#' This method reweights observations to create pseudo-populations where treatment
#' assignment is independent of measured confounders.
#'
#' @param data Study cohort data (data frame or rwe_data object)
#' @param ps_object Optional rwe_propensity_score object (if NULL, estimates PS internally)
#' @param treatment Treatment variable name
#' @param outcome Outcome variable name
#' @param covariates Character vector of covariate names (if ps_object is NULL)
#' @param stabilized Logical; use stabilized weights (recommended)
#' @param truncate Numeric vector of length 2 for weight truncation (e.g., c(0.01, 0.99))
#'
#' @return rwe_iptw object containing weights and balance assessment
#'
#' @examples
#' \dontrun{
#' # With existing propensity scores
#' iptw <- rwe_iptw(
#'   data = study_data,
#'   ps_object = ps_result,
#'   outcome = "survival_days"
#' )
#'
#' # Estimate PS internally
#' iptw <- rwe_iptw(
#'   data = study_data,
#'   treatment = "treated",
#'   outcome = "survival_days",
#'   covariates = c("age", "sex", "comorbidities")
#' )
#' }
#'
#' @export
rwe_iptw <- function(data,
                     ps_object = NULL,
                     treatment = NULL,
                     outcome = NULL,
                     covariates = NULL,
                     stabilized = TRUE,
                     truncate = NULL) {

  log_info("Starting IPTW analysis", context = "rwe_iptw")

  # Extract data if rwe_data object
  if (is_rwe_data(data)) {
    data_df <- data$data
  } else {
    data_df <- data
  }

  validate_inputs(data_df)

  # If no PS object provided, estimate internally
  if (is.null(ps_object)) {
    if (is.null(treatment) || is.null(covariates)) {
      stop("If ps_object is NULL, must provide treatment and covariates", call. = FALSE)
    }

    cat("Estimating propensity scores for IPTW...\n")
    ps_object <- rwe_propensity_score(
      data = data_df,
      treatment = treatment,
      covariates = covariates,
      method = "logistic"
    )
    # Update data_df with propensity scores
    data_df <- ps_object$data
  } else {
    treatment <- ps_object$treatment
    covariates <- ps_object$covariates
    data_df <- ps_object$data
  }

  # Validate outcome
  if (!is.null(outcome) && !outcome %in% names(data_df)) {
    stop("Outcome variable '", outcome, "' not found in data", call. = FALSE)
  }

  cat("\nCalculating inverse probability weights...\n")

  # Calculate IPTW weights
  ps <- data_df$propensity_score
  trt <- data_df[[treatment]]

  # Trim extreme propensity scores to avoid infinite weights
  ps <- pmax(0.01, pmin(ps, 0.99))

  # Basic weights: 1/P(T=1|X) for treated, 1/P(T=0|X) for control
  weights <- ifelse(trt == 1,
                   1 / ps,
                   1 / (1 - ps))

  # Stabilized weights (recommended to reduce variance)
  if (stabilized) {
    cat("Using stabilized weights...\n")
    p_treatment <- mean(trt)
    weights <- ifelse(trt == 1,
                     p_treatment / ps,
                     (1 - p_treatment) / (1 - ps))
  }

  # Truncate extreme weights if requested
  if (!is.null(truncate)) {
    cat("Truncating weights at quantiles:", truncate[1], "-", truncate[2], "\n")
    weight_quantiles <- quantile(weights, probs = truncate, na.rm = TRUE)
    weights <- pmax(weight_quantiles[1], pmin(weights, weight_quantiles[2]))
  }

  # Add weights to data
  data_df$iptw <- weights

  # Assess weighted balance
  cat("Assessing weighted covariate balance...\n")
  weighted_balance <- assess_covariate_balance(
    data = data_df,
    treatment = treatment,
    covariates = covariates,
    weights = weights
  )

  # Compare to unweighted balance
  balance_improvement <- calculate_balance_improvement(
    ps_object$balance_before,
    weighted_balance
  )

  # Weight diagnostics
  weight_summary <- summarize_weights(weights, trt)

  cat("\nIPTW analysis complete!\n")
  cat("Mean weight (treated):", round(weight_summary$mean_treated, 3), "\n")
  cat("Mean weight (control):", round(weight_summary$mean_control, 3), "\n")
  cat("Balance improvement:", round(balance_improvement * 100, 1), "%\n\n")

  log_info("IPTW analysis complete", context = "rwe_iptw")

  # Create IPTW object
  iptw_obj <- structure(
    list(
      data = data_df,
      weights = weights,
      treatment = treatment,
      outcome = outcome,
      covariates = covariates,
      stabilized = stabilized,
      truncate = truncate,
      balance_unweighted = ps_object$balance_before,
      balance_weighted = weighted_balance,
      balance_improvement = balance_improvement,
      weight_summary = weight_summary,
      ps_object = ps_object
    ),
    class = c("rwe_iptw", "rwe_analysis")
  )

  # Add audit trail
  iptw_obj$audit_trail <- create_audit_trail(
    operation = "iptw",
    input_data = list(
      n_rows = nrow(data_df),
      n_treated = sum(trt),
      n_control = sum(!trt)
    ),
    output_data = list(
      mean_weight = mean(weights),
      weight_range = range(weights),
      balance_improvement = balance_improvement
    ),
    parameters = list(
      stabilized = stabilized,
      truncated = !is.null(truncate)
    )
  )

  iptw_obj
}


#' Estimate Treatment Effect with IPTW
#'
#' @description
#' Estimates average treatment effect (ATE) using IPTW-weighted regression
#'
#' @param iptw_object rwe_iptw object from rwe_iptw()
#' @param outcome Outcome variable name (if not specified in iptw_object)
#' @param covariates Optional covariates to adjust for in outcome model
#' @param family Family for outcome model ("gaussian", "binomial", etc.)
#'
#' @return Treatment effect estimate with confidence intervals
#'
#' @examples
#' \dontrun{
#' ate <- rwe_estimate_ate(iptw_result, outcome = "survival_days")
#' }
#'
#' @export
rwe_estimate_ate <- function(iptw_object,
                             outcome = NULL,
                             covariates = NULL,
                             family = "gaussian") {

  if (!inherits(iptw_object, "rwe_iptw")) {
    stop("iptw_object must be an rwe_iptw object", call. = FALSE)
  }

  # Get outcome
  if (is.null(outcome)) {
    outcome <- iptw_object$outcome
  }

  if (is.null(outcome)) {
    stop("outcome must be specified", call. = FALSE)
  }

  log_info("Estimating average treatment effect", context = "rwe_estimate_ate")

  data <- iptw_object$data
  treatment <- iptw_object$treatment
  weights <- iptw_object$weights

  cat("Estimating average treatment effect (ATE)...\n")
  cat("Outcome:", outcome, "\n")
  cat("Treatment:", treatment, "\n\n")

  # Build formula
  if (is.null(covariates)) {
    formula_str <- paste(outcome, "~", treatment)
  } else {
    formula_str <- paste(outcome, "~", treatment, "+", paste(covariates, collapse = " + "))
  }

  model_formula <- as.formula(formula_str)

  # Fit weighted regression
  model <- glm(model_formula,
               data = data,
               weights = weights,
               family = family)

  # Extract treatment effect
  treatment_coef <- coef(model)[treatment]
  treatment_se <- sqrt(diag(vcov(model)))[treatment]

  # Calculate confidence interval
  ci_lower <- treatment_coef - 1.96 * treatment_se
  ci_upper <- treatment_coef + 1.96 * treatment_se
  p_value <- 2 * pnorm(-abs(treatment_coef / treatment_se))

  cat("Average Treatment Effect (ATE):\n")
  cat("  Estimate:", round(treatment_coef, 4), "\n")
  cat("  95% CI: (", round(ci_lower, 4), ",", round(ci_upper, 4), ")\n")
  cat("  P-value:", format.pval(p_value, digits = 4), "\n\n")

  log_info("ATE estimation complete", context = "rwe_estimate_ate")

  # Return results
  ate_result <- list(
    estimate = treatment_coef,
    std_error = treatment_se,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    p_value = p_value,
    model = model,
    outcome = outcome,
    treatment = treatment,
    family = family
  )

  class(ate_result) <- c("rwe_ate", "rwe_analysis")

  ate_result
}


#' Doubly Robust Estimation
#'
#' @description
#' Estimates treatment effects using doubly robust (DR) estimation, which combines
#' propensity score methods with outcome regression. DR estimators are consistent
#' if either the propensity score model OR the outcome model is correctly specified.
#'
#' @param data Study cohort data
#' @param treatment Treatment variable name
#' @param outcome Outcome variable name
#' @param covariates Character vector of covariate names
#' @param ps_method Method for PS estimation ("logistic", "gbm", "random_forest")
#' @param outcome_family Family for outcome model ("gaussian", "binomial")
#'
#' @return rwe_doubly_robust object with treatment effect estimates
#'
#' @examples
#' \dontrun{
#' dr <- rwe_doubly_robust(
#'   data = study_data,
#'   treatment = "treated",
#'   outcome = "survival_days",
#'   covariates = c("age", "sex", "comorbidities")
#' )
#' }
#'
#' @export
rwe_doubly_robust <- function(data,
                              treatment,
                              outcome,
                              covariates,
                              ps_method = "logistic",
                              outcome_family = "gaussian") {

  log_info("Starting doubly robust estimation", context = "rwe_doubly_robust")

  # Extract data if rwe_data object
  if (is_rwe_data(data)) {
    data_df <- data$data
  } else {
    data_df <- data
  }

  validate_inputs(data_df)

  cat("Doubly Robust Estimation\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  # Step 1: Estimate propensity scores
  cat("Step 1: Estimating propensity scores...\n")
  ps_result <- rwe_propensity_score(
    data = data_df,
    treatment = treatment,
    covariates = covariates,
    method = ps_method
  )

  data_df <- ps_result$data
  ps <- data_df$propensity_score
  trt <- data_df[[treatment]]
  y <- data_df[[outcome]]

  # Step 2: Fit outcome models for both treatment groups
  cat("\nStep 2: Fitting outcome regression models...\n")

  # Outcome model formula
  outcome_formula <- as.formula(paste(outcome, "~", paste(covariates, collapse = " + ")))

  # Fit outcome model for treated
  model_treated <- glm(outcome_formula,
                      data = data_df[trt == 1, ],
                      family = outcome_family)

  # Fit outcome model for control
  model_control <- glm(outcome_formula,
                      data = data_df[trt == 0, ],
                      family = outcome_family)

  # Predict potential outcomes for all observations
  mu1 <- predict(model_treated, newdata = data_df, type = "response")
  mu0 <- predict(model_control, newdata = data_df, type = "response")

  cat("Outcome models fitted successfully\n")

  # Step 3: Calculate doubly robust estimator
  cat("\nStep 3: Calculating doubly robust estimates...\n")

  # DR estimator combines outcome regression with IPW adjustment
  # ATE = E[mu1 - mu0] + E[(T/ps)(Y - mu1)] - E[((1-T)/(1-ps))(Y - mu0)]

  dr_component1 <- mean(mu1 - mu0)

  dr_component2 <- mean((trt / ps) * (y - mu1))

  dr_component3 <- mean(((1 - trt) / (1 - ps)) * (y - mu0))

  ate_dr <- dr_component1 + dr_component2 - dr_component3

  # Calculate standard error using bootstrap or influence function
  # For now, use simple variance estimation
  n <- nrow(data_df)

  # Influence function for variance estimation
  influence <- (mu1 - mu0 - ate_dr) +
               (trt / ps) * (y - mu1) -
               ((1 - trt) / (1 - ps)) * (y - mu0)

  se_dr <- sqrt(var(influence) / n)

  # Confidence interval
  ci_lower <- ate_dr - 1.96 * se_dr
  ci_upper <- ate_dr + 1.96 * se_dr
  p_value <- 2 * pnorm(-abs(ate_dr / se_dr))

  cat("\nDoubly Robust Treatment Effect:\n")
  cat("  ATE:", round(ate_dr, 4), "\n")
  cat("  SE:", round(se_dr, 4), "\n")
  cat("  95% CI: (", round(ci_lower, 4), ",", round(ci_upper, 4), ")\n")
  cat("  P-value:", format.pval(p_value, digits = 4), "\n\n")

  log_info("Doubly robust estimation complete", context = "rwe_doubly_robust")

  # Create result object
  dr_obj <- structure(
    list(
      ate = ate_dr,
      std_error = se_dr,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      p_value = p_value,
      treatment = treatment,
      outcome = outcome,
      covariates = covariates,
      ps_result = ps_result,
      model_treated = model_treated,
      model_control = model_control,
      mu1 = mu1,
      mu0 = mu0,
      data = data_df
    ),
    class = c("rwe_doubly_robust", "rwe_analysis")
  )

  # Add audit trail
  dr_obj$audit_trail <- create_audit_trail(
    operation = "doubly_robust_estimation",
    input_data = list(
      n_rows = n,
      n_treated = sum(trt),
      n_control = sum(!trt)
    ),
    output_data = list(
      ate = ate_dr,
      se = se_dr,
      p_value = p_value
    ),
    parameters = list(
      ps_method = ps_method,
      outcome_family = outcome_family
    )
  )

  dr_obj
}


#' Summarize IPW Weights
#'
#' @description
#' Summarizes the distribution of IPW weights by treatment group
#'
#' @param weights Numeric vector of weights
#' @param treatment Binary treatment indicator
#'
#' @return List with weight summaries
#' @keywords internal
summarize_weights <- function(weights, treatment) {

  treated_idx <- treatment == 1

  list(
    mean_treated = mean(weights[treated_idx], na.rm = TRUE),
    sd_treated = sd(weights[treated_idx], na.rm = TRUE),
    mean_control = mean(weights[!treated_idx], na.rm = TRUE),
    sd_control = sd(weights[!treated_idx], na.rm = TRUE),
    min_weight = min(weights, na.rm = TRUE),
    max_weight = max(weights, na.rm = TRUE),
    effective_sample_size = sum(weights)^2 / sum(weights^2)
  )
}


#' Print Method for IPTW Object
#'
#' @param x rwe_iptw object
#' @param ... Additional arguments
#'
#' @export
print.rwe_iptw <- function(x, ...) {
  cat("\nInverse Probability of Treatment Weighting (IPTW)\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  cat("Treatment variable:", x$treatment, "\n")
  if (!is.null(x$outcome)) {
    cat("Outcome variable:", x$outcome, "\n")
  }
  cat("Number of covariates:", length(x$covariates), "\n")
  cat("Stabilized weights:", x$stabilized, "\n")
  cat("Sample size:", nrow(x$data), "\n\n")

  cat("Weight Summary:\n")
  cat("  Treated  - Mean:", round(x$weight_summary$mean_treated, 3),
      "SD:", round(x$weight_summary$sd_treated, 3), "\n")
  cat("  Control  - Mean:", round(x$weight_summary$mean_control, 3),
      "SD:", round(x$weight_summary$sd_control, 3), "\n")
  cat("  Range: [", round(x$weight_summary$min_weight, 3), ",",
      round(x$weight_summary$max_weight, 3), "]\n")
  cat("  Effective N:", round(x$weight_summary$effective_sample_size, 1), "\n\n")

  cat("Covariate Balance:\n")
  cat("  Unweighted - Mean |SMD|:", round(mean(abs(x$balance_unweighted$smd)), 3), "\n")
  cat("  Weighted   - Mean |SMD|:", round(mean(abs(x$balance_weighted$smd)), 3), "\n")
  cat("  Improvement:", round(x$balance_improvement * 100, 1), "%\n\n")

  if (!is.null(x$outcome)) {
    cat("Use rwe_estimate_ate() to estimate treatment effects\n")
  }

  invisible(x)
}


#' Print Method for ATE Object
#'
#' @param x rwe_ate object
#' @param ... Additional arguments
#'
#' @export
print.rwe_ate <- function(x, ...) {
  cat("\nAverage Treatment Effect (ATE)\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  cat("Outcome:", x$outcome, "\n")
  cat("Treatment:", x$treatment, "\n")
  cat("Model family:", x$family, "\n\n")

  cat("Treatment Effect:\n")
  cat("  Estimate:", round(x$estimate, 4), "\n")
  cat("  Std. Error:", round(x$std_error, 4), "\n")
  cat("  95% CI: (", round(x$ci_lower, 4), ",", round(x$ci_upper, 4), ")\n")
  cat("  P-value:", format.pval(x$p_value, digits = 4), "\n")

  invisible(x)
}


#' Print Method for Doubly Robust Object
#'
#' @param x rwe_doubly_robust object
#' @param ... Additional arguments
#'
#' @export
print.rwe_doubly_robust <- function(x, ...) {
  cat("\nDoubly Robust Estimation\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  cat("Treatment:", x$treatment, "\n")
  cat("Outcome:", x$outcome, "\n")
  cat("Covariates:", length(x$covariates), "\n")
  cat("Sample size:", nrow(x$data), "\n\n")

  cat("Average Treatment Effect (ATE):\n")
  cat("  Estimate:", round(x$ate, 4), "\n")
  cat("  Std. Error:", round(x$std_error, 4), "\n")
  cat("  95% CI: (", round(x$ci_lower, 4), ",", round(x$ci_upper, 4), ")\n")
  cat("  P-value:", format.pval(x$p_value, digits = 4), "\n\n")

  cat("This estimator is consistent if either the propensity score model\n")
  cat("OR the outcome regression model is correctly specified.\n")

  invisible(x)
}
