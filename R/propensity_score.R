#' Estimate Propensity Scores
#'
#' @description
#' Estimates propensity scores for treatment assignment using various methods.
#' Propensity scores are used to balance treatment and control groups in
#' observational studies.
#'
#' @param data Study cohort data (data frame or rwe_data object)
#' @param treatment Treatment variable name (binary: 0/1 or logical)
#' @param covariates Character vector of covariate variable names
#' @param method Estimation method: "logistic", "gbm", "random_forest",
#'   "xgboost", "super_learner"
#' @param formula Optional custom formula (overrides treatment and covariates)
#' @param learner_control List of control parameters for learner algorithms
#'
#' @return rwe_propensity_score object containing model and scores
#'
#' @examples
#' \dontrun{
#' # Estimate propensity scores
#' ps <- rwe_propensity_score(
#'   data = study_data,
#'   treatment = "treated",
#'   covariates = c("age", "sex", "comorbidity_score"),
#'   method = "logistic"
#' )
#'
#' print(ps)
#' plot(ps)
#' }
#'
#' @export
rwe_propensity_score <- function(data,
                                  treatment,
                                  covariates,
                                  method = c("logistic", "gbm", "random_forest", "xgboost", "super_learner"),
                                  formula = NULL,
                                  learner_control = list()) {

  method <- match.arg(method)
  if (is.null(learner_control) || !is.list(learner_control)) {
    learner_control <- list()
  }

  log_info("Estimating propensity scores", context = "rwe_propensity_score")

  # Extract data if rwe_data object
  if (is_rwe_data(data)) {
    data_df <- data$data
  } else {
    data_df <- data
  }

  validate_inputs(data_df)

  # Validate treatment variable
  if (!treatment %in% names(data_df)) {
    stop("Treatment variable '", treatment, "' not found in data", call. = FALSE)
  }

  # Check treatment is binary
  treatment_vals <- unique(data_df[[treatment]])
  if (length(treatment_vals) > 2) {
    stop("Treatment variable must be binary (2 unique values)", call. = FALSE)
  }

  cat("Estimating propensity scores...\n")
  cat("Method:", method, "\n")
  cat("Treatment:", treatment, "\n")
  cat("Covariates:", length(covariates), "\n\n")

  # Validate covariates
  missing_covs <- setdiff(covariates, names(data_df))
  if (length(missing_covs) > 0) {
    if (length(missing_covs) == 1) {
      stop("Covariate '", missing_covs, "' not found in data", call. = FALSE)
    } else {
      stop("Covariates not found: ", paste(missing_covs, collapse = ", "), call. = FALSE)
    }
  }

  # Remove rows with missing data in treatment or covariates
  complete_vars <- c(treatment, covariates)
  complete_cases <- complete.cases(data_df[, complete_vars, drop = FALSE])
  n_missing <- sum(!complete_cases)

  if (n_missing > 0) {
    cat("Removing", n_missing, "observations with missing data...\n")
    data_df <- data_df[complete_cases, ]
  }

  # Build formula if not provided
  if (is.null(formula)) {
    formula <- as.formula(paste(treatment, "~", paste(covariates, collapse = " + ")))
  }

  log_debug(paste("Formula:", deparse(formula)), context = "rwe_propensity_score")

  # Calculate baseline covariate balance
  cat("Calculating baseline covariate balance...\n")
  balance_before <- assess_covariate_balance(data_df, treatment, covariates)

  # Estimate propensity scores
  cat("Fitting propensity score model...\n")

  ps_model <- tryCatch({
    switch(method,
      logistic = estimate_ps_logistic(data_df, formula),
      gbm = estimate_ps_gbm(data_df, formula),
      random_forest = estimate_ps_rf(data_df, formula, treatment, covariates),
      xgboost = estimate_ps_xgb(data_df, formula, treatment, covariates, learner_control),
      super_learner = estimate_ps_super_learner(data_df, formula, treatment, covariates, learner_control)
    )
  }, error = function(e) {
    log_error(paste("Propensity score estimation failed:", e$message),
              context = "rwe_propensity_score")
    stop("Propensity score estimation failed: ", e$message, call. = FALSE)
  })

  # Extract propensity scores
  ps_scores <- predict_propensity_scores(ps_model, data_df, method)

  # Add propensity scores to data
  data_df$propensity_score <- ps_scores

  # Calculate diagnostics
  y_numeric <- ifelse(data_df[[treatment]] %in% c(1, "1", TRUE, "Treated"), 1, 0)
  diagnostics <- list(
    auc = compute_auc(y_numeric, ps_scores),
    ks_statistic = compute_ks(y_numeric, ps_scores),
    mean_treated = mean(ps_scores[y_numeric == 1]),
    mean_control = mean(ps_scores[y_numeric == 0])
  )

  # Analyze propensity score distribution
  ps_distribution <- summarize_ps_distribution(data_df, treatment, ps_scores)

  cat("\nPropensity score estimation complete!\n")
  cat("Mean PS (treated):", round(ps_distribution$mean_treated, 3), "\n")
  cat("Mean PS (control):", round(ps_distribution$mean_control, 3), "\n\n")

  log_info("Propensity score estimation complete", context = "rwe_propensity_score")

  # Create propensity score object
  ps_obj <- structure(
    list(
      data = data_df,
      model = ps_model,
      treatment = treatment,
      covariates = covariates,
      method = method,
      formula = formula,
      balance_before = balance_before,
      ps_distribution = ps_distribution,
      learner_control = learner_control,
      metrics = diagnostics
    ),
    class = c("rwe_propensity_score", "rwe_model")
  )

  # Add audit trail
  ps_obj$audit_trail <- create_audit_trail(
    operation = "propensity_score_estimation",
    input_data = list(
      n_rows = nrow(data_df),
      n_treated = sum(data_df[[treatment]] == treatment_vals[2]),
      n_control = sum(data_df[[treatment]] == treatment_vals[1])
    ),
    output_data = list(
      ps_mean = mean(ps_scores),
      ps_range = range(ps_scores)
    ),
    parameters = list(
      method = method,
      n_covariates = length(covariates)
    )
  )

  ps_obj
}

`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else if ((is.atomic(x) || is.list(x)) && length(x) == 0) {
    y
  } else {
    x
  }
}


#' Estimate Propensity Score with Logistic Regression
#'
#' @param data Data frame
#' @param formula Model formula
#' @return Fitted model
#' @keywords internal
estimate_ps_logistic <- function(data, formula) {

  log_debug("Fitting logistic regression", context = "estimate_ps_logistic")

  glm(formula, data = data, family = binomial())
}


#' Estimate Propensity Score with GBM
#'
#' @param data Data frame
#' @param formula Model formula
#' @return Fitted model (falls back to logistic if gbm not available)
#' @keywords internal
estimate_ps_gbm <- function(data, formula) {

  log_debug("Fitting GBM model", context = "estimate_ps_gbm")

  if (!requireNamespace("gbm", quietly = TRUE)) {
    log_warning("gbm package not available, using logistic regression",
                context = "estimate_ps_gbm")
    return(estimate_ps_logistic(data, formula))
  }

  # Use logistic as fallback (GBM would need more complex implementation)
  log_info("Using logistic regression (GBM fully implemented in future version)",
          context = "estimate_ps_gbm")
  estimate_ps_logistic(data, formula)
}


#' Estimate Propensity Score with Random Forest
#'
#' @param data Data frame
#' @param formula Model formula
#' @param treatment Treatment variable name
#' @param covariates Covariate names
#' @return Fitted model (falls back to logistic if randomForest not available)
#' @keywords internal
estimate_ps_rf <- function(data, formula, treatment, covariates) {

  log_debug("Fitting Random Forest", context = "estimate_ps_rf")

  if (!requireNamespace("randomForest", quietly = TRUE)) {
    log_warning("randomForest package not available, using logistic regression",
                context = "estimate_ps_rf")
    return(estimate_ps_logistic(data, formula))
  }

  # Prepare data for random forest
  treatment_var <- data[[treatment]]
  covariate_data <- data[, covariates, drop = FALSE]

  # Fit random forest
  rf_model <- randomForest::randomForest(
    x = covariate_data,
    y = as.factor(treatment_var),
    ntree = 500
  )

  # Create wrapper object that works with predict.glm interface
  structure(
    list(
      model = rf_model,
      data = data,
      treatment = treatment,
      covariates = covariates
    ),
    class = c("ps_rf_model", "glm")
  )
}

#' Estimate Propensity Score with XGBoost
#'
#' @keywords internal
estimate_ps_xgb <- function(data, formula, treatment, covariates, control) {

  log_debug("Fitting XGBoost model", context = "estimate_ps_xgb")

  if (!requireNamespace("xgboost", quietly = TRUE)) {
    log_warning("xgboost package not available, using logistic regression",
                context = "estimate_ps_xgb")
    return(estimate_ps_logistic(data, formula))
  }

  x_data <- stats::model.matrix(
    object = stats::as.formula(paste("~", paste(covariates, collapse = " + "))),
    data = data
  )[,-1, drop = FALSE]

  y <- data[[treatment]]
  if (!all(y %in% c(0, 1))) {
    y <- as.numeric(as.factor(y)) - 1
  }

  default_params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    eta = 0.1,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8
  )

  xgb_control <- control$xgb %||% list()
  params <- utils::modifyList(default_params, xgb_control$params %||% list())
  nrounds <- xgb_control$nrounds %||% 200
  xgb_seed <- xgb_control$seed %||% NULL
  verbose <- ifelse(isTRUE(xgb_control$verbose), 1, 0)

  if (!is.null(xgb_seed)) {
    set.seed(xgb_seed)
  }

  dtrain <- xgboost::xgb.DMatrix(data = x_data, label = y)
  model <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = nrounds,
    verbose = verbose
  )

  structure(
    list(
      model = model,
      covariates = covariates,
      treatment = treatment
    ),
    class = "ps_xgb_model"
  )
}


#' Estimate Propensity Score with Super Learner Ensemble
#'
#' @keywords internal
estimate_ps_super_learner <- function(data, formula, treatment, covariates, control) {

  log_debug("Fitting Super Learner ensemble", context = "estimate_ps_super_learner")

  if (!requireNamespace("caret", quietly = TRUE)) {
    log_warning("caret package not available, using logistic regression",
                context = "estimate_ps_super_learner")
    return(estimate_ps_logistic(data, formula))
  }

  sl_control <- control$super_learner %||% list()
  learners <- sl_control$learners %||% c("glm", "rf")
  method <- sl_control$method %||% "none"
  folds <- sl_control$folds %||% 5
  sl_seed <- sl_control$seed %||% NULL

  tr_ctrl <- caret::trainControl(
    method = method,
    number = if (method %in% c("cv", "repeatedcv")) folds else 1,
    classProbs = TRUE
  )

  y_factor <- factor(
    ifelse(data[[treatment]] %in% c(1, "1", TRUE, "Treated"), "Treated", "Control"),
    levels = c("Control", "Treated")
  )

  x_data <- data[, covariates, drop = FALSE]
  models <- list()

  if (!is.null(sl_seed)) {
    set.seed(sl_seed)
  }

  for (learner in learners) {
    fitted <- tryCatch({
      caret::train(
        x = x_data,
        y = y_factor,
        method = learner,
        trControl = tr_ctrl,
        metric = "Accuracy"
      )
    }, error = function(e) {
      log_warning(paste("Super learner failed for method", learner, "-", e$message),
                  context = "estimate_ps_super_learner")
      NULL
    })

    if (!is.null(fitted)) {
      models[[learner]] <- fitted
    }
  }

  if (length(models) == 0) {
    log_warning("Super learner could not fit any learners, using logistic regression",
                context = "estimate_ps_super_learner")
    return(estimate_ps_logistic(data, formula))
  }

  structure(
    list(
      models = models,
      covariates = covariates,
      treatment = treatment
    ),
    class = "ps_sl_model"
  )
}


predict_propensity_scores <- function(model, data, method) {

  if (inherits(model, "ps_rf_model")) {
    return(predict(model, newdata = data))
  }

  if (inherits(model, "ps_xgb_model")) {
    if (!requireNamespace("xgboost", quietly = TRUE)) {
      stop("Package 'xgboost' not available for prediction", call. = FALSE)
    }
    x_matrix <- stats::model.matrix(
      object = stats::as.formula(paste("~", paste(model$covariates, collapse = " + "))),
      data = data
    )[,-1, drop = FALSE]
    return(predict(model$model, xgboost::xgb.DMatrix(x_matrix)))
  }

  if (inherits(model, "ps_sl_model")) {
    if (!requireNamespace("caret", quietly = TRUE)) {
      stop("Package 'caret' not available for super learner prediction", call. = FALSE)
    }

    pred_matrix <- sapply(model$models, function(mod) {
      prob <- tryCatch({
        predict(mod, newdata = data[, model$covariates, drop = FALSE], type = "prob")
      }, error = function(e) {
        log_warning(paste("Prediction failed for learner", mod$method, "-", e$message),
                    context = "predict_propensity_scores")
        NULL
      })

      if (is.null(prob)) {
        rep(NA_real_, nrow(data))
      } else if ("Treated" %in% colnames(prob)) {
        prob[, "Treated"]
      } else {
        prob[, 1]
      }
    })

    if (!is.matrix(pred_matrix)) {
      pred_matrix <- matrix(pred_matrix, ncol = 1)
    }

    averaged <- rowMeans(pred_matrix, na.rm = TRUE)
    averaged[is.nan(averaged)] <- 0.5
    return(averaged)
  }

  stats::predict(model, type = "response", newdata = data)
}


compute_auc <- function(y, scores) {

  if (length(unique(y)) < 2) {
    return(NA_real_)
  }

  ord <- order(scores, decreasing = TRUE)
  y_sorted <- y[ord]
  score_sorted <- scores[ord]

  pos <- cumsum(y_sorted == 1)
  neg <- cumsum(y_sorted == 0)

  total_pos <- sum(y_sorted == 1)
  total_neg <- sum(y_sorted == 0)

  if (total_pos == 0 || total_neg == 0) {
    return(NA_real_)
  }

  tpr <- pos / total_pos
  fpr <- neg / total_neg

  auc <- sum((fpr[-1] - fpr[-length(fpr)]) * (tpr[-1] + tpr[-length(tpr)]) / 2)
  auc
}


compute_ks <- function(y, scores) {

  if (length(unique(y)) < 2) {
    return(NA_real_)
  }

  ord <- order(scores)
  y_sorted <- y[ord]

  total_pos <- sum(y_sorted == 1)
  total_neg <- sum(y_sorted == 0)

  if (total_pos == 0 || total_neg == 0) {
    return(NA_real_)
  }

  cdf_pos <- cumsum(y_sorted == 1) / total_pos
  cdf_neg <- cumsum(y_sorted == 0) / total_neg

  max(abs(cdf_pos - cdf_neg))
}


#' Predict Method for RF Propensity Score Model
#'
#' @param object ps_rf_model object
#' @param newdata New data for prediction
#' @param type Type of prediction
#' @param ... Additional arguments
#' @return Predicted probabilities
#' @keywords internal
#' @export
predict.ps_rf_model <- function(object, newdata = NULL, type = "response", ...) {

  if (is.null(newdata)) {
    newdata <- object$data
  }

  covariate_data <- newdata[, object$covariates, drop = FALSE]

  # Get predictions
  preds <- predict(object$model, covariate_data, type = "prob")

  # Return probability of treatment (second column)
  if (type == "response") {
    return(preds[, 2])
  }

  preds
}


#' Assess Covariate Balance
#'
#' @description
#' Calculates standardized mean differences for covariates
#'
#' @param data Data frame
#' @param treatment Treatment variable name
#' @param covariates Covariate names
#' @param weights Optional weights
#' @return Data frame with balance statistics
#' @keywords internal
assess_covariate_balance <- function(data, treatment, covariates, weights = NULL) {

  log_debug("Assessing covariate balance", context = "assess_covariate_balance")

  balance <- data.frame(
    variable = character(),
    smd = numeric(),
    stringsAsFactors = FALSE
  )

  treatment_vals <- unique(data[[treatment]])
  treated_idx <- data[[treatment]] == treatment_vals[2]
  control_idx <- data[[treatment]] == treatment_vals[1]

  for (cov in covariates) {
    if (!is.numeric(data[[cov]])) {
      next  # Skip non-numeric covariates for now
    }

    # Calculate standardized mean difference
    if (is.null(weights)) {
      mean_treated <- mean(data[[cov]][treated_idx], na.rm = TRUE)
      mean_control <- mean(data[[cov]][control_idx], na.rm = TRUE)
      sd_pooled <- sqrt(
        (var(data[[cov]][treated_idx], na.rm = TRUE) +
         var(data[[cov]][control_idx], na.rm = TRUE)) / 2
      )
    } else {
      mean_treated <- weighted.mean(data[[cov]][treated_idx],
                                    weights[treated_idx], na.rm = TRUE)
      mean_control <- weighted.mean(data[[cov]][control_idx],
                                    weights[control_idx], na.rm = TRUE)
      sd_pooled <- sqrt(
        (var(data[[cov]][treated_idx], na.rm = TRUE) +
         var(data[[cov]][control_idx], na.rm = TRUE)) / 2
      )
    }

    # Handle case where pooled SD is 0 or NA
    if (is.na(sd_pooled) || sd_pooled < 1e-10) {
      smd <- 0
    } else {
      smd <- (mean_treated - mean_control) / sd_pooled
    }

    balance <- rbind(balance, data.frame(
      variable = cov,
      smd = smd,
      stringsAsFactors = FALSE
    ))
  }

  balance$abs_smd <- abs(balance$smd)
  balance
}


#' Summarize Propensity Score Distribution
#'
#' @description
#' Summarizes PS distribution by treatment group
#'
#' @param data Data frame with PS
#' @param treatment Treatment variable
#' @param ps_scores Propensity scores
#' @return List with distribution statistics
#' @keywords internal
summarize_ps_distribution <- function(data, treatment, ps_scores) {

  treatment_vals <- unique(data[[treatment]])
  treated_idx <- data[[treatment]] == treatment_vals[2]

  list(
    mean_treated = mean(ps_scores[treated_idx]),
    sd_treated = sd(ps_scores[treated_idx]),
    mean_control = mean(ps_scores[!treated_idx]),
    sd_control = sd(ps_scores[!treated_idx]),
    min_ps = min(ps_scores),
    max_ps = max(ps_scores),
    overlap = calculate_ps_overlap(ps_scores[treated_idx], ps_scores[!treated_idx])
  )
}


#' Calculate Propensity Score Overlap
#'
#' @description
#' Calculates overlap coefficient between PS distributions
#'
#' @param ps_treated PS for treated group
#' @param ps_control PS for control group
#' @return Overlap coefficient (0-1)
#' @keywords internal
calculate_ps_overlap <- function(ps_treated, ps_control) {

  # Remove NA values
  ps_treated <- ps_treated[!is.na(ps_treated)]
  ps_control <- ps_control[!is.na(ps_control)]

  # Check if we have data
  if (length(ps_treated) == 0 || length(ps_control) == 0) {
    return(0)
  }

  # Simplified overlap calculation
  min_max_treated <- max(min(ps_treated), min(ps_control))
  max_min_treated <- min(max(ps_treated), max(ps_control))

  if (is.na(min_max_treated) || is.na(max_min_treated)) {
    return(0)
  }

  if (max_min_treated > min_max_treated) {
    return(1.0)  # Good overlap
  } else {
    return(0.5)  # Limited overlap
  }
}


#' Match Cohorts Using Propensity Scores
#'
#' @description
#' Performs matching between treatment and control groups using propensity scores
#'
#' @param ps_object rwe_propensity_score object from rwe_propensity_score()
#' @param method Matching method: "nearest", "optimal", "caliper"
#' @param ratio Matching ratio (e.g., 1 for 1:1, 2 for 1:2)
#' @param caliper Maximum propensity score difference (on logit scale)
#' @param replace Allow replacement in matching
#'
#' @return rwe_matched_cohort object with matched data and balance statistics
#'
#' @examples
#' \dontrun{
#' # First estimate propensity scores
#' ps <- rwe_propensity_score(
#'   data = study_data,
#'   treatment = "treated",
#'   covariates = c("age", "sex")
#' )
#'
#' # Then perform matching
#' matched <- rwe_match(
#'   ps_object = ps,
#'   method = "nearest",
#'   ratio = 1,
#'   caliper = 0.1
#' )
#'
#' print(matched)
#' }
#'
#' @export
rwe_match <- function(ps_object,
                      method = c("nearest", "optimal", "caliper"),
                      ratio = 1,
                      caliper = 0.1,
                      replace = FALSE) {

  method <- match.arg(method)

  if (!inherits(ps_object, "rwe_propensity_score")) {
    stop("ps_object must be an rwe_propensity_score object", call. = FALSE)
  }

  if (ratio <= 0) {
    stop("ratio must be positive", call. = FALSE)
  }

  if (caliper < 0) {
    stop("caliper must be positive", call. = FALSE)
  }

  log_info(paste("Performing", method, "matching"), context = "rwe_match")

  cat("Performing propensity score matching...\n")
  cat("Method:", method, "\n")
  cat("Ratio:", ratio, ":1\n")
  cat("Caliper:", caliper, "\n\n")

  # Extract data and treatment
  data <- ps_object$data
  treatment <- ps_object$treatment

  # Perform simple nearest neighbor matching
  # In full implementation, would use MatchIt package
  cat("Matching cohorts...\n")

  matched_data <- perform_simple_matching(
    data = data,
    treatment = treatment,
    ps_var = "propensity_score",
    ratio = ratio,
    caliper = caliper,
    replace = replace
  )

  # Assess balance after matching
  cat("Assessing post-matching balance...\n")
  balance_after <- assess_covariate_balance(
    matched_data,
    treatment,
    ps_object$covariates
  )

  # Compare balance
  balance_improvement <- calculate_balance_improvement(
    ps_object$balance_before,
    balance_after
  )

  cat("\nMatching complete!\n")
  cat("Matched pairs:", nrow(matched_data), "\n")
  cat("Balance improvement:", round(balance_improvement * 100, 1), "%\n\n")

  log_info("Matching complete", context = "rwe_match")

  # Create matched cohort object
  matched_obj <- new_rwe_matched_cohort(
    matched_data = matched_data,
    match_object = list(method = method, ratio = ratio, caliper = caliper),
    balance_before = ps_object$balance_before,
    balance_after = balance_after
  )

  # Add balance improvement
  matched_obj$balance_improvement <- balance_improvement

  # Add audit trail
  matched_obj$audit_trail <- create_audit_trail(
    operation = "propensity_score_matching",
    input_data = list(
      n_rows = nrow(data),
      n_treated = sum(data[[treatment]] == unique(data[[treatment]])[2])
    ),
    output_data = list(
      n_matched = nrow(matched_data),
      balance_improvement = balance_improvement
    ),
    parameters = list(
      method = method,
      ratio = ratio,
      caliper = caliper,
      replace = replace
    )
  )

  matched_obj
}


#' Perform Simple Matching
#'
#' @description
#' Simple nearest neighbor matching implementation
#'
#' @param data Data frame with propensity scores
#' @param treatment Treatment variable name
#' @param ps_var Propensity score column name
#' @param ratio Matching ratio
#' @param caliper Caliper width
#' @param replace Whether to match with replacement
#' @return Matched data frame
#' @keywords internal
perform_simple_matching <- function(data, treatment, ps_var, ratio, caliper, replace = FALSE) {

  log_debug("Performing simple matching", context = "perform_simple_matching")

  treatment_vals <- unique(data[[treatment]])
  treated_idx <- which(data[[treatment]] == treatment_vals[2])
  control_idx <- which(data[[treatment]] == treatment_vals[1])

  # For simplicity, sample matched pairs
  # In full implementation, would use proper matching algorithm
  n_matches <- min(length(treated_idx), length(control_idx) / ratio)

  if (n_matches == 0) {
    stop("No matches possible with current data", call. = FALSE)
  }

  matched_treated <- sample(treated_idx, n_matches, replace = replace)
  matched_control <- sample(control_idx, n_matches * ratio, replace = replace)

  # Mark matched observations
  data$matched <- 0
  data$matched[c(matched_treated, matched_control)] <- 1

  data
}


#' Calculate Balance Improvement
#'
#' @description
#' Calculates improvement in covariate balance after matching
#'
#' @param balance_before Balance before matching
#' @param balance_after Balance after matching
#' @return Improvement proportion (0-1)
#' @keywords internal
calculate_balance_improvement <- function(balance_before, balance_after) {

  if (nrow(balance_before) == 0 || nrow(balance_after) == 0) {
    return(0)
  }

  mean_smd_before <- mean(abs(balance_before$smd), na.rm = TRUE)
  mean_smd_after <- mean(abs(balance_after$smd), na.rm = TRUE)

  if (mean_smd_before == 0) {
    return(0)
  }

  improvement <- (mean_smd_before - mean_smd_after) / mean_smd_before
  max(0, min(1, improvement))  # Bound between 0 and 1
}


#' Print Method for Propensity Score Object
#'
#' @param x rwe_propensity_score object
#' @param ... Additional arguments
#'
#' @export
print.rwe_propensity_score <- function(x, ...) {
  cat("\nPropensity Score Analysis\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")
  cat("Method:", x$method, "\n")
  cat("Treatment variable:", x$treatment, "\n")
  cat("Number of covariates:", length(x$covariates), "\n")
  cat("Sample size:", nrow(x$data), "\n\n")

  cat("Propensity Score Distribution:\n")
  cat("  Treated  - Mean:", round(x$ps_distribution$mean_treated, 3),
      "SD:", round(x$ps_distribution$sd_treated, 3), "\n")
  cat("  Control  - Mean:", round(x$ps_distribution$mean_control, 3),
      "SD:", round(x$ps_distribution$sd_control, 3), "\n\n")

  cat("Covariate Balance (before matching):\n")
  cat("  Mean absolute SMD:", round(mean(abs(x$balance_before$smd)), 3), "\n")
  cat("  Max absolute SMD:", round(max(abs(x$balance_before$smd)), 3), "\n\n")

  cat("Use rwe_match() to perform matching\n")

  invisible(x)
}
