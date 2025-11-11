#' ML-Based Data Imputation
#'
#' @description
#' Imputes missing values using machine learning algorithms with
#' proper validation and uncertainty quantification
#'
#' @param data Data with missing values (data frame or rwe_data object)
#' @param method Imputation method: "mean", "median", "mode", "mice",
#'   "random_forest", "knn", "xgboost"
#' @param vars Variables to impute (NULL for all variables with missing data)
#' @param predictors Variables to use as predictors (NULL for all complete variables)
#' @param m Number of imputed datasets for multiple imputation (MICE only)
#' @param seed Random seed for reproducibility
#' @param k Number of neighbors for kNN imputation
#' @param xgb_params Named list of XGBoost parameters (see \\link[xgboost]{xgb.train})
#' @param xgb_nrounds Number of boosting rounds for XGBoost-based imputation
#' @param k Number of neighbors for kNN imputation
#' @param xgb_params Named list of XGBoost parameters
#' @param xgb_nrounds Number of boosting rounds for XGBoost imputation
#'
#' @return rwe_imputation object with imputed data and diagnostics
#'
#' @examples
#' \dontrun{
#' # Simple mean imputation
#' imputed <- rwe_impute(
#'   data = patient_data,
#'   method = "mean"
#' )
#'
#' # Random forest imputation
#' imputed_rf <- rwe_impute(
#'   data = patient_data,
#'   method = "random_forest",
#'   vars = c("lab_value", "blood_pressure"),
#'   seed = 123
#' )
#'
#' # Multiple imputation
#' imputed_mice <- rwe_impute(
#'   data = patient_data,
#'   method = "mice",
#'   m = 5,
#'   seed = 123
#' )
#' }
#'
#' @export
rwe_impute <- function(data,
                       method = c("mean", "median", "mode", "mice",
                                 "random_forest", "knn", "xgboost"),
                       vars = NULL,
                       predictors = NULL,
                       m = 5,
                       seed = NULL,
                       k = 5,
                       xgb_params = list(),
                       xgb_nrounds = 50) {

  method <- match.arg(method)

  log_info(paste("Starting imputation with method:", method), context = "rwe_impute")

  # Extract data if rwe_data object
  if (is_rwe_data(data)) {
    data_df <- data$data
  } else {
    data_df <- data
  }

  validate_inputs(data_df)

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
    log_debug(paste("Random seed set to:", seed), context = "rwe_impute")
  }

  cat("Starting data imputation...\n")
  cat("Method:", method, "\n")
  cat("Records:", nrow(data_df), "\n\n")

  # Identify variables to impute
  if (is.null(vars)) {
    vars <- names(data_df)[sapply(data_df, anyNA)]
    cat("Auto-detected", length(vars), "variables with missing data\n")
  }

  if (length(vars) == 0) {
    log_info("No missing data found", context = "rwe_impute")
    cat("No missing data to impute!\n")

    return(structure(
      list(
        imputed_data = data_df,
        method = method,
        vars_imputed = character(0),
        n_imputed = 0,
        diagnostics = list(message = "No missing data")
      ),
      class = c("rwe_imputation", "rwe_transformed")
    ))
  }

  # Calculate missingness before imputation
  missingness_before <- calculate_missingness(data_df, vars)

  cat("Total missing values:", sum(missingness_before$n_missing), "\n\n")

  # Identify predictors
  if (is.null(predictors)) {
    # Use all complete variables as predictors
    predictors <- setdiff(names(data_df), vars)
    predictors <- predictors[!sapply(data_df[predictors], anyNA)]
  }

  cat("Using", length(predictors), "predictor variables\n\n")

  # Perform imputation based on method
  cat("Imputing missing values...\n")

  imputed_data <- tryCatch({
    switch(method,
      mean = impute_mean(data_df, vars),
      median = impute_median(data_df, vars),
      mode = impute_mode(data_df, vars),
      mice = impute_mice(data_df, vars, m, seed),
      random_forest = impute_random_forest(data_df, vars, predictors),
      knn = impute_knn(data_df, vars, predictors, k = k),
      xgboost = impute_xgboost(data_df, vars, predictors, params = xgb_params, nrounds = xgb_nrounds)
    )
  }, error = function(e) {
    log_error(paste("Imputation failed:", e$message), context = "rwe_impute")
    stop("Imputation failed: ", e$message, "\nTry a simpler method like 'mean' or 'median'.",
         call. = FALSE)
  })

  # Calculate missingness after imputation
  missingness_after <- calculate_missingness(imputed_data, vars)

  # Generate diagnostics
  diagnostics <- generate_imputation_diagnostics(
    data_df, imputed_data, vars, method
  )

  cat("\nImputation complete!\n")
  cat("Values imputed:", sum(missingness_before$n_missing) - sum(missingness_after$n_missing), "\n")

  log_info("Imputation complete", context = "rwe_impute")

  # Create imputation object
  imputation_obj <- structure(
    list(
      imputed_data = imputed_data,
      method = method,
      vars_imputed = vars,
      n_imputed = sum(missingness_before$n_missing) - sum(missingness_after$n_missing),
      m = if (method == "mice") m else 1,
      diagnostics = diagnostics,
      missingness_before = missingness_before,
      missingness_after = missingness_after
    ),
    class = c("rwe_imputation", "rwe_transformed")
  )

  # Add audit trail
  imputation_obj$audit_trail <- create_audit_trail(
    operation = "imputation",
    input_data = list(
      n_rows = nrow(data_df),
      n_missing = sum(missingness_before$n_missing)
    ),
    output_data = list(
      n_rows = nrow(imputed_data),
      n_missing = sum(missingness_after$n_missing)
    ),
    parameters = list(
      method = method,
      n_vars = length(vars),
      n_predictors = length(predictors),
      m = m,
      seed = seed
    )
  )

  imputation_obj
}


#' Calculate Missingness
#'
#' @description
#' Calculates missingness statistics for specified variables
#'
#' @param data Data frame
#' @param vars Variables to check
#' @return Data frame with missingness statistics
#' @keywords internal
calculate_missingness <- function(data, vars) {

  data.frame(
    variable = vars,
    n_missing = sapply(vars, function(v) sum(is.na(data[[v]]))),
    pct_missing = sapply(vars, function(v) sum(is.na(data[[v]])) / nrow(data)),
    stringsAsFactors = FALSE
  )
}


#' Impute Mean
#'
#' @description
#' Imputes missing values with variable means
#'
#' @param data Data frame
#' @param vars Variables to impute
#' @return Data frame with imputed values
#' @keywords internal
impute_mean <- function(data, vars) {

  log_debug("Imputing with mean", context = "impute_mean")

  imputed <- data

  for (var in vars) {
    if (is.numeric(imputed[[var]])) {
      mean_val <- mean(imputed[[var]], na.rm = TRUE)
      imputed[[var]][is.na(imputed[[var]])] <- mean_val
    } else {
      log_warning(paste("Skipping non-numeric variable:", var),
                  context = "impute_mean")
    }
  }

  imputed
}


#' Impute Median
#'
#' @description
#' Imputes missing values with variable medians
#'
#' @param data Data frame
#' @param vars Variables to impute
#' @return Data frame with imputed values
#' @keywords internal
impute_median <- function(data, vars) {

  log_debug("Imputing with median", context = "impute_median")

  imputed <- data

  for (var in vars) {
    if (is.numeric(imputed[[var]])) {
      median_val <- median(imputed[[var]], na.rm = TRUE)
      imputed[[var]][is.na(imputed[[var]])] <- median_val
    } else {
      log_warning(paste("Skipping non-numeric variable:", var),
                  context = "impute_median")
    }
  }

  imputed
}


#' Impute Mode
#'
#' @description
#' Imputes missing values with variable modes
#'
#' @param data Data frame
#' @param vars Variables to impute
#' @return Data frame with imputed values
#' @keywords internal
impute_mode <- function(data, vars) {

  log_debug("Imputing with mode", context = "impute_mode")

  imputed <- data

  for (var in vars) {
    # Calculate mode
    tbl <- table(imputed[[var]])
    mode_val <- names(tbl)[which.max(tbl)]

    # Convert to appropriate type
    if (is.numeric(imputed[[var]])) {
      mode_val <- as.numeric(mode_val)
    }

    imputed[[var]][is.na(imputed[[var]])] <- mode_val
  }

  imputed
}


#' Impute with MICE
#'
#' @description
#' Multiple Imputation by Chained Equations
#'
#' @param data Data frame
#' @param vars Variables to impute
#' @param m Number of imputations
#' @param seed Random seed
#' @return Data frame with imputed values
#' @keywords internal
impute_mice <- function(data, vars, m, seed) {

  log_debug("Imputing with MICE", context = "impute_mice")

  # Check if mice package is available
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("The 'mice' package is required for this method.\n",
         "Install with: install.packages('mice')",
         call. = FALSE)
  }

  # Perform MICE
  mice_obj <- mice::mice(data, m = m, printFlag = FALSE, seed = seed)

  # Use the first imputed dataset
  # In full implementation, could pool across multiple
  imputed <- mice::complete(mice_obj, 1)

  imputed
}


#' Impute with Random Forest
#'
#' @description
#' Imputes using random forest algorithm
#'
#' @param data Data frame
#' @param vars Variables to impute
#' @param predictors Predictor variables
#' @return Data frame with imputed values
#' @keywords internal
impute_random_forest <- function(data, vars, predictors) {

  log_debug("Imputing with Random Forest", context = "impute_random_forest")

  # Check if randomForest package is available
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    log_warning("randomForest package not available, falling back to mean imputation",
                context = "impute_random_forest")
    return(impute_mean(data, vars))
  }

  imputed <- data

  for (var in vars) {
    if (!is.numeric(imputed[[var]])) {
      log_warning(paste("Skipping non-numeric variable:", var),
                  context = "impute_random_forest")
      next
    }

    # Prepare training data (complete cases)
    complete_idx <- !is.na(imputed[[var]])
    train_data <- imputed[complete_idx, c(var, predictors), drop = FALSE]

    # Prepare prediction data (missing cases)
    predict_data <- imputed[!complete_idx, predictors, drop = FALSE]

    if (nrow(predict_data) == 0) next

    # Remove predictors with missing values in prediction data
    complete_predictors <- predictors[!sapply(predict_data, anyNA)]
    if (length(complete_predictors) == 0) {
      log_warning(paste("No complete predictors for variable:", var),
                  context = "impute_random_forest")
      next
    }

    train_data <- train_data[, c(var, complete_predictors), drop = FALSE]
    predict_data <- predict_data[, complete_predictors, drop = FALSE]

    # Train random forest model
    tryCatch({
      rf_model <- randomForest::randomForest(
        x = train_data[, complete_predictors, drop = FALSE],
        y = train_data[[var]],
        ntree = 100
      )

      # Predict missing values
      predictions <- predict(rf_model, predict_data)

      # Fill in predictions
      imputed[[var]][!complete_idx] <- predictions
    }, error = function(e) {
      log_warning(paste("RF imputation failed for", var, "- using mean"),
                  context = "impute_random_forest")
      mean_val <- mean(imputed[[var]], na.rm = TRUE)
      imputed[[var]][!complete_idx] <<- mean_val
    })
  }

  imputed
}


#' Impute with K-Nearest Neighbors
#'
#' @description
#' Imputes using KNN algorithm
#'
#' @param data Data frame
#' @param vars Variables to impute
#' @param predictors Predictor variables
#' @return Data frame with imputed values
#' @keywords internal
impute_knn <- function(data, vars, predictors, k = 5, verbose = TRUE) {

  if (!requireNamespace("FNN", quietly = TRUE)) {
    stop("Package 'FNN' is required for kNN imputation. Install it or choose another method.",
         call. = FALSE)
  }

  if (k < 1) {
    stop("k must be >= 1 for kNN imputation", call. = FALSE)
  }

  log_debug("Imputing with KNN", context = "impute_knn")

  imputed <- data

  for (var in vars) {
    if (!is.numeric(imputed[[var]])) {
      log_warning(paste("Variable", var, "is non-numeric. Falling back to mode."),
                  context = "impute_knn")
      imputed <- impute_mode(imputed, var)
      next
    }

    candidate_predictors <- predictors
    if (length(candidate_predictors) == 0) {
      log_warning(paste("No predictors provided for", var, "- using mean."),
                  context = "impute_knn")
      imputed <- impute_mean(imputed, var)
      next
    }

    predictor_numerics <- candidate_predictors[sapply(imputed[candidate_predictors], is.numeric)]
    if (length(predictor_numerics) == 0) {
      log_warning(paste("No numeric predictors available for", var, "- using mean."),
                  context = "impute_knn")
      imputed <- impute_mean(imputed, var)
      next
    }

    complete_idx <- complete.cases(imputed[, c(var, predictor_numerics), drop = FALSE])
    missing_idx <- which(!complete.cases(imputed[, var, drop = FALSE]))

    if (!any(missing_idx)) next

    train_x <- imputed[complete_idx, predictor_numerics, drop = FALSE]
    train_y <- imputed[complete_idx, var]
    test_x <- imputed[missing_idx, predictor_numerics, drop = FALSE]

    # Remove predictors with missing values in test set
    no_missing_predictors <- predictor_numerics[!sapply(test_x, anyNA)]
    if (length(no_missing_predictors) == 0) {
      log_warning(paste("All predictors contain missing values for", var, "- using mean."),
                  context = "impute_knn")
      imputed <- impute_mean(imputed, var)
      next
    }

    train_x <- train_x[, no_missing_predictors, drop = FALSE]
    test_x <- test_x[, no_missing_predictors, drop = FALSE]

    k_use <- min(k, nrow(train_x))
    if (k_use < 1) {
      log_warning(paste("Not enough complete cases for", var, "- using mean."),
                  context = "impute_knn")
      imputed <- impute_mean(imputed, var)
      next
    }

    nn_result <- FNN::knn.reg(
      train = as.matrix(train_x),
      test = as.matrix(test_x),
      y = train_y,
      k = k_use
    )

    imputed[[var]][missing_idx] <- nn_result$pred
  }

  imputed
}


#' Impute with XGBoost Models
#'
#' @keywords internal
impute_xgboost <- function(data, vars, predictors, params = list(), nrounds = 50, verbose = TRUE) {

  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("Package 'xgboost' is required for XGBoost imputation. Install it or choose another method.",
         call. = FALSE)
  }

  default_params <- list(
    objective = "reg:squarederror",
    eta = 0.1,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8
  )

  params <- utils::modifyList(default_params, params)

  imputed <- data

  for (var in vars) {
    if (!is.numeric(imputed[[var]])) {
      log_warning(paste("Variable", var, "is non-numeric. Falling back to mode."),
                  context = "impute_xgboost")
      imputed <- impute_mode(imputed, var)
      next
    }

    candidate_predictors <- predictors
    if (length(candidate_predictors) == 0) {
      log_warning(paste("No predictors provided for", var, "- using mean."),
                  context = "impute_xgboost")
      imputed <- impute_mean(imputed, var)
      next
    }

    complete_idx <- complete.cases(imputed[, c(var, candidate_predictors), drop = FALSE])
    missing_idx <- which(is.na(imputed[[var]]))

    if (!any(missing_idx)) next

    train_data <- imputed[complete_idx, candidate_predictors, drop = FALSE]
    train_y <- imputed[complete_idx, var]
    predict_data <- imputed[missing_idx, candidate_predictors, drop = FALSE]

    if (nrow(train_data) < 5) {
      log_warning(paste("Not enough complete cases for", var, "- using mean."),
                  context = "impute_xgboost")
      imputed <- impute_mean(imputed, var)
      next
    }

    train_matrix <- stats::model.matrix(~ . - 1, data = train_data)
    predict_matrix <- stats::model.matrix(~ . - 1, data = predict_data)

    dtrain <- xgboost::xgb.DMatrix(data = train_matrix, label = train_y)

    model <- xgboost::xgboost(
      data = dtrain,
      params = params,
      nrounds = nrounds,
      verbose = 0
    )

    preds <- predict(model, predict_matrix)
    imputed[[var]][missing_idx] <- preds
  }

  imputed
}


#' Generate Imputation Diagnostics
#'
#' @description
#' Generates diagnostic information about imputation
#'
#' @param original Original data
#' @param imputed Imputed data
#' @param vars Variables that were imputed
#' @param method Imputation method
#' @return List with diagnostic information
#' @keywords internal
generate_imputation_diagnostics <- function(original, imputed, vars, method) {

  diagnostics <- list(
    method = method,
    variables = vars
  )

  # For each imputed variable, calculate statistics
  for (var in vars) {
    if (is.numeric(original[[var]])) {
      diagnostics[[var]] <- list(
        original_mean = mean(original[[var]], na.rm = TRUE),
        original_sd = sd(original[[var]], na.rm = TRUE),
        imputed_mean = mean(imputed[[var]], na.rm = TRUE),
        imputed_sd = sd(imputed[[var]], na.rm = TRUE),
        n_imputed = sum(is.na(original[[var]]))
      )
    }
  }

  diagnostics
}


#' Print Method for Imputation Object
#'
#' @param x rwe_imputation object
#' @param ... Additional arguments
#'
#' @export
print.rwe_imputation <- function(x, ...) {
  cat("\n<rwe_imputation>\n\n")
  cat("Method:", x$method, "\n")
  cat("Variables imputed:", length(x$vars_imputed), "\n")
  cat("Values imputed:", x$n_imputed, "\n\n")

  if (x$method == "mice") {
    cat("Multiple imputations (m):", x$m, "\n\n")
  }

  cat("Missingness before:\n")
  print(x$missingness_before)

  cat("\nMissingness after:\n")
  print(x$missingness_after)

  invisible(x)
}
