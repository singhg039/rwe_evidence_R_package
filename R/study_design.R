#' Study Design and External Control Arm Functions
#'
#' @description
#' Functions for designing Real-World Evidence studies, including external
#' control arm generation, matching criteria, and study design templates.
#'
#' @name study_design
NULL

#' Create External Control Arm from Historical Data
#'
#' @description
#' Generates an external control arm by selecting and matching patients from
#' historical or external datasets. This function implements several matching
#' strategies to ensure the external controls are comparable to the treatment arm.
#'
#' @param external_data Data frame or rwe_data object containing potential control patients
#' @param treatment_data Data frame or rwe_data object containing treatment arm patients
#' @param matching_vars Character vector of variables to use for matching
#' @param method Matching method: "propensity", "exact", "nearest", "optimal", "caliper"
#' @param ratio Numeric. Number of controls per treated patient (default: 1)
#' @param caliper Numeric. Maximum allowable difference for caliper matching (default: 0.2)
#' @param replace Logical. Whether to match with replacement (default: FALSE)
#' @param inclusion_criteria List of inclusion criteria (variable = c(min, max) or values)
#' @param exclusion_criteria List of exclusion criteria
#' @param time_window Numeric vector c(start, end) for temporal restrictions (days)
#' @param seed Integer. Random seed for reproducibility
#'
#' @return An object of class \code{rwe_external_control} containing:
#' \item{matched_controls}{Data frame of matched control patients}
#' \item{matching_summary}{Summary statistics of matching quality}
#' \item{balance_assessment}{Covariate balance before and after matching}
#' \item{inclusion_summary}{Number of patients passing each criterion}
#' \item{exclusion_summary}{Number of patients excluded by each criterion}
#' \item{method}{Matching method used}
#' \item{n_treatment}{Number of treatment patients}
#' \item{n_controls}{Number of matched controls}
#' \item{matching_ratio}{Ratio of controls to treatment}
#' \item{audit_trail}{Audit trail of the selection process}
#'
#' @examples
#' \dontrun{
#' # Generate external control arm using propensity score matching
#' external_controls <- rwe_external_control(
#'   external_data = historical_data,
#'   treatment_data = current_study,
#'   matching_vars = c("age", "sex", "disease_severity"),
#'   method = "propensity",
#'   ratio = 2,
#'   inclusion_criteria = list(
#'     age = c(18, 75),
#'     disease_stage = c("I", "II", "III")
#'   ),
#'   exclusion_criteria = list(
#'     prior_treatment = TRUE
#'   )
#' )
#' }
#'
#' @export
rwe_external_control <- function(external_data,
                                  treatment_data,
                                  matching_vars,
                                  method = c("propensity", "exact", "nearest", "optimal", "caliper"),
                                  ratio = 1,
                                  caliper = 0.2,
                                  replace = FALSE,
                                  inclusion_criteria = NULL,
                                  exclusion_criteria = NULL,
                                  time_window = NULL,
                                  seed = NULL) {

  log_info("Starting external control arm generation", context = "rwe_external_control")

  method <- match.arg(method)

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Extract data frames
  if (is_rwe_data(external_data)) {
    external_df <- external_data$data
  } else {
    external_df <- external_data
  }

  if (is_rwe_data(treatment_data)) {
    treatment_df <- treatment_data$data
  } else {
    treatment_df <- treatment_data
  }

  validate_inputs(external_df)
  validate_inputs(treatment_df)

  # Store original counts
  n_external_original <- nrow(external_df)
  n_treatment_original <- nrow(treatment_df)

  cat("External Control Arm Generation\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")
  cat("Initial Counts:\n")
  cat("  External pool:", n_external_original, "patients\n")
  cat("  Treatment arm:", n_treatment_original, "patients\n\n")

  # Apply inclusion criteria
  inclusion_summary <- list()
  if (!is.null(inclusion_criteria)) {
    cat("Applying inclusion criteria...\n")
    for (var in names(inclusion_criteria)) {
      if (!var %in% names(external_df)) {
        warning("Inclusion variable '", var, "' not found in external data")
        next
      }

      criteria <- inclusion_criteria[[var]]
      n_before <- nrow(external_df)

      if (is.numeric(criteria) && length(criteria) == 2) {
        # Range criteria
        external_df <- external_df[external_df[[var]] >= criteria[1] &
                                    external_df[[var]] <= criteria[2], ]
        cat("  ", var, "in [", criteria[1], ",", criteria[2], "]:",
            n_before, "->", nrow(external_df), "\n")
      } else {
        # Categorical criteria
        external_df <- external_df[external_df[[var]] %in% criteria, ]
        cat("  ", var, "in", paste(criteria, collapse = ", "), ":",
            n_before, "->", nrow(external_df), "\n")
      }

      inclusion_summary[[var]] <- list(
        n_before = n_before,
        n_after = nrow(external_df),
        n_excluded = n_before - nrow(external_df)
      )
    }
    cat("\n")
  }

  # Apply exclusion criteria
  exclusion_summary <- list()
  if (!is.null(exclusion_criteria)) {
    cat("Applying exclusion criteria...\n")
    for (var in names(exclusion_criteria)) {
      if (!var %in% names(external_df)) {
        warning("Exclusion variable '", var, "' not found in external data")
        next
      }

      criteria <- exclusion_criteria[[var]]
      n_before <- nrow(external_df)

      if (is.logical(criteria) && criteria == TRUE) {
        # Exclude if TRUE
        external_df <- external_df[!external_df[[var]] %in% c(TRUE, 1), ]
      } else {
        # Exclude specific values
        external_df <- external_df[!external_df[[var]] %in% criteria, ]
      }

      cat("  Excluding", var, ":", n_before, "->", nrow(external_df), "\n")

      exclusion_summary[[var]] <- list(
        n_before = n_before,
        n_after = nrow(external_df),
        n_excluded = n_before - nrow(external_df)
      )
    }
    cat("\n")
  }

  # Apply time window restriction
  if (!is.null(time_window)) {
    if ("index_date" %in% names(external_df)) {
      n_before <- nrow(external_df)
      reference_date <- Sys.Date()
      external_df$days_since_index <- as.numeric(difftime(reference_date,
                                                           external_df$index_date,
                                                           units = "days"))
      external_df <- external_df[external_df$days_since_index >= time_window[1] &
                                  external_df$days_since_index <= time_window[2], ]
      cat("Time window restriction:", n_before, "->", nrow(external_df), "\n\n")
    } else {
      warning("Cannot apply time window: 'index_date' not found in data")
    }
  }

  n_eligible_controls <- nrow(external_df)

  if (n_eligible_controls == 0) {
    stop("No eligible controls remaining after applying criteria", call. = FALSE)
  }

  cat("Eligible controls after criteria:", n_eligible_controls, "\n")
  cat("Matching method:", method, "\n")
  cat("Target ratio:", ratio, "controls per treatment patient\n\n")

  # Perform matching based on method
  cat("Performing matching...\n")

  if (method == "propensity") {
    # Propensity score matching
    matched_result <- match_propensity_score(
      external_df = external_df,
      treatment_df = treatment_df,
      matching_vars = matching_vars,
      ratio = ratio,
      caliper = caliper,
      replace = replace
    )
  } else if (method == "exact") {
    # Exact matching
    matched_result <- match_exact(
      external_df = external_df,
      treatment_df = treatment_df,
      matching_vars = matching_vars,
      ratio = ratio
    )
  } else if (method == "nearest") {
    # Nearest neighbor matching
    matched_result <- match_nearest(
      external_df = external_df,
      treatment_df = treatment_df,
      matching_vars = matching_vars,
      ratio = ratio,
      replace = replace
    )
  } else {
    stop("Matching method '", method, "' not yet implemented", call. = FALSE)
  }

  matched_controls <- matched_result$matched_controls
  matching_summary <- matched_result$summary

  cat("\nMatching complete!\n")
  cat("  Matched controls:", nrow(matched_controls), "\n")
  cat("  Actual ratio:", round(nrow(matched_controls) / n_treatment_original, 2), "\n\n")

  # Assess covariate balance
  cat("Assessing covariate balance...\n")
  balance_before <- assess_balance(external_df, treatment_df, matching_vars)
  balance_after <- assess_balance(matched_controls, treatment_df, matching_vars)

  balance_assessment <- list(
    before = balance_before,
    after = balance_after,
    improvement = calculate_matching_improvement(balance_before, balance_after)
  )

  log_info("External control arm generation complete", context = "rwe_external_control")

  # Create audit trail
  audit <- create_audit_trail(
    operation = "external_control_generation",
    input_data = list(
      n_external = n_external_original,
      n_treatment = n_treatment_original
    ),
    output_data = list(
      n_matched = nrow(matched_controls),
      matching_ratio = nrow(matched_controls) / n_treatment_original
    ),
    parameters = list(
      method = method,
      matching_vars = matching_vars,
      ratio = ratio,
      caliper = caliper,
      replace = replace,
      n_eligible_controls = n_eligible_controls,
      inclusion_criteria = inclusion_criteria,
      exclusion_criteria = exclusion_criteria,
      time_window = time_window,
      seed = seed
    )
  )

  # Create result object
  result <- structure(
    list(
      matched_controls = matched_controls,
      matching_summary = matching_summary,
      balance_assessment = balance_assessment,
      inclusion_summary = inclusion_summary,
      exclusion_summary = exclusion_summary,
      method = method,
      n_treatment = n_treatment_original,
      n_controls = nrow(matched_controls),
      matching_ratio = nrow(matched_controls) / n_treatment_original,
      matching_vars = matching_vars,
      n_external_original = n_external_original,
      n_eligible = n_eligible_controls,
      audit_trail = audit
    ),
    class = c("rwe_external_control", "rwe_analysis")
  )

  return(result)
}


# Internal matching functions ---------------------------------------------

#' Propensity Score Matching for External Controls
#' @keywords internal
match_propensity_score <- function(external_df, treatment_df, matching_vars,
                                    ratio, caliper, replace) {

  # Add treatment indicator
  external_df$..treatment.. <- 0
  treatment_df$..treatment.. <- 1

  # Combine datasets
  combined <- rbind(external_df, treatment_df)

  # Estimate propensity scores
  ps_formula <- as.formula(paste("..treatment.. ~", paste(matching_vars, collapse = " + ")))
  ps_model <- glm(ps_formula, data = combined, family = binomial())
  combined$ps <- predict(ps_model, type = "response")

  # Extract PS
  external_ps <- combined$ps[combined$..treatment.. == 0]
  treatment_ps <- combined$ps[combined$..treatment.. == 1]

  external_df$ps <- external_ps
  treatment_df$ps <- treatment_ps

  # Match using nearest neighbor on PS
  matched_indices <- c()

  for (i in seq_len(nrow(treatment_df))) {
    trt_ps <- treatment_df$ps[i]

    # Calculate distances
    distances <- abs(external_df$ps - trt_ps)

    # Apply caliper
    eligible <- which(distances <= caliper)

    if (length(eligible) == 0) next

    # Select closest matches
    closest <- eligible[order(distances[eligible])][1:min(ratio, length(eligible))]

    matched_indices <- c(matched_indices, closest)

    # Remove from pool if without replacement
    if (!replace) {
      external_df <- external_df[-closest, ]
    }
  }

  matched_controls <- external_df[matched_indices, ]
  matched_controls$ps <- NULL  # Remove temporary PS column

  summary <- list(
    n_matched = length(unique(matched_indices)),
    n_unmatched_treatment = sum(sapply(seq_len(nrow(treatment_df)), function(i) {
      min(abs(external_ps - treatment_df$ps[i])) > caliper
    })),
    mean_ps_distance = mean(abs(matched_controls$ps - rep(treatment_df$ps, each = ratio)))
  )

  return(list(matched_controls = matched_controls, summary = summary))
}


#' Exact Matching for External Controls
#' @keywords internal
match_exact <- function(external_df, treatment_df, matching_vars, ratio) {

  matched_controls <- data.frame()

  for (i in seq_len(nrow(treatment_df))) {
    # Get values for this treatment patient
    trt_values <- treatment_df[i, matching_vars, drop = FALSE]

    # Find exact matches in external pool
    matches <- rep(TRUE, nrow(external_df))
    for (var in matching_vars) {
      matches <- matches & (external_df[[var]] == trt_values[[var]])
    }

    match_indices <- which(matches)

    if (length(match_indices) > 0) {
      # Sample up to ratio matches
      selected <- match_indices[1:min(ratio, length(match_indices))]
      matched_controls <- rbind(matched_controls, external_df[selected, ])
      external_df <- external_df[-selected, ]
    }
  }

  summary <- list(
    n_matched = nrow(matched_controls),
    n_unmatched_treatment = sum(sapply(seq_len(nrow(treatment_df)), function(i) {
      trt_values <- treatment_df[i, matching_vars, drop = FALSE]
      matches <- rep(TRUE, nrow(external_df))
      for (var in matching_vars) {
        matches <- matches & (external_df[[var]] == trt_values[[var]])
      }
      sum(matches) == 0
    }))
  )

  return(list(matched_controls = matched_controls, summary = summary))
}


#' Nearest Neighbor Matching for External Controls
#' @keywords internal
match_nearest <- function(external_df, treatment_df, matching_vars, ratio, replace) {

  # Keep original for final selection
  original_external <- external_df

  # Standardize numeric variables
  numeric_vars <- matching_vars[sapply(matching_vars, function(v) is.numeric(external_df[[v]]))]

  for (var in numeric_vars) {
    external_df[[var]] <- scale(external_df[[var]])
    treatment_df[[var]] <- scale(treatment_df[[var]])
  }

  matched_indices <- c()
  available_pool <- external_df
  available_indices <- seq_len(nrow(external_df))

  for (i in seq_len(nrow(treatment_df))) {
    if (nrow(available_pool) == 0) break

    # Calculate Euclidean distance for numeric variables
    distances <- rep(0, nrow(available_pool))

    for (var in matching_vars) {
      if (is.numeric(treatment_df[[var]])) {
        distances <- distances + (available_pool[[var]] - treatment_df[[var]][i])^2
      } else {
        # Exact match for categorical
        distances <- distances + (available_pool[[var]] != treatment_df[[var]][i]) * 1000
      }
    }

    distances <- sqrt(distances)

    # Select nearest neighbors from available pool
    closest_in_pool <- order(distances)[1:min(ratio, nrow(available_pool))]
    closest_original_indices <- available_indices[closest_in_pool]
    matched_indices <- c(matched_indices, closest_original_indices)

    if (!replace) {
      available_pool <- available_pool[-closest_in_pool, ]
      available_indices <- available_indices[-closest_in_pool]
    }
  }

  matched_controls <- original_external[matched_indices, ]

  summary <- list(
    n_matched = length(unique(matched_indices)),
    mean_distance = NA  # Would need to recalculate with original data
  )

  return(list(matched_controls = matched_controls, summary = summary))
}


#' Assess Covariate Balance Between Two Groups
#' @keywords internal
assess_balance <- function(group1, group2, vars) {

  balance <- list()

  for (var in vars) {
    if (!var %in% names(group1) || !var %in% names(group2)) next

    if (is.numeric(group1[[var]])) {
      # Standardized mean difference for numeric
      mean1 <- mean(group1[[var]], na.rm = TRUE)
      mean2 <- mean(group2[[var]], na.rm = TRUE)
      sd1 <- sd(group1[[var]], na.rm = TRUE)
      sd2 <- sd(group2[[var]], na.rm = TRUE)
      pooled_sd <- sqrt((sd1^2 + sd2^2) / 2)

      smd <- if (pooled_sd > 1e-10) {
        (mean1 - mean2) / pooled_sd
      } else {
        0
      }

      balance[[var]] <- list(
        type = "numeric",
        mean_group1 = mean1,
        mean_group2 = mean2,
        smd = smd
      )
    } else {
      # Proportions for categorical
      prop1 <- prop.table(table(group1[[var]]))
      prop2 <- prop.table(table(group2[[var]]))

      balance[[var]] <- list(
        type = "categorical",
        prop_group1 = prop1,
        prop_group2 = prop2
      )
    }
  }

  return(balance)
}


#' Calculate Matching Balance Improvement
#' @keywords internal
calculate_matching_improvement <- function(balance_before, balance_after) {

  numeric_vars <- names(balance_before)[sapply(balance_before, function(x) x$type == "numeric")]

  if (length(numeric_vars) == 0) return(NA)

  smd_before <- sapply(numeric_vars, function(v) abs(balance_before[[v]]$smd))
  smd_after <- sapply(numeric_vars, function(v) abs(balance_after[[v]]$smd))

  improvement <- mean((smd_before - smd_after) / smd_before * 100, na.rm = TRUE)

  return(improvement)
}


#' Print Method for External Control Objects
#'
#' @param x An object of class \code{rwe_external_control}
#' @param ... Additional arguments (not used)
#'
#' @export
print.rwe_external_control <- function(x, ...) {
  cat("External Control Arm\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  cat("Matching Summary:\n")
  cat("  Method:", x$method, "\n")
  cat("  Treatment patients:", x$n_treatment, "\n")
  cat("  Matched controls:", x$n_controls, "\n")
  cat("  Matching ratio:", round(x$matching_ratio, 2), "\n")
  cat("  Original external pool:", x$n_external_original, "\n")
  cat("  Eligible after criteria:", x$n_eligible, "\n\n")

  if (!is.null(x$balance_assessment$improvement)) {
    cat("Balance Improvement:", round(x$balance_assessment$improvement, 1), "%\n\n")
  }

  cat("Use summary() for detailed balance assessment\n")
  cat("Access matched controls via $matched_controls\n")

  invisible(x)
}


#' Summary Method for External Control Objects
#'
#' @param object An object of class \code{rwe_external_control}
#' @param ... Additional arguments (not used)
#'
#' @export
summary.rwe_external_control <- function(object, ...) {
  cat("External Control Arm - Detailed Summary\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  cat("Matching Information:\n")
  cat("  Method:", object$method, "\n")
  cat("  Matching variables:", paste(object$matching_vars, collapse = ", "), "\n")
  cat("  Treatment patients:", object$n_treatment, "\n")
  cat("  Matched controls:", object$n_controls, "\n")
  cat("  Matching ratio:", round(object$matching_ratio, 2), "\n\n")

  cat("Selection Flow:\n")
  cat("  Original external pool:", object$n_external_original, "\n")

  if (length(object$inclusion_summary) > 0) {
    total_excluded_inclusion <- sum(sapply(object$inclusion_summary, function(x) x$n_excluded))
    cat("  After inclusion criteria:", object$n_external_original - total_excluded_inclusion, "\n")
  }

  if (length(object$exclusion_summary) > 0) {
    total_excluded_exclusion <- sum(sapply(object$exclusion_summary, function(x) x$n_excluded))
    cat("  After exclusion criteria:", object$n_eligible, "\n")
  }

  cat("  Final matched controls:", object$n_controls, "\n\n")

  cat("Covariate Balance:\n")
  cat("  Balance improvement:", round(object$balance_assessment$improvement, 1), "%\n\n")

  cat("Standardized Mean Differences (SMD):\n")
  for (var in names(object$balance_assessment$before)) {
    if (object$balance_assessment$before[[var]]$type == "numeric") {
      smd_before <- object$balance_assessment$before[[var]]$smd
      smd_after <- object$balance_assessment$after[[var]]$smd
      cat(sprintf("  %-20s Before: %6.3f  After: %6.3f\n",
                  var, smd_before, smd_after))
    }
  }

  invisible(object)
}


#' Create Study Design Template
#'
#' @description
#' Creates a structured study design template for Real-World Evidence studies,
#' including cohort definitions, inclusion/exclusion criteria, endpoints,
#' and analysis plans. This function helps standardize study design and ensures
#' regulatory compliance.
#'
#' @param study_type Type of study: "cohort", "case_control", "external_control", "registry"
#' @param population_definition List defining target population characteristics
#' @param treatment_definition List defining treatment exposure
#' @param primary_endpoint List defining primary endpoint (type, variable, timepoint)
#' @param secondary_endpoints List of secondary endpoints
#' @param inclusion_criteria List of inclusion criteria
#' @param exclusion_criteria List of exclusion criteria
#' @param follow_up_duration Numeric. Follow-up duration in days
#' @param index_date_definition Character. How index date is defined
#' @param washout_period Numeric. Washout period in days (default: 0)
#' @param propensity_score_vars Character vector of variables for PS estimation
#' @param sensitivity_analyses List of planned sensitivity analyses
#' @param subgroup_analyses List of planned subgroup analyses
#' @param sample_size_target Integer. Target sample size
#' @param study_title Character. Study title
#' @param protocol_version Character. Protocol version (default: "1.0")
#'
#' @return An object of class \code{rwe_study_design} containing:
#' \item{study_info}{Study identification and metadata}
#' \item{population}{Population definition and criteria}
#' \item{treatment}{Treatment definition}
#' \item{endpoints}{Primary and secondary endpoints}
#' \item{analysis_plan}{Statistical analysis plan}
#' \item{design_summary}{Summary of key design elements}
#' \item{audit_trail}{Audit trail of design creation}
#'
#' @examples
#' \dontrun{
#' # Create study design for external control study
#' study_design <- rwe_design_study(
#'   study_type = "external_control",
#'   study_title = "Real-World Effectiveness of Drug X",
#'   population_definition = list(
#'     disease = "Advanced Cancer",
#'     age_range = c(18, 85),
#'     min_follow_up = 90
#'   ),
#'   treatment_definition = list(
#'     drug = "Drug X",
#'     min_exposure_days = 30,
#'     route = "oral"
#'   ),
#'   primary_endpoint = list(
#'     type = "time_to_event",
#'     variable = "overall_survival",
#'     timepoint = 365
#'   ),
#'   inclusion_criteria = list(
#'     confirmed_diagnosis = TRUE,
#'     adequate_baseline_data = TRUE
#'   ),
#'   exclusion_criteria = list(
#'     prior_treatment = TRUE,
#'     clinical_trial_participation = TRUE
#'   ),
#'   follow_up_duration = 730,
#'   propensity_score_vars = c("age", "sex", "disease_stage", "comorbidities")
#' )
#' }
#'
#' @export
rwe_design_study <- function(study_type = c("cohort", "case_control", "external_control", "registry"),
                              population_definition,
                              treatment_definition = NULL,
                              primary_endpoint,
                              secondary_endpoints = NULL,
                              inclusion_criteria = NULL,
                              exclusion_criteria = NULL,
                              follow_up_duration,
                              index_date_definition,
                              washout_period = 0,
                              propensity_score_vars = NULL,
                              sensitivity_analyses = NULL,
                              subgroup_analyses = NULL,
                              sample_size_target = NULL,
                              study_title,
                              protocol_version = "1.0") {

  log_info("Creating study design template", context = "rwe_design_study")

  study_type <- match.arg(study_type)

  cat("Real-World Evidence Study Design\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")
  cat("Study Title:", study_title, "\n")
  cat("Study Type:", study_type, "\n")
  cat("Protocol Version:", protocol_version, "\n\n")

  # Validate required parameters
  if (missing(population_definition)) {
    stop("population_definition is required", call. = FALSE)
  }

  if (missing(primary_endpoint)) {
    stop("primary_endpoint is required", call. = FALSE)
  }

  if (missing(follow_up_duration)) {
    stop("follow_up_duration is required", call. = FALSE)
  }

  if (missing(index_date_definition)) {
    stop("index_date_definition is required", call. = FALSE)
  }

  # Build study information
  study_info <- list(
    title = study_title,
    type = study_type,
    protocol_version = protocol_version,
    creation_date = Sys.Date(),
    follow_up_duration = follow_up_duration,
    index_date_definition = index_date_definition,
    washout_period = washout_period,
    sample_size_target = sample_size_target
  )

  # Build population definition
  population <- list(
    definition = population_definition,
    inclusion_criteria = inclusion_criteria,
    exclusion_criteria = exclusion_criteria,
    n_inclusion_criteria = length(inclusion_criteria),
    n_exclusion_criteria = length(exclusion_criteria)
  )

  cat("Population Definition:\n")
  for (item in names(population_definition)) {
    cat("  ", item, ":", population_definition[[item]], "\n")
  }
  cat("\n")

  if (!is.null(inclusion_criteria)) {
    cat("Inclusion Criteria:", length(inclusion_criteria), "criteria defined\n")
  }

  if (!is.null(exclusion_criteria)) {
    cat("Exclusion Criteria:", length(exclusion_criteria), "criteria defined\n")
  }

  cat("\n")

  # Build treatment definition
  if (!is.null(treatment_definition)) {
    cat("Treatment Definition:\n")
    for (item in names(treatment_definition)) {
      cat("  ", item, ":", treatment_definition[[item]], "\n")
    }
    cat("\n")
  }

  # Build endpoints
  cat("Primary Endpoint:\n")
  cat("  Type:", primary_endpoint$type, "\n")
  cat("  Variable:", primary_endpoint$variable, "\n")
  if (!is.null(primary_endpoint$timepoint)) {
    cat("  Timepoint:", primary_endpoint$timepoint, "days\n")
  }
  cat("\n")

  endpoints <- list(
    primary = primary_endpoint,
    secondary = secondary_endpoints,
    n_secondary = length(secondary_endpoints)
  )

  if (!is.null(secondary_endpoints) && length(secondary_endpoints) > 0) {
    cat("Secondary Endpoints:", length(secondary_endpoints), "endpoints defined\n\n")
  }

  # Build analysis plan
  analysis_plan <- list(
    propensity_score_vars = propensity_score_vars,
    sensitivity_analyses = sensitivity_analyses,
    subgroup_analyses = subgroup_analyses,
    n_sensitivity = length(sensitivity_analyses),
    n_subgroups = length(subgroup_analyses)
  )

  if (!is.null(propensity_score_vars)) {
    cat("Propensity Score Variables:", length(propensity_score_vars), "variables\n")
    cat(" ", paste(propensity_score_vars, collapse = ", "), "\n\n")
  }

  if (!is.null(sensitivity_analyses) && length(sensitivity_analyses) > 0) {
    cat("Sensitivity Analyses:", length(sensitivity_analyses), "analyses planned\n")
  }

  if (!is.null(subgroup_analyses) && length(subgroup_analyses) > 0) {
    cat("Subgroup Analyses:", length(subgroup_analyses), "subgroups planned\n")
  }

  # Create design summary
  design_summary <- list(
    study_type = study_type,
    has_treatment_definition = !is.null(treatment_definition),
    endpoint_type = primary_endpoint$type,
    n_inclusion_criteria = length(inclusion_criteria),
    n_exclusion_criteria = length(exclusion_criteria),
    n_secondary_endpoints = length(secondary_endpoints),
    has_propensity_score = !is.null(propensity_score_vars),
    n_ps_variables = length(propensity_score_vars),
    n_sensitivity_analyses = length(sensitivity_analyses),
    n_subgroup_analyses = length(subgroup_analyses),
    follow_up_days = follow_up_duration
  )

  cat("\nStudy design template created successfully!\n")

  log_info("Study design template created", context = "rwe_design_study")

  # Create audit trail
  audit <- create_audit_trail(
    operation = "study_design_creation",
    input_data = list(
      study_type = study_type,
      study_title = study_title
    ),
    output_data = list(
      design_created = TRUE,
      protocol_version = protocol_version
    ),
    parameters = list(
      study_type = study_type,
      follow_up_duration = follow_up_duration,
      n_inclusion_criteria = length(inclusion_criteria),
      n_exclusion_criteria = length(exclusion_criteria),
      population_definition = population_definition,
      treatment_definition = treatment_definition,
      primary_endpoint = primary_endpoint
    )
  )

  # Create result object
  result <- structure(
    list(
      study_info = study_info,
      population = population,
      treatment = treatment_definition,
      endpoints = endpoints,
      analysis_plan = analysis_plan,
      design_summary = design_summary,
      audit_trail = audit
    ),
    class = c("rwe_study_design", "rwe_analysis")
  )

  return(result)
}


#' Print Method for Study Design Objects
#'
#' @param x An object of class \code{rwe_study_design}
#' @param ... Additional arguments (not used)
#'
#' @export
print.rwe_study_design <- function(x, ...) {
  cat("RWE Study Design\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  cat("Study Information:\n")
  cat("  Title:", x$study_info$title, "\n")
  cat("  Type:", x$study_info$type, "\n")
  cat("  Protocol Version:", x$study_info$protocol_version, "\n")
  cat("  Creation Date:", as.character(x$study_info$creation_date), "\n\n")

  cat("Design Elements:\n")
  cat("  Follow-up duration:", x$study_info$follow_up_duration, "days\n")
  cat("  Inclusion criteria:", x$population$n_inclusion_criteria, "\n")
  cat("  Exclusion criteria:", x$population$n_exclusion_criteria, "\n")
  cat("  Primary endpoint type:", x$endpoints$primary$type, "\n")
  cat("  Secondary endpoints:", x$endpoints$n_secondary, "\n\n")

  if (x$design_summary$has_propensity_score) {
    cat("Analysis Plan:\n")
    cat("  Propensity score variables:", x$analysis_plan$n_ps_variables, "\n")
  }

  if (x$analysis_plan$n_sensitivity > 0) {
    cat("  Sensitivity analyses:", x$analysis_plan$n_sensitivity, "\n")
  }

  if (x$analysis_plan$n_subgroups > 0) {
    cat("  Subgroup analyses:", x$analysis_plan$n_subgroups, "\n")
  }

  cat("\nUse summary() for detailed design information\n")

  invisible(x)
}


#' Summary Method for Study Design Objects
#'
#' @param object An object of class \code{rwe_study_design}
#' @param ... Additional arguments (not used)
#'
#' @export
summary.rwe_study_design <- function(object, ...) {
  cat("RWE Study Design - Detailed Summary\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  cat("STUDY INFORMATION\n")
  cat("Title:", object$study_info$title, "\n")
  cat("Type:", object$study_info$type, "\n")
  cat("Protocol Version:", object$study_info$protocol_version, "\n")
  cat("Created:", as.character(object$study_info$creation_date), "\n")
  if (!is.null(object$study_info$sample_size_target)) {
    cat("Target Sample Size:", object$study_info$sample_size_target, "\n")
  }
  cat("\n")

  cat("POPULATION DEFINITION\n")
  for (item in names(object$population$definition)) {
    cat("  ", item, ":", object$population$definition[[item]], "\n")
  }
  cat("\n")

  if (!is.null(object$population$inclusion_criteria) && length(object$population$inclusion_criteria) > 0) {
    cat("INCLUSION CRITERIA (", object$population$n_inclusion_criteria, ")\n", sep = "")
    for (i in seq_along(object$population$inclusion_criteria)) {
      criterion <- names(object$population$inclusion_criteria)[i]
      value <- object$population$inclusion_criteria[[i]]
      cat("  ", i, ". ", criterion, ": ", value, "\n", sep = "")
    }
    cat("\n")
  }

  if (!is.null(object$population$exclusion_criteria) && length(object$population$exclusion_criteria) > 0) {
    cat("EXCLUSION CRITERIA (", object$population$n_exclusion_criteria, ")\n", sep = "")
    for (i in seq_along(object$population$exclusion_criteria)) {
      criterion <- names(object$population$exclusion_criteria)[i]
      value <- object$population$exclusion_criteria[[i]]
      cat("  ", i, ". ", criterion, ": ", value, "\n", sep = "")
    }
    cat("\n")
  }

  if (!is.null(object$treatment)) {
    cat("TREATMENT DEFINITION\n")
    for (item in names(object$treatment)) {
      cat("  ", item, ":", object$treatment[[item]], "\n")
    }
    cat("\n")
  }

  cat("ENDPOINTS\n")
  cat("Primary Endpoint:\n")
  cat("  Type:", object$endpoints$primary$type, "\n")
  cat("  Variable:", object$endpoints$primary$variable, "\n")
  if (!is.null(object$endpoints$primary$timepoint)) {
    cat("  Timepoint:", object$endpoints$primary$timepoint, "days\n")
  }
  cat("\n")

  if (!is.null(object$endpoints$secondary) && length(object$endpoints$secondary) > 0) {
    cat("Secondary Endpoints (", object$endpoints$n_secondary, "):\n", sep = "")
    for (i in seq_along(object$endpoints$secondary)) {
      ep <- object$endpoints$secondary[[i]]
      cat("  ", i, ". ", ep$variable, " (", ep$type, ")\n", sep = "")
    }
    cat("\n")
  }

  cat("FOLLOW-UP & TIMING\n")
  cat("  Index date definition:", object$study_info$index_date_definition, "\n")
  cat("  Follow-up duration:", object$study_info$follow_up_duration, "days\n")
  cat("  Washout period:", object$study_info$washout_period, "days\n\n")

  if (!is.null(object$analysis_plan$propensity_score_vars)) {
    cat("STATISTICAL ANALYSIS PLAN\n")
    cat("Propensity Score Variables (", length(object$analysis_plan$propensity_score_vars), "):\n", sep = "")
    cat("  ", paste(object$analysis_plan$propensity_score_vars, collapse = ", "), "\n\n")
  }

  if (!is.null(object$analysis_plan$sensitivity_analyses) && length(object$analysis_plan$sensitivity_analyses) > 0) {
    cat("Sensitivity Analyses (", object$analysis_plan$n_sensitivity, "):\n", sep = "")
    for (i in seq_along(object$analysis_plan$sensitivity_analyses)) {
      cat("  ", i, ". ", object$analysis_plan$sensitivity_analyses[[i]], "\n", sep = "")
    }
    cat("\n")
  }

  if (!is.null(object$analysis_plan$subgroup_analyses) && length(object$analysis_plan$subgroup_analyses) > 0) {
    cat("Subgroup Analyses (", object$analysis_plan$n_subgroups, "):\n", sep = "")
    for (i in seq_along(object$analysis_plan$subgroup_analyses)) {
      cat("  ", i, ". ", object$analysis_plan$subgroup_analyses[[i]], "\n", sep = "")
    }
    cat("\n")
  }

  invisible(object)
}


#' Export Study Design to Protocol Document
#'
#' @description
#' Exports a study design object to a structured protocol document in various formats.
#'
#' @param design An object of class \code{rwe_study_design}
#' @param output_file Path to output file (with extension: .txt, .json, .yaml)
#' @param format Output format: "text", "json", "yaml" (auto-detected from extension)
#'
#' @return Invisible NULL (file written as side effect)
#'
#' @examples
#' \dontrun{
#' export_study_design(study_design, "protocol_v1.0.txt")
#' export_study_design(study_design, "protocol_v1.0.json")
#' }
#'
#' @export
export_study_design <- function(design, output_file, format = NULL) {

  if (!inherits(design, "rwe_study_design")) {
    stop("design must be an rwe_study_design object", call. = FALSE)
  }

  # Auto-detect format from extension
  if (is.null(format)) {
    ext <- tolower(tools::file_ext(output_file))
    format <- switch(ext,
                    txt = "text",
                    json = "json",
                    yaml = "yaml",
                    yml = "yaml",
                    "text")
  }

  if (format == "text") {
    # Export as text protocol
    sink(output_file)
    summary(design)
    sink()
    cat("Study design exported to:", output_file, "\n")
  } else if (format == "json") {
    # Export as JSON
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Package 'jsonlite' required for JSON export", call. = FALSE)
    }
    jsonlite::write_json(design, output_file, pretty = TRUE, auto_unbox = TRUE)
    cat("Study design exported to:", output_file, "\n")
  } else if (format == "yaml") {
    # Export as YAML
    if (!requireNamespace("yaml", quietly = TRUE)) {
      stop("Package 'yaml' required for YAML export", call. = FALSE)
    }
    yaml::write_yaml(design, output_file)
    cat("Study design exported to:", output_file, "\n")
  } else {
    stop("Unsupported format: ", format, call. = FALSE)
  }

  invisible(NULL)
}
