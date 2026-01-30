# Domain Pitfalls: RWE Orchestration Platform

**Domain:** Real-World Evidence (RWE) Orchestration for Agentic AI
**Researched:** 2026-01-30
**Focus:** Refactoring standalone R package to orchestration layer

---

## Critical Pitfalls

Mistakes that cause rewrites, regulatory failures, or major issues.

### Pitfall 1: Breaking Audit Trail Integrity During Refactor

**What goes wrong:** When refactoring from custom implementations to orchestration of WeightIt/cobalt/MatchIt, the existing audit trail system loses traceability of which underlying package performed which operation, breaking 21 CFR Part 11 compliance.

**Why it happens:** The current `create_audit_trail()` function (logging.R) records high-level operations but doesn't capture delegated package calls. When switching from `estimate_ps_logistic()` to `WeightIt::weightit()`, the audit trail shows "propensity_score_estimation" but loses the fact that WeightIt v0.14.0 performed the actual calculation.

**Consequences:**
- FDA/EMA regulatory submissions rejected due to incomplete audit trails
- Cannot reproduce exact results because package versions aren't tracked
- Validation failures when auditors ask "which algorithm computed this?"
- Loss of required 21 CFR Part 11 requirement for "independently record[ing] the date and time of operator actions"

**Prevention:**
1. **Extend audit trail schema BEFORE refactoring:**
   ```r
   audit_trail <- list(
     operation = "propensity_score_estimation",
     delegated_to = list(
       package = "WeightIt",
       version = as.character(packageVersion("WeightIt")),
       function_called = "weightit",
       parameters_passed = list(...)
     ),
     timestamp = Sys.time(),
     ...
   )
   ```

2. **Create wrapper layer that always logs:**
   - Every call to WeightIt/cobalt/MatchIt must pass through a logging wrapper
   - Wrapper captures: package name, version, function, parameters, return metadata
   - Store delegated call chain for doubly-robust estimation (PS → WeightIt, Balance → cobalt)

3. **Add version pinning to DESCRIPTION:**
   - Change `WeightIt (>= 0.14.0)` to `WeightIt (>= 0.14.0, < 0.15.0)`
   - Document why: "Regulatory reproducibility requires known algorithm versions"

4. **Test audit trail export:**
   - Ensure `export_audit_trail()` includes full delegation chain
   - JSON/YAML output must be parseable by validation tools

**Detection:**
- Warning sign: Audit trail JSON doesn't mention WeightIt when PS weighting is used
- Test: Can you reproduce results 2 years later with only the audit trail?
- Validation: Do audit logs pass `DataQualityDashboard::executeDqChecks()`?

**Phase mapping:** Address in Phase 1 (Architecture) - audit trail must be designed before integration starts.

---

### Pitfall 2: Propensity Score Trimming/Capping Mismatch with WeightIt

**What goes wrong:** The current codebase has hardcoded PS trimming at 0.025-0.975 (lines 101, 362 in causal_inference.R) and automatic weight capping at 99th percentile (line 126). When delegating to WeightIt, you either (a) apply trimming twice, causing overly aggressive trimming, or (b) disable custom trimming but lose the protection it provides.

**Why it happens:** WeightIt has its own trimming mechanisms (`trim` parameter, `stabilize` parameter), but the interfaces don't match rwevidence's current approach. You can't just "pass through" the current behavior.

**Consequences:**
- **Statistical bias:** Double-trimming removes too many observations, changing the estimand
- **Inflated standard errors:** Over-trimming reduces effective sample size
- **Manipulation potential:** Without guidance on optimal trimming, "there exists the dangerous potential for trimming being used to artificially achieve a desired result" (Weight Trimming and Propensity Score Weighting, PMC3069059)
- **Results divergence:** Studies run pre-refactor vs post-refactor produce different estimates even on identical data

**Prevention:**

1. **Document trimming responsibility boundaries:**
   ```
   rwevidence role: Detect and warn about extreme PS (diagnostics)
   WeightIt role: Apply trimming during weight estimation
   cobalt role: Assess balance after trimming
   ```

2. **Use WeightIt's native trimming:**
   ```r
   # Instead of pre-trimming PS then calling WeightIt
   weightit(formula, data = data,
            method = "ps",
            stabilize = TRUE,          # Use WeightIt's stabilization
            trim.weights = 0.01)        # Symmetric trimming at 1st/99th percentile
   ```

3. **Add diagnostic layer:**
   ```r
   # Before calling WeightIt
   check_ps_extremes <- function(ps) {
     extreme_count <- sum(ps < 0.05 | ps > 0.95)
     if (extreme_count / length(ps) > 0.05) {
       log_warning(paste(extreme_count, "PS values outside [0.05, 0.95].",
                        "Consider overlap weighting instead of ATE."))
     }
   }
   ```

4. **Deprecate custom trimming gracefully:**
   ```r
   rwe_iptw <- function(..., truncate = deprecated()) {
     if (lifecycle::is_present(truncate)) {
       lifecycle::deprecate_warn(
         "0.2.0",
         "rwe_iptw(truncate)",
         details = "Use WeightIt's trim.weights parameter instead"
       )
     }
   }
   ```

**Detection:**
- Warning sign: Mean propensity scores change by >0.02 after refactor
- Warning sign: Effective sample size (ESS) drops by >15%
- Test: Compare SMD values before/after on test dataset - should be within 0.01

**Phase mapping:** Address in Phase 2 (Core Integration) - requires coordination with WeightIt API design.

**Reference:** [Weight Trimming and Propensity Score Weighting (PMC3069059)](https://pmc.ncbi.nlm.nih.gov/articles/PMC3069059/)

---

### Pitfall 3: Covariate Balance Assessment Inconsistency (Manual SMD vs cobalt)

**What goes wrong:** Current `assess_covariate_balance()` (propensity_score.R:550-604) manually calculates standardized mean differences (SMD) using pooled standard deviation. cobalt has sophisticated balance assessment that handles:
- Binary, continuous, and categorical covariates differently
- Interactions and polynomial terms
- Cluster-robust variance
- Multiple imputation datasets
- Time-varying treatments

Mixing manual SMD with cobalt's `bal.tab()` creates inconsistent balance reports, especially for complex covariates.

**Why it happens:**
- Manual implementation predates cobalt integration
- Custom SMD formula seems "simple enough"
- Developers don't realize cobalt handles edge cases (zero variance, missing data, factor levels)

**Consequences:**
- Balance reports show "good balance" (manual SMD < 0.1) but cobalt shows imbalance
- Categorical variables assessed incorrectly (current code skips non-numeric covariates, line 565)
- Cannot compare balance across studies that use different SMD calculations
- Regulatory reviewers question discrepancies between methods

**Prevention:**

1. **Always delegate balance assessment to cobalt:**
   ```r
   # Instead of manual calculation
   assess_covariate_balance <- function(data, treatment, covariates, weights = NULL) {
     if (!requireNamespace("cobalt", quietly = TRUE)) {
       stop("cobalt package required for balance assessment")
     }

     formula <- as.formula(paste(treatment, "~", paste(covariates, collapse = " + ")))

     bal <- cobalt::bal.tab(formula,
                           data = data,
                           weights = weights,
                           binary = "std",      # SMD for binary
                           continuous = "std",  # SMD for continuous
                           s.d.denom = "pooled")

     # Convert to rwevidence format for backwards compatibility
     convert_cobalt_to_rwe_balance(bal)
   }
   ```

2. **Standardize on cobalt's conventions:**
   - Use "pooled" SD denominator for SMD (matches MatchIt default)
   - Report both mean differences and variance ratios for continuous variables
   - Include automated checks: `cobalt::bal.plot()` for distribution overlap

3. **Handle edge cases cobalt solves:**
   - Multicollinearity: cobalt detects perfectly correlated covariates
   - Rare categories: cobalt warns about sparse cells in categorical variables
   - Clustered data: cobalt supports `cluster` parameter for hierarchical data

4. **Backwards compatibility:**
   ```r
   # Keep balance_before/balance_after structure
   # But populate from cobalt internals
   balance_before <- list(
     smd = extract_smd_from_cobalt(bal_before),
     variance_ratio = extract_vr_from_cobalt(bal_before),
     cobalt_object = bal_before  # Store full object for advanced users
   )
   ```

**Detection:**
- Warning sign: Manual SMD shows balance but cobalt::love.plot() shows imbalance
- Test: Run both methods on dataset with categorical variables - should agree within 0.01
- Validation: cobalt produces warnings your manual function doesn't catch

**Phase mapping:** Address in Phase 2 (Core Integration) alongside WeightIt integration.

**Reference:** [Covariate Balance Tables and Plots: A Guide to cobalt](https://ngreifer.github.io/cobalt/articles/cobalt.html)

---

### Pitfall 4: OMOP CDM Mapping Errors Compound Through Pipeline

**What goes wrong:** Data quality issues in OMOP CDM transformation (wrong concept_id mappings, referential integrity violations, chronological relationship errors) propagate through the entire RWE pipeline. A patient's birth_date > observation_start_date breaks propensity score estimation when age is a covariate.

**Why it happens:**
- ETL to OMOP happens upstream of rwevidence
- rwevidence assumes OMOP data is clean
- No validation layer between data ingestion and analysis
- Standard terms mapped to non-standard concepts (common error in multi-center studies)

**Consequences:**
- Propensity score models fail with cryptic errors ("NA/NaN/Inf in foreign function call")
- Treatment effects biased due to miscoded exposure windows
- Patient timelines corrupted (treatments before diagnoses)
- Regulatory rejection: "Data quality insufficient for evidence generation"

**Prevention:**

1. **Add OMOP validation layer in Phase 1:**
   ```r
   validate_omop_cdm <- function(data) {
     checks <- list()

     # Chronological integrity
     checks$timeline <- check_date_logic(
       birth_datetime < observation_period_start_date,
       observation_period_start_date < observation_period_end_date,
       death_datetime > birth_datetime
     )

     # Referential integrity
     checks$foreign_keys <- check_concept_ids_exist(
       data$concept_id %in% valid_concepts$concept_id
     )

     # Standard vs non-standard concepts
     checks$standard_concepts <- check_standard_mapping(
       filter(data, standard_concept != 'S')
     )

     # Run DataQualityDashboard checks
     if (requireNamespace("DataQualityDashboard", quietly = TRUE)) {
       checks$dqd <- DataQualityDashboard::executeDqChecks(
         connectionDetails,
         cdmDatabaseSchema
       )
     }

     report_validation_results(checks)
   }
   ```

2. **Fail fast on data quality:**
   - Don't allow propensity score estimation if OMOP validation fails
   - Provide actionable error messages: "Patient 12345: birth_date (1985-03-15) > drug_exposure_start (1982-01-10)"

3. **Document data quality assumptions:**
   ```r
   #' @section Data Quality Requirements:
   #' This function requires OMOP CDM v5.4+ with:
   #' - All standard concepts mapped (standard_concept = 'S')
   #' - Chronological integrity (birth < observation < death)
   #' - Complete person, observation_period, drug_exposure tables
   #'
   #' Run \code{validate_omop_cdm()} before analysis.
   ```

4. **Integration with OHDSI tools:**
   - Use `DatabaseConnector` for consistent DB access
   - Use `CohortMethod` for standardized OMOP-based studies
   - Use `DataQualityDashboard` for systematic checks (3300+ validation rules)

**Detection:**
- Warning sign: Propensity score model fails to converge
- Warning sign: Extreme propensity scores (>0.99 or <0.01) for >10% of patients
- Test: Run `DataQualityDashboard::executeDqChecks()` - should pass >95% of checks

**Phase mapping:** Address in Phase 1 (Data Validation) - before any statistical methods.

**Reference:**
- [Multi-Center Healthcare Data Quality Measurement Model and Assessment Using OMOP CDM](https://www.mdpi.com/2076-3417/11/19/9188)
- [OHDSI DataQualityDashboard](https://ohdsi.github.io/DataQualityDashboard/)

---

### Pitfall 5: API Design for AI Agents - Vague Error Messages

**What goes wrong:** Current error messages like "Treatment variable must be binary" (propensity_score.R:64) don't tell an AI agent HOW to fix the problem. Agent retries with same inputs, enters infinite loop, or gives up.

**Why it happens:**
- Error messages written for human data scientists who can inspect data
- Assumes user knows how to check `unique(data$treatment)`
- No machine-parseable error codes or suggested fixes

**Consequences:**
- AI agents fail 40% of tasks due to "unclear error resolution paths"
- Agents make wrong assumptions: "binary means 0/1" when it could be "Treated/Control"
- Support burden increases as agents log tickets for preventable errors
- Agent orchestration systems timeout waiting for manual intervention

**Prevention:**

1. **Structured error responses for APIs:**
   ```r
   stop_for_api <- function(error_code, message, details = list(),
                            suggested_fix = NULL) {
     error_obj <- list(
       error_code = error_code,
       error_message = message,
       details = details,
       suggested_fix = suggested_fix,
       timestamp = Sys.time()
     )

     # For API calls, return JSON
     if (is_api_call()) {
       return(jsonlite::toJSON(error_obj, auto_unbox = TRUE))
     }

     # For interactive use, friendly message
     stop(format_error_for_human(error_obj), call. = FALSE)
   }

   # Usage
   if (length(treatment_vals) > 2) {
     stop_for_api(
       error_code = "E_TREATMENT_NOT_BINARY",
       message = "Treatment variable must be binary (2 unique values)",
       details = list(
         variable = treatment,
         found_values = treatment_vals,
         n_unique = length(treatment_vals)
       ),
       suggested_fix = list(
         action = "recode_treatment",
         example = paste0("data$", treatment, " <- ifelse(data$", treatment,
                         " == '", treatment_vals[1], "', 0, 1)")
       )
     )
   }
   ```

2. **Explicit output schemas:**
   ```r
   #' @return rwe_propensity_score object with schema:
   #' \describe{
   #'   \item{data}{data.frame with original data + propensity_score column (numeric 0-1)}
   #'   \item{model}{glm/rf/xgb model object (class varies by method)}
   #'   \item{metrics}{list(auc = numeric, ks_statistic = numeric)}
   #'   \item{audit_trail}{rwe_audit_trail object (see create_audit_trail)}
   #' }
   ```

3. **Examples in error messages:**
   ```r
   if (!treatment %in% names(data_df)) {
     available <- paste(head(names(data_df), 5), collapse = ", ")
     stop_for_api(
       "E_VARIABLE_NOT_FOUND",
       paste0("Treatment variable '", treatment, "' not found in data"),
       details = list(
         requested = treatment,
         available_columns = names(data_df),
         sample_columns = available
       ),
       suggested_fix = list(
         action = "check_spelling",
         hint = paste0("Did you mean one of: ", available, "?")
       )
     )
   }
   ```

4. **Validation endpoints for agents:**
   ```r
   #' @export
   rwe_validate_inputs <- function(data, treatment, covariates, outcome = NULL) {
     # Returns validation object instead of stopping
     validation <- list(
       valid = TRUE,
       errors = list(),
       warnings = list()
     )

     # Check each requirement
     if (!treatment %in% names(data)) {
       validation$valid <- FALSE
       validation$errors <- append(validation$errors, list(
         list(code = "E_TREATMENT_MISSING", field = "treatment")
       ))
     }

     # Agent can check validation$valid before calling main function
     return(validation)
   }
   ```

**Detection:**
- Warning sign: API error logs show same error repeated 3+ times in sequence
- Warning sign: Agent timeout rate >20%
- Test: Can an LLM fix errors using only error message text?

**Phase mapping:** Address in Phase 3 (API Layer) - after core integration is stable.

**Reference:**
- [Designing APIs for LLM Apps](https://www.gravitee.io/blog/designing-apis-for-llm-apps)
- [If an AI agent can't figure out how your API works, neither can your users](https://stytch.com/blog/if-an-ai-agent-cant-figure-out-how-your-api-works-neither-can-your-users/)

---

## Moderate Pitfalls

Mistakes that cause delays, technical debt, or rework.

### Pitfall 6: Wrapper Pattern Without Escape Hatches

**What goes wrong:** You create clean orchestration wrappers that call WeightIt/cobalt/MatchIt, but power users need access to advanced features (e.g., WeightIt's `method = "energy"` for energy balancing, cobalt's `int = TRUE` for interactions). Your wrapper doesn't expose these, forcing users to bypass your package entirely.

**Why it happens:**
- Impossible to expose every parameter from 4+ packages
- Wrapper designed for "common case" AI agent usage
- Assumption that orchestration = simplified interface only

**Prevention:**

1. **Expose escape hatch pattern:**
   ```r
   rwe_propensity_score <- function(...,
                                    .weightit_args = list(),
                                    .direct_call = FALSE) {

     if (.direct_call) {
       # Advanced users can call WeightIt directly through rwevidence
       # Still get audit trail and logging
       result <- do.call(WeightIt::weightit, .weightit_args)
       return(wrap_with_audit(result, "weightit_direct"))
     }

     # Standard orchestration path
     ...
   }
   ```

2. **Document advanced usage:**
   ```r
   #' @section Advanced Usage:
   #' For features not exposed by rwevidence, use \code{.direct_call = TRUE}:
   #'
   #' \code{rwe_ps(..., .direct_call = TRUE, .weightit_args = list(
   #'   formula = treat ~ age + sex,
   #'   data = data,
   #'   method = "energy",  # Not available in standard interface
   #'   estimand = "ATO"
   #' ))}
   ```

3. **Progressive disclosure:**
   - 80% of users: Simple `rwe_iptw(data, treatment, outcome, covariates)`
   - 15% of users: `rwe_iptw(..., stabilize = TRUE, trim = 0.01)`
   - 5% of users: `.direct_call = TRUE` with full WeightIt access

**Detection:**
- Warning sign: GitHub issues asking "How do I access [WeightIt feature]?"
- Warning sign: Users fork package to add one parameter

**Phase mapping:** Address in Phase 2 (Core Integration) during API design.

---

### Pitfall 7: admiral Integration Assumes ADaM Structure Too Early

**What goes wrong:** Attempting to integrate admiral's ADaM derivation functions while data is still in raw OMOP CDM format. admiral expects SDTM-like structure (one row per event), but OMOP is normalized (person, drug_exposure, condition_occurrence as separate tables).

**Why it happens:**
- Misunderstanding of admiral's position in clinical data pipeline: SDTM → ADaM → Analysis
- OMOP CDM ≠ SDTM (different standards from different organizations)
- admiral's `derive_vars_*` functions expect specific column names

**Prevention:**

1. **Map data flow clearly:**
   ```
   Raw EHR → OMOP CDM (your harmonization layer)
   OMOP CDM → SDTM-like intermediate (new transformation needed)
   SDTM-like → ADaM (admiral integration point)
   ADaM → RWE Analysis (your propensity/survival functions)
   ```

2. **Create OMOP-to-SDTM bridge:**
   ```r
   omop_to_sdtm_exposure <- function(omop_data) {
     # admiral expects ADSL (subject-level) + ADAE (adverse events)
     # OMOP has person + drug_exposure + condition_occurrence

     sdtm_dm <- omop_data$person %>%
       transmute(
         USUBJID = person_id,
         AGE = year(Sys.Date()) - year_of_birth,
         SEX = case_when(
           gender_concept_id == 8507 ~ "M",
           gender_concept_id == 8532 ~ "F"
         )
       )

     # Now admiral can process
     admiral::derive_vars_dt(sdtm_dm, ...)
   }
   ```

3. **Use admiral selectively:**
   - Don't force all data through ADaM if OMOP CDM is sufficient
   - admiral adds value for: treatment emergent flags, baseline calculations, study day derivations
   - admiral may be overkill for: simple propensity matching, survival analysis

4. **Document when to use admiral:**
   ```r
   #' @section When to use admiral integration:
   #' - Regulatory submissions requiring ADaM datasets (FDA/EMA)
   #' - Studies comparing RWE to clinical trial efficacy data
   #' - Complex baseline derivations (e.g., "on-treatment" periods)
   #'
   #' @section When OMOP CDM is sufficient:
   #' - Exploratory RWE analysis
   #' - Large-scale database studies (OHDSI network)
   #' - Population-level effect estimation
   ```

**Detection:**
- Warning sign: admiral functions throw "column not found" errors
- Warning sign: Need to rename 20+ columns before admiral works
- Test: Can you create ADSL from OMOP in <50 lines of code?

**Phase mapping:** Address in Phase 4 (ADaM Integration) - after core propensity/IPTW layers work.

**Reference:** [Leveraging Tidyverse and admiral for ADaM Data Set](https://pharmasug.org/proceedings/2025/OS/PharmaSUG-2025-OS-167.pdf)

---

### Pitfall 8: Deprecation Without Migration Path

**What goes wrong:** You deprecate `rwe_propensity_score(method = "gbm")` because you're delegating to WeightIt, but don't explain HOW users should migrate their existing scripts. Scripts break, users frustrated.

**Why it happens:**
- Focus on new architecture, not migration experience
- Assume deprecation warning is enough
- No runnable examples of old → new

**Prevention:**

1. **Soft deprecation with migration helper:**
   ```r
   rwe_propensity_score <- function(..., method = "logistic") {

     # Detect deprecated parameters
     dots <- list(...)
     if ("truncate" %in% names(dots)) {
       lifecycle::deprecate_soft(
         when = "0.2.0",
         what = "rwe_propensity_score(truncate)",
         with = "rwe_propensity_score(.weightit_args)",
         details = c(
           "The truncate parameter is superseded by WeightIt's trim.weights.",
           "To migrate, change:",
           "  OLD: rwe_ps(..., truncate = c(0.01, 0.99))",
           "  NEW: rwe_ps(..., .weightit_args = list(trim.weights = 0.01))"
         )
       )

       # Auto-migrate if possible
       if (is.null(dots$.weightit_args)) {
         dots$.weightit_args <- list()
       }
       dots$.weightit_args$trim.weights <- dots$truncate[1]
       dots$truncate <- NULL

       log_info("Auto-migrated truncate parameter to WeightIt::trim.weights")
     }

     # Continue with new implementation
     ...
   }
   ```

2. **Migration vignette:**
   ```r
   # vignettes/migration-v0.2.0.Rmd

   ## Migrating from rwevidence 0.1.0 to 0.2.0

   ### Propensity Score Estimation

   **Old code (0.1.0):**
   ```r
   ps <- rwe_propensity_score(
     data = data,
     treatment = "treated",
     covariates = covs,
     method = "logistic"
   )
   ```

   **New code (0.2.0):**
   ```r
   # Exact same interface - no changes needed for basic usage!
   ps <- rwe_propensity_score(
     data = data,
     treatment = "treated",
     covariates = covs,
     method = "ps"  # Now uses WeightIt internally
   )
   ```

   **What changed:**
   - Now delegates to WeightIt::weightit()
   - Audit trail includes WeightIt version
   - Access WeightIt advanced features via .weightit_args
   ```

3. **Version-to-version compatibility tests:**
   ```r
   test_that("v0.1.0 code still works in v0.2.0", {
     # Load saved v0.1.0 results
     old_result <- readRDS("tests/fixtures/ps_result_v0.1.0.rds")

     # Run same code with v0.2.0
     new_result <- rwe_propensity_score(
       data = test_data,
       treatment = "treat",
       covariates = c("age", "sex")
     )

     # Results should be numerically similar (not identical due to WeightIt)
     expect_equal(
       mean(old_result$data$propensity_score),
       mean(new_result$data$propensity_score),
       tolerance = 0.01
     )
   })
   ```

4. **Lifecycle badges in documentation:**
   ```r
   #' @description
   #' `r lifecycle::badge("stable")` for basic usage
   #'
   #' `r lifecycle::badge("deprecated")` for `truncate` parameter - use
   #' `.weightit_args = list(trim.weights = ...)` instead
   ```

**Detection:**
- Warning sign: GitHub issues titled "Code stopped working after update"
- Warning sign: Users pin to old version in DESCRIPTION
- Test: Run examples from v0.1.0 vignettes against v0.2.0 - should work or give helpful errors

**Phase mapping:** Address in Phase 5 (Deprecation) - after new features stabilize.

**Reference:** [R Packages (2e) - Lifecycle](https://r-pkgs.org/lifecycle.html)

---

### Pitfall 9: Missing Convergence Checks Create Silent Failures

**What goes wrong:** Current code checks `model_treated$converged` and `model_control$converged` (causal_inference.R:386, 405) but only logs warnings. AI agents don't see warnings, continue with unreliable doubly-robust estimates, produce nonsensical treatment effects.

**Why it happens:**
- Warnings meant for interactive R users
- No distinction between "approximate but usable" vs "completely unreliable"
- API returns success even when models didn't converge

**Prevention:**

1. **Fail loudly on critical non-convergence:**
   ```r
   if (!model_treated$converged && is_api_call()) {
     stop_for_api(
       "E_MODEL_CONVERGENCE_FAILED",
       "Outcome model for treated group did not converge",
       details = list(
         iterations = model_treated$iter,
         max_iterations = model_treated$control$maxit
       ),
       suggested_fix = list(
         action = "increase_iterations",
         example = "glm(..., control = list(maxit = 100))"
       )
     )
   }
   ```

2. **Tiered warning system:**
   ```r
   assess_convergence <- function(model) {
     if (!model$converged) {
       if (model$iter < model$control$maxit * 0.5) {
         # Severe: Failed early
         return(list(status = "FAIL", severity = "critical"))
       } else if (abs(tail(model$deviance.resid, 1)) > 0.01) {
         # Moderate: Close but unstable
         return(list(status = "WARN", severity = "moderate"))
       } else {
         # Minor: Numerically close enough
         return(list(status = "OK", severity = "minor"))
       }
     }
     list(status = "OK", severity = "none")
   }
   ```

3. **Add convergence to audit trail:**
   ```r
   audit_trail <- create_audit_trail(
     operation = "doubly_robust_estimation",
     ...,
     warnings = list(
       treated_model = assess_convergence(model_treated),
       control_model = assess_convergence(model_control)
     )
   )
   ```

4. **Diagnostic endpoint for agents:**
   ```r
   #' @export
   rwe_check_convergence <- function(result_object) {
     # Returns machine-readable convergence status
     list(
       converged = all_models_converged(result_object),
       details = extract_convergence_info(result_object),
       recommendation = if (!converged) "REFIT" else "OK"
     )
   }
   ```

**Detection:**
- Warning sign: Treatment effects with SE > |estimate|
- Warning sign: Confidence intervals spanning [-Inf, Inf]
- Test: Fit model on data known to cause convergence issues - should fail loudly

**Phase mapping:** Address in Phase 3 (API Layer) alongside error handling improvements.

---

## Minor Pitfalls

Mistakes that cause annoyance but are fixable.

### Pitfall 10: S3 Class Naming Conflicts

**What goes wrong:** Current code defines `ps_rf_model`, `ps_xgb_model`, `ps_sl_model` classes (propensity_score.R). If another package defines `ps_*` classes (e.g., `ps` package for posterior sampling), class dispatch breaks.

**Prevention:**
1. **Namespace S3 classes:**
   ```r
   # Instead of ps_rf_model
   class(obj) <- c("rwe_ps_rf_model", "rwe_ps_model", "rwe_model")
   ```

2. **Check for conflicts in tests:**
   ```r
   test_that("S3 classes don't conflict with other packages", {
     # Load potentially conflicting packages
     library(ps)  # posterior sampling

     # Should still dispatch correctly
     ps_result <- rwe_propensity_score(...)
     expect_s3_class(ps_result, "rwe_propensity_score")
   })
   ```

**Phase mapping:** Address in Phase 2 (Core Integration).

---

### Pitfall 11: Vignette Examples Use Toy Data

**What goes wrong:** Package examples use `data.frame(age = c(25, 30, 35), ...)` toy data. AI agents learn from examples, generate RWE studies on 3-row datasets, wonder why results are unreliable.

**Prevention:**
1. **Include realistic example datasets:**
   ```r
   # data-raw/synthetic_rwe_cohort.R
   # Generate 1000-patient synthetic cohort with realistic properties
   # - Age distribution: 45-75 years
   # - Treatment assignment: 40% treated
   # - Confounding: Older patients more likely treated
   # - Outcome: Treatment reduces event rate by 20%

   usethis::use_data(synthetic_rwe_cohort, overwrite = TRUE)
   ```

2. **Examples demonstrate realistic workflows:**
   ```r
   #' @examples
   #' \dontrun{
   #' # Load realistic synthetic cohort (N=1000)
   #' data(synthetic_rwe_cohort)
   #'
   #' # Typical RWE workflow
   #' ps <- rwe_propensity_score(
   #'   data = synthetic_rwe_cohort,
   #'   treatment = "exposure_new_drug",
   #'   covariates = c("age", "sex", "charlson_score", "prior_hospitalization")
   #' )
   #' # Propensity scores estimated (N=1000, AUC=0.72)
   #' }
   ```

**Phase mapping:** Address in Phase 6 (Documentation).

---

### Pitfall 12: Hardcoded Paths in Logging

**What goes wrong:** If users call `setup_logger(file = "rwevidence.log")` from different working directories, logs scatter across filesystem.

**Prevention:**
1. **Use project-relative or temp paths:**
   ```r
   setup_logger <- function(file = NULL, ...) {
     if (is.null(file)) {
       # Default to temp file
       file <- tempfile("rwevidence_", fileext = ".log")
     } else if (!fs::is_absolute_path(file)) {
       # Make relative paths explicit
       file <- here::here(file)
       message("Logging to: ", file)
     }
     ...
   }
   ```

**Phase mapping:** Address in Phase 7 (Polish).

---

## Phase-Specific Warnings

| Phase Topic | Likely Pitfall | Mitigation |
|-------------|---------------|------------|
| Phase 1: Architecture | Breaking audit trail integrity | Design extended audit schema BEFORE touching code |
| Phase 2: WeightIt Integration | PS trimming mismatch | Document responsibility boundaries, use WeightIt's native trimming |
| Phase 2: cobalt Integration | Manual SMD vs cobalt inconsistency | Always delegate balance to cobalt, deprecate manual calculation |
| Phase 3: API Layer | Vague error messages for AI | Implement structured errors with suggested fixes |
| Phase 4: ADaM Integration | Assuming OMOP = SDTM | Create OMOP-to-SDTM bridge, use admiral selectively |
| Phase 5: Deprecation | No migration path | Soft deprecation + auto-migration + migration vignette |
| Phase 6: CRAN Compliance | Audit trail export breaks | Test with `R CMD check --as-cran`, verify JSON export |
| Phase 7: Performance | Memory leaks in long pipelines | Profile with `profvis`, test 100K+ patient datasets |

---

## Research Methodology

### Sources Consulted

**RWE and Propensity Score Methods:**
- [Real-World Evidence: A Guide to RWE Analysis & Application - IntuitionLabs](https://intuitionlabs.ai/articles/real-world-evidence-analysis)
- [Understanding propensity score weighting methods - Aetion Evidence Hub](https://evidence-hub.aetion.com/understanding-propensity-score-weighting-methods-rwe)
- [Weight Trimming and Propensity Score Weighting - PLOS One](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0018174)
- [Addressing Extreme Propensity Scores via the Overlap Weights - American Journal of Epidemiology](https://academic.oup.com/aje/article/188/1/250/5090958)
- [Propensity Score Weighting and Trimming Strategies - PMC8327194](https://pmc.ncbi.nlm.nih.gov/articles/PMC8327194/)

**R Package Integration:**
- [Covariate Balance Tables and Plots: A Guide to cobalt](https://ngreifer.github.io/cobalt/articles/cobalt.html)
- [cobalt GitHub Repository](https://github.com/ngreifer/cobalt)
- [WeightIt GitHub Repository](https://github.com/ngreifer/WeightIt)
- [admiral: ADaM in R Asset Library - Posit Interview](https://posit.co/blog/admiral-maintainers-interview/)
- [Leveraging Tidyverse and admiral for ADaM - PharmaSUG 2025](https://pharmasug.org/proceedings/2025/OS/PharmaSUG-2025-OS-167.pdf)

**API Design for AI Agents:**
- [Designing APIs for LLM Apps - Gravitee](https://www.gravitee.io/blog/designing-apis-for-llm-apps)
- [If an AI agent can't figure out how your API works - Stytch](https://stytch.com/blog/if-an-ai-agent-cant-figure-out-how-your-api-works-neither-can-your-users/)
- [7 Practical Guidelines for Designing AI-Friendly APIs - Medium](https://medium.com/@chipiga86/7-practical-guidelines-for-designing-ai-friendly-apis-c5527f6869e6)
- [The 2025 AI Agent Report - Composio](https://composio.dev/blog/why-ai-agent-pilots-fail-2026-integration-roadmap)

**CRAN and R Package Lifecycle:**
- [R Packages (2e) - Lifecycle](https://r-pkgs.org/lifecycle.html)
- [lifecycle package documentation](https://lifecycle.r-lib.org/)
- [R: Regulatory Compliance and Validation Issues - R FDA](https://www.r-project.org/doc/R-FDA.pdf)

**Regulatory and Validation:**
- [21 CFR Part 11 Audit Trail Requirements](https://simplerqms.com/21-cfr-part-11-audit-trail/)
- [FDA 21 CFR Part 11 Compliance Guide - Kneat](https://kneat.com/articles/regulatory/fda-21-cfr-part-11-compliance-guide/)
- [Automating Audit Trail Compliance - IntuitionLabs](https://intuitionlabs.ai/articles/audit-trails-21-cfr-part-11-annex-11-compliance)

**OMOP CDM and Data Quality:**
- [A Guide to the OMOP Common Data Model - IQVIA](https://www.iqvia.com/blogs/2025/06/a-guide-to-the-omop-common-data-model)
- [Multi-Center Healthcare Data Quality Measurement Model - MDPI](https://www.mdpi.com/2076-3417/11/19/9188)
- [Increasing trust in real-world evidence through evaluation of observational data quality - PMC](https://pmc.ncbi.nlm.nih.gov/articles/PMC8449628/)
- [OHDSI Data Quality Dashboard](https://ohdsi.github.io/DataQualityDashboard/)

**Orchestration Patterns:**
- [Orchestration Pattern - Medium/GBTech](https://medium.com/gbtech/orchestration-pattern-3d8f5abc3be3)
- [Simplify your architecture using the Orchestrator Pattern - Jamie Maguire](https://jamiemaguire.net/index.php/2017/05/06/simplify-your-architecture-using-the-orchestrator-pattern/)
- [Orchestrating Complex AI Workflows - Knit](https://www.getknit.dev/blog/orchestrating-complex-ai-workflows-advanced-integration-patterns)

### Confidence Assessment

| Pitfall Category | Confidence | Evidence Source |
|-----------------|------------|-----------------|
| Propensity Score Trimming | HIGH | Multiple peer-reviewed papers + current codebase analysis |
| Audit Trail 21 CFR Part 11 | HIGH | FDA guidance + R validation docs + current implementation |
| cobalt Balance Assessment | HIGH | Official cobalt documentation + codebase comparison |
| API Error Messages for AI | MEDIUM | Recent blog posts (2025) + industry trends |
| admiral Integration Timing | MEDIUM | Admiral docs + OMOP/SDTM differences (training knowledge) |
| OMOP Data Quality | HIGH | OHDSI tools documentation + recent research (2025) |
| Deprecation Strategy | HIGH | Official R packages book + lifecycle package docs |
| Convergence Checking | HIGH | Current codebase shows the issue + statistical practice |

### Limitations

1. **admiral-specific pitfalls:** Limited 2025 research on common admiral integration mistakes. Relied more on understanding OMOP ≠ SDTM conceptual mismatch.

2. **AI agent error handling:** Rapidly evolving field. 2025 sources available, but best practices still emerging.

3. **Package version specifics:** Could not verify exact WeightIt 0.14.0 vs 0.15.0 breaking changes without access to CRAN version history.

---

## Next Steps for Roadmap Planning

Based on this pitfalls research, the roadmap should:

1. **Phase ordering:**
   - Phase 1 MUST include audit trail extension (Pitfall #1) before any integration
   - OMOP validation layer (Pitfall #4) gates entry to statistical methods
   - admiral integration (Pitfall #7) comes AFTER core propensity/IPTW work

2. **Research flags:**
   - Phase 2 (WeightIt integration): Deep dive needed on trimming parameter mapping
   - Phase 4 (admiral): Research OMOP-to-SDTM transformation patterns
   - Phase 5 (Deprecation): Test migration on real user scripts if available

3. **Quality gates:**
   - Every phase: Audit trail must validate
   - Phase 2+: Balance assessment must match cobalt
   - Phase 3+: API errors must be agent-parseable
   - Phase 6: CRAN check with `--as-cran` flag

4. **Validation requirements:**
   - Maintain v0.1.0 test fixtures for compatibility testing
   - Add DataQualityDashboard to test dependencies
   - Create synthetic realistic datasets (not toy data)

---

**Last Updated:** 2026-01-30
**Researcher:** GSD Project Researcher (Pitfalls dimension)
**Review Status:** Ready for roadmap creation
