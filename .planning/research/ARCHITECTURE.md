# Architecture Patterns: RWE Orchestration Platform

**Domain:** Real-World Evidence Platform for Agentic AI Consumption
**Researched:** 2026-01-30
**Confidence:** HIGH

## Executive Summary

An RWE orchestration platform must balance three architectural concerns: (1) delegating to best-in-class R packages (WeightIt, cobalt, MatchIt, admiral) for domain expertise, (2) maintaining a unified API surface for AI agent consumption, and (3) preserving regulatory compliance and audit trails. The recommended architecture follows a **Facade Orchestrator Pattern** with stateless API design and modular delegation to specialized packages.

This research synthesizes patterns from:
- Pharmaverse's modular admiral architecture for clinical trials
- Plumber's stateless API execution model
- OpenAPI specification standards for AI agent consumption
- Healthcare data integration patterns from 2026 RWE platforms
- Orchestration anti-patterns to avoid

## Recommended Architecture

### High-Level Structure

```
┌─────────────────────────────────────────────────────────────┐
│                    AI Agent / Client                        │
└────────────────────────┬────────────────────────────────────┘
                         │ HTTP/JSON (OpenAPI 3.0)
                         │
┌────────────────────────▼────────────────────────────────────┐
│               ORCHESTRATION LAYER (rwevidence)              │
│  ┌──────────────────────────────────────────────────────┐   │
│  │  REST API Facade (Plumber)                           │   │
│  │  - OpenAPI 3.0 specification                         │   │
│  │  - Stateless request handlers                        │   │
│  │  - Session state management (cookies/tokens)         │   │
│  │  - Request validation & error handling               │   │
│  └────────────────────┬─────────────────────────────────┘   │
│                       │                                      │
│  ┌────────────────────▼─────────────────────────────────┐   │
│  │  Unified Interface Layer                             │   │
│  │  - rwe_propensity_score()                            │   │
│  │  - rwe_match()                                       │   │
│  │  - rwe_iptw()                                        │   │
│  │  - rwe_assess_quality()                              │   │
│  │  - rwe_generate_regulatory_report()                  │   │
│  └────────────────────┬─────────────────────────────────┘   │
│                       │                                      │
│  ┌────────────────────▼─────────────────────────────────┐   │
│  │  Delegation & Translation Layer                      │   │
│  │  - Package adapters                                  │   │
│  │  - Parameter translation                             │   │
│  │  - Result standardization                            │   │
│  │  - Audit trail creation                              │   │
│  └──────────┬──────────┬──────────┬──────────┬───────────┘   │
└─────────────┼──────────┼──────────┼──────────┼───────────────┘
              │          │          │          │
    ┌─────────▼──┐  ┌────▼────┐ ┌──▼─────┐ ┌─▼────────┐
    │  WeightIt  │  │ MatchIt │ │ cobalt │ │ admiral  │
    │ (weights)  │  │(matching)│ │(balance)│ │  (ADaM)  │
    └────────────┘  └─────────┘ └────────┘ └──────────┘
```

### Component Boundaries

| Component | Responsibility | Communicates With | State |
|-----------|---------------|-------------------|-------|
| **REST API Facade** | HTTP request handling, authentication, OpenAPI spec generation | AI agents (external), Unified Interface (internal) | Stateless (session via cookies/tokens) |
| **Unified Interface Layer** | Consistent R function API, parameter validation, workflow orchestration | REST API, Delegation Layer | Stateless |
| **Delegation & Translation Layer** | Package selection, parameter mapping, result normalization, audit trail | Unified Interface (up), Specialized Packages (down) | Stateless |
| **WeightIt Adapter** | Propensity score weighting methods, weight diagnostics | Delegation Layer (up), WeightIt package | Stateless |
| **MatchIt Adapter** | Matching algorithms, match quality assessment | Delegation Layer (up), MatchIt package | Stateless |
| **cobalt Adapter** | Covariate balance assessment, balance visualization | Delegation Layer (up), cobalt package | Stateless |
| **admiral Adapter** | ADaM dataset derivations, regulatory compliance structures | Delegation Layer (up), admiral package | Stateless |
| **Session Manager** | Multi-request workflows, analysis state persistence | REST API, File system / Redis | Stateful (external storage) |
| **Audit Trail System** | Operation logging, parameter tracking, provenance | All layers | Append-only |

### Data Flow

#### Synchronous Flow (Single Request)
```
1. AI Agent → REST API: POST /propensity_score
   {treatment: "arm", covariates: ["age", "sex"], method: "gbm"}

2. REST API → Unified Interface: rwe_propensity_score(...)
   - Validate request body against OpenAPI schema
   - Extract parameters
   - Call internal function

3. Unified Interface → Delegation Layer: delegate_ps_estimation(method="gbm")
   - Select appropriate package (WeightIt for "gbm")
   - Translate rwevidence parameters to WeightIt parameters
   - Create audit entry

4. Delegation Layer → WeightIt: weightit(formula, data, method="gbm")
   - Execute specialized algorithm
   - Return WeightIt object

5. Delegation Layer → Unified Interface: standardize_ps_result()
   - Extract propensity scores from WeightIt object
   - Normalize result structure to rwe_propensity_score S3 class
   - Add audit trail metadata

6. Unified Interface → REST API: rwe_propensity_score object
   - Serialize to JSON (with prepare_for_json())
   - Return structured response

7. REST API → AI Agent: JSON response
   {ps_scores: [...], balance_before: {...}, method: "gbm", audit: {...}}
```

#### Asynchronous Flow (Multi-Step Workflow)
```
1. AI Agent → POST /session/create
   Response: {session_id: "abc123", expires_at: "..."}

2. AI Agent → POST /session/abc123/upload_data
   Response: {dataset_id: "data_xyz", rows: 5000, columns: 25}

3. AI Agent → POST /session/abc123/propensity_score
   Request: {dataset_id: "data_xyz", ...}
   Response: {ps_id: "ps_001", scores_computed: 5000}

4. AI Agent → POST /session/abc123/matching
   Request: {ps_id: "ps_001", method: "optimal"}
   Response: {matched_id: "match_001", matched_pairs: 2300}

5. AI Agent → POST /session/abc123/balance_assessment
   Request: {matched_id: "match_001"}
   Response: {balance_improved: true, mean_smd_before: 0.45, mean_smd_after: 0.08}

6. AI Agent → POST /session/abc123/export_report
   Response: {report_url: "/reports/abc123/regulatory_report.docx"}
```

## Patterns to Follow

### Pattern 1: Facade Orchestrator

**What:** Present a unified API while delegating to specialized packages underneath.

**When:** When multiple best-in-class packages exist for domain tasks (propensity scores, matching, balance assessment) but clients need a consistent interface.

**Why:** Avoids creating "orchestration services" anti-pattern with unnecessary technical layers. Each rwevidence function is a thin facade that delegates to the appropriate specialized package.

**Example:**
```r
# Facade function in rwevidence
rwe_propensity_score <- function(data, treatment, covariates, method = "logistic", ...) {
  # Validation & audit setup
  validate_inputs(data, required_cols = c(treatment, covariates))
  audit_entry <- create_audit_trail(operation = "propensity_score_estimation")

  # Delegate to appropriate package based on method
  ps_result <- if (method %in% c("gbm", "cbps", "ebal", "sbw")) {
    # Use WeightIt for advanced methods
    delegate_to_weightit(data, treatment, covariates, method, ...)
  } else if (method == "optimal") {
    # Use MatchIt for optimal matching-based PS
    delegate_to_matchit(data, treatment, covariates, ...)
  } else {
    # Use internal logistic regression for simple cases
    estimate_ps_logistic(data, treatment, covariates)
  }

  # Standardize result to rwevidence S3 class
  standardize_ps_result(ps_result, audit_entry)
}

# Delegation adapter
delegate_to_weightit <- function(data, treatment, covariates, method, ...) {
  check_required_packages("WeightIt")

  # Translate rwevidence parameters to WeightIt parameters
  formula <- as.formula(paste(treatment, "~", paste(covariates, collapse = " + ")))
  estimand <- "ATE"  # Default to Average Treatment Effect

  # Call WeightIt
  weightit_obj <- WeightIt::weightit(
    formula = formula,
    data = data,
    method = method,
    estimand = estimand,
    ...
  )

  # Return WeightIt object (will be standardized by caller)
  weightit_obj
}
```

**Benefits:**
- Clients get stable API even as underlying packages change
- Can swap implementations without breaking contracts
- Regulatory audit trails maintained at orchestration level
- Each package used for its strengths (WeightIt for weights, cobalt for balance)

### Pattern 2: OpenAPI-First Design for AI Agents

**What:** Define API contract in OpenAPI 3.0 specification before implementation, with rich descriptions for AI agent reasoning.

**When:** Building APIs consumed by autonomous AI agents that need to discover and reason about capabilities.

**Why:** [Modern API design for AI agents](https://www.xano.com/blog/modern-api-design-best-practices/) requires more than REST conventions - agents need semantic context, consistent schemas, and discoverable workflows.

**Implementation:**
```yaml
# openapi.yaml
openapi: 3.0.0
info:
  title: rwevidence RWE Orchestration API
  version: 1.0.0
  description: |
    Real-World Evidence generation API for causal inference and regulatory reporting.
    Designed for consumption by AI agents orchestrating healthcare data analyses.

paths:
  /propensity_score:
    post:
      summary: Estimate propensity scores
      description: |
        Estimates propensity scores for treatment assignment using various methods.
        Propensity scores balance treatment and control groups in observational studies.

        **When to use:** Before matching or weighting to estimate treatment effects.
        **Returns:** Propensity score estimates, balance diagnostics, and audit trail.
        **Next steps:** Use results in /matching or /iptw endpoints.

      requestBody:
        required: true
        content:
          application/json:
            schema:
              $ref: '#/components/schemas/PropensityScoreRequest'
            examples:
              logistic_simple:
                summary: Simple logistic regression
                value:
                  treatment: "treatment_arm"
                  covariates: ["age", "sex", "baseline_score"]
                  method: "logistic"
              gbm_advanced:
                summary: Gradient boosting with WeightIt
                value:
                  treatment: "treatment_arm"
                  covariates: ["age", "sex", "baseline_score", "comorbidity_count"]
                  method: "gbm"
                  learner_control:
                    n_trees: 1000
                    interaction_depth: 3

      responses:
        '200':
          description: Propensity scores estimated successfully
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/PropensityScoreResponse'
        '400':
          description: Invalid request parameters
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/ErrorResponse'

components:
  schemas:
    PropensityScoreRequest:
      type: object
      required:
        - treatment
        - covariates
      properties:
        treatment:
          type: string
          description: Name of binary treatment variable (0/1 or TRUE/FALSE)
          example: "treatment_arm"
        covariates:
          type: array
          items:
            type: string
          description: List of covariate variable names for adjustment
          example: ["age", "sex", "baseline_score"]
        method:
          type: string
          enum: [logistic, gbm, random_forest, xgboost, super_learner, cbps, ebal]
          default: logistic
          description: |
            Estimation method:
            - logistic: Logistic regression (fast, interpretable)
            - gbm: Gradient boosting (requires WeightIt, high accuracy)
            - random_forest: Random forest (robust to outliers)
            - xgboost: XGBoost (state-of-the-art performance)
            - super_learner: Ensemble of methods (best balance/variance tradeoff)
            - cbps: Covariate Balancing Propensity Score (WeightIt)
            - ebal: Entropy balancing (WeightIt)
```

**Benefits:**
- AI agents can reason about "when to use" from descriptions
- Examples provide concrete guidance for parameter construction
- Enum constraints prevent invalid method selection
- Schema validation catches errors before execution

### Pattern 3: Adapter Pattern for Package Delegation

**What:** Create lightweight adapter modules for each external package (WeightIt, MatchIt, cobalt, admiral) that translate between rwevidence's unified interface and package-specific APIs.

**When:** Integrating multiple packages with different parameter conventions, return structures, and error handling.

**Why:** Prevents "leaky abstractions" where package-specific details bleed into the orchestration layer. [Following admiral's modular architecture](https://pharmaverse.github.io/admiral/), adapters isolate package-specific code.

**Structure:**
```
R/
  adapters/
    weightit_adapter.R      # WeightIt integration
    matchit_adapter.R       # MatchIt integration
    cobalt_adapter.R        # cobalt integration
    admiral_adapter.R       # admiral integration
```

**Example:**
```r
# R/adapters/cobalt_adapter.R

#' Assess Covariate Balance Using cobalt
#'
#' @param match_object Matched data object (from MatchIt or manual matching)
#' @param ps_object Optional propensity score object for IPTW balance
#' @param covariates Character vector of covariates to assess
#' @param ... Additional arguments passed to cobalt::bal.tab()
#' @keywords internal
assess_balance_with_cobalt <- function(match_object = NULL,
                                       ps_object = NULL,
                                       covariates = NULL,
                                       ...) {
  check_required_packages("cobalt")

  # Determine input type and route to appropriate cobalt function
  if (!is.null(match_object) && inherits(match_object, "matchit")) {
    # cobalt integrates natively with MatchIt
    bal_result <- cobalt::bal.tab(match_object,
                                  stats = c("mean.diffs", "variance.ratios"),
                                  ...)
  } else if (!is.null(ps_object) && inherits(ps_object, "weightit")) {
    # cobalt integrates natively with WeightIt
    bal_result <- cobalt::bal.tab(ps_object,
                                  stats = c("mean.diffs", "variance.ratios"),
                                  ...)
  } else {
    # Manual balance assessment
    # Construct formula from covariates
    stop("Manual balance assessment not yet implemented", call. = FALSE)
  }

  # Standardize cobalt output to rwevidence structure
  standardize_cobalt_balance(bal_result)
}

#' Standardize cobalt Balance Results
#' @keywords internal
standardize_cobalt_balance <- function(cobalt_bal) {
  # Extract balance table
  balance_df <- as.data.frame(cobalt_bal$Balance)

  # Standardize column names
  balance_df <- data.frame(
    variable = rownames(balance_df),
    smd_before = balance_df$Diff.Un,
    smd_after = balance_df$Diff.Adj,
    var_ratio_before = balance_df$V.Ratio.Un,
    var_ratio_after = balance_df$V.Ratio.Adj,
    stringsAsFactors = FALSE
  )

  # Calculate summary statistics
  list(
    balance_table = balance_df,
    mean_abs_smd_before = mean(abs(balance_df$smd_before), na.rm = TRUE),
    mean_abs_smd_after = mean(abs(balance_df$smd_after), na.rm = TRUE),
    max_abs_smd_after = max(abs(balance_df$smd_after), na.rm = TRUE),
    balance_threshold_met = max(abs(balance_df$smd_after), na.rm = TRUE) < 0.1,
    package = "cobalt",
    version = as.character(packageVersion("cobalt"))
  )
}
```

**Benefits:**
- Package updates isolated to adapter modules
- Consistent error handling across all packages
- Version tracking for regulatory compliance
- Easy to add new packages (e.g., future integration with PSAgraphics)

### Pattern 4: Session State Externalization

**What:** Keep R processes stateless by externalizing session state to cookies, tokens, or external storage (Redis, file system).

**When:** Multi-step workflows where AI agents make sequential API calls building on prior results.

**Why:** [R is single-threaded](https://www.rplumber.io/articles/execution-model.html) and Plumber's execution model is stateless. Horizontal scaling requires stateless R processes.

**Implementation:**
```r
# R/session_manager.R

#' Create Analysis Session
#'
#' @param session_id Optional session ID (generated if NULL)
#' @param storage_backend "filesystem", "redis", or "s3"
#' @param ttl Time-to-live in seconds (default: 3600 = 1 hour)
#' @keywords internal
create_session <- function(session_id = NULL,
                          storage_backend = "filesystem",
                          ttl = 3600) {
  if (is.null(session_id)) {
    session_id <- generate_session_id()
  }

  session_data <- list(
    id = session_id,
    created_at = Sys.time(),
    expires_at = Sys.time() + ttl,
    datasets = list(),
    analyses = list(),
    version = as.character(packageVersion("rwevidence"))
  )

  # Store externally based on backend
  if (storage_backend == "filesystem") {
    session_dir <- file.path(tempdir(), "rwevidence_sessions", session_id)
    dir.create(session_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(session_data, file.path(session_dir, "session.rds"))
  } else if (storage_backend == "redis") {
    # Redis integration for production (requires redux package)
    check_required_packages("redux")
    redis <- redux::hiredis()
    redis$SET(paste0("session:", session_id), serialize(session_data, NULL))
    redis$EXPIRE(paste0("session:", session_id), ttl)
  }

  session_data
}

#' Load Session from External Storage
#' @keywords internal
load_session <- function(session_id, storage_backend = "filesystem") {
  if (storage_backend == "filesystem") {
    session_file <- file.path(tempdir(), "rwevidence_sessions", session_id, "session.rds")
    if (!file.exists(session_file)) {
      stop("Session not found: ", session_id, call. = FALSE)
    }
    session_data <- readRDS(session_file)
  } else if (storage_backend == "redis") {
    check_required_packages("redux")
    redis <- redux::hiredis()
    session_bytes <- redis$GET(paste0("session:", session_id))
    if (is.null(session_bytes)) {
      stop("Session not found or expired: ", session_id, call. = FALSE)
    }
    session_data <- unserialize(session_bytes)
  }

  # Check expiration
  if (Sys.time() > session_data$expires_at) {
    stop("Session expired: ", session_id, call. = FALSE)
  }

  session_data
}

# Plumber API endpoint
#* Create analysis session
#* @post /session/create
#* @serializer json
function(req, res, ttl = 3600) {
  session <- create_session(ttl = as.numeric(ttl))

  # Set session cookie
  res$setCookie("rwevidence_session", session$id,
                http = TRUE,
                secure = TRUE,
                maxAge = ttl)

  list(
    session_id = session$id,
    created_at = format(session$created_at, tz = "UTC"),
    expires_at = format(session$expires_at, tz = "UTC")
  )
}
```

**Benefits:**
- R processes remain stateless (can scale horizontally)
- Session persistence across R process restarts
- Compatible with load balancers and Kubernetes deployments
- Production-ready with Redis backend

### Pattern 5: Audit Trail as First-Class Citizen

**What:** Every orchestration operation creates immutable audit trail entries capturing inputs, outputs, parameters, package versions, and execution metadata.

**When:** Healthcare/regulatory contexts requiring FDA 21 CFR Part 11 compliance and reproducibility.

**Why:** [FDA's 2026 RWE guidance](https://www.globenewswire.com/news-release/2026/01/21/3222925/0/en/Redefining-Real-World-Evidence-John-Snow-Labs-Introduces-First-FDA-Ready-Patient-Journey-Platform.html) requires clinical facts be "sufficiently complete and accurate" with full provenance tracking.

**Implementation:**
```r
#' Create Audit Trail Entry
#'
#' @param operation Operation name (e.g., "propensity_score_estimation")
#' @param input_data Summary of input data (no PHI)
#' @param output_data Summary of output data
#' @param parameters Analysis parameters
#' @param packages Packages used with versions
#' @keywords internal
create_audit_trail <- function(operation,
                              input_data = list(),
                              output_data = list(),
                              parameters = list(),
                              packages = NULL) {

  # Auto-detect packages if not specified
  if (is.null(packages)) {
    packages <- detect_packages_used(operation)
  }

  audit_entry <- list(
    operation = operation,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
    user = Sys.getenv("USER", "unknown"),
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    platform = R.version$platform,
    rwevidence_version = as.character(packageVersion("rwevidence")),
    packages = packages,
    input_summary = input_data,
    output_summary = output_data,
    parameters = parameters,
    execution_time_ms = NULL,  # Set by wrapper
    success = TRUE,
    warnings = character(),
    errors = character(),
    audit_id = generate_audit_id()
  )

  class(audit_entry) <- c("rwe_audit_trail", "list")
  audit_entry
}

#' Detect Packages Used in Operation
#' @keywords internal
detect_packages_used <- function(operation) {
  # Map operations to packages
  package_map <- list(
    propensity_score_estimation = c("WeightIt", "MatchIt"),
    matching = c("MatchIt"),
    balance_assessment = c("cobalt"),
    iptw = c("WeightIt"),
    doubly_robust = c("WeightIt"),
    adam_derivation = c("admiral")
  )

  pkg_names <- package_map[[operation]]
  if (is.null(pkg_names)) return(list())

  # Get versions of installed packages
  versions <- lapply(pkg_names, function(pkg) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      list(name = pkg, version = as.character(packageVersion(pkg)))
    } else {
      NULL
    }
  })

  Filter(Negate(is.null), versions)
}

# Wrap analysis functions with audit trail
rwe_propensity_score <- function(...) {
  start_time <- Sys.time()
  audit <- create_audit_trail("propensity_score_estimation")

  result <- tryCatch({
    # Actual analysis logic
    ps_result <- perform_ps_estimation(...)
    audit$success <- TRUE
    ps_result
  }, warning = function(w) {
    audit$warnings <- c(audit$warnings, w$message)
    w
  }, error = function(e) {
    audit$success <- FALSE
    audit$errors <- c(audit$errors, e$message)
    stop(e)
  }, finally = {
    audit$execution_time_ms <- as.numeric(difftime(Sys.time(), start_time, units = "secs")) * 1000
  })

  result$audit_trail <- audit
  result
}
```

**Benefits:**
- Full reproducibility for regulatory submissions
- Package version tracking prevents "works on my machine" issues
- Failure diagnosis with captured errors/warnings
- Compliance with FDA 21 CFR Part 11 electronic records requirements

## Anti-Patterns to Avoid

### Anti-Pattern 1: Technical Layering (Orchestration Services)

**What goes wrong:** Creating separate "orchestration services," "business services," and "data services" as technical layers, each managed by different teams.

**Why it's bad:**
- Causes delivery complexity and runtime inefficiency
- No single owner of a capability
- Creates "wrapper services" with no domain value
- [Common microservices anti-pattern](https://www.infoq.com/articles/seven-uservices-antipatterns/) leading to organizational sprawl

**Example of anti-pattern:**
```
rwevidence (orchestration layer)
  ├── rwevidence.matching.orchestrator (technical layer)
  ├── rwevidence.matching.business (technical layer)
  └── rwevidence.matching.data (technical layer)
      └── MatchIt (actual implementation)
```

**Instead:** Keep orchestration minimal - rwevidence functions directly delegate to specialized packages:
```
rwevidence
  ├── rwe_match() → MatchIt adapter → MatchIt
  ├── rwe_propensity_score() → WeightIt adapter → WeightIt
  └── rwe_balance() → cobalt adapter → cobalt
```

**Prevention:** Each rwevidence function owns one business capability end-to-end, delegating only implementation details to packages.

### Anti-Pattern 2: Overambitious API Gateway

**What goes wrong:** Creating a "smart" API layer that makes multiple calls to underlying services, performs complex data transformations, aggregations, and business logic.

**Why it's bad:**
- API becomes a monolith disguised as orchestration
- Business logic scattered between API and domain packages
- Impossible to use packages directly without API
- Violates single responsibility principle

**Example of anti-pattern:**
```r
# BAD: API doing too much
#* @post /full_analysis
function(data, treatment, outcome, covariates) {
  # API orchestrating complex workflow
  ps <- rwe_propensity_score(data, treatment, covariates)
  matched <- rwe_match(ps)
  balance <- rwe_balance(matched)

  # API doing business logic
  if (balance$mean_smd < 0.1) {
    effect <- rwe_estimate_ate(matched, outcome)
    conclusion <- "Balance achieved, treatment effect estimated"
  } else {
    # Retry with different method
    ps2 <- rwe_propensity_score(data, treatment, covariates, method = "gbm")
    matched2 <- rwe_match(ps2)
    effect <- rwe_estimate_ate(matched2, outcome)
    conclusion <- "Retried with GBM method"
  }

  list(effect = effect, conclusion = conclusion)
}
```

**Instead:** API is a thin transport layer, business logic in R functions:
```r
# GOOD: Workflow orchestration in R package
rwe_causal_analysis_workflow <- function(data, treatment, outcome, covariates,
                                         balance_threshold = 0.1) {
  # Business logic in package function
  ps <- rwe_propensity_score(data, treatment, covariates)
  matched <- rwe_match(ps)
  balance <- rwe_balance(matched)

  if (balance$mean_smd >= balance_threshold) {
    warning("Balance threshold not met, retrying with GBM")
    ps <- rwe_propensity_score(data, treatment, covariates, method = "gbm")
    matched <- rwe_match(ps)
  }

  effect <- rwe_estimate_ate(matched, outcome)
  effect
}

# API just calls the workflow
#* @post /causal_analysis
function(req, res) {
  # Thin API layer
  result <- rwe_causal_analysis_workflow(
    data = api_state$dataset,
    treatment = req$body$treatment,
    outcome = req$body$outcome,
    covariates = req$body$covariates
  )
  prepare_for_json(result)
}
```

**Prevention:** API endpoints map 1:1 to R package functions. Complex workflows implemented as R functions, API just handles HTTP/JSON transport.

### Anti-Pattern 3: State in R Process Memory

**What goes wrong:** Storing session state in R global environment variables or package-level environments.

**Why it's bad:**
- Prevents horizontal scaling (can't add more R processes)
- Session lost on R process restart
- Incompatible with load balancers
- [Violates Plumber's stateless execution model](https://www.rplumber.io/articles/execution-model.html)

**Example of anti-pattern:**
```r
# BAD: Global state
.session_cache <- new.env(parent = emptyenv())

#* @post /upload
function(file) {
  data <- read.csv(file$datapath)
  .session_cache$current_data <- data  # WRONG: global state
  list(rows = nrow(data))
}

#* @post /analyze
function(treatment) {
  data <- .session_cache$current_data  # WRONG: assumes state exists
  rwe_propensity_score(data, treatment, ...)
}
```

**Instead:** Externalize state to cookies, tokens, or Redis:
```r
# GOOD: Externalized state
#* @post /upload
function(file, res) {
  data <- read.csv(file$datapath)
  session_id <- create_session()
  save_session_data(session_id, "data", data)

  res$setCookie("rwevidence_session", session_id, maxAge = 3600)
  list(session_id = session_id, rows = nrow(data))
}

#* @post /analyze
function(req, treatment) {
  session_id <- req$cookies$rwevidence_session
  data <- load_session_data(session_id, "data")
  rwe_propensity_score(data, treatment, ...)
}
```

**Prevention:** Use session manager with external storage backend (filesystem for development, Redis for production).

### Anti-Pattern 4: Tight Coupling to Package Internals

**What goes wrong:** Directly using internal (unexported) functions from WeightIt, MatchIt, cobalt, admiral.

**Why it's bad:**
- Internal functions change without notice
- No stability guarantees across package versions
- Breaks on package updates
- Violates package API contracts

**Example of anti-pattern:**
```r
# BAD: Using internal functions
delegate_to_weightit <- function(...) {
  result <- WeightIt::weightit(...)
  # WRONG: Accessing internal structure
  scores <- result$weights_internal  # Unexported field
  diagnostics <- WeightIt:::compute_diagnostics(result)  # ::: accesses internals
  list(scores = scores, diagnostics = diagnostics)
}
```

**Instead:** Use only exported functions and documented object structures:
```r
# GOOD: Public API only
delegate_to_weightit <- function(...) {
  result <- WeightIt::weightit(...)
  # Use documented accessors
  scores <- result$weights  # Documented public field
  # Use exported functions
  summary_obj <- summary(result)  # Exported S3 method
  list(scores = scores, summary = summary_obj)
}
```

**Prevention:** Code review checks for `:::` operator usage. Adapter tests break on package updates, forcing API review.

### Anti-Pattern 5: Leaking Package Abstractions

**What goes wrong:** Returning package-specific objects (matchit, weightit, bal.tab) directly from rwevidence functions, forcing clients to understand multiple package APIs.

**Why it's bad:**
- Clients must learn MatchIt, WeightIt, cobalt, admiral APIs
- Can't swap package implementations without breaking clients
- Defeats purpose of orchestration layer
- AI agents can't reason about inconsistent structures

**Example of anti-pattern:**
```r
# BAD: Returning raw package objects
rwe_match <- function(ps_object, ...) {
  matchit_result <- MatchIt::matchit(...)
  return(matchit_result)  # WRONG: leaks MatchIt object structure
}

# Client must know MatchIt API
result <- rwe_match(...)
matched_data <- MatchIt::match.data(result)  # Client couples to MatchIt
```

**Instead:** Standardize to rwevidence S3 classes:
```r
# GOOD: Standardized result class
rwe_match <- function(ps_object, ...) {
  matchit_result <- MatchIt::matchit(...)

  # Translate to rwevidence structure
  standardize_match_result(matchit_result)
}

standardize_match_result <- function(matchit_obj) {
  structure(
    list(
      matched_data = MatchIt::match.data(matchit_obj),
      match_summary = summary(matchit_obj),
      method = matchit_obj$method,
      n_matched = sum(matchit_obj$weights > 0),
      package = "MatchIt",
      version = as.character(packageVersion("MatchIt")),
      raw_object = matchit_obj  # Keep for advanced users
    ),
    class = c("rwe_matched_cohort", "rwe_analysis")
  )
}

# Client uses consistent rwevidence API
result <- rwe_match(...)
matched_data <- result$matched_data  # Same for all methods
```

**Prevention:** Define S3 classes for all result types. Adapters translate package objects to rwevidence classes.

## Integration Patterns for Specialized Packages

### WeightIt Integration

**Purpose:** Propensity score weighting, IPTW, entropy balancing, covariate balancing propensity scores (CBPS).

**When to delegate:** Methods beyond simple logistic regression (GBM, CBPS, entropy balancing, super learner).

**Key functions:**
- `WeightIt::weightit()` - Main weighting function
- `WeightIt::get.w()` - Extract weights
- `summary.weightit()` - Weight diagnostics

**Adapter pattern:**
```r
# R/adapters/weightit_adapter.R
delegate_to_weightit <- function(data, treatment, covariates, method, estimand = "ATE", ...) {
  check_required_packages("WeightIt")

  # Translate method names
  weightit_method <- switch(method,
    "gbm" = "gbm",
    "cbps" = "cbps",
    "ebal" = "ebal",
    "super_learner" = "super",
    stop("Unsupported WeightIt method: ", method)
  )

  # Build formula
  formula <- as.formula(paste(treatment, "~", paste(covariates, collapse = " + ")))

  # Call WeightIt
  w_obj <- WeightIt::weightit(
    formula = formula,
    data = data,
    method = weightit_method,
    estimand = estimand,
    ...
  )

  # Standardize result
  list(
    weights = w_obj$weights,
    ps = w_obj$ps,  # Some methods provide PS
    method = method,
    estimand = estimand,
    ess = summary(w_obj)$effective.sample.size,
    package_object = w_obj
  )
}
```

**Integration with cobalt:**
```r
# cobalt integrates natively with WeightIt objects
balance_after_weighting <- function(weightit_obj) {
  cobalt::bal.tab(weightit_obj, stats = c("mean.diffs", "variance.ratios"))
}
```

### MatchIt Integration

**Purpose:** Matching algorithms (nearest neighbor, optimal, genetic, full), match quality diagnostics.

**When to delegate:** Matching-based causal inference, creating matched cohorts.

**Key functions:**
- `MatchIt::matchit()` - Main matching function
- `MatchIt::match.data()` - Extract matched dataset
- `summary.matchit()` - Match quality diagnostics

**Adapter pattern:**
```r
# R/adapters/matchit_adapter.R
delegate_to_matchit <- function(data, treatment, covariates, method = "nearest",
                                ratio = 1, caliper = NULL, ...) {
  check_required_packages("MatchIt")

  # Build formula
  formula <- as.formula(paste(treatment, "~", paste(covariates, collapse = " + ")))

  # Call MatchIt
  m_obj <- MatchIt::matchit(
    formula = formula,
    data = data,
    method = method,
    ratio = ratio,
    caliper = caliper,
    ...
  )

  # Extract matched data
  matched_data <- MatchIt::match.data(m_obj)

  # Standardize result
  list(
    matched_data = matched_data,
    n_matched = sum(m_obj$weights > 0),
    n_treated = sum(matched_data[[treatment]] == 1),
    n_control = sum(matched_data[[treatment]] == 0),
    method = method,
    ratio = ratio,
    package_object = m_obj
  )
}
```

**Integration with cobalt:**
```r
# cobalt integrates natively with MatchIt objects
balance_after_matching <- function(matchit_obj) {
  cobalt::bal.tab(matchit_obj, stats = c("mean.diffs", "variance.ratios"))
}
```

### cobalt Integration

**Purpose:** Unified covariate balance assessment across matching, weighting, and subclassification methods.

**When to delegate:** Balance assessment for propensity scores, matching, or IPTW.

**Key functions:**
- `cobalt::bal.tab()` - Balance table (works with MatchIt, WeightIt, data.frames)
- `cobalt::love.plot()` - Love plot visualization
- `cobalt::bal.plot()` - Distributional balance plots

**Why use cobalt as the single balance tool:**
[cobalt documentation](https://ngreifer.github.io/cobalt/) states: "cobalt presents one table in its balance output, and it contains all the information required to assess balance. In comparison, twang and CBPS present two tables, MatchIt presents three tables, and Matching presents as many tables as there are covariates."

**Adapter pattern:**
```r
# R/adapters/cobalt_adapter.R
assess_balance_cobalt <- function(object, covariates = NULL, ...) {
  check_required_packages("cobalt")

  # cobalt::bal.tab() works with matchit, weightit, or data.frames
  bal_result <- cobalt::bal.tab(
    object,
    stats = c("mean.diffs", "variance.ratios"),
    thresholds = c(m = 0.1),  # SMD threshold
    ...
  )

  # Extract balance table
  balance_df <- as.data.frame(bal_result$Balance)

  # Standardize to rwevidence structure
  list(
    balance_table = balance_df,
    mean_abs_smd_before = if ("Diff.Un" %in% names(balance_df)) {
      mean(abs(balance_df$Diff.Un), na.rm = TRUE)
    } else NA,
    mean_abs_smd_after = if ("Diff.Adj" %in% names(balance_df)) {
      mean(abs(balance_df$Diff.Adj), na.rm = TRUE)
    } else NA,
    threshold_met = bal_result$Balanced.mean.diffs,
    package = "cobalt",
    version = as.character(packageVersion("cobalt"))
  )
}
```

### admiral Integration

**Purpose:** ADaM (Analysis Data Model) dataset derivations for regulatory submissions (CDISC compliance).

**When to delegate:** Creating ADSL (subject-level), BDS (basic data structure), or OCCDS (occurrence data structure) datasets for FDA submissions.

**Why admiral matters:** [admiral is the industry-standard](https://pharmaverse.github.io/admiral/) for CDISC-compliant ADaM datasets in R, used in New Drug Applications (NDAs) and Biologics License Applications (BLAs).

**Architecture pattern:** [admiral follows modular architecture](https://pharmaverse.github.io/admiral/):
- Core package: General derivations, utility functions
- TA extensions: Therapeutic area-specific (admiralonco, admiralvaccine)
- Company extensions: Company-specific metadata access

**Integration pattern:**
```r
# R/adapters/admiral_adapter.R

#' Derive ADSL Dataset Using admiral
#'
#' @param sdtm_data List of SDTM datasets (DM, EX, AE, etc.)
#' @param derivations Character vector of derivations to apply
#' @keywords internal
derive_adsl_admiral <- function(sdtm_data, derivations = "standard") {
  check_required_packages("admiral")

  # Start with DM (demographics) as base
  adsl <- sdtm_data$DM

  # Derive standard ADSL variables using admiral
  adsl <- adsl %>%
    admiral::derive_vars_merged(
      dataset_add = sdtm_data$EX,
      by_vars = admiral::exprs(STUDYID, USUBJID),
      new_vars = admiral::exprs(TRTSDTC, TRTEDTC),
      filter_add = EXDOSE > 0
    ) %>%
    admiral::derive_vars_dt(
      new_vars_prefix = "TRTS",
      dtc = TRTSDTC
    ) %>%
    admiral::derive_vars_dt(
      new_vars_prefix = "TRTE",
      dtc = TRTEDTC
    ) %>%
    admiral::derive_vars_duration(
      new_var = TRTDUR,
      start_date = TRTSDT,
      end_date = TRTEDT
    )

  # Add audit metadata
  adsl$ADMIRAL_VERSION <- as.character(packageVersion("admiral"))
  adsl$DERIVED_DTM <- Sys.time()

  structure(
    adsl,
    class = c("rwe_adsl", "data.frame"),
    derivations = derivations,
    admiral_version = as.character(packageVersion("admiral"))
  )
}
```

**Do NOT re-implement admiral:** admiral's derivation functions are validated for regulatory compliance. Delegation maintains validation.

## Scalability Considerations

### At 10 Users (Development/Pilot)

| Concern | Approach |
|---------|----------|
| **API Hosting** | Single Plumber process on development server |
| **Session Storage** | Filesystem-based (tempdir) |
| **Data Volume** | In-memory data frames (<10K rows) |
| **Compute** | Synchronous R execution |
| **Authentication** | Basic API key or no auth |

### At 100 Users (Production Pilot)

| Concern | Approach |
|---------|----------|
| **API Hosting** | Multiple Plumber processes behind nginx load balancer |
| **Session Storage** | Redis for shared session state |
| **Data Volume** | Arrow/Parquet for >100K rows, chunked processing |
| **Compute** | Asynchronous execution with {future} for long-running analyses |
| **Authentication** | OAuth 2.0 with token-based sessions |
| **Rate Limiting** | nginx rate limits per API key |
| **Monitoring** | Prometheus metrics, Grafana dashboards |

### At 1000+ Users (Enterprise Scale)

| Concern | Approach |
|---------|----------|
| **API Hosting** | Kubernetes deployment with auto-scaling pods |
| **Session Storage** | Redis cluster (HA) or managed service (AWS ElastiCache) |
| **Data Volume** | Distributed processing with {sparklyr} or database-backed (DuckDB, PostgreSQL) |
| **Compute** | Job queue system (RabbitMQ + worker pools) for analyses >30s |
| **Authentication** | Enterprise SSO integration (SAML, OIDC) |
| **Rate Limiting** | API Gateway (AWS API Gateway, Kong) with per-user quotas |
| **Monitoring** | Full observability stack (OpenTelemetry, DataDog, Splunk) |
| **Compliance** | Audit log immutable storage (S3 Glacier), encryption at rest/transit |

## Build Order & Dependencies

Recommended phase structure for refactoring to orchestration architecture:

### Phase 1: Core Delegation Layer (Foundation)

**Build first because:** Other layers depend on standardized package adapters.

1. Define S3 classes for standardized results:
   - `rwe_propensity_score`
   - `rwe_matched_cohort`
   - `rwe_balance_assessment`
   - `rwe_iptw`
   - `rwe_doubly_robust`

2. Create adapter modules:
   - `R/adapters/weightit_adapter.R`
   - `R/adapters/matchit_adapter.R`
   - `R/adapters/cobalt_adapter.R`
   - `R/adapters/admiral_adapter.R`

3. Implement audit trail system:
   - `create_audit_trail()`
   - `append_audit_entry()`
   - `serialize_audit_trail()`

**Success criteria:**
- All adapters have unit tests
- Can delegate propensity score estimation to WeightIt
- Can delegate matching to MatchIt
- Can assess balance with cobalt
- Audit trails captured for all operations

### Phase 2: Unified Interface Layer (Orchestration)

**Build second because:** Depends on adapters being complete.

1. Refactor existing rwevidence functions to use adapters:
   - `rwe_propensity_score()` delegates to WeightIt adapter for advanced methods
   - `rwe_match()` delegates to MatchIt adapter
   - `rwe_iptw()` uses WeightIt for weight calculation
   - `rwe_doubly_robust()` composes multiple adapters

2. Implement parameter translation logic:
   - Map rwevidence method names to package-specific methods
   - Validate parameters before delegation
   - Handle package-specific errors gracefully

3. Standardize result transformation:
   - Normalize package objects to rwevidence S3 classes
   - Extract relevant fields
   - Attach audit trails

**Success criteria:**
- All existing rwevidence functions work with delegation
- Unit tests pass with both internal and delegated implementations
- Can switch between packages without API changes
- Audit trails include package versions

### Phase 3: Session Management (Stateful Workflows)

**Build third because:** Depends on unified interface being stable.

1. Implement session manager:
   - `create_session()`
   - `load_session()`
   - `save_session_data()`
   - `delete_session()`

2. Add filesystem backend (development):
   - Session data in `tempdir()/rwevidence_sessions/`
   - TTL-based expiration

3. Add Redis backend (production):
   - Redis integration with {redux}
   - Session replication
   - Automatic expiration

**Success criteria:**
- Can create session, upload data, run multi-step analysis, retrieve results
- Sessions persist across R process restarts (with Redis)
- Sessions expire correctly based on TTL
- Horizontal scaling works (multiple R processes share Redis)

### Phase 4: REST API Enhancement (AI Agent Interface)

**Build fourth because:** Depends on session management for multi-step workflows.

1. Generate OpenAPI 3.0 specification:
   - Rich descriptions for each endpoint
   - Parameter schemas with examples
   - Enum constraints for method selection
   - Response schemas

2. Add session-aware endpoints:
   - `POST /session/create`
   - `POST /session/{id}/upload_data`
   - `POST /session/{id}/propensity_score`
   - `POST /session/{id}/matching`
   - `GET /session/{id}/results`

3. Implement error handling:
   - Standardized error response schema
   - HTTP status codes (400, 404, 500)
   - Detailed error messages for AI agent debugging

**Success criteria:**
- OpenAPI spec validates with Swagger UI
- AI agent can discover capabilities from spec
- Session-based workflows complete successfully
- Error responses include actionable information

### Phase 5: Production Readiness (Deployment)

**Build fifth because:** Depends on all previous layers being complete.

1. Implement authentication:
   - API key validation
   - Token-based sessions
   - OAuth 2.0 integration (optional)

2. Add monitoring:
   - Request/response logging
   - Performance metrics (latency, throughput)
   - Error rate tracking
   - Prometheus endpoint (`/metrics`)

3. Configure deployment:
   - Docker containerization
   - Kubernetes manifests
   - Load balancer configuration
   - Auto-scaling policies

**Success criteria:**
- Can deploy to Kubernetes cluster
- Horizontal scaling works (multiple pods share Redis)
- Monitoring dashboards show real-time metrics
- Authentication enforced for all endpoints

## Decision Matrix: When to Delegate vs. Implement

| Capability | Implement in rwevidence | Delegate to Package | Rationale |
|-----------|-------------------------|---------------------|-----------|
| **Logistic regression PS** | Implement | - | Simple, no dependencies, educational value |
| **GBM/CBPS/Entropy balancing** | - | WeightIt | Complex algorithms, WeightIt is gold standard |
| **Nearest neighbor matching** | Implement (fallback) | MatchIt (primary) | MatchIt has optimized algorithms, keep simple fallback |
| **Optimal/genetic matching** | - | MatchIt | Complex optimization, MatchIt validated |
| **Balance assessment** | - | cobalt | Unified interface across methods, handles all packages |
| **Love plots** | - | cobalt | Visualization standards, ggplot2 integration |
| **ADaM derivations** | - | admiral | Regulatory validation required, industry standard |
| **Data quality checks** | Implement | - | Domain-specific validation rules |
| **Regulatory reporting** | Implement | officer/flextable | Formatting/layout is orchestration concern |

**Rule of thumb:** Delegate when:
1. Package is industry standard (admiral, WeightIt, cobalt)
2. Algorithm complexity high (optimal matching, entropy balancing)
3. Regulatory validation required (ADaM derivations)

Implement when:
1. Simple algorithm (logistic regression)
2. Domain-specific logic (RWE quality dimensions)
3. Integration/formatting task (report generation)

## Sources

### Orchestration Patterns
- [The {targets} R package design specification - Orchestration](https://books.ropensci.org/targets-design/orchestration.html)
- [Multi-Agent AI Orchestration: Enterprise Strategy for 2025-2026](https://www.onabout.ai/p/mastering-multi-agent-orchestration-architectures-patterns-roi-benchmarks-for-2025-2026)
- [Microservices Pattern: Pattern: Saga](https://microservices.io/patterns/data/saga.html)

### Package Integration
- [Covariate Balance Tables and Plots - cobalt](https://ngreifer.github.io/cobalt/)
- [Covariate Balance Tables and Plots: A Guide to the cobalt Package](https://cran.r-project.org/web/packages/cobalt/vignettes/cobalt.html)
- [GitHub - ngreifer/WeightIt](https://github.com/ngreifer/WeightIt)
- [ADaM in R Asset Library - admiral](https://pharmaverse.github.io/admiral/)
- [admiral - pharmaverse](https://pharmaverse.org/e2eclinical/adam/)

### API Design for AI Agents
- [Modern API Design Best Practices for 2026](https://www.xano.com/blog/modern-api-design-best-practices/)
- [How To Prepare Your API for AI Agents](https://thenewstack.io/how-to-prepare-your-api-for-ai-agents/)
- [Rethinking API Design for Agentic AI](https://blogs.mulesoft.com/automation/api-design-for-agentic-ai/)
- [OpenAPI specification for AI agent consumption - Amazon Bedrock](https://docs.aws.amazon.com/bedrock/latest/userguide/agents-api-schema.html)
- [Empowering AI Agents with Tools via OpenAPI](https://devblogs.microsoft.com/semantic-kernel/empowering-ai-agents-with-tools-via-openapi-a-hands-on-guide-with-microsoft-semantic-kernel-agents/)

### RWE Platform Architecture
- [RWE Platform Architecture: Integrating EMR, Claims & Genomics](https://intuitionlabs.ai/articles/rwe-platform-architecture-guide)
- [Redefining Real-World Evidence: John Snow Labs FDA-Ready Patient Journey Platform](https://www.globenewswire.com/news-release/2026/01/21/3222925/0/en/Redefining-Real-World-Evidence-John-Snow-Labs-Introduces-First-FDA-Ready-Patient-Journey-Platform.html)

### R Plumber & API Patterns
- [Runtime - plumber](https://www.rplumber.io/articles/execution-model.html)
- [REST APIs with plumber](https://rstudio.github.io/cheatsheets/html/plumber.html)
- [Wrapping APIs - httr2](https://httr2.r-lib.org/articles/wrapping-apis.html)

### Anti-Patterns
- [Seven Microservices Anti-patterns - InfoQ](https://www.infoq.com/articles/seven-uservices-antipatterns/)
- [10 Microservice Anti-Patterns Every Engineer Must Avoid](https://medium.com/@leela.kumili/10-microservice-anti-patterns-every-engineer-must-avoid-639f068a8249)
- [Emerging technologies in healthcare data integration: A microservices approach](https://wjaets.com/node/665)
