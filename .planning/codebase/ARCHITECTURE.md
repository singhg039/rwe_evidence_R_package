# Architecture

**Analysis Date:** 2026-01-30

## Pattern Overview

**Overall:** Modular Layered Pipeline Architecture

The rwevidence package implements a pharmaceutical research workflow pipeline following a layered functional architecture. Data flows through distinct processing stages (ingest → harmonize → assess quality → analyze → report) with S3 class-based polymorphism for domain objects and extensive logging/auditing throughout.

**Key Characteristics:**
- **Functional pipeline composition**: Each major workflow stage is a separate module with dedicated R files
- **S3 object-oriented design**: Domain entities (rwe_ehr, rwe_propensity_score, rwe_analysis, etc.) wrap data and metadata with domain-specific methods
- **Data immutability pattern**: Functions consume input objects and return new objects; original data is preserved through metadata
- **Cross-cutting logging**: Centralized logging system with context tracking throughout the pipeline
- **Schema-driven validation**: Constants define data schemas, quality thresholds, and terminology mappings applied consistently

## Layers

**Data Ingestion Layer:**
- Purpose: Load and detect various healthcare data formats (OMOP CDM, FHIR, EHR systems, claims data)
- Location: `R/ehr_readers.R`
- Contains: `rwe_read_ehr()` function with format detection, lazy loading, and validation options
- Depends on: S3 classes (rwe_data, rwe_ehr), validation helpers, logging system
- Used by: All downstream analysis functions (they accept rwe_ehr objects or raw data)

**Data Quality & Harmonization Layer:**
- Purpose: Assess data quality across five dimensions (completeness, validity, consistency, timeliness, uniqueness) and standardize data to target schemas
- Location: `R/quality_assessment.R`, `R/harmonization.R`
- Contains:
  - `rwe_assess_quality()` - Multi-dimensional quality scoring with issue detection
  - `rwe_harmonize()` - Schema transformation with terminology mapping
- Depends on: Constants (quality thresholds, schema definitions, terminology maps), S3 classes (rwe_quality_report, rwe_harmonized)
- Used by: Pre-analysis data preparation workflows

**Study Design Layer:**
- Purpose: Define cohort inclusion/exclusion criteria and external control arm generation
- Location: `R/study_design.R`
- Contains: `rwe_external_control()` function for matching external controls to treatment groups
- Depends on: Propensity score estimation, matching algorithms
- Used by: Effectiveness and safety analyses

**Causal Inference Layer:**
- Purpose: Estimate treatment effects while controlling for confounding through propensity scores and inverse probability weighting
- Location: `R/propensity_score.R`, `R/causal_inference.R`
- Contains:
  - `rwe_propensity_score()` - Estimates PS with multiple methods (logistic, GBM, random forest, XGBoost, super learner)
  - `rwe_iptw()` - Inverse probability of treatment weighting with stabilization options
  - `rwe_doubly_robust()` - Doubly robust estimation combining propensity scores and outcome modeling
- Depends on: Machine learning packages (gbm, randomForest, xgboost), balance assessment utilities
- Used by: Treatment effect and survival analyses

**Analysis Layer:**
- Purpose: Conduct effectiveness, safety, and survival analyses with standardized outputs
- Location: `R/effectiveness_analysis.R`, `R/survival_analysis.R`, `R/safety_surveillance.R`
- Contains:
  - `rwe_relative_risk()`, `rwe_odds_ratio()`, `rwe_rate_ratio()` - Effect measures
  - `rwe_cox_regression()`, `rwe_kaplan_meier()` - Time-to-event analysis
  - `rwe_incidence_rate()` - Safety rate calculations
  - `rwe_analyze_safety()` - Comprehensive safety assessment
- Depends on: Effectiveness calculation helpers (.extract_analysis_frame, .coerce_binary, .prepare_analysis_inputs), survival package, statistical test functions
- Used by: Regulatory reporting workflows

**Reporting & Visualization Layer:**
- Purpose: Generate regulatory-compliant tables, figures, and reports for submission
- Location: `R/regulatory_reporting.R`, `R/results_tables.R`, `R/rwe_figures.R`
- Contains:
  - `rwe_table_one()` - Baseline characteristics with statistical tests and SMD
  - `rwe_results_table()` - Formatted analysis results tables
  - `rwe_consort_diagram()`, `rwe_figure_forest()`, `rwe_figure_survival()` - Publication figures
  - `rwe_generate_regulatory_report()` - Bundle all results for submission
- Depends on: Report formatting packages (flextable, officer, gt), visualization (ggplot2)
- Used by: Final regulatory submission workflows

**Utility Layers:**
- **Constants & Configuration** (`R/constants.R`): OMOP CDM 5.4 schemas, quality thresholds, supported data formats, terminology mappings, analysis method options
- **Helpers & Validation** (`R/helpers.R`): Input validation, data structure checking, coercion utilities
- **Data Classes** (`R/s3_classes.R`): S3 class definitions (rwe_data, rwe_ehr, rwe_quality_report, rwe_harmonized, rwe_matched_cohort, rwe_analysis)
- **Logging** (`R/logging.R`): Structured logging with context, log history, and export

## Data Flow

**Standard RWE Study Workflow:**

1. **Data Loading**: `rwe_read_ehr()` loads source data (CSV, database, API) → returns `rwe_ehr` object
2. **Quality Assessment**: `rwe_assess_quality()` analyzes data quality dimensions → returns `rwe_quality_report`
3. **Harmonization**: `rwe_harmonize()` maps source schema to OMOP CDM or target format → returns `rwe_harmonized`
4. **Cohort Definition**: `rwe_external_control()` selects and matches patients based on criteria → returns `rwe_external_control` object
5. **Propensity Scoring**: `rwe_propensity_score()` estimates treatment propensity → returns `rwe_propensity_score`
6. **Weighting/Matching**: `rwe_iptw()` or matching functions balance treatment groups → returns `rwe_iptw` or `rwe_matched_cohort`
7. **Effectiveness Analysis**: `rwe_cox_regression()` + `rwe_kaplan_meier()` estimate treatment effect → returns `rwe_survival` objects
8. **Safety Analysis**: `rwe_analyze_safety()` assess adverse events → returns `rwe_safety_analysis`
9. **Table & Figures**: `rwe_table_one()`, `rwe_figure_forest()` format results → return display objects
10. **Regulatory Report**: `rwe_generate_regulatory_report()` bundles all components → returns `rwe_regulatory_report`

**State Management:**
- Objects maintain immutability: each function returns a new object with original data in `$data` field and metadata/results in other fields
- Metadata is enriched at each stage (source, format, validation results, timestamps, audit trail)
- rwe_data is the base class; specialized classes inherit from it (rwe_ehr extends rwe_data, rwe_harmonized extends rwe_data, etc.)
- Analysis results stored in dedicated object fields (e.g., rwe_analysis contains $primary_analysis, $method, $analyzed_at)

## Key Abstractions

**rwe_data:**
- Purpose: Universal base class for all data objects in the package
- Implementation: `R/s3_classes.R` lines 23-35
- Pattern: Structure with `$data` (tibble/data frame), `$metadata` (list), optional `$schema`, `$mapping_config`, `$validation`
- Usage: All read_* functions return rwe_data subclasses; all analysis functions accept rwe_data or raw data

**rwe_ehr:**
- Purpose: Wrapper for EHR data with format and schema information
- Implementation: `R/s3_classes.R` lines 102-136
- Pattern: Extends rwe_data with `$schema` field for table/column specifications
- Usage: Output of `rwe_read_ehr()`; input to `rwe_harmonize()`, `rwe_assess_quality()`

**rwe_quality_report:**
- Purpose: Structured quality assessment results with scoring, issues, and recommendations
- Implementation: `R/s3_classes.R` lines 154-205
- Pattern: Contains `$dimensions` (list of dimension results), `$overall_score`, `$issues`, `$recommendations`, `$timestamp`
- Usage: Output of `rwe_assess_quality()`

**rwe_harmonized:**
- Purpose: Transformed data with mapping provenance and validation results
- Implementation: `R/s3_classes.R` lines 223-263
- Pattern: Extends rwe_data with `$mapping_config` and `$validation` fields tracking transformation history
- Usage: Output of `rwe_harmonize()`

**rwe_propensity_score:**
- Purpose: Propensity score model with predictions and covariate information
- Implementation: `R/propensity_score.R`
- Pattern: Contains fitted model object, propensity_score column in data, method, treatment, covariates metadata
- Usage: Input to `rwe_iptw()`, used for balance assessment

**rwe_analysis (and subclasses):**
- Purpose: Base class for all analysis results (rwe_survival, rwe_relative_risk, rwe_safety_analysis, etc.)
- Implementation: `R/s3_classes.R` lines 339-364
- Pattern: Contains `$primary_analysis` (results data frame), `$method`, `$analyzed_at`
- Usage: All analysis functions return rwe_analysis subclasses with print/summary methods

## Entry Points

**User-Facing Workflows:**
- Location: Vignettes in `vignettes/` (complete-workflow.Rmd, case-study-oncology.Rmd) demonstrate end-to-end usage
- Triggers: User calls `library(rwevidence)` followed by pipeline of function calls
- Responsibilities: Each function processes data and returns next stage input

**Data Loading:**
- Location: `R/ehr_readers.R` - `rwe_read_ehr()`
- Triggers: Called first to load source data
- Responsibilities: Detect format, validate against schema, return rwe_ehr object

**Quality Assessment:**
- Location: `R/quality_assessment.R` - `rwe_assess_quality()`
- Triggers: Called after data loading for data preparation
- Responsibilities: Score data quality across five dimensions, identify issues, recommend fixes

**Analysis Entry Points:**
- Location: `R/effectiveness_analysis.R`, `R/survival_analysis.R`
- Functions: `rwe_cox_regression()`, `rwe_relative_risk()`, `rwe_kaplan_meier()`
- Triggers: Called to estimate treatment effects
- Responsibilities: Perform statistical analysis and return domain-specific result objects

**Reporting Entry Point:**
- Location: `R/regulatory_reporting.R` - `rwe_generate_regulatory_report()`
- Triggers: Called as final step to bundle analysis results
- Responsibilities: Collect all analysis objects, format tables/figures, export regulatory-compliant report

## Error Handling

**Strategy:** Explicit error checking with informative messages; logging of errors for audit trail

**Patterns:**
1. **Input validation first**: All functions call `validate_inputs()` on data frames before processing
2. **Type checking**: Explicit `is.data.frame()`, `inherits(x, "rwe_data")` checks before operations
3. **Column existence checks**: `required_cols %in% names(data)` validation before subset operations
4. **Informative stop() calls**: Error messages include variable names and context (e.g., `stop("Treatment variable '", treatment, "' not found in data", call. = FALSE)`)
5. **Try-catch for external operations**: Database reads and API calls wrapped in `tryCatch()` with `log_error()` calls
6. **Logging context**: All errors logged through `log_error(message, context = "function_name")`

## Cross-Cutting Concerns

**Logging:**
- Framework: Custom logging system in `R/logging.R`
- Approach: Functions call `log_info()`, `log_debug()`, `log_warning()`, `log_error()` with context parameter
- History maintained in memory; exportable via `export_audit_trail()`

**Validation:**
- Approach: `validate_inputs(data, required_cols = c(...))` called at function entry
- Data structure validation: `check_data_structure(data, expected_schema)` for schema compliance
- Statistical assumptions: P.S. overlap check (`calculate_ps_overlap()`), balance assessment (`assess_balance()`)

**Authentication:**
- For database connections: Credentials passed through DBI connection objects
- For API endpoints: `add_api_auth()` utility function in inst/api/
- No credentials stored in code; environment variables expected

**Metadata & Audit:**
- Every rwe_data object includes `$metadata$created_at` timestamp
- Analysis results include `analyzed_at`, method, and parameter information
- `create_audit_trail()` function generates provenance history for regulatory submissions
