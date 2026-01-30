# Codebase Structure

**Analysis Date:** 2026-01-30

## Directory Layout

```
rwe_evidence_R_package/
├── R/                          # Core package functions (19 modules)
│   ├── rwevidence-package.R    # Package documentation & namespace setup
│   ├── s3_classes.R            # S3 class definitions (rwe_data, rwe_ehr, etc.)
│   ├── constants.R             # Package-wide constants & configurations
│   ├── logging.R               # Logging system
│   ├── helpers.R               # Utility functions
│   ├── ehr_readers.R           # Data ingestion (read_ehr, format detection)
│   ├── harmonization.R         # Data transformation & schema mapping
│   ├── quality_assessment.R    # Data quality scoring (5 dimensions)
│   ├── study_design.R          # Cohort definition & external control arms
│   ├── imputation.R            # Missing data handling
│   ├── propensity_score.R      # Propensity score estimation
│   ├── causal_inference.R      # IPTW, doubly robust, ATE estimation
│   ├── effectiveness_analysis.R # Treatment effect measures (RR, OR, RR)
│   ├── survival_analysis.R     # Time-to-event analysis (Cox, KM)
│   ├── safety_surveillance.R   # Adverse event monitoring & rates
│   ├── regulatory_reporting.R  # Table 1, results tables, regulatory outputs
│   ├── results_tables.R        # Formatted result table generation
│   ├── rwe_figures.R           # Publication figures (forest, survival, etc.)
│   └── data.R                  # Package documentation for data objects
│
├── man/                         # Auto-generated documentation (181 .Rd files)
│   └── [function_name].Rd       # One .Rd per exported function
│
├── tests/
│   ├── testthat.R              # Test harness setup
│   └── testthat/               # Unit and integration tests
│       ├── test-s3_classes.R
│       ├── test-ehr_readers.R
│       ├── test-harmonization.R
│       ├── test-quality_assessment.R
│       ├── test-study_design.R
│       ├── test-imputation.R
│       ├── test-propensity_score.R
│       ├── test-causal_inference.R
│       ├── test-effectiveness_analysis.R
│       ├── test-survival_analysis.R
│       ├── test-safety_surveillance.R
│       ├── test-regulatory_reporting.R
│       ├── test-results_tables.R
│       ├── test-rwe_figures.R
│       ├── test-helpers.R
│       └── Rplots.pdf          # Generated during test run
│
├── vignettes/                   # User-facing documentation & tutorials
│   ├── getting-started.Rmd      # Quick start guide
│   ├── complete-workflow.Rmd    # End-to-end pipeline example
│   ├── case-study-oncology.Rmd  # Real-world case study (NSCLC)
│   ├── validation-diagnostics.Rmd # Quality & propensity score diagnostics
│   └── .gitignore               # Ignore generated HTML vignettes
│
├── inst/                        # Installation-time supporting files
│   ├── api/
│   │   └── plumber.R           # REST API server for package functions
│   ├── examples/
│   │   └── case_studies/
│   │       └── oncology_nsclc_case_study.R # Full case study code
│   └── validation/
│       └── run_validation.R    # Validation suite runner
│
├── data/                        # Pre-processed package datasets (none currently)
│
├── data-raw/                    # Scripts to generate package data
│   └── create_example_data.R    # Generate example datasets for vignettes
│
├── .planning/
│   └── codebase/                # Analysis documents for code orchestration
│       ├── STACK.md
│       ├── INTEGRATIONS.md
│       ├── CONVENTIONS.md
│       ├── TESTING.md
│       ├── ARCHITECTURE.md      # THIS FILE
│       └── STRUCTURE.md
│
├── DESCRIPTION                  # Package metadata (version, dependencies)
├── NAMESPACE                    # Auto-generated; defines exports & imports
├── README.md                    # User-facing package overview
├── LICENSE                      # MIT license reference
├── LICENSE.md                   # Full license text
├── NEWS.md                      # Release notes
├── PACKAGE_STATUS.md            # Development status & checklist
├── CRAN_RELEASE_CHECKLIST.md   # CRAN submission checklist
├── cran-comments.md             # CRAN release notes
├── _pkgdown.yml                 # Website configuration
├── benchmark_performance.R      # Performance testing script
├── .gitignore                   # Git ignore patterns
├── .gitattributes               # Git attributes for CRAN
└── .Rbuildignore                # Files to exclude from R build

```

## Directory Purposes

**R/ (Core Logic):**
- Purpose: All package functions organized by functional domain
- Contains: 19 R modules covering ingestion, transformation, analysis, and reporting
- Key files: `constants.R` (configuration), `s3_classes.R` (domain objects), `helpers.R` (shared utilities)

**man/ (Documentation):**
- Purpose: Auto-generated Roxygen2 documentation for all exported functions
- Contains: 181 .Rd files (one per function)
- Generated from: Roxygen2 comments in R/ files

**tests/testthat/ (Unit & Integration Tests):**
- Purpose: Test coverage for all major functions and edge cases
- Contains: 15 test-*.R files covering each module
- Pattern: Test framework is testthat (ver 3+); uses `test_that()` blocks with `expect_*()` assertions

**vignettes/ (User Documentation):**
- Purpose: Tutorial and case study documentation in R Markdown format
- Contains: 4 vignettes (getting-started, complete-workflow, case-study-oncology, validation-diagnostics)
- Execution: Knitted to HTML during `devtools::build()` or `pkgdown::build_site()`

**inst/ (Installation Resources):**
- Purpose: Non-R files bundled with package installation
- Contains:
  - `api/plumber.R`: REST API exposing package functions
  - `examples/`: Standalone example scripts for users
  - `validation/`: Validation suite for post-installation testing

**data-raw/ (Data Generation):**
- Purpose: Scripts that generate package data sets (if any)
- Contains: `create_example_data.R` for vignette examples

**.planning/codebase/ (Code Orchestration):**
- Purpose: Analysis documents for code generation and refactoring workflows
- Contains: STACK.md, INTEGRATIONS.md, ARCHITECTURE.md, STRUCTURE.md, CONVENTIONS.md, TESTING.md

## Key File Locations

**Entry Points:**
- Package load: `R/rwevidence-package.R` - Package declaration, imports, global variables
- Data loading: `R/ehr_readers.R` - `rwe_read_ehr()` is typical starting point
- Quality checks: `R/quality_assessment.R` - `rwe_assess_quality()`
- Analysis: `R/effectiveness_analysis.R`, `R/survival_analysis.R`
- Reporting: `R/regulatory_reporting.R` - `rwe_generate_regulatory_report()`

**Configuration:**
- Constants: `R/constants.R` - OMOP CDM schema, quality thresholds, terminology maps, method options
- Package metadata: `DESCRIPTION` - Dependencies, version, authors
- Exports: `NAMESPACE` - Auto-generated; defines public API

**Core Logic:**
- Class definitions: `R/s3_classes.R` - Base class hierarchy
- Input validation: `R/helpers.R` - `validate_inputs()`, `check_data_structure()`
- Data transformation: `R/harmonization.R` - Schema mapping, terminology standardization
- Propensity scores: `R/propensity_score.R` - PS estimation with 5 methods
- Causal inference: `R/causal_inference.R` - IPTW, doubly robust estimation
- Statistical tests: `R/effectiveness_analysis.R` - Effect measures (RR, OR, IR)
- Survival analysis: `R/survival_analysis.R` - Cox regression, Kaplan-Meier
- Safety: `R/safety_surveillance.R` - Adverse event rates and signals
- Reporting: `R/regulatory_reporting.R` - Table 1, results tables

**Testing:**
- Test configuration: `tests/testthat.R` - Test harness setup
- Per-module tests: `tests/testthat/test-*.R` - 15 test files matching R/ modules
- Test data: Generated inline in test-*.R files using `data.frame()` constructor

## Naming Conventions

**Files:**
- Module files: lowercase with underscores (e.g., `effectiveness_analysis.R`)
- Test files: `test-[module_name].R` matching R/ modules (e.g., `test-effectiveness_analysis.R`)
- Documentation: Auto-generated as `[function_name].Rd` in man/
- Vignettes: kebab-case with .Rmd extension (e.g., `case-study-oncology.Rmd`)

**Functions:**
- Public functions: `rwe_[action]_[object]()` (e.g., `rwe_read_ehr()`, `rwe_propensity_score()`)
- Internal helpers: Prefixed with `.` (e.g., `.extract_analysis_frame()`, `.coerce_binary()`)
- Constructor functions: `new_rwe_[class]()` (e.g., `new_rwe_ehr()`, `new_rwe_harmonized()`)
- Checker functions: `is_rwe_[class]()` (e.g., `is_rwe_data()`, `is_rwe_ehr()`)
- Logging: `log_[level]()` (e.g., `log_info()`, `log_error()`)

**Classes (S3):**
- Base class: `rwe_data`
- Domain classes: `rwe_ehr`, `rwe_quality_report`, `rwe_harmonized`, `rwe_propensity_score`
- Analysis classes: `rwe_survival`, `rwe_relative_risk`, `rwe_odds_ratio`, `rwe_safety_analysis`
- Report classes: `rwe_table_one`, `rwe_results_table`, `rwe_regulatory_report`
- All are lowercase with underscores (e.g., `rwe_external_control`, `rwe_matched_cohort`)

**Variables:**
- camelCase: `propensity_score`, `treatment_data`, `matching_vars`, `overall_quality_score`
- Convention: Underscores separate words; lowercase throughout
- Constant names (in constants.R): UPPERCASE_WITH_UNDERSCORES (e.g., `OMOP_CDM_5.4_SCHEMA`, `DEFAULT_QUALITY_THRESHOLDS`)

## Where to Add New Code

**New Feature (analysis method or data source):**
- Primary code location:
  - For new analysis: Create function in appropriate existing module (`R/effectiveness_analysis.R`, `R/survival_analysis.R`, etc.) or new `R/[method]_analysis.R`
  - For new data format: Add reader logic to `R/ehr_readers.R` or new `R/[format]_readers.R`
  - For new harmonization: Extend `R/harmonization.R` with terminology maps in `R/constants.R`
- Test location: Create/extend `tests/testthat/test-[module].R`
- Documentation: Roxygen2 comments in function definition (no separate .Rd files needed)
- Update: `R/rwevidence-package.R` if adding new S3 methods (@export in roxygen comment)

**New Component/Module:**
- Create new file: `R/[module_name].R` following naming convention
- Define classes: If introducing new domain object, add S3 constructor to `R/s3_classes.R`
- Add constants: If method options needed, extend `R/constants.R` (e.g., new ANALYSIS_METHODS option)
- Create tests: `tests/testthat/test-[module_name].R`
- Update documentation: Roxygen2 comments with `@export`, `@param`, `@return`, `@examples`
- Update NAMESPACE: Auto-regenerated by `devtools::document()`

**Utilities & Helpers:**
- Shared helper functions: `R/helpers.R` for general input validation, coercion, data extraction
- Internal helpers (dot-prefixed): Place in module where used (e.g., `.extract_analysis_frame()` in `effectiveness_analysis.R`)
- Constants: Centralized in `R/constants.R` - no hardcoded values in function bodies

**Vignette/Tutorial:**
- Location: `vignettes/[title].Rmd` (e.g., `vignettes/my-new-workflow.Rmd`)
- Format: R Markdown with YAML header including `%\VignetteIndexEntry{}` and `%\VignetteEngine{knitr::rmarkdown}`
- Execution: Marked `eval = FALSE` for code blocks that require external data
- Build: Automatic during `pkgdown::build_site()` or `devtools::build_vignettes()`

## Special Directories

**inst/api/ (REST API):**
- Purpose: Expose package functions as HTTP endpoints via Plumber
- Contents: `plumber.R` defines routes mapping to rwevidence functions
- Execution: Standalone R script; users call `source("inst/api/plumber.R")` and `pr$run()`
- Generated: Not committed to repo (development-only)
- Usage: For web UI or external system integration

**inst/examples/case_studies/ (Real-World Examples):**
- Purpose: Complete, runnable case studies demonstrating package workflows
- Contents: `oncology_nsclc_case_study.R` shows full RWE generation pipeline
- Execution: Standalone R script; users `source()` or copy code
- Generated: No
- Usage: Learning reference; basis for vignettes

**inst/validation/ (Post-Install Validation):**
- Purpose: Verify package installation and dependencies are correct
- Contents: `run_validation.R` runs diagnostic checks
- Execution: User-initiated `source("inst/validation/run_validation.R")`
- Generated: No
- Usage: Quality assurance after installation

**data/ (Package Data):**
- Purpose: Pre-processed data objects included with package
- Contents: Currently empty (examples generated in vignettes)
- Generated: Yes, if data-raw scripts are run
- Committed: Yes (minimal size)
- Access: Via `data("dataset_name")` in user code

**data-raw/ (Data Generation Scripts):**
- Purpose: Scripts to build datasets for package data/ directory
- Contents: `create_example_data.R` generates example patient datasets
- Generated: No (source scripts)
- Committed: Yes
- Usage: `devtools::load_all()` then `source("data-raw/create_example_data.R")`

**.planning/codebase/ (Code Orchestration):**
- Purpose: Analysis documents for automated code generation workflows
- Contents: STACK.md, INTEGRATIONS.md, ARCHITECTURE.md, STRUCTURE.md, CONVENTIONS.md, TESTING.md
- Generated: No (manually created)
- Committed: Yes
- Usage: Consumed by `/gsd:plan-phase` and `/gsd:execute-phase` commands
