# Technology Stack

**Analysis Date:** 2026-01-30

## Languages

**Primary:**
- R 4.1.0+ - Core package language for all statistical analysis, data manipulation, and RWE workflows

## Runtime

**Environment:**
- R (>= 4.1.0) - Execution environment

**Package Manager:**
- CRAN - Package installation and management
- Lockfile: DESCRIPTION file (standard R package format)

## Frameworks

**Core Analysis:**
- `dplyr` (>= 1.1.0) - Data manipulation and transformation pipeline
- `ggplot2` (>= 3.4.0) - Visualization and graphics generation
- `zoo` (>= 1.8-12) - Time series and regular/irregular data handling

**Statistical Analysis:**
- `survival` (>= 3.5-0) - Kaplan-Meier, Cox proportional hazards regression
- `caret` (>= 6.0-94) - Machine learning model training and evaluation
- `mlr3` - Machine learning framework
- `randomForest` (>= 4.7-1) - Random forest models
- `xgboost` (>= 1.7.0) - Gradient boosting machine models
- `gbm` - Gradient boosting machines

**Causal Inference & Propensity Scoring:**
- `MatchIt` (>= 4.5.0) - Propensity score matching algorithms
- `WeightIt` (>= 0.14.0) - Weighting methods for covariate balance
- `cobalt` (>= 4.5.0) - Balance assessment and covariate diagnostics

**Imputation & Missing Data:**
- `mice` (>= 3.15.0) - Multiple imputation by chained equations
- `naniar` (>= 1.0.0) - Missing data visualization and analysis

**Regulatory Reporting:**
- `admiral` - ADaM dataset generation framework (FDA/EMA compliance)
- `rtables` - Regulatory table generation
- `tern` - Tabulation and reporting utilities
- `gt` (>= 0.10.0) - Table formatting for reports
- `officer` (>= 0.6.0) - PowerPoint/Word document generation
- `flextable` (>= 0.9.0) - Flexible table formatting
- `kableExtra` (>= 1.3.0) - Advanced table formatting
- `xtable` (>= 1.8-4) - LaTeX table formatting
- `htmltools` - HTML utilities for report generation
- `base64enc` - Base64 encoding for embedded content

**Data Quality & Validation:**
- `validate` (>= 1.1.0) - Data validation rules engine
- `pointblank` (>= 0.11.0) - Data validation and quality assessment

**API & Web:**
- `plumber` (>= 1.2.0) - REST API framework for exposing functions as endpoints
- `httr2` (>= 1.0.0) - HTTP client for API calls
- `jsonlite` - JSON serialization/deserialization

**Database Access:**
- `DBI` (>= 1.1.0) - Database interface abstraction layer
- `RPostgres` (>= 1.4.0) - PostgreSQL driver
- `odbc` (>= 1.3.0) - ODBC database connections
- `arrow` (>= 10.0.0) - Apache Arrow/Parquet file handling and lazy evaluation
- `data.table` (>= 1.14.0) - High-performance data manipulation
- `readr` - CSV and delimited file reading

**Documentation & Visualization:**
- `knitr` - Dynamic document generation
- `rmarkdown` - R Markdown document format
- `pkgdown` (>= 2.0.0) - Website generation from documentation
- `DiagrammeR` - Diagram and flowchart generation
- `covr` (>= 3.6.0) - Code coverage measurement

**Testing:**
- `testthat` (>= 3.0.0) - Unit testing framework
- `withr` (>= 2.5.0) - Safe temporary state changes for testing

**Configuration:**
- `yaml` - YAML configuration file parsing

## Key Dependencies

**Critical:**
- `dplyr` - Foundation of data transformation workflows, used extensively in data harmonization and analysis
- `survival` - Enables core survival analysis functions (`rwe_cox_regression`, `rwe_kaplan_meier`)
- `MatchIt` & `WeightIt` - Implements propensity score matching for causal inference
- `plumber` - Enables API exposure of package functions via REST endpoints
- `DBI` + `RPostgres` / `odbc` - Database connectivity for EHR data sources
- `httr2` - REST API client for reading data from remote endpoints
- `arrow` - Enables lazy evaluation for large datasets; supports Parquet format
- `mice` - Multiple imputation for missing data handling

**Infrastructure:**
- `admiral` - FDA/EMA compliant dataset generation
- `rtables` + `tern` - Regulatory table generation standards
- `officer` + `flextable` - Report generation and document formatting
- `validate` + `pointblank` - Data quality assessment frameworks
- `knitr` + `rmarkdown` - Documentation generation

## Configuration

**Environment:**
- R package configured via `DESCRIPTION` file (standard R package manifest)
- Logging: Configured via `setup_logger()` function in `R/logging.R`
  - Log levels: DEBUG, INFO, WARNING, ERROR
  - Optional file output for audit trails
  - Console output configurable
- API configuration: Managed via `plumber.R` in `inst/api/`
  - CORS enabled by default
  - JSON serialization for all responses
  - In-memory state management via `api_state` environment

**Key configs required:**
- Database connection strings (for DBI connections)
- API endpoints and authentication tokens (for remote data sources)
- File paths or URLs for EHR data sources

**Build:**
- DESCRIPTION - R package metadata and dependencies
- NAMESPACE - Exported functions and imports
- _pkgdown.yml - Website generation configuration

## Platform Requirements

**Development:**
- R >= 4.1.0
- RTools (Windows) or equivalent C/C++ compiler for packages like `arrow`, `xgboost`
- Git for version control

**Production:**
- R >= 4.1.0 runtime
- PostgreSQL driver (RPostgres) if connecting to PostgreSQL
- ODBC drivers if using ODBC connections
- Apache Arrow libraries (included with arrow package)
- Plumber server for API deployment

---

*Stack analysis: 2026-01-30*
