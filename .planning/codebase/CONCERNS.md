# Codebase Concerns

**Analysis Date:** 2026-01-30

## Skipped Tests - Known Issues

### Causal Inference Module
**Issue Area:** Propensity score trimming and IPTW calculations
- **Files:** `R/causal_inference.R`, `tests/testthat/test-causal_inference.R`
- **Skipped Tests:** 4 tests (lines 64, 69, 101, 256 in test file)
  - "Stabilized weights test temporarily disabled - needs PS trimming fix"
  - "Weight truncation test temporarily disabled - needs PS trimming fix"
  - "ATE estimation tests temporarily disabled - needs further debugging"
  - "ATE print test temporarily disabled - needs further debugging"
- **Impact:** Core treatment effect estimation functionality may produce unreliable weights for extreme propensity scores; extreme PS values (near 0 or 1) can cause weight inflation or numerical instability
- **Current Behavior:** Code has basic PS trimming (line ~100 in causal_inference.R: `ps <- pmax(0.01, pmin(ps, 0.99))`) but tests indicate this may not be sufficient
- **Fix Approach:**
  1. Implement proper stabilized weight calculation with robust variance estimation
  2. Add sensitivity analysis for weight truncation thresholds
  3. Verify convergence in binary outcome models before enabling tests
  4. Add diagnostics for weight distribution extremes

### Binary Outcome Handling
**Issue Area:** Doubly robust estimation with binary outcomes
- **Files:** `R/causal_inference.R`, `tests/testthat/test-causal_inference.R`
- **Skipped Tests:** 2 tests (lines 354, 359)
  - "Binary outcome tests temporarily disabled due to convergence issues"
  - "rwe_doubly_robust works with binary outcomes"
- **Impact:** Users may encounter model convergence failures when applying doubly robust methods to binary outcomes; produces cryptic error messages
- **Cause:** Logistic regression convergence issues in outcome modeling within doubly robust estimation
- **Fix Approach:**
  1. Add convergence checking with informative error messages
  2. Implement fallback to simpler modeling approaches
  3. Add quasi-separation detection
  4. Provide user guidance on data requirements for binary outcomes

## Test Coverage Gaps

### Optional Package Dependencies
**Issue Area:** 6 conditional test skips for missing optional packages
- **Files:** `tests/testthat/test-*.R` (multiple files)
- **Skipped Tests:**
  - `test-effectiveness_analysis.R:94,124` - ggplot2 not installed (forest plots)
  - `test-imputation.R:399,427` - FNN, xgboost not installed (advanced imputation)
  - `test-propensity_score.R:42,68` - xgboost, caret not installed (ML methods)
  - `test-regulatory_reporting.R:858,938-940` - htmltools, ggplot2, survival, zoo
  - `test-results_tables.R:67,97` - ggplot2, zoo
  - `test-rwe_figures.R:7` - ggplot2
- **Impact:** Incomplete test coverage for optional features; functionality is tested only if packages are installed in test environment
- **Affected Functions:** Forest plot generation, advanced imputation methods, survival analysis reporting
- **Fix Approach:**
  1. Create mock/stub tests for optional package features when unavailable
  2. Document minimum package requirements for each feature
  3. Add integration tests in CI/CD for all optional combinations
  4. Implement more graceful fallback implementations

## Performance Considerations

### Nested Loop Inefficiency in Table Generation
**Issue Area:** Nested iteration over groups and variables
- **Files:** `R/regulatory_reporting.R` (lines ~100-200, ~300-400)
- **Pattern:** Double nested `for` loops in `rwe_table_one()` - iterates over continuous variables × groups, then categorical variables × groups
- **Impact:** Performance degrades with large datasets (>100K rows) or many variables (>50); time complexity is O(n × p × g) where n=rows, p=variables, g=groups
- **Current Benchmarks:**
  - Data quality assessment on 1K rows: ~0.009 seconds ✓
  - Propensity scoring on 1K rows: ~0.006 seconds ✓
  - Kaplan-Meier on 1K rows: ~0.376 seconds (acceptable)
  - Table 1 generation: not explicitly benchmarked in codebase
- **Scaling Limits:** Estimated to struggle with n>100K when generating comprehensive baseline tables
- **Fix Approach:**
  1. Vectorize group statistics calculations using `tapply()`, `by()`, or `data.table`
  2. Consider `data.table` backend for large datasets
  3. Profile with real-world datasets to identify bottlenecks
  4. Add progress indicators for long-running operations

## Fragile Areas - Weak Input Validation

### API Data Reading Error Handling
**Issue Area:** Limited recovery from API failures
- **Files:** `R/ehr_readers.R` (lines 400-600)
- **Concerns:**
  - API errors return generic messages without context (e.g., status codes not always included)
  - JSON parsing failures not differentiated from network failures
  - Pagination logic assumes consistent API response structure (line ~530)
  - No validation of API response schema before data frame conversion
- **Fragility:** API changes or malformed responses can cause cryptic failures
- **Example:** Line 468: `stop("API request failed with status: ", httr2::resp_status(resp), ...)` works but response validation is minimal
- **Fix Approach:**
  1. Add schema validation for API responses using jsonlite or validateSQL patterns
  2. Implement exponential backoff for transient failures
  3. Add logging of raw API responses for debugging
  4. Create test fixtures for common API error scenarios

### Database Connection Handling
**Issue Area:** Limited error handling for database operations
- **Files:** `R/ehr_readers.R` (lines 330-360)
- **Concern:** Connection pooling not implemented; no connection lifecycle management
- **Impact:** Long-running analyses may exhaust database connections
- **Current Code:** Line 340: `if (!inherits(con, "DBIConnection"))` - basic type check only
- **Fix Approach:**
  1. Implement connection pooling via `pool` package
  2. Add automatic connection closing via `on.exit()`
  3. Add timeout configuration for long-running queries
  4. Document best practices for database usage

## Security Considerations

### API Authentication Handling
**Issue Area:** Multiple authentication methods with varying security levels
- **Files:** `R/ehr_readers.R` (lines 420-450)
- **Risk:** Bearer tokens and API keys passed in function arguments may be exposed in:
  - R history files (.Rhistory)
  - Script files in version control
  - Function call logs
  - Error messages
- **Current Implementation:** Supports Bearer, Basic, API Key, and no-auth (lines 44-50 of ehr_readers.R)
- **Recommendation:**
  1. Document requirement to use environment variables for secrets
  2. Add warning if credentials detected in plain text arguments
  3. Suppress credentials from logging and error messages
  4. Consider using keyring package for local credential storage
  5. Add examples using `Sys.getenv()` pattern

### Data Sensitivity in Examples
**Issue Area:** Example datasets may contain realistic sensitive patterns
- **Files:** `R/data.R`, `data-raw/create_example_data.R`
- **Concern:** Synthetic data with realistic healthcare patterns could be reverse-engineered
- **Current:** Use of realistic variable names (patient_id, age, diagnosis codes)
- **Fix Approach:**
  1. Document data is synthetic and for demonstration only
  2. Consider using more abstract variable names in quick examples
  3. Add disclaimer in data documentation

## Dependencies at Risk

### Optional Package Ecosystem
**Issue Area:** 26 optional packages with varying maintenance status
- **Files:** `DESCRIPTION` (Suggests field)
- **Risk Packages:**
  - `caret` - Large, less actively maintained
  - `xgboost`, `gbm` - ML packages with system dependencies
  - `arrow` - Rapid development, API changes
  - `officer`, `flextable`, `gt` - Report generation packages with version sensitivity
  - `mlr3`, `mlr3learners` - Active but less adoption than caret/mlr
  - `RPostgres`, `odbc` - Database drivers (system-dependent)
- **Impact:** Users unable to install optional features may face cryptic `requireNamespace()` errors
- **Mitigation:**
  1. Document all optional package requirements clearly
  2. Add platform-specific installation guides (especially for RPostgres)
  3. Test installation of all Suggests packages in CI/CD
  4. Consider creating lightweight alternative implementations

### R Version Dependency
**Minimum:** R >= 4.1.0 (DESCRIPTION)
- **Impact:** Functions using pipe operator `|>` (introduced in R 4.1) will fail on older versions
- **Files:** Wide usage of `|>` throughout codebase
- **Risk:** Very low (R 4.1 released April 2021, now mature)
- **Fix Approach:** No action needed; version is reasonable

## Test Coverage Limitations

### Vignette Code Evaluation
**Issue Area:** Two vignettes with `eval = FALSE` - code not executed during package check
- **Files:** `vignettes/case-study-oncology.Rmd`, `vignettes/validation-diagnostics.Rmd`
- **Lines:** eval = FALSE in knitr options
- **Reason:** Reference external data files not included in package
- **Impact:** Example code may contain errors not caught during CRAN check
- **Risk:** Users following vignette examples may encounter issues
- **Fix Approach:**
  1. Create minimal version of external data files for vignette execution
  2. Implement `eval = TRUE` with embedded synthetic data
  3. Add `purl = FALSE` to prevent code extraction (already done, cran-comments.md line 7)
  4. Add explicit note in vignette that code requires external files

### Missing Edge Case Tests
**Issue Area:** Limited testing for unusual but valid scenarios
- **Files:** `tests/testthat/test-*.R` (all test files)
- **Gaps:**
  - No tests for single-row datasets (some functions may fail)
  - No tests for datasets with 100% missing values in subset of variables
  - No tests for extremely large sample sizes (>1M rows)
  - Limited tests for factor variables with many levels (>100)
  - No tests for data frames with special column names (spaces, symbols)
- **Risk:** Silent failures or unexpected behavior in production
- **Current Coverage:** 658 tests passing, >80% coverage (acceptable baseline)
- **Fix Approach:**
  1. Add property-based testing using `{quickcheck}` or `{fuzz}` packages
  2. Create generative test cases for edge cases
  3. Add stress tests for large datasets

## Known Limitations - Design Constraints

### Matching Algorithm Fallback
**Issue Area:** Advanced matching may not work if required packages missing
- **Files:** `R/study_design.R` (matching functions)
- **Limitation:** MatchIt package provides optimal/genetic matching; graceful fallback implemented but may produce suboptimal matches
- **Impact:** Users without MatchIt get nearest neighbor matching only (acceptable but limited)
- **Fix Approach:**
  1. Document matching method availability by package
  2. Consider pure-R implementations of common matching algorithms
  3. Add warning when falling back to simpler method

### Terminology Mapping Completeness
**Issue Area:** Terminology mappings may be incomplete
- **Files:** `R/harmonization.R` (terminology mapping logic)
- **Concern:** SNOMED, RxNorm, LOINC mappings depend on external data; package includes curated mappings but not comprehensive
- **Impact:** Some terminologies may not map to target system
- **Current Behavior:** Returns NA for unmapped codes (reasonable behavior)
- **Fix Approach:**
  1. Document coverage percentages for each terminology system
  2. Allow users to supply custom mapping tables
  3. Add mapping validation reports

### Data Format Limitations
**Issue Area:** Supported data formats may not include all common EHR exports
- **Files:** `R/ehr_readers.R` (reader functions)
- **Supported:** CSV, Parquet, Arrow, RDS, Database, API
- **Not Supported:** XML, HL7, DICOM, direct database adapters for Epic/Cerner/Allscripts
- **Impact:** Users with proprietary EHR systems need custom data preparation
- **Recommendation:** Document data preparation requirements; provide examples

## Code Quality Observations

### Logging Behavior
**Issue Area:** Logging configured but may be verbose by default
- **Files:** `R/logging.R` (logging system)
- **Concern:** Default log level could produce excessive console output in interactive use
- **Fix Approach:**
  1. Test default log level in interactive vs. batch mode
  2. Consider quieter defaults for interactive usage
  3. Add option to suppress logging

### Print Method Output
**Issue Area:** Print methods produce verbose console output
- **Files:** `R/s3_classes.R` (print methods for all rwe_* classes)
- **Concern:** May overwhelm users when objects printed in interactive sessions
- **Fix Approach:**
  1. Add `max_rows` parameter to limit output
  2. Consider delegating detailed output to `str()` method
  3. Add option to suppress audit trail in print output

## Documentation Quality

### README Coverage
**Issue Area:** README is clear but could include more troubleshooting
- **Files:** `README.md`
- **Missing Sections:**
  - Common error messages and solutions
  - System requirements (especially for optional packages)
  - Performance tips for large datasets
  - Example workflows for different use cases
- **Fix Approach:** Expand troubleshooting section

### Function Parameter Documentation
**Issue Area:** Some complex parameters could use better explanation
- **Files:** Various R files with roxygen2 documentation
- **Example:** `xgb_params` in `rwe_impute()` references xgboost documentation without explanation
- **Fix Approach:**
  1. Add examples showing parameter usage
  2. Link to original package documentation
  3. Add common parameter presets

## CRAN Submission Status

### Current State
- **Version:** 0.1.0 (resubmission after initial fixes)
- **Status:** Ready for submission (as per PACKAGE_STATUS.md)
- **R CMD Check:** 0 errors, 0 warnings, 1 note (new submission)
- **Test Results:** 658 passing, 27 optional warnings, 6 intentional skips

### Outstanding Pre-Submission Items (cran-comments.md)
- [ ] Build source tarball: `R CMD build rwevidence`
- [ ] Final R CMD check on tarball
- [ ] Test on multiple R versions if possible

### Minor CRAN Note
- **Type:** Informational (not an error)
- **Reason:** New submission, optional Suggests packages not installed in check environment
- **Severity:** None - this is expected and acceptable per CRAN policies

## Recommendations by Priority

### High Priority (Before CRAN)
1. Enable causal inference tests - fix PS trimming and weight stabilization
2. Implement mock tests for optional package features
3. Add connection pooling for database operations
4. Verify API error handling with real API failures

### Medium Priority (Post-CRAN, v0.2)
1. Vectorize table generation for better scaling
2. Add comprehensive stress tests for edge cases
3. Implement comprehensive terminology mapping coverage
4. Add troubleshooting section to README

### Low Priority (Future Enhancement)
1. Implement pure-R matching algorithms
2. Add support for additional data formats (XML, HL7)
3. Create Shiny app integration examples
4. Expand example datasets with more realistic scenarios

---

*Concerns audit: 2026-01-30*
