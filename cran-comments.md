# CRAN Submission Comments

## Test environments
* local macOS (aarch64-apple-darwin20), R 4.4.1
* R CMD check with --as-cran flag

## R CMD check results

0 errors ✓ | 0 warnings ✓ | 2 notes ✓

### Notes:

1. **New submission**
   - This is a new package submission to CRAN

2. **Possibly invalid URLs** (404 status)
   - GitHub Pages URLs return 404 because the site is not yet published
   - URLs will be valid once the package website is deployed post-CRAN acceptance
   - All GitHub repository URLs are correct and accessible

3. **Missing suggested packages during check**
   - Several Suggests packages are not available in the check environment
   - This is expected and handled gracefully by the package
   - All code checks for package availability before use
   - Examples that require unavailable packages are wrapped in \dontrun{}

## Vignette notes

Two vignettes (case-study-oncology.Rmd and validation-diagnostics.Rmd) have eval = FALSE in their knitr options because they reference external data files not included in the package. This is intentional design to provide code examples.

## Package dependencies

All Imports dependencies are available on CRAN. All Suggests packages are optional and enhance functionality but are not required for core features.

## Downstream dependencies

None (new package)

## Additional information

This package provides tools for Real-World Evidence generation following FDA and EMA regulatory guidance. It includes 56 exported functions, 658 passing unit tests, 3 example datasets, and comprehensive documentation.

## Maintainer contact

Gagandeep Singh <gagans01399@gmail.com>
