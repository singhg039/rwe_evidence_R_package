# CRAN Resubmission Comments

## Resubmission

This is a resubmission fixing issues from initial automated checks.

### Changes made:
1. Added single quotes around acronyms in DESCRIPTION ('RWE', 'RWD', 'FDA', 'EMA') per CRAN guidelines
2. Removed redundant Author and Maintainer fields (auto-generated from Authors@R)
3. Removed non-existent GitHub Pages URLs and references from README.md
4. Fixed incomplete EMA URL in vignettes/getting-started.Rmd
5. Added data-raw/ to .Rbuildignore to exclude it from package
6. Removed all macOS resource fork files (._*)
7. Added purl = FALSE to vignettes with eval = FALSE to prevent code extraction

## Test environments
* local macOS (aarch64-apple-darwin20), R 4.4.1
* R CMD check with --as-cran flag
* win-builder (r-devel)

## R CMD check results

0 errors ✓ | 0 warnings ✓ | 1 note ✓

### Note:

1. **New submission**
   - This is a new package submission to CRAN

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
