---
created: 2026-01-30T12:00
title: Review CRAN fixes for issues or better approaches
area: testing
files:
  - R/causal_inference.R:99-131
  - R/causal_inference.R:359-407
  - R/regulatory_reporting.R:1563-1753
  - tests/testthat/test-causal_inference.R:63-117
  - tests/testthat/test-causal_inference.R:147-180
  - tests/testthat/test-causal_inference.R:336-354
  - tests/testthat/test-causal_inference.R:452-517
---

## Problem

We made several fixes to pass CRAN submission, but should review whether these are optimal:

1. **PS trimming threshold** - Changed from 0.01-0.99 to 0.025-0.975
   - Is this threshold appropriate for all use cases?
   - Should it be configurable rather than hardcoded?
   - Literature review: what do other packages (WeightIt, twang) use?

2. **Weight capping at 99th percentile** - Added automatic capping
   - Could this mask important extreme cases?
   - Should users be warned when capping occurs?
   - Alternative: use robust variance estimation instead?

3. **Convergence handling in doubly robust** - Added tryCatch with fallback
   - Fallback just increases maxit - is this sufficient?
   - Should we implement quasi-separation detection?
   - Should we suggest alternative estimators when convergence fails?

4. **Enabled tests** - 6 tests now implemented
   - Are the test tolerances appropriate?
   - Should we add more edge case tests?
   - Binary outcome tests use n=300 - sufficient for stability?

## Solution

TBD - Review each fix against:
- Statistical literature best practices
- Similar R packages (WeightIt, cobalt, twang, AIPW)
- CRAN reviewer expectations
- User experience implications

Consider:
- Adding configurable trimming thresholds
- More informative warnings when adjustments occur
- Alternative robust methods as fallbacks
- Property-based testing for edge cases
