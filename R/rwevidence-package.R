#' @keywords internal
#' @importFrom graphics legend
#' @importFrom stats aggregate aov as.formula binomial chisq.test coef complete.cases fisher.test glm median na.omit pchisq pnorm predict quantile sd t.test time var vcov weighted.mean
#' @importFrom utils head object.size str tail write.csv
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# Global variable declarations for R CMD check
utils::globalVariables(c(
  "ae", "estimate", "label", "ci_lower", "ci_upper", "time", "surv",
  "strata", "lower", "upper", "metric", "group", "hr", "variable"
))
