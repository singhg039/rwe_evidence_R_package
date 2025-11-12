# Oncology Case Study: Non-Small Cell Lung Cancer (NSCLC)
#
# This case study demonstrates a complete RWE workflow for evaluating
# a new immunotherapy agent vs. standard chemotherapy in advanced NSCLC
#
# Study Design: Retrospective cohort using EHR data with propensity score matching
# Primary Endpoint: Progression-Free Survival (PFS)
# Secondary Endpoints: Overall Survival (OS), Objective Response Rate (ORR)

library(rwevidence)
library(dplyr)
library(survival)

# Generate synthetic NSCLC patient cohort
set.seed(123)

n_patients <- 350

nsclc_cohort <- data.frame(
  patient_id = 1:n_patients,

  # Demographics
  age = round(rnorm(n_patients, mean = 67, sd = 10)),
  sex = sample(c("Male", "Female"), n_patients, replace = TRUE, prob = c(0.58, 0.42)),
  race = sample(c("White", "Black", "Asian", "Other"), n_patients,
                replace = TRUE, prob = c(0.65, 0.15, 0.12, 0.08)),
  ethnicity = sample(c("Hispanic", "Not Hispanic"), n_patients,
                     replace = TRUE, prob = c(0.15, 0.85)),

  # Disease characteristics
  histology = sample(c("Adenocarcinoma", "Squamous Cell", "Other"), n_patients,
                     replace = TRUE, prob = c(0.60, 0.30, 0.10)),
  stage = sample(c("IIIB", "IV"), n_patients, replace = TRUE, prob = c(0.25, 0.75)),
  pd_l1_expression = sample(c("<1%", "1-49%", ">=50%", "Unknown"), n_patients,
                            replace = TRUE, prob = c(0.30, 0.35, 0.25, 0.10)),
  egfr_mutation = sample(c("Positive", "Negative", "Unknown"), n_patients,
                         replace = TRUE, prob = c(0.15, 0.75, 0.10)),

  # Performance status and comorbidities
  ecog_ps = sample(0:2, n_patients, replace = TRUE, prob = c(0.30, 0.50, 0.20)),
  smoking_status = sample(c("Current", "Former", "Never"), n_patients,
                          replace = TRUE, prob = c(0.25, 0.50, 0.25)),
  comorbidity_score = sample(0:4, n_patients, replace = TRUE,
                             prob = c(0.20, 0.30, 0.25, 0.15, 0.10)),

  # Baseline labs (with realistic correlations)
  baseline_hemoglobin = round(rnorm(n_patients, mean = 12.5, sd = 1.8), 1),
  baseline_platelet = round(rnorm(n_patients, mean = 240, sd = 70)),
  baseline_albumin = round(rnorm(n_patients, mean = 3.8, sd = 0.5), 1),
  baseline_ldh = round(rnorm(n_patients, mean = 210, sd = 80)),

  # Treatment assignment (stratified by key prognostic factors)
  stringsAsFactors = FALSE
)

# Create propensity for treatment assignment (realistic confounding)
nsclc_cohort$propensity <- with(nsclc_cohort,
  plogis(
    -0.5 +
    -0.02 * (age - 67) +
    0.3 * (ecog_ps == 0) +
    -0.4 * (ecog_ps == 2) +
    0.5 * (pd_l1_expression == ">=50%") +
    -0.3 * (stage == "IV") +
    0.2 * (histology == "Adenocarcinoma") +
    rnorm(n_patients, 0, 0.3)
  )
)

nsclc_cohort$treatment <- ifelse(
  runif(n_patients) < nsclc_cohort$propensity,
  "Immunotherapy",
  "Chemotherapy"
)

# Treatment dates
nsclc_cohort$treatment_start_date <- as.Date("2020-01-01") +
  sample(0:1095, n_patients, replace = TRUE)  # 3 years

# Generate outcomes based on treatment effect
# True treatment effect: HR = 0.70 for PFS, 0.75 for OS
baseline_pfs_rate <- with(nsclc_cohort,
  1.5 +
  0.3 * (ecog_ps == 2) +
  0.2 * (stage == "IV") +
  -0.2 * (pd_l1_expression == ">=50%") +
  0.1 * (comorbidity_score / 2)
)

treatment_effect_pfs <- ifelse(nsclc_cohort$treatment == "Immunotherapy", 0.70, 1.0)
pfs_rate <- baseline_pfs_rate * treatment_effect_pfs

nsclc_cohort$pfs_time_months <- round(rexp(n_patients, rate = 1/pfs_rate), 1)
nsclc_cohort$pfs_event <- ifelse(
  nsclc_cohort$pfs_time_months < 24,  # 24 month follow-up
  1,
  0
)
nsclc_cohort$pfs_time_months <- pmin(nsclc_cohort$pfs_time_months, 24)

# Overall survival (longer than PFS)
baseline_os_rate <- baseline_pfs_rate * 0.6
treatment_effect_os <- ifelse(nsclc_cohort$treatment == "Immunotherapy", 0.75, 1.0)
os_rate <- baseline_os_rate * treatment_effect_os

nsclc_cohort$os_time_months <- round(
  pmax(
    nsclc_cohort$pfs_time_months,
    rexp(n_patients, rate = 1/os_rate)
  ),
  1
)
nsclc_cohort$os_event <- ifelse(
  nsclc_cohort$os_time_months < 36,  # 36 month follow-up
  1,
  0
)
nsclc_cohort$os_time_months <- pmin(nsclc_cohort$os_time_months, 36)

# Best overall response
response_prob <- with(nsclc_cohort,
  plogis(
    -1.5 +
    1.2 * (treatment == "Immunotherapy") +
    0.8 * (pd_l1_expression == ">=50%") +
    -0.5 * (ecog_ps == 2) +
    0.3 * (stage == "IIIB")
  )
)

nsclc_cohort$best_response <- sapply(response_prob, function(p) {
  sample(
    c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"),
    1,
    prob = c(p * 0.1, p * 0.9, (1-p) * 0.5, (1-p) * 0.5)
  )
})

# Add missing values (realistic pattern - more in older records)
add_missing <- function(x, rate) {
  x[sample(1:length(x), size = floor(length(x) * rate))] <- NA
  x
}

nsclc_cohort$baseline_albumin <- add_missing(nsclc_cohort$baseline_albumin, 0.08)
nsclc_cohort$baseline_ldh <- add_missing(nsclc_cohort$baseline_ldh, 0.10)
nsclc_cohort$pd_l1_expression[nsclc_cohort$pd_l1_expression == "Unknown"] <- NA
nsclc_cohort$egfr_mutation[nsclc_cohort$egfr_mutation == "Unknown"] <- NA

# Remove helper column
nsclc_cohort$propensity <- NULL

# Save dataset
saveRDS(
  nsclc_cohort,
  file = "inst/examples/case_studies/nsclc_cohort.rds"
)

cat("✓ Created NSCLC case study cohort:", nrow(nsclc_cohort), "patients\n")
cat("  Treatment groups:\n")
cat("    - Immunotherapy:", sum(nsclc_cohort$treatment == "Immunotherapy"), "\n")
cat("    - Chemotherapy:", sum(nsclc_cohort$treatment == "Chemotherapy"), "\n")
cat("  Median PFS:\n")
cat("    - Immunotherapy:",
    median(nsclc_cohort$pfs_time_months[nsclc_cohort$treatment == "Immunotherapy"]), "months\n")
cat("    - Chemotherapy:",
    median(nsclc_cohort$pfs_time_months[nsclc_cohort$treatment == "Chemotherapy"]), "months\n")

cat("\n✓ Dataset saved to: inst/examples/case_studies/nsclc_cohort.rds\n")
