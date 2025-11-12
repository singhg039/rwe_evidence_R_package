# Script to generate synthetic example datasets for rwevidence package
# Run this to regenerate the example data

set.seed(42)  # For reproducibility

# 1. Example EHR Data (OMOP CDM format) =====================================
n_patients <- 100

example_ehr_data <- data.frame(
  person_id = 1:n_patients,
  age = round(rnorm(n_patients, mean = 65, sd = 12)),
  sex = sample(c("Male", "Female"), n_patients, replace = TRUE, prob = c(0.48, 0.52)),
  race = sample(c("White", "Black", "Asian", "Other"), n_patients, replace = TRUE,
                prob = c(0.60, 0.15, 0.15, 0.10)),
  ethnicity = sample(c("Hispanic", "Not Hispanic"), n_patients, replace = TRUE,
                     prob = c(0.18, 0.82)),

  # Clinical measurements
  baseline_egfr = round(rnorm(n_patients, mean = 75, sd = 20), 1),
  baseline_hemoglobin = round(rnorm(n_patients, mean = 13.5, sd = 1.8), 1),
  baseline_platelet = round(rnorm(n_patients, mean = 250, sd = 80)),

  # Disease characteristics
  cancer_stage = sample(c("I", "II", "III", "IV"), n_patients, replace = TRUE,
                        prob = c(0.15, 0.25, 0.35, 0.25)),
  ecog_ps = sample(0:2, n_patients, replace = TRUE, prob = c(0.40, 0.45, 0.15)),

  # Treatment
  treatment = sample(c("Control", "Treatment"), n_patients, replace = TRUE),
  treatment_start_date = as.Date("2020-01-01") + sample(0:730, n_patients, replace = TRUE),

  # Outcomes
  progression_event = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.35, 0.65)),
  progression_time_days = round(rexp(n_patients, rate = 1/365)),

  death_event = sample(c(0, 1), n_patients, replace = TRUE, prob = c(0.50, 0.50)),
  survival_time_days = round(rexp(n_patients, rate = 1/730))
)

# Add some missing values to make it realistic
example_ehr_data$baseline_egfr[sample(1:n_patients, 8)] <- NA
example_ehr_data$baseline_hemoglobin[sample(1:n_patients, 5)] <- NA
example_ehr_data$baseline_platelet[sample(1:n_patients, 6)] <- NA
example_ehr_data$ecog_ps[sample(1:n_patients, 3)] <- NA


# 2. Example Trial Data ======================================================
n_trial <- 80

example_trial_data <- data.frame(
  subject_id = 1:n_trial,
  age = round(rnorm(n_trial, mean = 63, sd = 10)),
  sex = sample(c("Male", "Female"), n_trial, replace = TRUE, prob = c(0.50, 0.50)),

  # Eligibility criteria enforced (narrower ranges than RWD)
  baseline_egfr = round(rnorm(n_trial, mean = 85, sd = 15), 1),
  baseline_hemoglobin = round(rnorm(n_trial, mean = 13.8, sd = 1.2), 1),

  cancer_stage = sample(c("III", "IV"), n_trial, replace = TRUE, prob = c(0.60, 0.40)),
  ecog_ps = sample(0:1, n_trial, replace = TRUE, prob = c(0.55, 0.45)),

  # Treatment arm
  treatment_arm = sample(c("Placebo", "Experimental"), n_trial, replace = TRUE),
  randomization_date = as.Date("2020-06-01") + sample(0:365, n_trial, replace = TRUE),

  # Primary endpoint
  pfs_event = sample(c(0, 1), n_trial, replace = TRUE, prob = c(0.40, 0.60)),
  pfs_time_months = round(rexp(n_trial, rate = 1/12), 1),

  # Secondary endpoints
  os_event = sample(c(0, 1), n_trial, replace = TRUE, prob = c(0.55, 0.45)),
  os_time_months = round(rexp(n_trial, rate = 1/24), 1),

  # Response
  objective_response = sample(c("CR", "PR", "SD", "PD"), n_trial, replace = TRUE,
                               prob = c(0.08, 0.25, 0.42, 0.25))
)

# No missing values in trial data (per protocol)


# 3. Example Safety Data =====================================================
n_safety <- 120  # Some patients may have multiple AE records

example_safety_data <- data.frame(
  subject_id = sample(1:100, n_safety, replace = TRUE),

  # Adverse event details
  ae_term = sample(c("Nausea", "Fatigue", "Anemia", "Neutropenia", "Diarrhea",
                     "Rash", "Hypertension", "Headache", "Fever", "Pain"),
                   n_safety, replace = TRUE),

  ae_severity = sample(c("Grade 1", "Grade 2", "Grade 3", "Grade 4"), n_safety,
                       replace = TRUE, prob = c(0.45, 0.35, 0.15, 0.05)),

  ae_serious = sample(c("No", "Yes"), n_safety, replace = TRUE, prob = c(0.85, 0.15)),

  ae_related = sample(c("No", "Yes", "Possibly"), n_safety, replace = TRUE,
                      prob = c(0.25, 0.50, 0.25)),

  # Timing
  ae_onset_day = sample(1:365, n_safety, replace = TRUE),
  ae_resolved = sample(c("Yes", "No", "Ongoing"), n_safety, replace = TRUE,
                       prob = c(0.70, 0.10, 0.20)),

  # Person-time for rate calculations
  person_years = round(runif(n_safety, min = 0.1, max = 2.0), 2),

  # Treatment group
  treatment_group = sample(c("Control", "Treatment"), n_safety, replace = TRUE)
)

# Sort by subject_id for clarity
example_safety_data <- example_safety_data[order(example_safety_data$subject_id), ]
rownames(example_safety_data) <- NULL


# Save all datasets ==========================================================
usethis::use_data(example_ehr_data, overwrite = TRUE)
usethis::use_data(example_trial_data, overwrite = TRUE)
usethis::use_data(example_safety_data, overwrite = TRUE)

message("✓ Created example_ehr_data: ", nrow(example_ehr_data), " patients")
message("✓ Created example_trial_data: ", nrow(example_trial_data), " subjects")
message("✓ Created example_safety_data: ", nrow(example_safety_data), " adverse events")
message("\nDatasets saved to data/")
