# Example Data Documentation

#' Example Electronic Health Record (EHR) Data
#'
#' @description
#' A synthetic dataset representing electronic health record data for 100
#' patients in OMOP CDM-like format. This dataset is designed for testing
#' and demonstration of RWE workflows.
#'
#' @format A data frame with 100 rows and 16 variables:
#' \describe{
#'   \item{person_id}{Patient identifier (integer 1-100)}
#'   \item{age}{Age in years (numeric)}
#'   \item{sex}{Sex (character: "Male", "Female")}
#'   \item{race}{Race category (character)}
#'   \item{ethnicity}{Ethnicity (character: "Hispanic", "Not Hispanic")}
#'   \item{baseline_egfr}{Baseline estimated GFR in mL/min/1.73m² (numeric, some NA)}
#'   \item{baseline_hemoglobin}{Baseline hemoglobin in g/dL (numeric, some NA)}
#'   \item{baseline_platelet}{Baseline platelet count in 10^9/L (numeric, some NA)}
#'   \item{cancer_stage}{Cancer stage (character: "I", "II", "III", "IV")}
#'   \item{ecog_ps}{ECOG performance status (integer 0-2, some NA)}
#'   \item{treatment}{Treatment group (character: "Control", "Treatment")}
#'   \item{treatment_start_date}{Treatment start date (Date)}
#'   \item{progression_event}{Progression event indicator (integer 0/1)}
#'   \item{progression_time_days}{Time to progression in days (numeric)}
#'   \item{death_event}{Death event indicator (integer 0/1)}
#'   \item{survival_time_days}{Overall survival time in days (numeric)}
#' }
#'
#' @details
#' This synthetic dataset includes realistic missingness patterns (~5-8% for
#' lab values) and is suitable for demonstrating:
#' - Data quality assessment
#' - Missing data imputation
#' - Propensity score methods
#' - Survival analysis
#' - External control arm generation
#'
#' **Note:** This is synthetic data generated for demonstration purposes only.
#' It does not represent any real patients or clinical trial.
#'
#' @examples
#' data(example_ehr_data)
#' head(example_ehr_data)
#' summary(example_ehr_data)
#'
#' # Check missingness
#' colSums(is.na(example_ehr_data))
#'
#' # Basic quality assessment
#' \donttest{
#' quality <- rwe_assess_quality(example_ehr_data)
#' print(quality)
#' }
#'
#' @source Synthetically generated using \code{data-raw/create_example_data.R}
"example_ehr_data"


#' Example Clinical Trial Data
#'
#' @description
#' A synthetic dataset representing a randomized clinical trial with 80
#' subjects. This dataset demonstrates typical trial data structure and
#' can be used for comparative effectiveness analyses.
#'
#' @format A data frame with 80 rows and 12 variables:
#' \describe{
#'   \item{subject_id}{Subject identifier (integer 1-80)}
#'   \item{age}{Age in years (numeric)}
#'   \item{sex}{Sex (character: "Male", "Female")}
#'   \item{baseline_egfr}{Baseline estimated GFR in mL/min/1.73m² (numeric)}
#'   \item{baseline_hemoglobin}{Baseline hemoglobin in g/dL (numeric)}
#'   \item{cancer_stage}{Cancer stage (character: "III", "IV")}
#'   \item{ecog_ps}{ECOG performance status (integer 0-1)}
#'   \item{treatment_arm}{Treatment assignment (character: "Placebo", "Experimental")}
#'   \item{randomization_date}{Randomization date (Date)}
#'   \item{pfs_event}{Progression-free survival event (integer 0/1)}
#'   \item{pfs_time_months}{PFS time in months (numeric)}
#'   \item{os_event}{Overall survival event (integer 0/1)}
#'   \item{os_time_months}{OS time in months (numeric)}
#'   \item{objective_response}{Best overall response (character: "CR", "PR", "SD", "PD")}
#' }
#'
#' @details
#' This dataset represents a typical Phase III oncology trial with:
#' - Strict eligibility criteria (reflected in narrower demographic ranges)
#' - Complete data (no missing values per protocol)
#' - Standard efficacy endpoints (PFS, OS, ORR)
#'
#' Can be used with \code{\link{example_ehr_data}} to demonstrate:
#' - External control arm generation
#' - Trial vs. RWD comparability assessment
#' - Propensity score methods for observational data
#'
#' **Note:** This is synthetic data generated for demonstration purposes only.
#'
#' @examples
#' data(example_trial_data)
#' head(example_trial_data)
#' table(example_trial_data$treatment_arm)
#'
#' # Compare trial eligibility to RWD
#' data(example_ehr_data)
#' summary(example_trial_data$age)
#' summary(example_ehr_data$age)
#'
#' @source Synthetically generated using \code{data-raw/create_example_data.R}
"example_trial_data"


#' Example Safety/Adverse Event Data
#'
#' @description
#' A synthetic dataset of adverse events from 120 event records across
#' multiple patients. This dataset demonstrates safety surveillance and
#' pharmacovigilance workflows.
#'
#' @format A data frame with 120 rows and 8 variables:
#' \describe{
#'   \item{subject_id}{Subject identifier (integer, some patients have multiple AEs)}
#'   \item{ae_term}{Adverse event term (character)}
#'   \item{ae_severity}{CTCAE grade (character: "Grade 1" through "Grade 4")}
#'   \item{ae_serious}{Serious adverse event flag (character: "Yes", "No")}
#'   \item{ae_related}{Relationship to treatment (character: "Yes", "No", "Possibly")}
#'   \item{ae_onset_day}{Day of AE onset relative to treatment start (integer)}
#'   \item{ae_resolved}{Resolution status (character: "Yes", "No", "Ongoing")}
#'   \item{person_years}{Person-time at risk in years (numeric)}
#'   \item{treatment_group}{Treatment group (character: "Control", "Treatment")}
#' }
#'
#' @details
#' This dataset includes common adverse events and can be used to demonstrate:
#' - Safety surveillance methods
#' - Incidence rate calculations
#' - Safety signal detection
#' - Comparative safety analysis
#' - Adverse event profiling
#'
#' The \code{person_years} variable allows for proper incidence rate calculations
#' accounting for variable exposure time.
#'
#' **Note:** This is synthetic data generated for demonstration purposes only.
#'
#' @examples
#' data(example_safety_data)
#' head(example_safety_data)
#'
#' # AE frequency
#' table(example_safety_data$ae_term)
#'
#' # Severity distribution
#' table(example_safety_data$ae_severity)
#'
#' # Safety analysis example
#' \dontrun{
#' # Requires proper 0/1 treatment coding
#' safety <- rwe_analyze_safety(
#'   data = example_safety_data,
#'   events = "ae_term",
#'   person_time = "person_years",
#'   treatment = "treatment_group"
#' )
#' print(safety)
#' }
#'
#' @source Synthetically generated using \code{data-raw/create_example_data.R}
"example_safety_data"
