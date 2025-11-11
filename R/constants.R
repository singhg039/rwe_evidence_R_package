#' Package Constants and Configurations
#'
#' @description
#' Defines standard constants, schemas, and configurations used throughout the package
#'
#' @keywords internal


# Data Quality Thresholds -----------------------------------------------

#' Default Data Quality Thresholds
#'
#' @name quality_thresholds
#' @format Named list of quality dimension thresholds
#' @keywords internal
DEFAULT_QUALITY_THRESHOLDS <- list(
  completeness = 0.80,  # 80% completeness required
  validity = 0.95,      # 95% valid values required
  consistency = 0.90,   # 90% consistency required
  timeliness = 0.85,    # 85% timely data required
  uniqueness = 0.99     # 99% unique records required
)


# Data Schema Definitions ------------------------------------------------

#' OMOP CDM 5.4 Core Tables
#'
#' @format Named list of OMOP CDM table schemas
#' @keywords internal
OMOP_CDM_5.4_SCHEMA <- list(
  person = c(
    person_id = "integer",
    gender_concept_id = "integer",
    year_of_birth = "integer",
    month_of_birth = "integer",
    day_of_birth = "integer",
    birth_datetime = "POSIXct",
    race_concept_id = "integer",
    ethnicity_concept_id = "integer"
  ),
  observation_period = c(
    observation_period_id = "integer",
    person_id = "integer",
    observation_period_start_date = "Date",
    observation_period_end_date = "Date",
    period_type_concept_id = "integer"
  ),
  visit_occurrence = c(
    visit_occurrence_id = "integer",
    person_id = "integer",
    visit_concept_id = "integer",
    visit_start_date = "Date",
    visit_start_datetime = "POSIXct",
    visit_end_date = "Date",
    visit_end_datetime = "POSIXct"
  ),
  condition_occurrence = c(
    condition_occurrence_id = "integer",
    person_id = "integer",
    condition_concept_id = "integer",
    condition_start_date = "Date",
    condition_start_datetime = "POSIXct",
    condition_end_date = "Date",
    condition_type_concept_id = "integer"
  ),
  drug_exposure = c(
    drug_exposure_id = "integer",
    person_id = "integer",
    drug_concept_id = "integer",
    drug_exposure_start_date = "Date",
    drug_exposure_start_datetime = "POSIXct",
    drug_exposure_end_date = "Date",
    drug_type_concept_id = "integer"
  ),
  measurement = c(
    measurement_id = "integer",
    person_id = "integer",
    measurement_concept_id = "integer",
    measurement_date = "Date",
    measurement_datetime = "POSIXct",
    value_as_number = "numeric",
    value_as_concept_id = "integer",
    unit_concept_id = "integer"
  )
)


# Terminology Systems -----------------------------------------------------

#' Supported Medical Terminology Systems
#'
#' @format Character vector of supported terminology systems
#' @keywords internal
SUPPORTED_TERMINOLOGIES <- c(
  "ICD9CM", "ICD10CM", "ICD10PCS",
  "SNOMED", "LOINC", "RxNorm",
  "NDC", "CPT4", "HCPCS"
)


#' Standard Terminology Mapping Pairs
#'
#' @format Named list of common terminology mappings
#' @keywords internal
STANDARD_TERMINOLOGY_MAPS <- list(
  diagnosis = list(
    from = c("ICD9CM", "ICD10CM"),
    to = "SNOMED"
  ),
  medications = list(
    from = c("NDC", "RxNorm"),
    to = "RxNorm"
  ),
  procedures = list(
    from = c("ICD10PCS", "CPT4", "HCPCS"),
    to = "SNOMED"
  ),
  laboratory = list(
    from = "local_lab_code",
    to = "LOINC"
  )
)

#' Measurement Unit Conversion Table
#'
#' @format Data frame of common unit conversions with multiplicative factors
#' @keywords internal
UNIT_CONVERSIONS <- data.frame(
  source_unit = c("g/dL", "mg/dL", "mmHg", "kPa", "mg/L"),
  target_unit = c("g/L", "g/L", "kPa", "mmHg", "mg/dL"),
  factor = c(10, 0.01, 0.133322368, 7.500615613, 0.1),
  offset = c(0, 0, 0, 0, 0),
  stringsAsFactors = FALSE
)

#' Standard Terminology Mapping Tables
#'
#' @format Named list of small reference mapping tables for validation/testing
#' @keywords internal
STANDARD_TERMINOLOGY_TABLES <- list(
  ICD9CM__TO__SNOMED = data.frame(
    source_code = c("250.00", "401.9", "414.01"),
    target_code = c("44054006", "38341003", "53741008"),
    stringsAsFactors = FALSE
  ),
  ICD10CM__TO__SNOMED = data.frame(
    source_code = c("E11.9", "I10", "J44.0"),
    target_code = c("44054006", "38341003", "195967001"),
    stringsAsFactors = FALSE
  ),
  NDC__TO__RXNORM = data.frame(
    source_code = c("00009328401", "00023396001", "00078062315"),
    target_code = c("617314", "617320", "1049630"),
    stringsAsFactors = FALSE
  ),
  LOCAL_LAB_CODE__TO__LOINC = data.frame(
    source_code = c("LAB_HGB", "LAB_A1C", "LAB_LDL"),
    target_code = c("718-7", "4548-4", "2089-1"),
    stringsAsFactors = FALSE
  )
)


# Data Format Specifications ----------------------------------------------

#' Supported Data Source Formats
#'
#' @format Character vector of supported data formats
#' @keywords internal
SUPPORTED_DATA_FORMATS <- c(
  "omop_cdm",
  "fhir",
  "cerner",
  "epic",
  "custom",
  "claims_837",
  "registry"
)


#' File Type Extensions
#'
#' @format Named list of file extensions by type
#' @keywords internal
FILE_EXTENSIONS <- list(
  tabular = c("csv", "tsv", "txt", "parquet", "arrow", "feather"),
  database = c("db", "sqlite", "duckdb"),
  compressed = c("gz", "zip", "bz2", "xz"),
  excel = c("xls", "xlsx"),
  sas = c("sas7bdat", "xpt"),
  stata = c("dta"),
  spss = c("sav")
)


# Analysis Method Options -------------------------------------------------

#' Supported Imputation Methods
#'
#' @format Character vector of imputation methods
#' @keywords internal
IMPUTATION_METHODS <- c(
  "mice",           # Multiple Imputation by Chained Equations
  "random_forest",  # Random Forest imputation
  "xgboost",        # XGBoost imputation
  "knn",            # K-Nearest Neighbors
  "mean",           # Mean imputation
  "median",         # Median imputation
  "mode"            # Mode imputation
)


#' Supported Propensity Score Methods
#'
#' @format Character vector of propensity score estimation methods
#' @keywords internal
PS_METHODS <- c(
  "logistic",       # Logistic regression
  "gbm",            # Gradient boosted models
  "random_forest",  # Random forest
  "xgboost",        # Extreme Gradient Boosting
  "super_learner"   # Super learner ensemble
)


#' Supported Matching Methods
#'
#' @format Character vector of matching methods
#' @keywords internal
MATCHING_METHODS <- c(
  "nearest",        # Nearest neighbor matching
  "optimal",        # Optimal matching
  "genetic",        # Genetic matching
  "full",           # Full matching
  "subclass",       # Subclassification
  "cem"             # Coarsened Exact Matching
)


#' Supported Statistical Analysis Methods
#'
#' @format Character vector of analysis methods
#' @keywords internal
ANALYSIS_METHODS <- c(
  "cox",                    # Cox proportional hazards
  "logrank",                # Log-rank test
  "competing_risks",        # Competing risks analysis
  "marginal_structural",    # Marginal structural models
  "kaplan_meier",          # Kaplan-Meier estimation
  "linear_regression",      # Linear regression
  "logistic_regression"     # Logistic regression
)


# Regulatory Templates ----------------------------------------------------

#' Available Regulatory Report Templates
#'
#' @format Character vector of report templates
#' @keywords internal
REGULATORY_TEMPLATES <- c(
  "fda_rwe",          # FDA Real-World Evidence template
  "ema_rwe",          # EMA Real-World Evidence template
  "ich_e6",           # ICH E6 GCP guideline template
  "ich_e9",           # ICH E9 Statistical Principles
  "pmda_japan",       # PMDA Japan template
  "custom"            # Custom template
)


#' Report Output Formats
#'
#' @format Character vector of output formats
#' @keywords internal
REPORT_FORMATS <- c(
  "docx",   # Microsoft Word
  "pdf",    # PDF
  "html",   # HTML
  "rtf",    # Rich Text Format
  "latex"   # LaTeX
)


# Validation Rules --------------------------------------------------------

#' Standard Age Validation Range
#'
#' @format Numeric vector of min and max age
#' @keywords internal
AGE_RANGE <- c(0, 120)


#' Date Range for Valid Healthcare Data
#'
#' @format Date vector of min and max dates
#' @keywords internal
VALID_DATE_RANGE <- c(
  as.Date("1900-01-01"),
  Sys.Date() + 365  # Allow up to 1 year in future for scheduled visits
)


# Package Metadata --------------------------------------------------------

#' Package Version Information
#'
#' @format List with version metadata
#' @keywords internal
PACKAGE_METADATA <- list(
  name = "rwevidence",
  full_name = "Real-World Evidence Generation Package",
  version = "0.1.0",
  citation = "rwevidence: Real-World Evidence Generation for Pharmaceutical Research"
)


# Color Palettes for Visualizations --------------------------------------

#' Color Palette for Quality Reports
#'
#' @format Named character vector of colors
#' @keywords internal
QUALITY_COLORS <- c(
  excellent = "#2ecc71",  # Green
  good = "#3498db",       # Blue
  fair = "#f39c12",       # Orange
  poor = "#e74c3c",       # Red
  critical = "#c0392b"    # Dark red
)


#' Treatment Group Colors
#'
#' @format Character vector of colors for treatment groups
#' @keywords internal
TREATMENT_COLORS <- c(
  "#1f77b4",  # Blue
  "#ff7f0e",  # Orange
  "#2ca02c",  # Green
  "#d62728",  # Red
  "#9467bd",  # Purple
  "#8c564b"   # Brown
)


# Helper Functions for Constants ------------------------------------------

#' Get Schema Definition
#'
#' @description
#' Retrieves schema definition for specified format and version
#'
#' @param format Data format name
#' @param version Optional version string
#'
#' @return Named list with schema definition
#' @keywords internal
get_schema_definition <- function(format, version = NULL) {

  schema_name <- if (!is.null(version)) {
    paste0(toupper(format), "_", version, "_SCHEMA")
  } else {
    paste0(toupper(format), "_SCHEMA")
  }

  # Try to get schema from package constants
  schema <- tryCatch(
    get(schema_name, mode = "list"),
    error = function(e) NULL
  )

  if (is.null(schema)) {
    warning("Schema not found for format: ", format,
            if (!is.null(version)) paste0(" version: ", version),
            call. = FALSE)
    return(list())
  }

  schema
}


#' Get Quality Threshold
#'
#' @description
#' Retrieves quality threshold for specified dimension
#'
#' @param dimension Quality dimension name
#'
#' @return Numeric threshold value
#' @keywords internal
get_quality_threshold <- function(dimension) {

  if (!dimension %in% names(DEFAULT_QUALITY_THRESHOLDS)) {
    warning("Unknown quality dimension: ", dimension,
            ". Using default threshold of 0.8",
            call. = FALSE)
    return(0.8)
  }

  DEFAULT_QUALITY_THRESHOLDS[[dimension]]
}
