#' Read EHR Data
#'
#' @description
#' Reads electronic health record data from various sources with automatic
#' format detection and validation.
#'
#' @param source Character. Path to file, database connection, or API endpoint
#' @param format Character. Data format: "omop_cdm", "fhir", "cerner", "epic", "custom"
#' @param version Character. Format version (e.g., "5.4" for OMOP CDM)
#' @param lazy Logical. Use lazy evaluation for large datasets
#' @param validate Logical. Perform data validation on load
#' @param date_range Date vector. Optional date range filter c(start, end)
#' @param schema Character. Database schema name (for database connections)
#' @param ... Additional arguments passed to reader functions
#'
#' @return An rwe_ehr object containing:
#'   \item{data}{The loaded data (tibble or arrow dataset)}
#'   \item{metadata}{Source information, load timestamp, validation results}
#'   \item{schema}{Detected or specified schema information}
#'
#' @examples
#' \dontrun{
#' # Read OMOP CDM data from CSV
#' ehr <- rwe_read_ehr(
#'   source = "data/omop_cdm.csv",
#'   format = "omop_cdm",
#'   version = "5.4"
#' )
#'
#' # Read from database
#' library(DBI)
#' con <- dbConnect(RPostgres::Postgres(),
#'                  dbname = "ehr_db",
#'                  host = "localhost")
#' ehr <- rwe_read_ehr(
#'   source = con,
#'   format = "omop_cdm",
#'   schema = "cdm_data"
#' )
#' }
#'
#' @export
rwe_read_ehr <- function(source,
                         format = c("omop_cdm", "fhir", "cerner", "epic", "custom"),
                         version = NULL,
                         lazy = FALSE,
                         validate = TRUE,
                         date_range = NULL,
                         schema = NULL,
                         ...) {

  format <- match.arg(format)

  # Input validation
  if (missing(source)) {
    stop("source must be provided", call. = FALSE)
  }

  log_info("Starting EHR data load", context = "rwe_read_ehr")
  log_debug(paste("Format:", format, "| Lazy:", lazy, "| Validate:", validate),
            context = "rwe_read_ehr")

  # Detect source type
  source_type <- detect_source_type(source)
  log_debug(paste("Detected source type:", source_type), context = "rwe_read_ehr")

  # Load data based on source type
  data <- tryCatch({
    switch(source_type,
      file = read_ehr_file(source, format, lazy, ...),
      database = read_ehr_database(source, format, schema, ...),
      api = read_ehr_api(source, format, ...)
    )
  }, error = function(e) {
    log_error(paste("Failed to load data:", e$message), context = "rwe_read_ehr")
    stop("Error loading EHR data: ", e$message, call. = FALSE)
  })

  # Apply date filtering if specified
  if (!is.null(date_range)) {
    if (length(date_range) != 2) {
      stop("date_range must be a vector of length 2 (start, end)", call. = FALSE)
    }
    log_info("Applying date range filter", context = "rwe_read_ehr")
    data <- filter_by_date(data, date_range)
  }

  # Validate data structure
  validation_results <- NULL
  if (validate) {
    log_info("Validating EHR schema", context = "rwe_read_ehr")
    validation_results <- validate_ehr_schema(data, format, version)

    if (!validation_results$passed) {
      log_warning(paste("Schema validation failed:", validation_results$message),
                  context = "rwe_read_ehr")
      warning("Schema validation failed: ", validation_results$message, call. = FALSE)
    } else {
      log_info("Schema validation passed", context = "rwe_read_ehr")
    }
  }

  # Get schema definition
  schema_def <- get_schema_definition(format, version)

  # Calculate record count
  n_records <- if (lazy) {
    NA_integer_
  } else {
    nrow(data)
  }

  log_info(paste("EHR data loaded successfully.",
                if (!is.na(n_records)) paste(n_records, "records")),
          context = "rwe_read_ehr")

  # Create rwe_ehr object
  ehr_obj <- new_rwe_ehr(
    data = data,
    metadata = list(
      source = if (is.character(source)) source else "<connection>",
      format = format,
      version = version,
      source_type = source_type,
      loaded_at = Sys.time(),
      n_records = n_records,
      validation = validation_results,
      lazy = lazy
    ),
    schema = schema_def
  )

  # Create audit trail
  audit <- create_audit_trail(
    operation = "read_ehr",
    input_data = list(source = source, format = format),
    output_data = list(n_records = n_records, n_cols = ncol(data)),
    parameters = list(
      format = format,
      version = version,
      lazy = lazy,
      validate = validate
    )
  )

  ehr_obj$audit_trail <- audit

  ehr_obj
}


#' Detect Source Type
#'
#' @description
#' Determines if source is a file, database connection, or API endpoint
#'
#' @param source Source to detect
#' @return Character: "file", "database", or "api"
#' @keywords internal
detect_source_type <- function(source) {

  if (inherits(source, "DBIConnection")) {
    return("database")
  }

  if (is.character(source)) {
    # Check if it's a URL (API endpoint)
    if (grepl("^https?://", source)) {
      return("api")
    }
    # Otherwise assume it's a file path
    return("file")
  }

  stop("Unable to detect source type. Source must be a file path, ",
       "database connection, or URL", call. = FALSE)
}


#' Read EHR File
#'
#' @description
#' Reads EHR data from file (CSV, Parquet, Arrow, etc.)
#'
#' @param source File path
#' @param format Data format
#' @param lazy Use lazy evaluation
#' @param ... Additional arguments
#'
#' @return Data frame or arrow dataset
#' @keywords internal
read_ehr_file <- function(source, format, lazy = FALSE, ...) {

  # Check file exists
  if (!file.exists(source)) {
    stop("File not found: ", source, call. = FALSE)
  }

  # Detect file type from extension
  file_ext <- tolower(tools::file_ext(source))

  log_debug(paste("Reading file with extension:", file_ext),
            context = "read_ehr_file")

  # Read based on file type
  data <- switch(file_ext,
    csv = read_csv_file(source, ...),
    tsv = read_csv_file(source, delim = "\t", ...),
    txt = read_csv_file(source, ...),
    parquet = read_parquet_file(source, lazy, ...),
    arrow = read_arrow_file(source, lazy, ...),
    feather = read_feather_file(source, ...),
    rds = readRDS(source),
    # Default: try to read as CSV
    read_csv_file(source, ...)
  )

  if (is.null(data) || (is.data.frame(data) && nrow(data) == 0)) {
    warning("File loaded but contains no data", call. = FALSE)
  }

  data
}


#' Read CSV File
#'
#' @description
#' Reads CSV file using readr or data.table
#'
#' @param file File path
#' @param ... Additional arguments passed to read function
#'
#' @return Data frame
#' @keywords internal
read_csv_file <- function(file, ...) {

  # Try readr first (part of tidyverse)
  if (requireNamespace("readr", quietly = TRUE)) {
    log_debug("Using readr::read_csv", context = "read_csv_file")
    data <- readr::read_csv(file, show_col_types = FALSE, ...)
    return(as.data.frame(data))
  }

  # Fall back to base R
  log_debug("Using utils::read.csv", context = "read_csv_file")
  utils::read.csv(file, stringsAsFactors = FALSE, ...)
}


#' Read Parquet File
#'
#' @description
#' Reads Parquet file using arrow package
#'
#' @param file File path
#' @param lazy Use lazy evaluation
#' @param ... Additional arguments
#'
#' @return Data frame or arrow dataset
#' @keywords internal
read_parquet_file <- function(file, lazy = FALSE, ...) {

  check_required_packages("arrow", suggest_install = TRUE)

  log_debug(paste("Reading Parquet file, lazy =", lazy),
            context = "read_parquet_file")

  if (lazy) {
    arrow::open_dataset(file, format = "parquet")
  } else {
    as.data.frame(arrow::read_parquet(file, ...))
  }
}


#' Read Arrow File
#'
#' @description
#' Reads Arrow IPC file using arrow package
#'
#' @param file File path
#' @param lazy Use lazy evaluation
#' @param ... Additional arguments
#'
#' @return Data frame or arrow dataset
#' @keywords internal
read_arrow_file <- function(file, lazy = FALSE, ...) {

  check_required_packages("arrow", suggest_install = TRUE)

  log_debug(paste("Reading Arrow file, lazy =", lazy),
            context = "read_arrow_file")

  if (lazy) {
    arrow::open_dataset(file, format = "arrow")
  } else {
    as.data.frame(arrow::read_ipc_file(file, ...))
  }
}


#' Read Feather File
#'
#' @description
#' Reads Feather file using arrow package
#'
#' @param file File path
#' @param ... Additional arguments
#'
#' @return Data frame
#' @keywords internal
read_feather_file <- function(file, ...) {

  check_required_packages("arrow", suggest_install = TRUE)

  log_debug("Reading Feather file", context = "read_feather_file")

  as.data.frame(arrow::read_feather(file, ...))
}


#' Read EHR Database
#'
#' @description
#' Reads EHR data from database connection
#'
#' @param connection DBI connection object
#' @param format Data format
#' @param schema Database schema name
#' @param ... Additional arguments
#'
#' @return Data frame or dbplyr lazy tibble
#' @keywords internal
read_ehr_database <- function(connection, format, schema = NULL, ...) {

  check_required_packages("DBI", suggest_install = TRUE)

  if (!DBI::dbIsValid(connection)) {
    stop("Invalid database connection", call. = FALSE)
  }

  log_info("Reading from database", context = "read_ehr_database")

  # Get table name based on format
  table_name <- get_main_table_for_format(format)

  # Construct full table name with schema if provided
  full_table_name <- if (!is.null(schema)) {
    paste0(schema, ".", table_name)
  } else {
    table_name
  }

  # Check if table exists
  if (!DBI::dbExistsTable(connection, full_table_name)) {
    stop("Table not found: ", full_table_name, call. = FALSE)
  }

  log_debug(paste("Reading table:", full_table_name),
            context = "read_ehr_database")

  # Read table
  data <- DBI::dbReadTable(connection, full_table_name)

  as.data.frame(data)
}


#' Read EHR API
#'
#' @description
#' Reads EHR data from API endpoint (placeholder for future implementation)
#'
#' @param url API endpoint URL
#' @param format Data format
#' @param ... Additional arguments
#'
#' @return Data frame
#' @keywords internal
read_ehr_api <- function(url, format, ...) {

  log_warning("API data loading is not yet fully implemented",
              context = "read_ehr_api")

  stop("API data loading is not yet implemented. ",
       "Please use file or database sources.", call. = FALSE)
}


#' Filter by Date Range
#'
#' @description
#' Filters data by date range
#'
#' @param data Data frame
#' @param date_range Vector of start and end dates
#'
#' @return Filtered data frame
#' @keywords internal
filter_by_date <- function(data, date_range) {

  # Try to find date columns
  date_cols <- names(data)[sapply(data, function(x) {
    inherits(x, c("Date", "POSIXct", "POSIXt"))
  })]

  if (length(date_cols) == 0) {
    warning("No date columns found for filtering", call. = FALSE)
    return(data)
  }

  # Use the first date column found
  date_col <- date_cols[1]
  log_debug(paste("Filtering by date column:", date_col),
            context = "filter_by_date")

  start_date <- as.Date(date_range[1])
  end_date <- as.Date(date_range[2])

  # Filter
  filtered <- data[data[[date_col]] >= start_date &
                   data[[date_col]] <= end_date, ]

  log_info(paste("Filtered from", nrow(data), "to", nrow(filtered), "records"),
          context = "filter_by_date")

  filtered
}


#' Validate EHR Schema
#'
#' @description
#' Validates data against expected EHR schema
#'
#' @param data Data frame to validate
#' @param format Data format
#' @param version Format version
#'
#' @return List with validation results
#' @keywords internal
validate_ehr_schema <- function(data, format, version) {

  # Get expected schema
  expected_schema <- get_schema_definition(format, version)

  if (length(expected_schema) == 0) {
    log_debug("No schema definition available for validation",
              context = "validate_ehr_schema")
    return(list(
      passed = TRUE,
      message = "No schema validation performed (schema not available)"
    ))
  }

  # Check data structure
  validation <- check_data_structure(data, expected_schema)

  list(
    passed = validation$passed,
    message = validation$message,
    missing_columns = validation$missing_columns,
    type_mismatches = validation$type_mismatches
  )
}


#' Get Main Table for Format
#'
#' @description
#' Returns the main table name for a given format
#'
#' @param format Data format
#' @return Character table name
#' @keywords internal
get_main_table_for_format <- function(format) {

  switch(format,
    omop_cdm = "person",
    fhir = "patient",
    cerner = "person",
    epic = "patient",
    custom = "patient"
  )
}


#' Read Claims Data
#'
#' @description
#' Reads claims data from various sources
#'
#' @param source Character. Path to file or database connection
#' @param format Character. Claims format: "837", "custom"
#' @param ... Additional arguments passed to rwe_read_ehr
#'
#' @return An rwe_ehr object with claims data
#'
#' @examples
#' \dontrun{
#' claims <- rwe_read_claims(
#'   source = "data/claims.csv",
#'   format = "custom"
#' )
#' }
#'
#' @export
rwe_read_claims <- function(source, format = "custom", ...) {

  log_info("Reading claims data", context = "rwe_read_claims")

  # Use EHR reader with claims-specific handling
  rwe_read_ehr(source, format = "custom", ...)
}


#' Read Registry Data
#'
#' @description
#' Reads registry data from various sources
#'
#' @param source Character. Path to file or database connection
#' @param registry_name Character. Registry name for metadata
#' @param ... Additional arguments passed to rwe_read_ehr
#'
#' @return An rwe_ehr object with registry data
#'
#' @examples
#' \dontrun{
#' registry <- rwe_read_registry(
#'   source = "data/cancer_registry.csv",
#'   registry_name = "SEER"
#' )
#' }
#'
#' @export
rwe_read_registry <- function(source, registry_name = NULL, ...) {

  log_info(paste("Reading registry data:",
                if (!is.null(registry_name)) registry_name),
          context = "rwe_read_registry")

  obj <- rwe_read_ehr(source, format = "custom", ...)
  obj$metadata$registry_name <- registry_name

  obj
}
