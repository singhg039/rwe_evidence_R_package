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
#' Reads EHR data from REST API endpoint with support for common patterns
#' including FHIR, SMART on FHIR, and custom JSON APIs
#'
#' @param url API endpoint URL
#' @param format Data format
#' @param auth_token Optional authentication token
#' @param auth_type Authentication type: "bearer", "basic", "api_key"
#' @param api_key API key for authentication
#' @param username Username for basic auth
#' @param password Password for basic auth
#' @param headers Named list of additional HTTP headers
#' @param query_params Named list of query parameters
#' @param page_size Number of records per page (for pagination)
#' @param max_pages Maximum number of pages to fetch (NULL for all)
#' @param timeout Request timeout in seconds
#' @param ... Additional arguments
#'
#' @return Data frame
#' @keywords internal
read_ehr_api <- function(url,
                         format,
                         auth_token = NULL,
                         auth_type = c("bearer", "basic", "api_key", "none"),
                         api_key = NULL,
                         username = NULL,
                         password = NULL,
                         headers = NULL,
                         query_params = NULL,
                         page_size = 100,
                         max_pages = NULL,
                         timeout = 30,
                         ...) {

  auth_type <- match.arg(auth_type)

  # Check for httr2 package
  if (!requireNamespace("httr2", quietly = TRUE)) {
    log_error("httr2 package required for API access", context = "read_ehr_api")
    stop("The httr2 package is required for API data loading.\n",
         "Install it with: install.packages('httr2')", call. = FALSE)
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    log_error("jsonlite package required for JSON parsing", context = "read_ehr_api")
    stop("The jsonlite package is required for API data loading.\n",
         "Install it with: install.packages('jsonlite')", call. = FALSE)
  }

  log_info(paste("Reading data from API:", url), context = "read_ehr_api")

  # Build base request
  req <- httr2::request(url) |>
    httr2::req_timeout(timeout)

  # Add authentication
  req <- add_api_auth(req, auth_type, auth_token, api_key, username, password)

  # Add custom headers
  if (!is.null(headers)) {
    req <- httr2::req_headers(req, .headers = headers)
  }

  # Add query parameters
  if (!is.null(query_params)) {
    req <- httr2::req_url_query(req, !!!query_params)
  }

  # Handle pagination
  all_data <- list()
  page_num <- 1
  has_more <- TRUE

  while (has_more) {
    log_debug(paste("Fetching page", page_num), context = "read_ehr_api")

    # Add pagination parameters (common patterns)
    page_req <- req |>
      httr2::req_url_query(
        limit = page_size,
        offset = (page_num - 1) * page_size,
        page = page_num
      )

    # Execute request
    resp <- tryCatch({
      httr2::req_perform(page_req)
    }, error = function(e) {
      log_error(paste("API request failed:", e$message), context = "read_ehr_api")
      stop("API request failed: ", e$message, call. = FALSE)
    })

    # Check response status
    if (httr2::resp_status(resp) != 200) {
      log_error(paste("API returned status:", httr2::resp_status(resp)),
                context = "read_ehr_api")
      stop("API request failed with status: ", httr2::resp_status(resp), call. = FALSE)
    }

    # Parse JSON response
    content <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    # Extract data based on format
    page_data <- extract_api_data(content, format)

    if (is.null(page_data) || nrow(page_data) == 0) {
      log_debug("No more data to fetch", context = "read_ehr_api")
      has_more <- FALSE
    } else {
      all_data[[page_num]] <- page_data
      log_info(paste("Fetched", nrow(page_data), "records from page", page_num),
               context = "read_ehr_api")

      # Check if we should continue
      if (!is.null(max_pages) && page_num >= max_pages) {
        log_info("Reached maximum page limit", context = "read_ehr_api")
        has_more <- FALSE
      } else if (nrow(page_data) < page_size) {
        log_debug("Received fewer records than page size, assuming last page",
                  context = "read_ehr_api")
        has_more <- FALSE
      } else {
        page_num <- page_num + 1
      }
    }
  }

  # Combine all pages
  if (length(all_data) == 0) {
    log_warning("No data retrieved from API", context = "read_ehr_api")
    return(data.frame())
  }

  combined_data <- do.call(rbind, all_data)
  log_info(paste("Total records retrieved:", nrow(combined_data)),
           context = "read_ehr_api")

  as.data.frame(combined_data)
}


#' Add API Authentication
#'
#' @description
#' Adds authentication to httr2 request
#'
#' @param req httr2 request object
#' @param auth_type Authentication type
#' @param auth_token Bearer token
#' @param api_key API key
#' @param username Username for basic auth
#' @param password Password for basic auth
#'
#' @return Modified request object
#' @keywords internal
add_api_auth <- function(req, auth_type, auth_token, api_key, username, password) {

  switch(auth_type,
    bearer = {
      if (!is.null(auth_token)) {
        req <- httr2::req_auth_bearer_token(req, auth_token)
      }
    },
    basic = {
      if (!is.null(username) && !is.null(password)) {
        req <- httr2::req_auth_basic(req, username, password)
      }
    },
    api_key = {
      if (!is.null(api_key)) {
        req <- httr2::req_headers(req, "X-API-Key" = api_key)
      }
    },
    none = {
      # No authentication
    }
  )

  req
}


#' Extract Data from API Response
#'
#' @description
#' Extracts data from JSON API response based on format
#'
#' @param content Parsed JSON content
#' @param format Data format
#'
#' @return Data frame
#' @keywords internal
extract_api_data <- function(content, format) {

  # Handle different API response structures
  if (format == "fhir") {
    # FHIR bundle structure
    if (!is.null(content$entry)) {
      return(as.data.frame(content$entry))
    }
  }

  # Common JSON array patterns
  if (is.data.frame(content)) {
    return(content)
  }

  # Check for common wrapper keys
  common_keys <- c("data", "results", "records", "entries", "items", "resources")
  for (key in common_keys) {
    if (!is.null(content[[key]])) {
      data <- content[[key]]
      if (is.data.frame(data) || is.list(data)) {
        return(as.data.frame(data))
      }
    }
  }

  # If content is a list, try to convert directly
  if (is.list(content)) {
    tryCatch({
      return(as.data.frame(content))
    }, error = function(e) {
      log_warning(paste("Could not convert API response to data frame:", e$message),
                  context = "extract_api_data")
    })
  }

  # Return empty data frame if unable to extract
  log_warning("Unable to extract data from API response",
              context = "extract_api_data")
  data.frame()
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
