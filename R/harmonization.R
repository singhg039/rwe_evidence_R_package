#' Harmonize Data to Target Schema
#'
#' @description
#' Transforms RWD from source schema to target schema with terminology mapping
#' and data standardization
#'
#' @param data Source data object (data frame or rwe_data)
#' @param target_schema Target schema name (e.g., "omop_cdm_5.4")
#' @param mapping_config Mapping configuration list (NULL for auto-generation)
#' @param terminology_maps Named list of terminology mappings
#' @param validate Validate output against target schema
#' @param standardize_dates Standardize date/datetime formats
#' @param standardize_units Standardize measurement units
#'
#' @return rwe_harmonized object with transformed data
#'
#' @examples
#' \dontrun{
#' # Harmonize to OMOP CDM
#' harmonized <- rwe_harmonize(
#'   data = ehr_data,
#'   target_schema = "omop_cdm_5.4",
#'   terminology_maps = list(
#'     diagnosis = "icd10_to_snomed"
#'   )
#' )
#'
#' print(harmonized)
#' }
#'
#' @export
rwe_harmonize <- function(data,
                          target_schema,
                          mapping_config = NULL,
                          terminology_maps = list(),
                          validate = TRUE,
                          standardize_dates = TRUE,
                          standardize_units = TRUE) {

  log_info("Starting data harmonization", context = "rwe_harmonize")
  log_debug(paste("Target schema:", target_schema), context = "rwe_harmonize")

  # Extract data if rwe_data object
  source_schema <- NULL
  if (is_rwe_data(data)) {
    if (is_rwe_ehr(data)) {
      source_schema <- data$metadata$format
    }
    data_df <- data$data
  } else {
    data_df <- data
  }

  validate_inputs(data_df)

  cat("Starting data harmonization...\n")
  cat("Records:", nrow(data_df), "\n")
  cat("Target schema:", target_schema, "\n\n")

  # Load target schema definition
  target_def <- get_schema_definition(target_schema, extract_version(target_schema))

  if (length(target_def) == 0) {
    log_warning("Target schema definition not found", context = "rwe_harmonize")
    warning("Target schema definition not found. Proceeding without validation.",
            call. = FALSE)
  }

  # Detect source schema if not provided
  if (is.null(source_schema)) {
    source_schema <- detect_schema(data_df)
    log_debug(paste("Detected source schema:", source_schema),
              context = "rwe_harmonize")
  }

  # Load or generate mapping configuration
  if (is.null(mapping_config)) {
    cat("Generating mapping configuration...\n")
    mapping_config <- generate_mapping_config(data_df, source_schema, target_schema)
  }

  # Apply structural transformations
  cat("Applying structural transformations...\n")
  transformed_data <- apply_structural_mapping(data_df, mapping_config, target_def)

  # Apply terminology mappings
  if (length(terminology_maps) > 0) {
    cat("Applying terminology mappings...\n")
    transformed_data <- apply_terminology_mappings(transformed_data, terminology_maps)
  }

  # Standardize dates and times
  if (standardize_dates) {
    cat("Standardizing dates and times...\n")
    transformed_data <- standardize_datetime(transformed_data, target_def)
  }

  # Standardize units
  if (standardize_units) {
    cat("Standardizing measurement units...\n")
    transformed_data <- standardize_measurement_units(transformed_data, target_def)
  }

  # Validate harmonized data
  validation <- NULL
  if (validate && length(target_def) > 0) {
    cat("Validating harmonized data...\n")
    validation <- validate_against_schema(transformed_data, target_def)

    if (!validation$passed) {
      log_warning("Harmonization validation issues found", context = "rwe_harmonize")
      warning("Harmonization validation issues found. Check validation results.",
              call. = FALSE)
    } else {
      log_info("Harmonization validation passed", context = "rwe_harmonize")
    }
  }

  cat("\nHarmonization complete!\n")
  cat("Output records:", nrow(transformed_data), "\n")
  cat("Output variables:", ncol(transformed_data), "\n")

  log_info("Data harmonization complete", context = "rwe_harmonize")

  # Create harmonized object
  harmonized_obj <- new_rwe_harmonized(
    data = transformed_data,
    source_schema = source_schema,
    target_schema = target_schema,
    mapping_config = mapping_config,
    validation = validation
  )

  # Add audit trail
  harmonized_obj$audit_trail <- create_audit_trail(
    operation = "harmonization",
    input_data = list(
      n_rows = nrow(data_df),
      n_cols = ncol(data_df),
      source_schema = source_schema
    ),
    output_data = list(
      n_rows = nrow(transformed_data),
      n_cols = ncol(transformed_data),
      target_schema = target_schema
    ),
    parameters = list(
      target_schema = target_schema,
      n_terminology_maps = length(terminology_maps),
      standardize_dates = standardize_dates,
      standardize_units = standardize_units
    )
  )

  harmonized_obj
}


#' Detect Data Schema
#'
#' @description
#' Attempts to detect the schema/format of the input data
#'
#' @param data Input data frame
#' @return Character schema name
#' @keywords internal
detect_schema <- function(data) {

  col_names <- names(data)

  # Check for OMOP CDM patterns
  omop_indicators <- c("person_id", "visit_occurrence_id", "condition_concept_id",
                       "drug_concept_id", "measurement_concept_id")
  if (any(omop_indicators %in% col_names)) {
    return("omop_cdm")
  }

  # Check for FHIR patterns
  fhir_indicators <- c("resourceType", "identifier", "subject")
  if (any(fhir_indicators %in% col_names)) {
    return("fhir")
  }

  # Default to custom
  "custom"
}


#' Extract Version from Schema Name
#'
#' @description
#' Extracts version number from schema name (e.g., "omop_cdm_5.4" -> "5.4")
#'
#' @param schema_name Schema name
#' @return Version string or NULL
#' @keywords internal
extract_version <- function(schema_name) {

  version_pattern <- "([0-9]+\\.?[0-9]*)"
  match <- regmatches(schema_name, regexpr(version_pattern, schema_name))

  if (length(match) > 0) {
    return(match[1])
  }

  NULL
}


#' Generate Mapping Configuration
#'
#' @description
#' Generates mapping configuration between source and target schemas
#'
#' @param data Source data
#' @param source_schema Source schema name
#' @param target_schema Target schema name
#' @return List with mapping configuration
#' @keywords internal
generate_mapping_config <- function(data, source_schema, target_schema) {

  log_debug("Generating mapping configuration", context = "generate_mapping_config")

  # Get source and target column names
  source_cols <- names(data)

  # Simple mapping: try to match by similar names
  mapping <- list()

  for (col in source_cols) {
    # Standardize column name
    std_col <- standardize_variable_names(col, style = "snake_case")

    # For now, use direct mapping
    # In full implementation, would use lookup tables
    mapping[[col]] <- list(
      target = std_col,
      transform = "direct",
      required = FALSE
    )
  }

  list(
    source_schema = source_schema,
    target_schema = target_schema,
    column_mappings = mapping,
    generated_at = Sys.time()
  )
}


#' Apply Structural Mapping
#'
#' @description
#' Applies structural transformations based on mapping configuration
#'
#' @param data Source data
#' @param mapping_config Mapping configuration
#' @param target_def Target schema definition
#' @return Transformed data frame
#' @keywords internal
apply_structural_mapping <- function(data, mapping_config, target_def) {

  log_debug("Applying structural mapping", context = "apply_structural_mapping")

  # Create output data frame
  output_data <- data[, 0, drop = FALSE]

  # Apply column mappings
  for (source_col in names(mapping_config$column_mappings)) {
    if (!source_col %in% names(data)) {
      next
    }

    mapping <- mapping_config$column_mappings[[source_col]]
    target_col <- mapping$target

    # Apply transformation
    transformed_values <- switch(
      mapping$transform,
      direct = data[[source_col]],
      uppercase = toupper(data[[source_col]]),
      lowercase = tolower(data[[source_col]]),
      # Default: direct copy
      data[[source_col]]
    )

    output_data[[target_col]] <- transformed_values
  }

  # If output is empty, return original data with standardized names
  if (ncol(output_data) == 0) {
    output_data <- data
    names(output_data) <- standardize_variable_names(names(output_data))
  }

  output_data
}


#' Apply Terminology Mappings
#'
#' @description
#' Maps medical codes between terminology systems
#'
#' @param data Data with codes to map
#' @param terminology_maps Named list of mappings
#' @return Data with mapped codes
#' @keywords internal
apply_terminology_mappings <- function(data, terminology_maps) {

  log_debug("Applying terminology mappings", context = "apply_terminology_mappings")

  for (map_name in names(terminology_maps)) {
    map_type <- terminology_maps[[map_name]]

    # Apply mapping based on type
    data <- apply_single_terminology_map(data, map_type, map_name = map_name)
  }

  data
}

# Helper utilities -------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x)) {
    return(y)
  }
  if ((is.atomic(x) || is.list(x)) && length(x) == 0) {
    return(y)
  }
  x
}

normalize_terminology_system <- function(system) {
  upper <- toupper(system)
  gsub("[^A-Z0-9]", "", upper)
}

detect_terminology_columns <- function(data, from_system) {
  from_norm <- normalize_terminology_system(from_system)
  column_patterns <- list(
    ICD10CM = c("icd10", "icd_10", "dx", "diagnosis"),
    ICD9CM = c("icd9", "icd_9", "dx", "diagnosis"),
    SNOMED = c("snomed", "sctid", "concept_id"),
    NDC = c("ndc"),
    RXNORM = c("rxnorm", "rx_norm"),
    LOINC = c("loinc"),
    ICD10PCS = c("icd10pcs", "procedure"),
    CPT4 = c("cpt"),
    HCPCS = c("hcpcs"),
    LOCAL_LAB_CODE = c("lab_code", "local_lab", "local_test")
  )

  patterns <- column_patterns[[from_norm]]

  if (is.null(patterns)) {
    return(character())
  }

  cols <- names(data)
  matched <- vapply(
    cols,
    function(col) any(vapply(patterns, function(pat) grepl(pat, col, ignore.case = TRUE), logical(1))),
    logical(1)
  )

  cols[matched]
}


#' Apply Single Terminology Map
#'
#' @description
#' Applies a single terminology mapping
#'
#' @param data Data to transform
#' @param map_type Mapping type (e.g., "icd10_to_snomed")
#' @return Transformed data
#' @keywords internal
apply_single_terminology_map <- function(data, map_type, map_name = NULL) {

  map_info <- list()

  if (is.character(map_type) && length(map_type) == 1) {
    parts <- strsplit(tolower(map_type), "_to_")[[1]]
    if (length(parts) != 2) {
      stop("Invalid map_type string. Expected format 'from_to_to'.", call. = FALSE)
    }
    map_info$from <- parts[1]
    map_info$to <- parts[2]
  } else if (is.list(map_type)) {
    map_info <- map_type
  } else {
    stop("terminology map must be a string identifier or a list specification.", call. = FALSE)
  }

  if (is.null(map_info$from) || is.null(map_info$to)) {
    stop("Terminology mapping requires 'from' and 'to' systems.", call. = FALSE)
  }

  from_system <- map_info$from
  to_system <- map_info$to
  replace_original <- isTRUE(map_info$replace)
  mapping_table <- map_info$mapping_table %||% NULL
  columns <- map_info$columns %||% detect_terminology_columns(data, from_system)

  log_debug(
    paste("Applying terminology map:", from_system, "->", to_system, "for columns:", paste(columns, collapse = ", ")),
    context = "apply_single_terminology_map"
  )

  if (length(columns) == 0) {
    log_warning(
      paste0("No columns detected for ", from_system, " terminology mapping (", map_name %||% "unnamed", ")."),
      context = "apply_single_terminology_map"
    )
    return(data)
  }

  to_suffix <- tolower(normalize_terminology_system(to_system))

  for (col in columns) {
    mapped <- rwe_map_terminology(
      codes = as.character(data[[col]]),
      from_system = from_system,
      to_system = to_system,
      mapping_table = mapping_table
    )

    new_col_name <- map_info$new_column %||% if (replace_original) col else paste0(col, "_", to_suffix)

    if (replace_original) {
      data[[col]] <- mapped$target_code
    } else {
      data[[new_col_name]] <- mapped$target_code
    }
  }

  data
}


#' Standardize DateTime
#'
#' @description
#' Standardizes date and datetime columns to consistent format
#'
#' @param data Input data
#' @param target_def Target schema definition
#' @return Data with standardized dates
#' @keywords internal
standardize_datetime <- function(data, target_def) {

  log_debug("Standardizing datetime columns", context = "standardize_datetime")

  # Find date columns
  date_cols <- names(data)[sapply(data, function(x) {
    inherits(x, c("Date", "POSIXct", "POSIXt", "character"))
  })]

  for (col in date_cols) {
    # Try to parse as date if character
    if (is.character(data[[col]])) {
      data[[col]] <- tryCatch({
        as.Date(data[[col]])
      }, error = function(e) {
        # If parsing fails, return original
        data[[col]]
      })
    }

    # Convert POSIXct to Date if appropriate
    if (inherits(data[[col]], "POSIXct") && !grepl("time|datetime", col, ignore.case = TRUE)) {
      data[[col]] <- as.Date(data[[col]])
    }
  }

  data
}


#' Standardize Measurement Units
#'
#' @description
#' Standardizes measurement units to consistent system
#'
#' @param data Input data
#' @param target_def Target schema definition
#' @return Data with standardized units
#' @keywords internal
standardize_measurement_units <- function(data, target_def) {

  log_debug("Standardizing measurement units", context = "standardize_measurement_units")

  value_cols <- names(data)[grepl("value", names(data), ignore.case = TRUE)]
  for (value_col in value_cols) {
    unit_col_candidates <- c(
      paste0(value_col, "_unit"),
      gsub("value", "unit", value_col, ignore.case = TRUE),
      gsub("_value", "_unit", value_col, ignore.case = TRUE)
    )
    unit_col <- unit_col_candidates[unit_col_candidates %in% names(data)][1]

    if (is.na(unit_col) || is.null(unit_col)) {
      next
    }

    units_present <- unique(na.omit(as.character(data[[unit_col]])))

    for (unit in units_present) {
      conversion <- UNIT_CONVERSIONS[UNIT_CONVERSIONS$source_unit == unit, ]
      if (nrow(conversion) == 0) {
        next
      }

      idx <- which(as.character(data[[unit_col]]) == unit)
      data[[value_col]][idx] <- (data[[value_col]][idx] + conversion$offset) * conversion$factor
      data[[unit_col]][idx] <- conversion$target_unit
    }
  }

  data
}


#' Validate Against Schema
#'
#' @description
#' Validates data against target schema definition
#'
#' @param data Data to validate
#' @param schema_def Schema definition
#' @return List with validation results
#' @keywords internal
validate_against_schema <- function(data, schema_def) {

  log_debug("Validating against schema", context = "validate_against_schema")

  # Use existing check_data_structure function
  validation <- check_data_structure(data, schema_def)

  list(
    passed = validation$passed,
    message = validation$message,
    missing_columns = validation$missing_columns,
    type_mismatches = validation$type_mismatches
  )
}


#' Map Medical Terminology
#'
#' @description
#' Maps medical codes between different terminology systems
#' (e.g., ICD-10 to SNOMED, NDC to RxNorm)
#'
#' @param codes Character vector of codes to map
#' @param from_system Source terminology system
#' @param to_system Target terminology system
#' @param mapping_table Optional custom mapping table (data frame with 'source_code' and 'target_code')
#'
#' @return Data frame with original and mapped codes
#'
#' @examples
#' \dontrun{
#' # Map ICD-10 to SNOMED
#' mapped <- rwe_map_terminology(
#'   codes = c("E11.9", "I10", "J44.0"),
#'   from_system = "icd10cm",
#'   to_system = "snomed"
#' )
#'
#' print(mapped)
#' }
#'
#' @export
rwe_map_terminology <- function(codes,
                                from_system,
                                to_system,
                                mapping_table = NULL) {

  log_info(paste("Mapping terminology from", from_system, "to", to_system),
          context = "rwe_map_terminology")

  # Validate inputs
  if (!is.character(codes)) {
    stop("codes must be a character vector", call. = FALSE)
  }

  # Create output data frame
  result <- data.frame(
    source_code = codes,
    source_system = from_system,
    target_code = NA_character_,
    target_system = to_system,
    mapping_quality = "unmapped",
    stringsAsFactors = FALSE
  )

  # Load mapping table if not provided
  if (is.null(mapping_table)) {
    mapping_table <- load_standard_mapping(from_system, to_system)
  }

  # Perform mapping if table available
  if (!is.null(mapping_table) && nrow(mapping_table) > 0) {
    match_idx <- match(result$source_code, mapping_table$source_code)
    result$target_code <- mapping_table$target_code[match_idx]
    if ("mapping_quality" %in% names(mapping_table)) {
      result$mapping_quality <- mapping_table$mapping_quality[match_idx]
      result$mapping_quality[is.na(result$mapping_quality) & !is.na(result$target_code)] <- "mapped"
    } else {
      result$mapping_quality <- ifelse(!is.na(result$target_code), "mapped", "unmapped")
    }
  }

  log_info(paste("Mapped", sum(!is.na(result$target_code)), "of", length(codes), "codes"),
          context = "rwe_map_terminology")

  # Clean up result
  result <- result[, c("source_code", "source_system", "target_code",
                      "target_system", "mapping_quality")]

  class(result) <- c("rwe_terminology_mapping", "data.frame")

  result
}


#' Load Standard Mapping
#'
#' @description
#' Loads standard terminology mapping table
#'
#' @param from_system Source system
#' @param to_system Target system
#' @return Mapping data frame or NULL
#' @keywords internal
load_standard_mapping <- function(from_system, to_system) {

  log_debug(paste("Loading standard mapping:", from_system, "->", to_system),
            context = "load_standard_mapping")

  key <- paste0(
    normalize_terminology_system(from_system),
    "__TO__",
    normalize_terminology_system(to_system)
  )

  mapping <- STANDARD_TERMINOLOGY_TABLES[[key]]

  if (is.null(mapping)) {
    log_warning(
      paste("No standard mapping table available for", from_system, "->", to_system),
      context = "load_standard_mapping"
    )
    return(NULL)
  }

  mapping
}


#' Print Method for Terminology Mapping
#'
#' @param x rwe_terminology_mapping object
#' @param ... Additional arguments
#'
#' @export
print.rwe_terminology_mapping <- function(x, ...) {
  cat("\n<rwe_terminology_mapping>\n\n")
  cat("Mapping:", unique(x$source_system), "->", unique(x$target_system), "\n")
  cat("Total codes:", nrow(x), "\n")
  cat("Mapped:", sum(x$mapping_quality == "mapped"), "\n")
  cat("Unmapped:", sum(x$mapping_quality == "unmapped"), "\n\n")

  cat("Sample:\n")
  print(utils::head(x, 10))

  invisible(x)
}
