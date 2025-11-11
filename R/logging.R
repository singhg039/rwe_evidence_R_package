#' Logging Utilities
#'
#' @description
#' Provides logging functionality for audit trails and debugging
#'
#' @keywords internal
#' @name logging


# Logger Environment ------------------------------------------------------

.logger_env <- new.env(parent = emptyenv())
.logger_env$log_level <- "INFO"
.logger_env$log_file <- NULL
.logger_env$log_history <- list()
.logger_env$console_output <- TRUE


# Setup Functions ---------------------------------------------------------

#' Setup Logger
#'
#' @description
#' Configures the logging system
#'
#' @param level Log level: "DEBUG", "INFO", "WARNING", "ERROR"
#' @param file Optional file path for log output
#' @param console Logical. Output to console (default: TRUE)
#' @param append Logical. Append to existing log file (default: TRUE)
#'
#' @return Invisibly returns logger configuration
#' @export
#'
#' @examples
#' \dontrun{
#' # Setup logging to file
#' setup_logger(level = "INFO", file = "rwevidence.log")
#'
#' # Console only logging
#' setup_logger(level = "DEBUG", console = TRUE)
#' }
setup_logger <- function(level = c("DEBUG", "INFO", "WARNING", "ERROR"),
                        file = NULL,
                        console = TRUE,
                        append = TRUE) {

  level <- match.arg(level)

  .logger_env$log_level <- level
  .logger_env$console_output <- console

  # Setup file logging
  if (!is.null(file)) {
    .logger_env$log_file <- file

    # Create or clear log file
    if (!append && file.exists(file)) {
      unlink(file)
    }

    # Write header
    write_log_header(file, append)
  }

  config <- list(
    level = level,
    file = file,
    console = console
  )

  message("Logger configured: level = ", level,
          if (!is.null(file)) paste0(", file = ", file))

  invisible(config)
}


#' Write Log Header
#'
#' @description
#' Writes header information to log file
#'
#' @param file Log file path
#' @param append Whether to append
#'
#' @keywords internal
write_log_header <- function(file, append) {

  header <- paste0(
    "\n",
    paste(rep("=", 70), collapse = ""), "\n",
    "rwevidence Log File\n",
    "Started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
    "R Version: ", R.version.string, "\n",
    paste(rep("=", 70), collapse = ""), "\n\n"
  )

  cat(header, file = file, append = append)
}


# Core Logging Functions --------------------------------------------------

#' Log Message
#'
#' @description
#' Core logging function
#'
#' @param level Log level
#' @param message Log message
#' @param context Optional context (function name)
#' @param ... Additional objects to log
#'
#' @keywords internal
log_message <- function(level, message, context = NULL, ...) {

  # Check if level should be logged
  level_order <- c("DEBUG" = 1, "INFO" = 2, "WARNING" = 3, "ERROR" = 4)
  if (level_order[level] < level_order[.logger_env$log_level]) {
    return(invisible(NULL))
  }

  # Format timestamp
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # Build log entry
  entry <- sprintf("[%s] [%s]%s %s",
                  timestamp,
                  level,
                  if (!is.null(context)) paste0(" [", context, "]") else "",
                  message)

  # Add additional objects if provided
  dots <- list(...)
  if (length(dots) > 0) {
    extra_info <- paste(
      sapply(dots, function(x) {
        if (is.atomic(x) && length(x) == 1) {
          as.character(x)
        } else {
          paste(utils::capture.output(str(x)), collapse = "\n  ")
        }
      }),
      collapse = "\n  "
    )
    entry <- paste0(entry, "\n  ", extra_info)
  }

  # Store in history
  .logger_env$log_history[[length(.logger_env$log_history) + 1]] <- list(
    timestamp = Sys.time(),
    level = level,
    context = context,
    message = message
  )

  # Output to console
  if (.logger_env$console_output) {
    if (level == "ERROR") {
      message(entry)  # Use message() for errors to show in red
    } else if (level == "WARNING") {
      message(entry)
    } else {
      cat(entry, "\n")
    }
  }

  # Write to file
  if (!is.null(.logger_env$log_file)) {
    cat(entry, "\n", file = .logger_env$log_file, append = TRUE)
  }

  invisible(entry)
}


#' Log Debug Message
#'
#' @description
#' Logs debug-level message (for detailed troubleshooting)
#'
#' @param message Log message
#' @param context Optional context
#' @param ... Additional objects
#'
#' @export
#'
#' @examples
#' \dontrun{
#' log_debug("Processing started", context = "rwe_read_ehr")
#' }
log_debug <- function(message, context = NULL, ...) {
  log_message("DEBUG", message, context, ...)
}


#' Log Info Message
#'
#' @description
#' Logs informational message
#'
#' @param message Log message
#' @param context Optional context
#' @param ... Additional objects
#'
#' @export
#'
#' @examples
#' \dontrun{
#' log_info("Data loaded successfully", context = "rwe_read_ehr")
#' }
log_info <- function(message, context = NULL, ...) {
  log_message("INFO", message, context, ...)
}


#' Log Warning Message
#'
#' @description
#' Logs warning message
#'
#' @param message Log message
#' @param context Optional context
#' @param ... Additional objects
#'
#' @export
#'
#' @examples
#' \dontrun{
#' log_warning("Missing values detected", context = "data_validation")
#' }
log_warning <- function(message, context = NULL, ...) {
  log_message("WARNING", message, context, ...)
}


#' Log Error Message
#'
#' @description
#' Logs error message
#'
#' @param message Log message
#' @param context Optional context
#' @param ... Additional objects
#'
#' @export
#'
#' @examples
#' \dontrun{
#' log_error("File not found", context = "rwe_read_ehr")
#' }
log_error <- function(message, context = NULL, ...) {
  log_message("ERROR", message, context, ...)
}


# Audit Trail Functions ---------------------------------------------------

#' Create Audit Trail
#'
#' @description
#' Creates audit trail for data transformations
#'
#' @param operation Operation name
#' @param input_data Input data summary
#' @param output_data Output data summary
#' @param parameters Operation parameters
#' @param user Optional user identifier
#'
#' @return Audit trail object
#' @export
#'
#' @examples
#' \dontrun{
#' audit <- create_audit_trail(
#'   operation = "harmonization",
#'   input_data = list(n_rows = 1000, n_cols = 50),
#'   output_data = list(n_rows = 1000, n_cols = 45),
#'   parameters = list(target_schema = "omop_cdm_5.4")
#' )
#' }
create_audit_trail <- function(operation,
                               input_data,
                               output_data,
                               parameters = list(),
                               user = Sys.info()["user"]) {

  trail <- list(
    operation = operation,
    timestamp = Sys.time(),
    user = user,
    input_summary = input_data,
    output_summary = output_data,
    parameters = parameters,
    session_info = list(
      r_version = R.version.string,
      platform = R.version$platform,
      package_version = tryCatch(
        as.character(utils::packageVersion("rwevidence")),
        error = function(e) "dev"
      )
    )
  )

  class(trail) <- c("rwe_audit_trail", "list")

  log_info(
    paste0("Audit trail created for operation: ", operation),
    context = "audit_trail"
  )

  trail
}


#' Print Audit Trail
#'
#' @param x Audit trail object
#' @param ... Additional arguments
#'
#' @export
print.rwe_audit_trail <- function(x, ...) {
  cat("\n=== RWE Audit Trail ===\n\n")
  cat("Operation:", x$operation, "\n")
  cat("Timestamp:", format(x$timestamp, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("User:", x$user, "\n\n")

  cat("Input Summary:\n")
  print(x$input_summary)

  cat("\nOutput Summary:\n")
  print(x$output_summary)

  if (length(x$parameters) > 0) {
    cat("\nParameters:\n")
    str(x$parameters)
  }

  cat("\nSession Info:\n")
  cat("  R Version:", x$session_info$r_version, "\n")
  cat("  Platform:", x$session_info$platform, "\n")
  cat("  Package Version:", x$session_info$package_version, "\n")

  invisible(x)
}


# Utility Functions -------------------------------------------------------

#' Get Log History
#'
#' @description
#' Retrieves log history
#'
#' @param n Number of recent entries to retrieve (NULL for all)
#' @param level Filter by log level (NULL for all)
#'
#' @return Data frame of log entries
#' @export
#'
#' @examples
#' \dontrun{
#' # Get last 10 log entries
#' recent_logs <- get_log_history(n = 10)
#'
#' # Get all error logs
#' errors <- get_log_history(level = "ERROR")
#' }
get_log_history <- function(n = NULL, level = NULL) {

  history <- .logger_env$log_history

  if (length(history) == 0) {
    return(data.frame(
      timestamp = character(),
      level = character(),
      context = character(),
      message = character()
    ))
  }

  # Convert to data frame
  df <- do.call(rbind, lapply(history, function(entry) {
    data.frame(
      timestamp = as.character(entry$timestamp),
      level = entry$level,
      context = if (is.null(entry$context)) NA_character_ else entry$context,
      message = entry$message,
      stringsAsFactors = FALSE
    )
  }))

  # Filter by level if specified
  if (!is.null(level)) {
    df <- df[df$level == level, ]
  }

  # Limit to n recent entries
  if (!is.null(n) && nrow(df) > n) {
    df <- tail(df, n)
  }

  df
}


#' Clear Log History
#'
#' @description
#' Clears in-memory log history
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clear_log_history()
#' }
clear_log_history <- function() {
  .logger_env$log_history <- list()
  log_info("Log history cleared", context = "logging")
  invisible(NULL)
}


#' Export Audit Trail
#'
#' @description
#' Exports audit trail to file
#'
#' @param trail Audit trail object or list of trails
#' @param file Output file path
#' @param format Output format: "json", "yaml", "rds"
#'
#' @export
#'
#' @examples
#' \dontrun{
#' export_audit_trail(audit, "audit_trail.json", format = "json")
#' }
export_audit_trail <- function(trail, file, format = c("json", "yaml", "rds")) {

  format <- match.arg(format)

  if (format == "json") {
    jsonlite::write_json(trail, file, pretty = TRUE, auto_unbox = TRUE)
  } else if (format == "yaml") {
    yaml::write_yaml(trail, file)
  } else if (format == "rds") {
    saveRDS(trail, file)
  }

  log_info(paste0("Audit trail exported to: ", file), context = "audit_trail")

  invisible(file)
}
