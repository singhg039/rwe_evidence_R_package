#' ---
#' title: rwevidence API
#' plumber: TRUE
#' ---

library(rwevidence)
library(jsonlite)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

make_id <- function(prefix = "id") {
  stamp <- format(Sys.time(), "%Y%m%d%H%M%OS3")
  stamp <- gsub("[^0-9]", "", stamp)
  paste0(prefix, "_", stamp)
}

as_rows <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.data.frame(x)) {
    return(x)
  }
  jsonlite::fromJSON(jsonlite::toJSON(x, dataframe = "rows", auto_unbox = TRUE, na = "null"))
}

prepare_for_json <- function(x) {
  if (inherits(x, "data.frame")) {
    return(as_rows(x))
  }
  if (inherits(x, "matrix")) {
    return(as_rows(as.data.frame(x)))
  }
  if (inherits(x, "POSIXt")) {
    return(format(x, tz = "UTC", usetz = TRUE))
  }
  if (inherits(x, "Date")) {
    return(as.character(x))
  }
  if (is.list(x)) {
    return(lapply(x, prepare_for_json))
  }
  x
}

serialize_dataset <- function(data, name = NULL) {
  info <- extract_data_info(data)
  variables <- Map(
    function(col, type) {
      list(
        name = col,
        type = type,
        missing = sum(is.na(data[[col]]))
      )
    },
    info$col_names,
    info$col_types
  )

  list(
    name = name %||% "dataset",
    rows = info$n_rows,
    columns = info$n_cols,
    uploadedAt = format(Sys.time(), tz = "UTC", usetz = TRUE),
    variables = variables
  )
}

read_uploaded_dataset <- function(file_info, format = "custom") {
  filename <- file_info$filename %||% basename(file_info$datapath)
  datapath <- file_info$datapath
  ext <- tolower(tools::file_ext(filename))

  reader <- function() {
    obj <- rwe_read_ehr(datapath, format = format, validate = FALSE)
    if (inherits(obj, "rwe_ehr")) {
      return(obj$data)
    }
    obj
  }

  data <- tryCatch(reader(), error = function(e) {
    switch(ext,
      csv = utils::read.csv(datapath, stringsAsFactors = FALSE, check.names = FALSE),
      tsv = utils::read.delim(datapath, stringsAsFactors = FALSE, check.names = FALSE),
      txt = utils::read.table(datapath, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE),
      rds = readRDS(datapath),
      parquet = if (requireNamespace("arrow", quietly = TRUE)) arrow::read_parquet(datapath) else stop(e),
      stop(e)
    )
  })

  if (inherits(data, "tbl_df")) {
    data <- as.data.frame(data)
  }

  if (!is.data.frame(data)) {
    stop("Uploaded file could not be parsed as a data frame", call. = FALSE)
  }

  list(data = data, filename = filename)
}

api_state <- new.env(parent = emptyenv())
api_state$dataset <- NULL
api_state$dataset_name <- NULL
api_state$dataset_summary <- NULL
api_state$quality <- NULL
api_state$matching <- NULL
api_state$survival <- NULL
api_state$effectiveness <- NULL
api_state$safety <- NULL
api_state$reports <- list()

column_class <- function(data, column) {
  class(data[[column]])[1]
}

guess_column <- function(data, keywords, types = NULL) {
  cols <- names(data)
  lower <- tolower(cols)
  for (key in keywords) {
    hits <- which(grepl(key, lower, fixed = TRUE))
    if (length(hits) > 0) {
      for (idx in hits) {
        col <- cols[idx]
        cls <- column_class(data, col)
        if (is.null(types) || cls %in% types) {
          return(col)
        }
      }
    }
  }
  NULL
}

#* Enable CORS
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  if (identical(req$REQUEST_METHOD, "OPTIONS")) {
    res$status <- 200
    return(list())
  }
  forward()
}

#* Health check
#* @get /health
#* @serializer json
function() {
  list(status = "ok", timestamp = format(Sys.time(), tz = "UTC", usetz = TRUE))
}

#* Upload dataset
#* @post /data/upload
#* @parser multipart
#* @serializer json
function(req, res, format = "custom") {
  if (is.null(req$files) || length(req$files) == 0) {
    res$status <- 400
    return(list(error = "No file uploaded"))
  }

  uploaded <- tryCatch(
    read_uploaded_dataset(req$files[[1]], format),
    error = function(e) {
      res$status <- 400
      return(list(error = e$message))
    }
  )

  if (is.list(uploaded) && !is.null(uploaded$error)) {
    return(uploaded)
  }

  data <- uploaded$data
  summary <- serialize_dataset(data, uploaded$filename)

  api_state$dataset <- data
  api_state$dataset_name <- uploaded$filename
  api_state$dataset_summary <- summary

  summary
}

#* Dataset summary
#* @get /data/summary
#* @serializer json
function(res) {
  if (is.null(api_state$dataset_summary)) {
    res$status <- 404
    return(list(error = "No dataset uploaded"))
  }
  api_state$dataset_summary
}

#* Run quality assessment
#* @post /quality/run
#* @parser json
#* @serializer json
function(req, res) {
  if (is.null(api_state$dataset)) {
    res$status <- 400
    return(list(error = "No dataset uploaded"))
  }

  body <- req$body %||% list()
  dimensions <- body$dimensions %||% "all"

  report <- tryCatch(
    rwe_assess_quality(api_state$dataset, dimensions = dimensions, output_format = "list"),
    error = function(e) {
      res$status <- 400
      return(list(error = e$message))
    }
  )

  if (is.list(report) && !is.null(report$error)) {
    return(report)
  }

  response <- list(
    overall_score = report$overall_score,
    thresholds = report$thresholds,
    dimensions = prepare_for_json(report$dimensions),
    issues = prepare_for_json(report$issues),
    recommendations = prepare_for_json(report$recommendations)
  )

  api_state$quality <- response
  response
}

#* Run matching
#* @post /matching/run
#* @parser json
#* @serializer json
function(req, res) {
  if (is.null(api_state$dataset)) {
    res$status <- 400
    return(list(error = "No dataset uploaded"))
  }

  body <- req$body %||% list()
  treatment <- body$treatment
  covariates <- body$covariates
  ps_method <- body$ps_method %||% "logistic"
  match_method <- body$method %||% "nearest"
  ratio <- body$ratio %||% 1
  caliper <- body$caliper %||% 0.1

  if (is.null(treatment) || is.null(covariates)) {
    res$status <- 400
    return(list(error = "Parameters 'treatment' and 'covariates' are required"))
  }

  covariates <- if (is.character(covariates)) {
    trimws(unlist(strsplit(paste(covariates, collapse = ","), ",")))
  } else {
    as.character(covariates)
  }
  covariates <- covariates[nzchar(covariates)]

  result <- tryCatch({
    ps <- rwe_propensity_score(
      data = api_state$dataset,
      treatment = treatment,
      covariates = covariates,
      method = ps_method
    )
    rwe_match(
      ps_object = ps,
      method = match_method,
      ratio = ratio,
      caliper = caliper
    )
  }, error = function(e) {
    res$status <- 400
    return(list(error = e$message))
  })

  if (is.list(result) && !is.null(result$error)) {
    return(result)
  }

  response <- list(
    method = result$match_object$method,
    ratio = result$match_object$ratio,
    matched = result$metadata$n_matched,
    balance_improvement = result$balance_improvement %||% NA_real_,
    balance_before = prepare_for_json(result$balance_before),
    balance_after = prepare_for_json(result$balance_after)
  )

  api_state$matching <- response
  response
}

#* Run survival analysis
#* @post /survival/run
#* @parser json
#* @serializer json
function(req, res) {
  if (is.null(api_state$dataset)) {
    res$status <- 400
    return(list(error = "No dataset uploaded"))
  }

  body <- req$body %||% list()
  time_var <- body$time_var
  event_var <- body$event_var
  treatment <- body$treatment
  covariates <- body$covariates %||% NULL

  if (is.null(time_var)) {
    time_var <- guess_column(api_state$dataset,
      keywords = c("time_to_event", "survival_time", "time", "followup"),
      types = c("numeric", "integer", "double")
    )
  }
  if (is.null(event_var)) {
    event_var <- guess_column(api_state$dataset,
      keywords = c("event", "status", "outcome", "survival_event", "progression"),
      types = c("numeric", "integer", "logical", "double")
    )
  }
  if (is.null(treatment)) {
    treatment <- guess_column(api_state$dataset,
      keywords = c("treatment", "arm", "group", "cohort", "exposure")
    )
  }

  if (is.null(time_var) || is.null(event_var) || is.null(treatment)) {
    res$status <- 400
    return(list(error = "Parameters 'time_var', 'event_var', and 'treatment' are required"))
  }

  covariates <- if (is.null(covariates)) NULL else as.character(covariates)
  if (is.null(covariates)) {
    covariates <- setdiff(names(api_state$dataset), unique(c(time_var, event_var, treatment)))
  } else {
    covariates <- covariates[nzchar(covariates)]
  }

  result <- tryCatch(
    rwe_cox_regression(
      data = api_state$dataset,
      time_var = time_var,
      event_var = event_var,
      treatment = treatment,
      covariates = covariates
    ),
    error = function(e) {
      res$status <- 400
      return(list(error = e$message))
    }
  )

  if (is.list(result) && !is.null(result$error)) {
    return(result)
  }

  response <- list(
    treatment = result$treatment,
    covariates = result$covariates,
    n = result$n,
    n_events = result$n_events,
    coefficients = prepare_for_json(as.data.frame(result$coefficients)),
    conf_int = prepare_for_json(as.data.frame(result$conf_int)),
    concordance = result$concordance
  )

  api_state$survival <- response
  response
}

#* Run effectiveness metrics
#* @post /effectiveness/run
#* @parser json
#* @serializer json
function(req, res) {
  if (is.null(api_state$dataset)) {
    res$status <- 400
    return(list(error = "No dataset uploaded"))
  }

  body <- req$body %||% list()
  outcome <- body$outcome
  treatment <- body$treatment
  confounders <- body$confounders %||% NULL

  if (is.null(outcome)) {
    outcome <- guess_column(api_state$dataset,
      keywords = c("outcome", "event", "status", "response", "survival_event"),
      types = c("numeric", "integer", "logical", "double")
    )
  }
  if (is.null(treatment)) {
    treatment <- guess_column(api_state$dataset,
      keywords = c("treatment", "arm", "group", "cohort", "exposure")
    )
  }

  if (is.null(outcome) || is.null(treatment)) {
    res$status <- 400
    return(list(error = "Parameters 'outcome' and 'treatment' are required"))
  }

  confounders <- if (is.null(confounders)) {
    setdiff(names(api_state$dataset), unique(c(outcome, treatment)))
  } else {
    as.character(confounders)
  }
  confounders <- if (is.null(confounders)) NULL else confounders[nzchar(confounders)]

  rr <- tryCatch(
    rwe_relative_risk(
      data = api_state$dataset,
      outcome = outcome,
      treatment = treatment,
      confounders = confounders,
      quiet = TRUE
    ),
    error = function(e) e
  )

  if (inherits(rr, "error")) {
    res$status <- 400
    return(list(error = rr$message))
  }

  or <- rwe_odds_ratio(
    data = api_state$dataset,
    outcome = outcome,
    treatment = treatment,
    confounders = confounders,
    quiet = TRUE
  )

  nnt <- rwe_nnt(
    data = api_state$dataset,
    outcome = outcome,
    treatment = treatment,
    beneficial = body$beneficial %||% FALSE,
    quiet = TRUE
  )

  response <- list(
    relative_risk = list(
      estimate = rr$risk_ratio,
      ci_lower = rr$ci_lower,
      ci_upper = rr$ci_upper,
      p_value = rr$p_value
    ),
    odds_ratio = list(
      estimate = or$odds_ratio,
      ci_lower = or$ci_lower,
      ci_upper = or$ci_upper,
      p_value = or$p_value
    ),
    nnt = list(
      estimate = nnt$nnt,
      ci_lower = nnt$ci_lower,
      ci_upper = nnt$ci_upper,
      risk_difference = nnt$risk_difference
    )
  )

  api_state$effectiveness <- response
  response
}

#* Run safety surveillance
#* @post /safety/run
#* @parser json
#* @serializer json
function(req, res) {
  if (is.null(api_state$dataset)) {
    res$status <- 400
    return(list(error = "No dataset uploaded"))
  }

  body <- req$body %||% list()
  events <- body$events
  person_time <- body$person_time
  treatment <- body$treatment

  if (is.null(events)) {
    events <- guess_column(api_state$dataset,
      keywords = c("ae_count", "event_count", "events", "adverse_events", "events_total"),
      types = c("numeric", "integer", "double")
    )
  }
  if (is.null(person_time)) {
    person_time <- guess_column(api_state$dataset,
      keywords = c("person_time", "person_years", "py", "exposure_time", "followup"),
      types = c("numeric", "integer", "double")
    )
  }
  if (is.null(treatment)) {
    treatment <- guess_column(api_state$dataset,
      keywords = c("treatment", "arm", "group", "cohort", "exposure")
    )
  }

  if (is.null(events) || is.null(person_time) || is.null(treatment)) {
    res$status <- 400
    return(list(error = "Parameters 'events', 'person_time', and 'treatment' are required"))
  }

  result <- tryCatch(
    rwe_analyze_safety(
      data = api_state$dataset,
      events = events,
      person_time = person_time,
      treatment = treatment,
      confounders = body$confounders %||% NULL,
      time_var = body$time_var %||% NULL,
      alpha = body$alpha %||% 0.01,
      surveillance_method = body$surveillance_method %||% "cumulative_poisson",
      window_size = body$window_size %||% 25,
      scale = body$scale %||% 100,
      confidence_level = body$confidence_level %||% 0.95,
      quiet = TRUE
    ),
    error = function(e) {
      res$status <- 400
      return(list(error = e$message))
    }
  )

  if (is.list(result) && !is.null(result$error)) {
    return(result)
  }

  response <- list(
    incidence = prepare_for_json(result$incidence),
    rate_ratio = prepare_for_json(result$rate_ratio),
    safety_signal = prepare_for_json(result$safety_signal)
  )

  api_state$safety <- response
  response
}

#* Generate report metadata
#* @post /reports/<format>
#* @parser json
#* @serializer json
function(req, res, format) {
  if (is.null(api_state$dataset)) {
    res$status <- 400
    return(list(error = "No dataset uploaded"))
  }

  entry <- list(
    id = make_id("report"),
    label = sprintf("%s report", toupper(format)),
    format = format,
    generatedAt = format(Sys.time(), tz = "UTC", usetz = TRUE),
    author = "rwevidence-api",
    status = "ready"
  )

  api_state$reports <- c(list(entry), api_state$reports)
  entry
}
