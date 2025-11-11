# Regulatory Figure Templates

#' Forest Plot for Subgroup Analyses
#'
#' @description
#' Generates a standardized forest plot for subgroup effectiveness analyses.
#' Accepts either an \code{rwe_subgroup_analysis} object or a data frame with
#' columns \code{subgroup}, \code{level}, \code{estimate}, \code{ci_lower},
#' and \code{ci_upper}.
#'
#' @param results An \code{rwe_subgroup_analysis} object or compatible data frame.
#' @param reference_line Numeric value for the null effect (default: 1).
#'   Use 0 when plotting risk differences.
#' @param title Plot title.
#' @param subtitle Optional subtitle.
#' @param x_label Label for the x axis (default determined from data).
#' @param caption Optional caption text.
#'
#' @return A \code{ggplot} object.
#' @export
#'
#' @examples
#' \dontrun{
#' forest <- rwe_subgroup_analysis(
#'   data = cohort,
#'   outcome = "event",
#'   treatment = "treated",
#'   subgroup_vars = c("sex", "age_group")
#' )
#' p <- rwe_figure_forest(forest, reference_line = 1)
#' }
rwe_figure_forest <- function(results,
                              reference_line = 1,
                              title = "Subgroup Effect Estimates",
                              subtitle = NULL,
                              x_label = NULL,
                              caption = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.", call. = FALSE)
  }

  data <- if (inherits(results, "rwe_subgroup_analysis")) {
    results$results
  } else if (is.data.frame(results)) {
    results
  } else {
    stop("results must be an rwe_subgroup_analysis or data frame.", call. = FALSE)
  }

  required_cols <- c("subgroup", "level", "estimate", "ci_lower", "ci_upper")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  data$label <- paste(data$subgroup, data$level, sep = ": ")
  data$label <- factor(data$label, levels = rev(unique(data$label)))

  if (is.null(x_label)) {
    x_label <- if (reference_line == 0) "Effect Estimate (Difference)" else "Effect Estimate"
  }

  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = estimate, y = label)
  ) +
    ggplot2::geom_point(color = "#1f78b4", size = 3) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
      height = 0.2,
      color = "#1f78b4"
    ) +
    ggplot2::geom_vline(
      xintercept = reference_line,
      linetype = "dashed",
      color = "#6b6b6b"
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = NULL,
      caption = caption
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  plot
}


#' Kaplan-Meier Survival Figure
#'
#' @description
#' Creates a ggplot-based Kaplan-Meier survival curve with confidence intervals.
#' Accepts an \code{rwe_km} object or a \code{survival::survfit} object.
#'
#' @param results An \code{rwe_km} object or \code{survival::survfit}.
#' @param title Plot title.
#' @param subtitle Optional subtitle.
#' @param time_label X-axis label (default: "Time").
#' @param survival_label Y-axis label (default: "Survival Probability").
#' @param caption Optional caption.
#'
#' @return A \code{ggplot} object.
#' @export
rwe_figure_survival <- function(results,
                                title = "Kaplan-Meier Survival Curves",
                                subtitle = NULL,
                                time_label = "Time",
                                survival_label = "Survival Probability",
                                caption = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.", call. = FALSE)
  }

  survfit_obj <- if (inherits(results, "rwe_km")) {
    results$survfit
  } else if (inherits(results, "survfit")) {
    results
  } else {
    stop("results must be an rwe_km or survfit object.", call. = FALSE)
  }

  surv_summary <- summary(survfit_obj)
  data <- data.frame(
    time = surv_summary$time,
    surv = surv_summary$surv,
    lower = surv_summary$lower,
    upper = surv_summary$upper,
    strata = surv_summary$strata,
    stringsAsFactors = FALSE
  )

  if (is.null(data$strata)) {
    data$strata <- "Overall"
  }

  data$strata <- factor(data$strata)

  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = time, y = surv, color = strata, fill = strata)
  ) +
    ggplot2::geom_step(linewidth = 0.9) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower, ymax = upper),
      alpha = 0.15,
      color = NA
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = time_label,
      y = survival_label,
      color = "Stratum",
      fill = "Stratum",
      caption = caption
    ) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank()
    )

  plot
}


#' Top Adverse Event Bar Plot
#'
#' @description
#' Generates a bar plot for the most frequent adverse events by treatment group
#' using an \code{rwe_safety_report} object.
#'
#' @param safety_report An \code{rwe_safety_report} object.
#' @param top_n Number of top adverse events to display (default: 10).
#' @param percent Logical; if \code{TRUE}, plot patient-normalized rates
#'   (AEs per 100 patients). Otherwise, plots absolute counts.
#' @param title Plot title.
#' @param subtitle Optional subtitle.
#' @param caption Optional caption.
#'
#' @return A \code{ggplot} object.
#' @export
rwe_figure_top_ae <- function(safety_report,
                              top_n = 10,
                              percent = TRUE,
                              title = "Top Adverse Events",
                              subtitle = NULL,
                              caption = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.", call. = FALSE)
  }

  if (!inherits(safety_report, "rwe_safety_report")) {
    stop("safety_report must be an rwe_safety_report object.", call. = FALSE)
  }

  groups <- safety_report$groups

  ae_frame <- lapply(groups, function(grp) {
    info <- safety_report$summary[[as.character(grp)]]
    ae_names <- names(info$ae_counts)
    counts <- as.numeric(info$ae_counts)
    names(counts) <- ae_names
    denom <- if (!is.null(info$n_patients)) info$n_patients else safety_report$n_per_group[[as.character(grp)]]
    rates <- (counts / denom) * 100

    data.frame(
      group = grp,
      ae = ae_names,
      count = counts,
      rate = rates,
      stringsAsFactors = FALSE
    )
  })

  ae_data <- do.call(rbind, ae_frame)

  if (nrow(ae_data) == 0) {
    stop("No adverse event data available in safety_report.", call. = FALSE)
  }

  aggregated <- aggregate(
    list(total = if (percent) ae_data$rate else ae_data$count),
    by = list(ae = ae_data$ae),
    FUN = sum
  )

  top_ae <- head(aggregated[order(-aggregated$total), "ae"], top_n)

  plot_data <- ae_data[ae_data$ae %in% top_ae, ]
  plot_data$metric <- if (percent) plot_data$rate else plot_data$count
  metric_label <- if (percent) "Events per 100 Patients" else "AE Count"

  plot_data$ae <- factor(
    plot_data$ae,
    levels = rev(top_ae)
  )

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = ae, y = metric, fill = group)
  ) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.85)) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = metric_label,
      fill = "Group",
      caption = caption
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank()
    )

  plot
}


#' CONSORT Flow Diagram Figure
#'
#' @description
#' Creates a visual CONSORT diagram using DiagrammeR from an
#' \code{rwe_consort} object.
#'
#' @param consort An \code{rwe_consort} object from \code{rwe_consort_diagram()}.
#' @param layout Graph layout direction: "LR" (left-to-right) or "TB" (top-to-bottom).
#'
#' @return A \code{DiagrammeR::grViz} object.
#' @export
rwe_figure_consort <- function(consort, layout = c("LR", "TB")) {

  layout <- match.arg(layout)

  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("Package 'DiagrammeR' is required but not installed.", call. = FALSE)
  }

  if (!inherits(consort, "rwe_consort")) {
    stop("consort must be an rwe_consort object.", call. = FALSE)
  }

  label_screened <- sprintf("Assessed for eligibility\\n(n = %s)", format(consort$screened, big.mark = ","))
  label_enrolled <- sprintf("Enrolled / Allocated\\n(n = %s)", format(consort$enrolled, big.mark = ","))

  nodes <- list(
    sprintf('screened [label="%s"]', label_screened),
    sprintf('enrolled [label="%s"]', label_enrolled)
  )
  edges <- c("screened -> enrolled")

  if (!is.null(consort$excluded) && length(consort$excluded) > 0) {
    excluded_total <- sum(unlist(consort$excluded))
    reason_lines <- paste(sprintf("%s: %s", names(consort$excluded),
                                  unlist(consort$excluded)), collapse = "\\n")
    label_excluded <- sprintf("Excluded\\n(n = %s)\\n%s",
                              format(excluded_total, big.mark = ","),
                              reason_lines)
    nodes <- c(nodes, sprintf('excluded [label="%s"]', label_excluded))
    edges <- c(sprintf('screened -> excluded [label="Excluded"]'), edges)
  }

  for (arm in consort$arms) {
    alloc <- consort$allocated[[arm]]
    alloc_label <- sprintf("Allocated to %s\\n(n = %s)", arm, format(alloc, big.mark = ","))
    alloc_node <- paste0("alloc_", arm)
    nodes <- c(nodes, sprintf('%s [label="%s"]', alloc_node, alloc_label))
    edges <- c(edges, sprintf("enrolled -> %s", alloc_node))

    n_lost <- if (!is.null(consort$lost_to_followup)) consort$lost_to_followup[[arm]] else 0
    discontinued <- if (!is.null(consort$discontinued)) consort$discontinued[[arm]] else NULL
    n_discontinued <- if (is.null(discontinued)) 0 else sum(unlist(discontinued))

    follow_label_lines <- c()
    if (n_lost > 0) {
      follow_label_lines <- c(follow_label_lines, sprintf("Lost to follow-up: %s", format(n_lost, big.mark = ",")))
    }
    if (n_discontinued > 0) {
      follow_label_lines <- c(follow_label_lines, sprintf("Discontinued intervention: %s", format(n_discontinued, big.mark = ",")))
      if (is.list(discontinued)) {
        detailed <- sprintf("  %s: %s",
                             names(discontinued),
                             format(unlist(discontinued), big.mark = ","))
        follow_label_lines <- c(follow_label_lines, paste(detailed, collapse = "\\n"))
      }
    }

    follow_label <- if (length(follow_label_lines) > 0) {
      paste(c("Follow-up", follow_label_lines), collapse = "\\n")
    } else {
      NULL
    }

    follow_node <- paste0("follow_", arm)
    if (!is.null(follow_label)) {
      nodes <- c(nodes, sprintf('%s [label="%s"]', follow_node, follow_label))
      edges <- c(edges, sprintf("%s -> %s", alloc_node, follow_node))
    } else {
      follow_node <- alloc_node
    }

    analyzed <- consort$analyzed[[arm]]
    retention <- consort$retention_rates[[arm]]
    analyzed_label <- sprintf("Analyzed (%s)\\n(n = %s)\\nRetention: %.1f%%",
                              arm,
                              format(analyzed, big.mark = ","),
                              retention)
    analyzed_node <- paste0("analyzed_", arm)
    nodes <- c(nodes, sprintf('%s [label="%s"]', analyzed_node, analyzed_label))
    edges <- c(edges, sprintf("%s -> %s", follow_node, analyzed_node))
  }

  overall_attrition <- sprintf("Overall attrition: %.1f%%%%", consort$overall_attrition)
  footer_node <- "attrition_summary"
  nodes <- c(nodes, sprintf('%s [label="%s", shape=plaintext]', footer_node, overall_attrition))
  for (arm in consort$arms) {
    analyzed_node <- paste0("analyzed_", arm)
    edges <- c(edges, sprintf("%s -> %s [style=dashed]", analyzed_node, footer_node))
  }

  graph <- paste(
    "digraph consort {",
    sprintf('  graph [rankdir = %s, labelloc = "t", label = "%s"];', layout, consort$title),
    '  node [shape = rectangle, fontname = "Helvetica", width = 3];',
    '  edge [fontname = "Helvetica"];',
    paste0("  ", nodes, collapse = "\n"),
    paste0("  ", edges, collapse = "\n"),
    "}",
    sep = "\n"
  )

  DiagrammeR::grViz(graph)
}
