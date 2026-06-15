# ============================================================================
# UHN De-Identification Tool — Pre-Computation Functions
# Run on raw data BEFORE de-identification.
#
# All survival functions share the same interface:
#   df         — data frame containing the variables
#   id         — name of the patient ID column
#   start      — name of the start date column (time origin)
#   event      — name of the event date column (NA if no event)
#   censor     — name of the censor date column (e.g. last follow-up)
#   time_unit  — "days", "months", or "years"
#
# For PFS where the event is progression OR death (whichever first),
# create a composite event date before calling the function:
#   df$pfs_event_date <- pmin(df$progression_date, df$death_date, na.rm = TRUE)
#
# Similarly for RFS where the event is relapse OR death:
#   df$rfs_event_date <- pmin(df$relapse_date, df$death_date, na.rm = TRUE)
# ============================================================================

# Load parse_date_column from deid_functions.R if not already loaded ----
if (!exists("parse_date_column")) {
  source("deid_functions.R")
}


# Time Unit Divisor ----
get_time_divisor <- function(time_unit) {
  switch(
    tolower(time_unit),
    "days" = 1,
    "months" = 30.44,
    "years" = 365.25,
    stop("time_unit must be 'days', 'months', or 'years'. Got: ", time_unit)
  )
}


# Safe Date Parser ----
# Ensures a column is Date class, parsing if needed.
safe_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXct")) {
    return(as.Date(x))
  }
  parse_date_column(x)
}


# Compute Age ----
# Returns: data.frame(id, age_years)
#
# Parameters:
#   df       — data frame
#   id       — name of the ID column
#   dob      — name of the date of birth column
#   ref_date — name of the reference date column (age computed at this date)
#
# Example:
#   age_df <- compute_age(df, "mrn", "dob", "diagnosis_date")
#   df <- merge(df, age_df, by = "mrn")
#
compute_age <- function(df, id, dob, ref_date) {
  # Extract and parse ----
  id_vals <- df[[id]]
  dob_vals <- safe_date(df[[dob]])
  ref_vals <- safe_date(df[[ref_date]])

  # Compute ----
  age_days <- as.numeric(difftime(ref_vals, dob_vals, units = "days"))
  age_years <- round(age_days / 365.25, 2)

  # Validate ----
  n_neg <- sum(age_years < 0, na.rm = TRUE)
  if (n_neg > 0) {
    warning(
      n_neg,
      " negative age(s) detected (DOB after reference date). Set to NA."
    )
    age_years[age_years < 0] <- NA_real_
  }

  result <- data.frame(id_vals, age_years)
  names(result) <- c(id, "age_years")
  result
}


# Compute Overall Survival ----
# Returns: data.frame(id, os_time_{unit}, os_status)
#
# Parameters:
#   df        — data frame
#   id        — name of the ID column
#   start     — name of the start date column (e.g. diagnosis date)
#   event     — name of the event date column (e.g. death date, NA if alive)
#   censor    — name of the censor date column (e.g. last follow-up)
#   time_unit — "days", "months", or "years"
#
# Example:
#   os_df <- compute_os(df, "mrn", "diagnosis_date", "death_date",
#                       "last_followup", time_unit = "years")
#   df <- merge(df, os_df, by = "mrn")
#
compute_os <- function(df, id, start, event, censor, time_unit = "years") {
  divisor <- get_time_divisor(time_unit)

  # Extract and parse ----
  id_vals <- df[[id]]
  start_vals <- safe_date(df[[start]])
  event_vals <- safe_date(df[[event]])
  censor_vals <- safe_date(df[[censor]])

  n <- nrow(df)
  os_time <- rep(NA_real_, n)
  os_status <- rep(NA_integer_, n)

  for (i in seq_len(n)) {
    if (is.na(start_vals[i])) {
      next
    }

    if (!is.na(event_vals[i])) {
      # Event occurred ----
      os_time[i] <- as.numeric(difftime(
        event_vals[i],
        start_vals[i],
        units = "days"
      ))
      os_status[i] <- 1L
    } else if (!is.na(censor_vals[i])) {
      # Censored ----
      os_time[i] <- as.numeric(difftime(
        censor_vals[i],
        start_vals[i],
        units = "days"
      ))
      os_status[i] <- 0L
    }
  }

  # Convert time unit ----
  os_time <- round(os_time / divisor, 2)

  # Validate ----
  n_neg <- sum(os_time < 0, na.rm = TRUE)
  if (n_neg > 0) {
    warning(n_neg, " negative OS time(s). Check date ordering.")
  }

  # Build output ----
  time_col <- paste0("os_time_", time_unit)
  result <- data.frame(id_vals, os_time, os_status)
  names(result) <- c(id, time_col, "os_status")
  result
}


# Compute Progression-Free Survival ----
# Returns: data.frame(id, pfs_time_{unit}, pfs_status)
#
# The event column should contain the EARLIEST of progression or death.
# Pre-compute this as:
#   df$pfs_event_date <- pmin(df$progression_date, df$death_date, na.rm = TRUE)
#
# Parameters:
#   df        — data frame
#   id        — name of the ID column
#   start     — name of the start date column (e.g. diagnosis date)
#   event     — name of the event date column (earliest of progression/death)
#   censor    — name of the censor date column (e.g. last follow-up)
#   time_unit — "days", "months", or "years"
#
# Example:
#   df$pfs_event_date <- pmin(df$progression_date, df$death_date, na.rm = TRUE)
#   pfs_df <- compute_pfs(df, "mrn", "diagnosis_date", "pfs_event_date",
#                         "last_followup", time_unit = "years")
#   df <- merge(df, pfs_df, by = "mrn")
#
compute_pfs <- function(df, id, start, event, censor, time_unit = "years") {
  divisor <- get_time_divisor(time_unit)

  # Extract and parse ----
  id_vals <- df[[id]]
  start_vals <- safe_date(df[[start]])
  event_vals <- safe_date(df[[event]])
  censor_vals <- safe_date(df[[censor]])

  n <- nrow(df)
  pfs_time <- rep(NA_real_, n)
  pfs_status <- rep(NA_integer_, n)

  for (i in seq_len(n)) {
    if (is.na(start_vals[i])) {
      next
    }

    if (!is.na(event_vals[i])) {
      # Event occurred ----
      pfs_time[i] <- as.numeric(difftime(
        event_vals[i],
        start_vals[i],
        units = "days"
      ))
      pfs_status[i] <- 1L
    } else if (!is.na(censor_vals[i])) {
      # Censored ----
      pfs_time[i] <- as.numeric(difftime(
        censor_vals[i],
        start_vals[i],
        units = "days"
      ))
      pfs_status[i] <- 0L
    }
  }

  # Convert time unit ----
  pfs_time <- round(pfs_time / divisor, 2)

  # Validate ----
  n_neg <- sum(pfs_time < 0, na.rm = TRUE)
  if (n_neg > 0) {
    warning(n_neg, " negative PFS time(s). Check date ordering.")
  }

  # Build output ----
  time_col <- paste0("pfs_time_", time_unit)
  result <- data.frame(id_vals, pfs_time, pfs_status)
  names(result) <- c(id, time_col, "pfs_status")
  result
}


# Compute Relapse-Free Survival ----
# Returns: data.frame(id, rfs_time_{unit}, rfs_status)
#
# Time origin is END OF TREATMENT, not diagnosis.
# The event column should contain the EARLIEST of relapse or death.
# Pre-compute this as:
#   df$rfs_event_date <- pmin(df$relapse_date, df$death_date, na.rm = TRUE)
#
# Patients without an EOT date get NA (RFS is undefined).
#
# Parameters:
#   df        — data frame
#   id        — name of the ID column
#   start     — name of the end-of-treatment date column
#   event     — name of the event date column (earliest of relapse/death)
#   censor    — name of the censor date column (e.g. last follow-up)
#   time_unit — "days", "months", or "years"
#
# Example:
#   df$rfs_event_date <- pmin(df$relapse_date, df$death_date, na.rm = TRUE)
#   rfs_df <- compute_rfs(df, "mrn", "eot_date", "rfs_event_date",
#                         "last_followup", time_unit = "years")
#   df <- merge(df, rfs_df, by = "mrn")
#
compute_rfs <- function(df, id, start, event, censor, time_unit = "years") {
  divisor <- get_time_divisor(time_unit)

  # Extract and parse ----
  id_vals <- df[[id]]
  start_vals <- safe_date(df[[start]])
  event_vals <- safe_date(df[[event]])
  censor_vals <- safe_date(df[[censor]])

  n <- nrow(df)
  rfs_time <- rep(NA_real_, n)
  rfs_status <- rep(NA_integer_, n)

  for (i in seq_len(n)) {
    # RFS undefined without end of treatment ----
    if (is.na(start_vals[i])) {
      next
    }

    if (!is.na(event_vals[i])) {
      # Event occurred ----
      rfs_time[i] <- as.numeric(difftime(
        event_vals[i],
        start_vals[i],
        units = "days"
      ))
      rfs_status[i] <- 1L
    } else if (!is.na(censor_vals[i])) {
      # Censored ----
      rfs_time[i] <- as.numeric(difftime(
        censor_vals[i],
        start_vals[i],
        units = "days"
      ))
      rfs_status[i] <- 0L
    }
  }

  # Convert time unit ----
  rfs_time <- round(rfs_time / divisor, 2)

  # Validate ----
  n_neg <- sum(rfs_time < 0, na.rm = TRUE)
  if (n_neg > 0) {
    warning(n_neg, " negative RFS time(s). Events before end of treatment?")
  }

  # Build output ----
  time_col <- paste0("rfs_time_", time_unit)
  result <- data.frame(id_vals, rfs_time, rfs_status)
  names(result) <- c(id, time_col, "rfs_status")
  result
}


#' Summarise Kaplan-Meier Survival Estimates
#'
#' Computes median survival time and survival probabilities at user-specified
#' timepoints from a Kaplan-Meier fit, stratified by a grouping variable.
#' Confidence intervals are estimated using the log-log transformation.
#'
#' @param data A data frame containing the survival data.
#' @param time Character string naming the time-to-event variable in \code{data}.
#' @param status Character string naming the censoring status variable in
#'   \code{data} (1 = event, 0 = censored).
#' @param group Character string naming the grouping variable in \code{data}.
#' @param times Numeric vector of timepoints at which to estimate survival
#'   probabilities.
#' @param time_unit Character string describing the unit of time (e.g.
#'   \code{"months"}, \code{"days"}). Required; used in column labels.
#' @param digits_rate Integer. Number of decimal places for survival
#'   probabilities. Default \code{2}.
#' @param digits_time Integer. Number of decimal places for time-based
#'   estimates (median, max follow-up). Default \code{1}.
#' @param time_lbls Optional character vector of custom column labels for the
#'   survival probability columns. Must be the same length as \code{times}.
#' @param overall Logical. If \code{TRUE}, an additional "Overall" row
#'   (ignoring the grouping variable) is appended. Default \code{FALSE}.
#' @param tableOnly Logical. If \code{TRUE}, return the raw data frame with
#'   generic column names and skip pretty labelling. Default \code{FALSE}.
#'
#' @return When \code{tableOnly = TRUE}, a data frame with one row per group
#'   (plus an optional overall row) and the following columns:
#'   \describe{
#'     \item{\code{<group>}}{Level of the grouping variable (column named after
#'       the \code{group} argument).}
#'     \item{\code{n}}{Number of subjects in the group.}
#'     \item{\code{max_fu_time}}{Maximum observed follow-up time in the group.}
#'     \item{\code{median}}{Median survival time with 95\% log-log CI, formatted
#'       as \code{"est (lcl, ucl)"}.}
#'     \item{\code{surv_t<X>}}{Survival probability (95\% CI) at each requested
#'       timepoint. An asterisk (\code{*}) is appended when the timepoint
#'       exceeds the maximum follow-up in that group. \code{"Not Estimable"}
#'       is returned when the point estimate is unavailable; individual CI
#'       bounds show \code{"Not Estimable"} when only the bound is missing.}
#'   }
#'   When \code{tableOnly = FALSE}, columns are relabelled and the result is
#'   passed to \code{outTable()} for display.
#'
#' @details
#' Observations with missing values in \code{time}, \code{status}, or
#' \code{group} are removed with a warning indicating how many were dropped.
#'
#' When \code{tableOnly = FALSE}, column labels are generated automatically
#' from \code{time_unit} and a survival endpoint label inferred from the
#' \code{status} variable name:
#' \itemize{
#'   \item \code{"os_status"} \eqn{\rightarrow} Overall Survival
#'   \item \code{"rfs_status"} \eqn{\rightarrow} RFS
#'   \item \code{"pfs_status"} \eqn{\rightarrow} PFS
#'   \item Otherwise \eqn{\rightarrow} Survival
#' }
#' Custom timepoint labels can be supplied via \code{time_lbls} to override
#' the automatic names.
#'
#' @note Requires the \pkg{survival} and \pkg{dplyr} packages.
#'   When \code{tableOnly = FALSE}, the function \code{outTable()} must be
#'   available in the calling environment.
#'
#' @examples
#' library(survival)
#'
#' # Using the lung dataset (recode status: 2 = event, 1 = censored -> 1/0)
#' lung$event   <- ifelse(lung$status == 2, 1, 0)
#' lung$sex_lbl <- factor(lung$sex, labels = c("Male", "Female"))
#'
#' # Raw table output
#' summarise_survival(
#'   data       = lung,
#'   time       = "time",
#'   status     = "event",
#'   group      = "sex_lbl",
#'   times      = c(180, 365, 540),
#'   time_unit  = "days",
#'   overall    = TRUE,
#'   tableOnly  = TRUE
#' )
#'
#' @export
summarise_survival <- function(
  data,
  time,
  status,
  group,
  times,
  time_unit,
  digits_rate = 2,
  digits_time = 1,
  time_lbls = NULL,
  overall = FALSE,
  tableOnly = FALSE
) {
  if (missing(time_unit)) {
    stop("Unit of time must be specified")
  }

  # Load required packages
  require(survival)
  require(dplyr)

  # Remove observations with missing time, status or group values
  n_entered <- nrow(data)
  data <- data[c(time, status, group)]
  data <- na.omit(data)
  n_analysed <- nrow(data)
  if (n_entered != n_analysed) {
    warning(
      n_entered - n_analysed,
      " patients had missing data and were removed"
    )
  }

  # Build survival formula
  surv_formula <- as.formula(
    paste0("Surv(", time, ", ", status, ") ~ ", group)
  )

  # Fit Kaplan-Meier with log-log CIs
  fit <- survfit(surv_formula, data = data, conf.type = "log-log")

  # Extract group levels
  groups <- levels(factor(data[[group]]))

  # Helper: format a single number
  fmt_num <- function(x, d) formatC(round(x, d), format = "f", digits = d)

  # Helper: format estimate (CI) or return Not Estimable
  fmt <- function(est, lcl, ucl, d = digits_time) {
    # Point estimate missing: just Not Estimable, no CIs
    if (is.na(est)) {
      return("Not Estimable")
    }

    # Format each component, replacing NA bounds individually
    lcl_str <- if (is.na(lcl)) "Not Estimable" else fmt_num(lcl, d)
    ucl_str <- if (is.na(ucl)) "Not Estimable" else fmt_num(ucl, d)

    paste0(fmt_num(est, d), " (", lcl_str, ", ", ucl_str, ")")
  }

  # Extract median table
  med_table <- summary(fit)$table
  if (length(groups) == 1) {
    med_table <- matrix(
      med_table,
      nrow = 1,
      dimnames = list(NULL, names(med_table))
    )
  }

  # Extract survival at requested timepoints
  surv_at_times <- summary(fit, times = times, extend = TRUE)

  # Identify max observed time per group
  max_time_per_group <- tapply(data[[time]], data[[group]], max, na.rm = TRUE)

  # Process each group
  result_list <- lapply(seq_along(groups), function(i) {
    g <- groups[i]

    # Sample size
    n <- med_table[i, "records"]

    # Median survival (CI)
    median_str <- fmt(
      med_table[i, "median"],
      med_table[i, "0.95LCL"],
      med_table[i, "0.95UCL"]
    )

    # Filter summary output to this stratum
    if (length(groups) > 1) {
      strata_label <- paste0(group, "=", g)
      idx <- which(surv_at_times$strata == strata_label)
    } else {
      idx <- seq_along(surv_at_times$time)
    }

    surv_vals <- surv_at_times$surv[idx]
    lower_vals <- surv_at_times$lower[idx]
    upper_vals <- surv_at_times$upper[idx]
    time_vals <- surv_at_times$time[idx]

    # Build survival probability strings
    surv_strings <- vapply(
      times,
      function(t) {
        j <- which(time_vals == t)
        if (length(j) == 0) {
          return("Not Estimable")
        }

        if (surv_vals[j] > 0 && surv_vals[j] < 1) {
          est_str <- fmt(
            surv_vals[j],
            lower_vals[j],
            upper_vals[j],
            d = digits_rate
          )
        } else {
          est_str <- as.character(surv_vals[j])
        }

        # Append * if timepoint exceeds max observed time
        if (t > max_time_per_group[g]) {
          est_str <- paste0(est_str, "*")
        }

        return(est_str)
      },
      character(1)
    )

    # Assemble row
    row <- data.frame(
      group = g,
      n = as.integer(n),
      max_fu_time = fmt_num(max_time_per_group[g], digits_time),
      median = median_str,
      stringsAsFactors = FALSE
    )

    for (k in seq_along(times)) {
      col_name <- paste0("surv_t", times[k])
      row[[col_name]] <- surv_strings[k]
    }

    row
  })

  # Combine into single data frame
  out <- do.call(rbind, result_list)
  rownames(out) <- NULL

  # Optionally add overall row
  if (overall) {
    surv_formula_overall <- as.formula(
      paste0("Surv(", time, ", ", status, ") ~ 1")
    )
    fit_overall <- survfit(
      surv_formula_overall,
      data = data,
      conf.type = "log-log"
    )
    med_overall <- summary(fit_overall)$table
    surv_overall <- summary(fit_overall, times = times, extend = TRUE)
    max_time_overall <- max(data[[time]], na.rm = TRUE)

    overall_median <- fmt(
      med_overall["median"],
      med_overall["0.95LCL"],
      med_overall["0.95UCL"]
    )

    overall_row <- data.frame(
      group = "Overall",
      n = as.integer(med_overall["records"]),
      max_fu_time = fmt_num(max_time_overall, digits_time),
      median = overall_median,
      stringsAsFactors = FALSE
    )

    for (k in seq_along(times)) {
      col_name <- paste0("surv_t", times[k])
      if (times[k] > max_time_overall) {
        overall_row[[col_name]] <- "Not Estimable"
      } else {
        overall_row[[col_name]] <- fmt(
          surv_overall$surv[k],
          surv_overall$lower[k],
          surv_overall$upper[k],
          d = digits_rate
        )
      }
    }

    out <- rbind(out, overall_row)
  }

  # Rename group column to the grouping variable
  names(out)[1] <- group

  if (interactive()) {
    message("Confidence intervals estimated with the log-log method")
  }
  if (tableOnly) {
    return(out)
  }

  # Apply pretty column labels
  surv_label <- case_when(
    status == "os_status" ~ "Overall Survival",
    status == "rfs_status" ~ "RFS",
    status == "pfs_status" ~ "PFS",
    TRUE ~ "Survival"
  )

  tm_cols <- grep("surv_t", names(out))
  if (!is.null(time_lbls)) {
    names(out)[tm_cols] <- time_lbls
  } else {
    names(out)[tm_cols] <- paste(
      times,
      gsub("s$", "", time_unit),
      surv_label,
      "Rate (95% CI)"
    )
  }
  names(out)[grep("median", names(out))] <- paste(
    "Median",
    surv_label,
    "in",
    time_unit,
    "(95% CI)"
  )
  names(out)[grep("max_fu_time", names(out))] <- paste(
    "Maximum Follow-up in",
    time_unit
  )

  reportRmd::outTable(out)
}
