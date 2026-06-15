# durations.R
# Compute durations between messy / partially-known dates (d-Mon-yy with
# unknown day or month). Returns precision flags and interval-arithmetic
# classifications for ambiguous gaps.
#
# Suggests packages: dplyr

# Lookup Tables ----
.month_lookup <- c(
  jan = 1L,
  feb = 2L,
  mar = 3L,
  apr = 4L,
  may = 5L,
  jun = 6L,
  jul = 7L,
  aug = 8L,
  sep = 9L,
  oct = 10L,
  nov = 11L,
  dec = 12L
)

.days_noleap <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
.days_leap <- c(31L, 29L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)

# Helper: Days in Month ----
.get_month_days <- function(m, y) {
  is_leap <- (y %% 4 == 0 & y %% 100 != 0) | (y %% 400 == 0)
  if (is_leap) .days_leap[m] else .days_noleap[m]
}

# Parse a Single Messy Date String ----
.parse_messy_date <- function(x) {
  missing <- list(year = NA, month = NA, day = NA, known = "missing")
  x <- trimws(x)
  if (is.na(x) || x == "" || toupper(x) %in% c("NA", "UNK")) {
    return(missing)
  }

  parts <- strsplit(x, "-")[[1]]
  if (length(parts) != 3) {
    return(missing)
  }

  day_raw <- parts[1]
  month_raw <- tolower(parts[2])
  year_raw <- parts[3]

  # Year ----
  year_val <- suppressWarnings(as.integer(year_raw))
  if (is.na(year_val)) {
    return(missing)
  }
  if (year_val < 100) {
    year_val <- year_val + 2000L
  }

  # Month ----
  month_unknown <- month_raw %in% c("unk", "000", "xxx")
  month_val <- NA_integer_
  if (!month_unknown) {
    month_val <- unname(.month_lookup[month_raw])
    if (is.na(month_val)) return(missing)
  }

  # Day ----
  day_unknown <- tolower(day_raw) %in% c("00", "un", "xx")
  day_val <- NA_integer_
  if (!day_unknown) {
    day_val <- suppressWarnings(as.integer(day_raw))
    if (is.na(day_val) || day_val < 1L || day_val > 31L) {
      day_unknown <- TRUE
      day_val <- NA_integer_
    }
  }

  # Precision ----
  known <- if (month_unknown) {
    "year_only"
  } else if (day_unknown) {
    "month_year"
  } else {
    "full"
  }

  list(year = year_val, month = month_val, day = day_val, known = known)
}

# Build Earliest & Latest Possible Dates ----
.date_bounds <- function(p) {
  if (p$known == "missing") {
    return(list(earliest = as.Date(NA), latest = as.Date(NA)))
  }
  if (p$known == "full") {
    d <- as.Date(paste(p$year, p$month, p$day, sep = "-"))
    return(list(earliest = d, latest = d))
  }
  if (p$known == "month_year") {
    earliest <- as.Date(paste(p$year, p$month, 1, sep = "-"))
    last_day <- .get_month_days(p$month, p$year)
    latest <- as.Date(paste(p$year, p$month, last_day, sep = "-"))
    return(list(earliest = earliest, latest = latest))
  }
  if (p$known == "year_only") {
    earliest <- as.Date(paste(p$year, 1, 1, sep = "-"))
    latest <- as.Date(paste(p$year, 12, 31, sep = "-"))
    return(list(earliest = earliest, latest = latest))
  }
}

# Main Function ----
#' Classify the gap between an ADT date and a start date
#'
#' @param df A data frame containing the two date columns.
#' @param adt_col Name of the ADT (initiation) date column.
#' @param start_col Name of the start date column.
#' @return `df` with `adt_precision`, `start_precision`, `duration_years`
#'   (exact only when both dates are full) and `over_under_1yr` appended.
#' @export
compute_adt_duration <- function(
  df,
  adt_col = "date_of_initiation_of_adt",
  start_col = "mcrpc_start_date"
) {
  n <- nrow(df)
  adt_precision <- character(n)
  start_precision <- character(n)
  duration_years <- rep(NA_real_, n)
  over_under_1yr <- character(n)

  for (i in seq_len(n)) {
    adt_p <- .parse_messy_date(df[[adt_col]][i])
    start_p <- .parse_messy_date(df[[start_col]][i])

    adt_precision[i] <- adt_p$known
    start_precision[i] <- start_p$known

    # Missing Dates ----
    if (adt_p$known == "missing" && start_p$known == "missing") {
      over_under_1yr[i] <- "both dates missing"
      next
    }
    if (adt_p$known == "missing") {
      over_under_1yr[i] <- "ADT date missing"
      next
    }
    if (start_p$known == "missing") {
      over_under_1yr[i] <- "start date missing"
      next
    }

    # Compute Bounds ----
    adt_b <- .date_bounds(adt_p)
    start_b <- .date_bounds(start_p)

    # Both Full Dates — Exact Calculation ----
    if (adt_p$known == "full" && start_p$known == "full") {
      dur <- as.numeric(difftime(
        start_b$earliest,
        adt_b$earliest,
        units = "days"
      )) /
        365.25
      duration_years[i] <- round(dur, 2)
      over_under_1yr[i] <- if (dur < 0) {
        "negative (start before ADT)"
      } else if (dur < 1) {
        "< 1 year"
      } else {
        "> 1 year"
      }
      next
    }

    # Partial Dates — Interval Arithmetic ----
    # Shortest possible gap: start_earliest - adt_latest ----
    # Longest possible gap:  start_latest   - adt_earliest ----
    min_days <- as.numeric(difftime(
      start_b$earliest,
      adt_b$latest,
      units = "days"
    ))
    max_days <- as.numeric(difftime(
      start_b$latest,
      adt_b$earliest,
      units = "days"
    ))
    min_yrs <- min_days / 365.25
    max_yrs <- max_days / 365.25

    over_under_1yr[i] <- if (max_days < 0) {
      "negative (start before ADT)"
    } else if (min_yrs >= 1) {
      "> 1 year"
    } else if (min_days < 0) {
      "not computable (ambiguous order)"
    } else if (max_yrs < 1) {
      "< 1 year"
    } else {
      "not computable"
    }
  }

  # Append Results ----
  df |>
    dplyr::mutate(
      adt_precision = adt_precision,
      start_precision = start_precision,
      duration_years = duration_years,
      over_under_1yr = over_under_1yr
    )
}

#' Determine whether treatment started before a CR date
#'
#' @param df A data frame containing the two date columns.
#' @param cr_date Name of the complete-response date column.
#' @param start_col Name of the treatment start date column.
#' @return `df` with `cr_precision`, `start_precision`, `duration_years` and
#'   `trt_before_cr` appended.
#' @export
compute_trt_start_to_cr <- function(
  df,
  cr_date = "date_of_mcrpc",
  start_col = "mcrpc_start_date"
) {
  n <- nrow(df)
  cr_precision <- character(n)
  start_precision <- character(n)
  duration_years <- rep(NA_real_, n)
  trt_before_cr <- character(n)

  for (i in seq_len(n)) {
    cr_p <- .parse_messy_date(df[[cr_date]][i])
    start_p <- .parse_messy_date(df[[start_col]][i])

    cr_precision[i] <- cr_p$known
    start_precision[i] <- start_p$known

    # Missing Dates ----
    if (cr_p$known == "missing" && start_p$known == "missing") {
      trt_before_cr[i] <- "both dates missing"
      next
    }
    if (cr_p$known == "missing") {
      trt_before_cr[i] <- "cr date missing"
      next
    }
    if (start_p$known == "missing") {
      trt_before_cr[i] <- "treatment start date missing"
      next
    }

    # Compute Bounds ----
    cr_b <- .date_bounds(cr_p)
    start_b <- .date_bounds(start_p)

    # Both Full Dates — Exact Calculation ----
    if (cr_p$known == "full" && start_p$known == "full") {
      dur <- as.numeric(difftime(
        cr_b$earliest,
        start_b$earliest,
        units = "days"
      )) /
        365.25
      duration_years[i] <- round(dur, 2)
      trt_before_cr[i] <- if (dur < 0) FALSE else TRUE

      next
    }

    # Partial Dates — Interval Arithmetic ----
    # Shortest possible gap: start_earliest - cr_latest ----
    # Longest possible gap:  start_latest   - cr_earliest ----
    min_days <- as.numeric(difftime(
      start_b$earliest,
      cr_b$latest,
      units = "days"
    ))
    max_days <- as.numeric(difftime(
      start_b$latest,
      cr_b$earliest,
      units = "days"
    ))
    min_yrs <- min_days / 365.25
    max_yrs <- max_days / 365.25

    trt_before_cr[i] <- if (max_days < 0) {
      FALSE
    } else if (min_yrs >= 1) {
      TRUE
    } else if (min_days < 0) {
      "not computable (ambiguous order)"
    } else if (max_yrs < 1) {
      TRUE
    } else {
      "not computable"
    }
  }

  # Append Results ----
  df |>
    dplyr::mutate(
      cr_precision = cr_precision,
      start_precision = start_precision,
      duration_years = duration_years,
      trt_before_cr = trt_before_cr
    )
}

# Days-to-Months Conversion ----
# Use average month length (365.25 / 12) for consistent conversion ----
.days_to_months <- function(d) d / (365.25 / 12)

# Main Function ----
#' Compute months between mCSPC diagnosis and mCRPC dates
#'
#' @param df A data frame containing the two date columns.
#' @param mcspc_col Name of the mCSPC diagnosis date column.
#' @param mcrpc_col Name of the mCRPC date column.
#' @return `df` with `mcspc_diagnosis_to_mcrpc_months` and
#'   `mcspc_diagnosis_to_mcrpc_precision` appended.
#' @export
compute_mcspc_to_mcrpc_months <- function(
  df,
  mcspc_col = "date_of_mcspc_diagnosis",
  mcrpc_col = "date_of_mcrpc"
) {
  n <- nrow(df)
  mcspc_precision <- character(n)
  mcrpc_precision <- character(n)
  duration_months <- rep(NA_real_, n)
  gap_class <- character(n)

  for (i in seq_len(n)) {
    mcspc_p <- .parse_messy_date(df[[mcspc_col]][i])
    mcrpc_p <- .parse_messy_date(df[[mcrpc_col]][i])

    mcspc_precision[i] <- mcspc_p$known
    mcrpc_precision[i] <- mcrpc_p$known

    # Missing Dates ----
    if (mcspc_p$known == "missing" && mcrpc_p$known == "missing") {
      gap_class[i] <- "both dates missing"
      next
    }
    if (mcspc_p$known == "missing") {
      gap_class[i] <- "mcspc date missing"
      next
    }
    if (mcrpc_p$known == "missing") {
      gap_class[i] <- "mcrpc date missing"
      next
    }

    # Compute Bounds ----
    mcspc_b <- .date_bounds(mcspc_p)
    mcrpc_b <- .date_bounds(mcrpc_p)

    # Both Full Dates — Exact Calculation ----
    if (mcspc_p$known == "full" && mcrpc_p$known == "full") {
      days <- as.numeric(difftime(
        mcrpc_b$earliest,
        mcspc_b$earliest,
        units = "days"
      ))
      mo <- .days_to_months(days)
      duration_months[i] <- round(mo, 2)
      gap_class[i] <- if (mo < 0) "negative (mcrpc before mcspc)" else "exact"
      next
    }

    # Partial Dates — Interval Arithmetic ----
    # Shortest possible gap: mcrpc_earliest - mcspc_latest ----
    # Longest possible gap:  mcrpc_latest   - mcspc_earliest ----
    min_days <- as.numeric(difftime(
      mcrpc_b$earliest,
      mcspc_b$latest,
      units = "days"
    ))
    max_days <- as.numeric(difftime(
      mcrpc_b$latest,
      mcspc_b$earliest,
      units = "days"
    ))

    if (max_days < 0) {
      duration_months[i] <- round(.days_to_months((min_days + max_days) / 2), 2)
      gap_class[i] <- "negative (mcrpc before mcspc)"
    } else if (min_days < 0) {
      gap_class[i] <- "not computable (ambiguous order)"
    } else {
      duration_months[i] <- round(.days_to_months((min_days + max_days) / 2), 2)
      gap_class[i] <- "approximate"
    }
  }

  # Append Results ----
  df |>
    dplyr::mutate(
      mcspc_diagnosis_to_mcrpc_months = duration_months,
      mcspc_diagnosis_to_mcrpc_precision = gap_class
    )
}
