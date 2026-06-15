# Data Cleaning Functions
# Excel import, dictionary-based processing, variable screening and summary
#see importExcel2

# Code Export Tools ------------------------------------------------------

#' Write an R function to a script file
#' @param func The function to export to script
#' @param file_path the filename (and path if not current dir)
#' @param append defaults to TRUE to add to an existing file
#' @export
write_function_to_file <- function(func, file_path, append = T) {
  if (!is.function(func)) {
    stop("The input must be a function.")
  }
  func_text <- capture.output(dump(
    deparse(substitute(func)),
    file = "",
    control = NULL
  ))
  func_text_out <- c(
    paste(func_text[1:3], collapse = ""),
    func_text[4:length(func_text)]
  )
  openmode <- ifelse(append, "at", "wt")
  con <- file(file_path, open = openmode)
  writeLines(func_text_out, con = con)
  close(con)
}


# Date Conversion ----------------------------------------------------------

#' Date conversion from Excel
#'
#' Tries janitor date conversion first, then lubridate formats
#'
#' @param date_var vector containing date data to be converted
#' @export
readExcelDate <- function(date_var) {
  dt_j = lubridate::ymd(janitor::excel_numeric_to_date(as.numeric(date_var)))
  dt_miss = date_var[is.na(dt_j)]
  dt_1 = lubridate::ymd(dt_miss)
  dt_2 = lubridate::ymd(format(lubridate::dmy(dt_miss), "%Y%m%d"))
  if (sum(is.na(dt_1)) < sum(is.na(dt_2))) {
    dt_l = dt_1
  } else {
    dt_l = dt_2
  }
  dt_n = dt_j
  dt_n[is.na(dt_j)] <- dt_l
  return(dt_n)
}


# Variable Info & Dictionary -----------------------------------------------

#' Clean variable names in a data frame
#' @param data data frame
#' @export
clean_names <- function(data) {
  to_clean <- names(data)
  cleaned <- sapply(
    to_clean,
    function(x) {
      x <- trimws(x)
      x <- gsub("[^a-zA-Z0-9]", "_", x)
      x <- gsub("__", "_", x)
      x <- gsub("_$", "", x)
      x <- ifelse(grepl("^[0-9]", x), paste0('V', x), x)
    },
    USE.NAMES = FALSE
  )
  names(data) <- cleaned
  return(data)
}

#' Get variable metadata
#' @param data data frame
#' @export
getVarInfo <- function(data) {
  if (!inherits(data, 'data.frame')) {
    stop('data must have class data.frame')
  }
  clean_nms <- sapply(
    names(data),
    function(x) {
      x <- gsub("[^[:alnum:]]", "_", x)
      x <- sub('_+', '_', x)
    },
    USE.NAMES = FALSE
  )
  var_info <- data.frame(
    varOrder = 1:ncol(data),
    varName = names(data),
    cleanVarName = clean_nms,
    varDesc = names(data)
  )
  var_info$type <- unlist(lapply(data, function(x) {
    paste0(class(x), collapse = '_')
  }))
  var_info$n_missing <- colSums(is.na(data))
  var_info$all_missing <- var_info$n_missing == nrow(data)
  var_info$n_unique <- unlist(lapply(data, function(x) {
    length(unique(x[!is.na(x)]))
  }))
  return(var_info)
}


#' Remove all-missing and high-cardinality text variables
#' @param data data frame
#' @param factorLimit threshold for text variable removal
#' @export
remove_extra_columns <- function(data, factorLimit = 10) {
  var_info <- getVarInfo(data)
  to_remove <- var_info$varName[var_info$all_missing]
  to_remove <- c(
    to_remove,
    var_info$varName[
      var_info$type == 'character' & var_info$n_unique > factorLimit
    ]
  )
  clean_data <- data[, setdiff(names(data), to_remove)]
  names(clean_data) <- sapply(
    names(clean_data),
    function(x) {
      x <- gsub("[^[:alnum:]]", "_", x)
      sub('_+', '_', x)
    },
    USE.NAMES = FALSE
  )
  return(clean_data)
}


# Variable Screening -------------------------------------------------------

#' Summarise variables in a data frame
#'
#' @param data data frame
#' @param varsToExclude character vector of variables to exclude
#' @param include_prop logical, include proportion of non-zero responses
#' @export
var_sum <- function(data, varsToExclude = NULL, include_prop = FALSE) {
  if (!inherits(data, 'data.frame')) {
    stop('data must be a data frame')
  }
  data <- data[, setdiff(names(data), varsToExclude)]
  out <- lapply(names(data), function(v) {
    x <- data[[v]]
    s1 <- data.frame(
      Variable = v,
      n_miss = sum(is.na(x)),
      n_unique = length(unique(x))
    )
    if (is.numeric(x)) {
      s1 <- cbind(
        s1,
        data.frame(
          mean = mean(x, na.rm = T),
          median = median(x, na.rm = T),
          sd = sd(x, na.rm = T),
          min = min(x, na.rm = T),
          max = max(x, na.rm = T)
        )
      )
      if (include_prop) {
        s1 <- cbind(s1, proportion = sum(x > 0, na.rm = T) / length(x))
      }
    }
    return(s1)
  })
  dplyr::bind_rows(out)
}

#' Summarise numeric variables only
#' @param data data frame
#' @param varsToExclude variables to exclude
#' @export
numericSummary <- function(data, varsToExclude = NULL) {
  numVars <- unlist(lapply(data, function(x) inherits(x, 'numeric')))
  numVars <- names(numVars)[numVars]
  sumVars <- setdiff(numVars, varsToExclude)
  out <- lapply(sumVars, function(z) {
    x = data[[z]]
    data.frame(
      Variable = z,
      mean = mean(x, na.rm = T),
      median = median(x, na.rm = T),
      sd = sd(x, na.rm = T),
      min = min(x, na.rm = T),
      max = max(x, na.rm = T),
      nmiss = sum(is.na(x))
    )
  })
  dplyr::bind_rows(out)
}

#' Screen for group differences
#'
#' Tests for differences in means/medians across groups using t-tests/ANOVA
#' and Wilcoxon/Kruskal-Wallis. Data must be in long format.
#'
#' @param data the data frame
#' @param id unique identifier
#' @param var column identifying different biomarkers
#' @param value column containing biomarker values
#' @param group grouping variable
#' @param showN show sample sizes
#' @param digits digits for output
#' @param p.adjust p-value adjustment method
#' @export
var_screen <- function(
  data,
  id,
  var,
  value,
  group,
  showN = TRUE,
  digits = 2,
  p.adjust = 'fdr'
) {
  if (missing(data)) {
    stop('This function requires data to be specified.')
  }
  if (missing(id)) {
    stop('This function requires an id to be specified for each subject')
  }
  if (missing(var)) {
    stop(
      'This function requires a var column to be specified to identify biomarkers'
    )
  }
  if (missing(value)) {
    stop('This function requires a value column to be specified')
  }
  if (missing(group)) {
    stop('This function requires a grouping column to be specfied')
  }

  argL <- as.list(match.call())[-1]
  argStr <- unlist(lapply(
    argL[names(argL) %in% c('id', 'var', 'value', 'group')],
    function(x) rlang::as_string(x)
  ))
  for (i in seq_along(argStr)) {
    eval(parse(text = paste0(names(argStr)[i], '="', argStr[i], '"')))
  }

  tt <- lapply(unique(data[[var]]), function(v) {
    df <- data[data[[var]] == v, ]
    k <- length(unique(df[[group]][!is.na(df[[value]])]))
    if (k == 2) {
      ht <- stats::t.test(df[[value]] ~ df[[group]])
      p_value <- ht$p.value
    } else if (k > 2) {
      ht <- stats::aov(df[[value]] ~ df[[group]])
      p_value <- summary(ht)[[1]][["Pr(>F)"]][1]
    } else {
      p_value <- NA
    }
    return(data.frame(variable = v, p_value))
  })
  out <- do.call(rbind, tt)
  out$p_mean = stats::p.adjust(out$p_value, method = p.adjust)

  wt <- lapply(unique(data[[var]]), function(v) {
    df <- data[data[[var]] == v, ]
    k <- length(unique(df[[group]][!is.na(df[[value]])]))
    if (k == 2) {
      ht <- stats::wilcox.test(df[[value]] ~ df[[group]])
      p_value <- ht$p.value
    } else if (k > 2) {
      ht <- stats::kruskal.test(df[[value]] ~ df[[group]])
      p_value <- ht$p.value
    } else {
      p_value <- NA
    }
    return(data.frame(variable = v, p_value))
  })
  out2 <- do.call(rbind, wt)
  out$p_rank = stats::p.adjust(out2$p_value, method = p.adjust)
  out$p_value <- NULL
  names(out) <- gsub('variable', var, names(out))
  return(out)
}


# Data Frame Utilities -----------------------------------------------------

#' Concatenate the text of two data frames element-wise
#' @param df1 data frame
#' @param df2 data frame
#' @param separator text separator
#' @export
concat_data_frames <- function(df1, df2, separator = "") {
  if (!(all(dim(df1) == dim(df2)))) {
    stop("Data frames must have the same dimensions")
  }
  result <- data.frame(matrix(ncol = ncol(df1), nrow = nrow(df1)))
  colnames(result) <- colnames(df1)
  for (col in 1:ncol(df1)) {
    result[[col]] <- paste(df1[[col]], df2[[col]], sep = separator)
  }
  return(result)
}

#' Cross tabs to check data recodings
#' @param data data frame
#' @param var1 variable for rows
#' @param var2 variable for columns
#' @param colTotals show column totals
#' @export
check_coding <- function(data, var1, var2, colTotals = TRUE) {
  out <- data |> janitor::tabyl({{ var1 }}, {{ var2 }})
  colspan <- ncol(out) - 1
  names(colspan) <- deparse(substitute(var2))
  names(out)[-1] <- paste0(deparse(substitute(var2)), ": ", names(out)[-1])
  if (colTotals) {
    col_sums <- colSums(out[, -1], na.rm = T)
    names(out)[-1] <- paste0(names(out)[-1], " (n=", col_sums, ")")
  }
  return(out)
}

#' Randomly select from an array equally by group allocation
#' @param x vector to sample from
#' @param group stratification variable
#' @param p proportion of cases to return
#' @export
sampleByGroup <- function(x, group, p = .5) {
  if (length(group) != length(x)) {
    stop("x and group must have the same length")
  }
  subsample = NULL
  for (g in unique(group)) {
    subsample = c(
      subsample,
      sample(x[group == g], p * length(x[group == g]), replace = F)
    )
  }
  return(subsample)
}

#' Return a random stratified subsample of a dataframe
#' @param data data frame
#' @param group stratification variable
#' @param p proportion of cases to return
#' @export
getSubSample <- function(data, group, p = .5) {
  if (!group %in% names(data)) {
    stop(paste(group, 'is not found in the data.'))
  }
  subsample = NULL
  for (g in unique(data[[group]])) {
    subsample = rbind(
      subsample,
      data[
        sample(
          (1:nrow(data))[data[[group]] == g],
          p * sum(data[[group]] == g),
          replace = F
        ),
      ]
    )
  }
  return(subsample)
}

#' Sample size for margin of error around a proportion
#'
#' @param p the expected proportion
#' @param moe the accepted margin of error (half the CI width)
#' @param CIwidth confidence level (default 0.95)
#' @param method 'exact' for Clopper-Pearson or 'normal' for normal approximation
#' @export
n_moe <- function(p, moe, CIwidth = .95, method = c('exact', 'normal')) {
  if (is.na(p) | is.na(moe)) {
    return(NA)
  }
  n_start <- ceiling(qnorm(1 - (1 - CIwidth) / 2)^2 * p * (1 - p) / moe^2)
  method <- match.arg(method)
  if (method == 'normal') {
    return(n_start)
  }

  n <- n_start
  alpha <- 1 - CIwidth
  repeat {
    repeat {
      x <- floor(p * n)
      if (x > 0) break else n = n + 1
    }
    ll <- 1 / (1 + (n - x + 1) / (x * qf(alpha / 2, 2 * x, 2 * (n - x + 1))))
    ul <- 1 /
      (1 + (n - x) / ((x + 1) * qf(1 - alpha / 2, 2 * (x + 1), 2 * (n - x))))
    if ((ul - ll) <= 2 * moe) {
      return(n)
    }
    n <- n + 1
  }
}
