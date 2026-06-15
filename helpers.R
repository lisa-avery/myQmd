# Helper Functions

#' Round Numbers with Trailing Zeros
#'
#' @description
#' Rounds numbers to a specified number of decimal places while preserving trailing zeros.
#' This function is useful for creating consistent numeric formatting in output tables.
#'
#' @param x Numeric vector to be rounded
#' @param digits Integer indicating the number of decimal places (default: 2)
#'
#' @return Character vector of the rounded numbers with trailing zeros preserved
#'
#' @examples
#' rnd(c(1.2345, 5.67, 9))  # Returns c("1.23", "5.67", "9.00")
#' rnd(c(1.2345, 5.67, 9), digits = 3)  # Returns c("1.235", "5.670", "9.000")
#'
#' @export
rnd <- function(x, digits = 2) {
  out <- sapply(x, function(x) {
    format(round(x, digits), nsmall = digits)
  })
  out <- unlist(out, use.names = FALSE)
  return(out)
}


#' Numbers nicely formatted to string
#'
#' @param x numbers to format
#' @param digits number of digits to retain after decimal place
#'   checked for and retained.
#' @keywords helper
#'
#' @return Character vector with formatted numbers
#'
#' @examples
#' niceNum(c(1.2345, 5.67, 9))  # Returns c("1.23", "5.67", "9.00")
#'
#' @export
niceNum <- function(x, digits = 2) {
  rndx = sapply(x, function(x) {
    if (is.na(x)) {
      return(x)
    }
    if (is.null(x)) {
      return(x)
    }
    format(round(as.numeric(x), digits), nsmall = digits)
  })
  return(gsub(" ", "", rndx))
}

nicename <- function(strings, check_numbers = TRUE) {
  out <- sapply(strings, function(x) {
    original_x <- x
    x <- chartr(".", " ", x)
    x <- chartr("_", " ", x)
    if (check_numbers) {
      p.positions <- gregexpr(pattern = '\\d\\.[0-9]+', original_x)[[1]] + 1
      for (pos in p.positions) {
        substr(x, pos, pos) <- '.'
      }
    }
    x <- gsub(" +", " ", x)
    return(x)
  })
  return(out)
}


rm_ <- function(x) {
  gsub("_", " ", x)
}

p_sig_stars <- function(adj_p) {
  ifelse(
    adj_p < .0001,
    "****",
    ifelse(
      adj_p < .001,
      "***",
      ifelse(adj_p < .01, "**", ifelse(adj_p < .05, "*", ""))
    )
  )
}


formatp <- function(pvalues) {
  p_out <- sapply(pvalues, function(x) {
    xsig <- suppressWarnings(as.numeric(x))
    fmtX <- ifelse(
      xsig < 0.001,
      "<0.001",
      ifelse(
        xsig < 0.1,
        format(round(xsig, 3), nsmall = 3),
        format(round(xsig, 2), nsmall = 2)
      )
    )
    x <- ifelse(x == 'excl', 'excl', fmtX)
  })
  p_out = unname(p_out)
  return(p_out)
}


pstprn <- function(x) {
  paste(x[1], " (", paste(x[-1], collapse = ", "), ")", sep = "")
}
psthr <- function(x, y = 2) {
  x <- sapply(x, function(x) {
    ifelse(
      abs(x) < 0.01 | abs(x) > 1000,
      format(x, scientific = TRUE, digits = y),
      format(round(x, y), nsmall = y)
    )
  })
  pstprn(x)
}

#' Convert Hypothesis Test Objects to Data Frames
#'
#' @description
#' Converts an object of class "htest" (returned by R's statistical test functions)
#' into a data frame for easier manipulation and integration with other analyses.
#'
#' @param htest_object An object of class "htest", typically returned by statistical test
#' functions like t.test(), wilcox.test(), cor.test(), etc.
#'
#' @return A data frame with one row containing all the components of the hypothesis test.
#' The columns include:
#' \itemize{
#'   \item method: The name of the statistical test
#'   \item statistic: The test statistic
#'   \item p.value: The p-value of the test
#'   \item parameter: Degrees of freedom or other parameters (if present)
#'   \item conf.int: Confidence intervals (if present, may be split into conf.int1, conf.int2)
#'   \item estimate: Point estimates (if present, may be split into multiple columns)
#'   \item alternative: The alternative hypothesis specification (if present)
#'   \item data.name: The name of the data used in the test (if present)
#'   \item null.value: The null hypothesis value (if present)
#' }
#'
#' @details
#' This function extracts all components from an "htest" object and arranges them into
#' a single-row data frame. For components that contain multiple values (like confidence
#' intervals or multiple estimates), the values are separated into different columns with
#' sequential numbering.
#'
#' The function is particularly useful when collecting results from multiple tests,
#' as the resulting data frames can be easily combined using dplyr::bind_rows() (but not rbind because the number of columns may differ)
#'
#' @examples
#' # Convert a t-test result to a data frame
#' t_result <- t.test(1:10, y = c(7:20))
#' t_df <- htest_dataframe(t_result)
#' print(t_df)
#'
#' # Convert a correlation test result
#' cor_result <- cor.test(1:10, 2:11, method = "spearman")
#' cor_df <- htest_dataframe(cor_result)
#' print(cor_df)
#'
#' # Combine multiple test results
#' tests_list <- list(
#'   t_test = t.test(1:10, y = c(7:20)),
#'   wilcox = wilcox.test(1:10, c(7:20))
#' )
#' results_df <- do.call(dplyr::bind_rows, lapply(tests_list, htest_dataframe))
#'
#' @export
htest_dataframe <- function(htest_object) {
  if (!inherits(htest_object, "htest")) {
    stop(
      "htest_object must be an object returned from a statistical test of class htest."
    )
  }
  if (is.null(htest_object$method)) {
    stop(
      "htest_object must be an object returned from a statistical test of class htest with a methods component."
    )
  }
  htest <- data.frame(method = htest_object$method)
  for (j in setdiff(names(htest_object), "method")) {
    if (!is.null(htest_object[[j]])) {
      rtn <- data.frame(matrix(unlist(htest_object[[j]]), nrow = 1))
      if (ncol(rtn) > 1) {
        names(rtn) <- paste0(j, 1:(ncol(rtn)))
        htest <- merge(htest, rtn, all = T)
      } else if (ncol(rtn) == 1) {
        names(rtn) <- j
        htest <- merge(htest, rtn, all = T)
      }
    }
  }
  return(htest)
}

#' Sample Skewness with Optional Robust Trimming
#'
#' Computes the adjusted Fisher-Pearson skewness coefficient, optionally trimming
#' to a central proportion of the data to reduce the influence of extreme values.
#'
#' @param x Numeric vector.
#' @param robust Optional numeric value between 0 and 1 specifying the central
#'   proportion of data to retain before computing skewness. For example, 0.95
#'   trims the outer 2.5% from each tail. If NULL (default), no trimming is applied.
#'
#' @return A single numeric value. Returns 0 if fewer than 3 non-missing values
#'   remain or if the standard deviation is zero.
#'
#' @examples
#' skewness(rlnorm(100))
#' skewness(rlnorm(100), robust = 0.95)
#'
#' @export
skewness <- function(x, robust = NULL) {
  x <- x[!is.na(x)]
  if (!is.null(robust)) {
    if (robust < 0 || robust > 1) {
      stop("robust must be a numeric value between 0 and 1.")
    }
    q <- quantile(x, probs = c((1 - robust) / 2, 1 - (1 - robust) / 2))
    x <- x[x >= q[1] & x <= q[2]]
  }
  n <- length(x)
  if (n < 3) {
    return(0)
  }
  m <- mean(x)
  s <- sd(x)
  if (s == 0) {
    return(0)
  }
  (n / ((n - 1) * (n - 2))) * sum(((x - m) / s)^3)
}


mad_winsorise <- function(x, k = 4) {
  med <- median(x, na.rm = TRUE)
  mad_val <- mad(x, na.rm = TRUE)
  if (mad_val == 0) {
    return(x)
  }
  lower <- med - k * mad_val
  upper <- med + k * mad_val
  pmin(pmax(x, lower), upper)
}

transform_marker <- function(x) {
  raw_skew <- abs(skewness(x, robust = 0.95))

  if (any(x < 0, na.rm = TRUE)) {
    return(list(values = x, transform = "none", skewness = raw_skew))
  }

  half_min <- min(x[x > 0], na.rm = TRUE) / 2
  x_nozero <- ifelse(x == 0, half_min, x)

  candidates <- list(
    none = x,
    sqrt = sqrt(x),
    log = log(x_nozero)
  )

  skews <- sapply(candidates, \(v) abs(skewness(v)))
  best <- names(which.min(skews))

  list(values = candidates[[best]], transform = best, skewness = skews[best])
}
