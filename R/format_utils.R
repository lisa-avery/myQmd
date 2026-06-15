# format_utils.R
# GLOBAL HELPER - source this whenever you keep any of the analysis scripts.
# Number/p-value formatting, name tidying, htest -> data frame, and
# skewness / winsorising / auto-transform helpers used across the template.
#
# Suggests packages: dplyr (for htest_dataframe downstream use)

# Formatting ---------------------------------------------------------------

#' Round Numbers with Trailing Zeros
#'
#' @param x Numeric vector to be rounded
#' @param digits Integer indicating the number of decimal places (default: 2)
#' @return Character vector of the rounded numbers with trailing zeros preserved
#' @examples
#' rnd(c(1.2345, 5.67, 9))
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
#' @return Character vector with formatted numbers
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

#' Format p-values for reporting
#'
#' @param pvalues numeric vector of p-values
#' @return character vector of formatted p-values
#' @export
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

#' Format Numbers with Context-Aware Precision
#'
#' Dynamically formats numeric values with different precision levels
#' based on their magnitude.
#'
#' @param x A vector, typically a column from a data frame
#' @return A character vector formatted by magnitude
#' @export
format_dynamic <- function(x) {
  if (is.numeric(x)) {
    sapply(x, function(value) {
      if (abs(value) < 1e-3) {
        return(format(round(value, 5), scientific = FALSE))
      } else if (abs(value) < 1) {
        return(format(round(value, 3), scientific = FALSE))
      } else if (abs(value) < 10) {
        return(format(round(value, 2), scientific = FALSE))
      } else if (abs(value) < 100) {
        return(format(round(value, 1), scientific = FALSE))
      } else {
        return(format(round(value, 0), scientific = FALSE))
      }
    })
  } else {
    return(x)
  }
}

#' Convert variable names for display
#'
#' Replace underscores and dots with spaces, preserving decimal numbers
#'
#' @param strings character vector of names to convert
#' @param check_numbers logical, should decimal numbers be preserved
#' @export
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

#' Replace underscores with spaces
#' @param x character string
#' @export
rm_ <- function(x) {
  gsub("_", " ", x)
}

#' Convert p-values to significance stars
#' @param adj_p numeric vector of p-values
#' @return character vector of significance stars
#' @export
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

#' Format estimate with CI: "est (LB, UB)"
#' @param x numeric vector of length >= 2 (estimate, bounds)
#' @export
pstprn <- function(x) {
  paste(x[1], " (", paste(x[-1], collapse = ", "), ")", sep = "")
}

#' Format vector as "est (LB, UB)" with scientific notation handling
#' @param x numeric vector
#' @param y number of digits
#' @export
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
#' Converts an object of class "htest" into a data frame. Useful for
#' batch processing statistical tests. Combine results with dplyr::bind_rows().
#'
#' @param htest_object An object of class "htest"
#' @return A data frame with one row containing all test components
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

#' Print a string vector for re-use
#'
#' @param string a character vector
#' @export
catOut <- function(string) {
  paste0("('", paste0(string, collapse = "','"), "')")
}


# Transforms ---------------------------------------------------------------

#' Sample Skewness with Optional Robust Trimming
#'
#' Computes the adjusted Fisher-Pearson skewness coefficient, optionally trimming
#' to a central proportion of the data to reduce the influence of extreme values.
#'
#' @param x Numeric vector.
#' @param robust Optional numeric value between 0 and 1 specifying the central
#'   proportion of data to retain before computing skewness.
#' @return A single numeric value.
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

#' Winsorize Outliers Using MAD
#'
#' @param x numeric vector
#' @param k number of MADs from median to use as bounds (default: 4)
#' @return numeric vector with outliers winsorised
#' @export
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

#' Auto-Select Transformation to Minimise Skewness
#'
#' Evaluates identity, sqrt, and log transforms and selects the one that
#' minimises the absolute skewness.
#'
#' @param x numeric vector
#' @return list with values, transform name, and skewness
#' @export
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
