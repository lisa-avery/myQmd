# Utility Functions
# Formatting, transforms, bibliography tools, figure extraction, and misc helpers

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


# Bibliography Tools -------------------------------------------------------

#' Generate a bibfile for a markdown document from a master *.bib file
#'
#' Searches through the current Rmd/Qmd document for citations and
#' extracts these from a central bib file to save as a file-specific bibfile.
#' Also includes citations to R packages.
#'
#' @param bib_file a master bib file containing all the references
#' @param output_file a filename to write the bibfile to (default bibfile.bib)
#' @export
createBibFile <- function(bib_file, output_file = "bibfile.bib") {
  markdown_file <- rstudioapi::getActiveDocumentContext()$path

  if (markdown_file == "") {
    stop("No active document path found - do you need to save the file?")
  }

  citations <- unique(unlist(stringr::str_extract_all(
    readLines(markdown_file),
    "@\\w+"
  )))
  citations <- setdiff(citations, c("@tbl", "@fig", "@R"))

  bib_text <- readLines(bib_file)
  entry_indices <- cumsum(grepl("^@", bib_text))
  entry_starts <- which(!duplicated(entry_indices))
  entry_ends <- c(entry_starts[-1], length(bib_text)) - 1

  cited_entries <- lapply(seq_along(citations), function(i) {
    citation_pattern <- paste0(
      "^@",
      ".*",
      "\\{",
      gsub("@", "", citations[i]),
      ","
    )
    entry_idx <- which(grepl(citation_pattern, bib_text[entry_starts]))
    if (length(entry_idx) > 0) {
      start <- entry_starts[entry_idx]
      end <- entry_ends[entry_idx]
      return(bib_text[start:end])
    } else {
      return(NULL)
    }
  })

  if (any(unlist(lapply(cited_entries, is.null)))) {
    message(paste0(
      "The following citations were not found in the bib_file:\n",
      paste(citations[unlist(lapply(cited_entries, is.null))], collapse = ", ")
    ))
  }
  cited_bib <- unlist(cited_entries[!unlist(lapply(cited_entries, is.null))])

  r_pckgs <- unique(unlist(stringr::str_extract_all(
    readLines(markdown_file),
    "\\[@R-(.*?)\\]"
  )))
  r_pckgs <- gsub("\\]", "", gsub("\\[@R-", "", r_pckgs))
  r_bib <- knitr::write_bib(r_pckgs, file = output_file)

  writeLines(c(cited_bib, unlist(r_bib, use.names = F), "\n"), output_file)
}


# Figure Extraction --------------------------------------------------------
#' Extract Figure Chunks and Generate a Save-File Chunk native R chunk that reproduces each figure and saves it to disk as PNG or TIFF.
#'
#' The generated chunk is written to a text file which can be manually appended
#' to the source document. All libraries and objects from earlier chunks are
#' assumed to already exist when the chunk is executed.
#'
#' @param path Character. Path to the `.Rmd` or `.qmd` file (required).
#' @param format Character. Output image format: `"png"` or `"tiff"`.
#'   Default is `"png"`.
#' @param output_file Character. Path to the text file where the generated
#'   chunk will be written. Default is `"save-figures.txt"`.
#' @param output_dir Character. Directory where figures will be saved when
#'   the generated chunk runs. Default is `"figures"`.
#' @param dpi Numeric. Resolution (dots per inch) for saved images.
#'   Default is `300`.
#' @param default_width Numeric. Default figure width in inches for chunks
#'   without explicit size. Default is `7`.
#' @param default_height Numeric. Default figure height in inches for chunks
#'   without explicit size. Default is `5`.
#' @param overwrite Logical. If `TRUE`, overwrite `output_file` if it exists.
#'   If `FALSE` (default), the function errors if the file exists.
#'
#' @details
#' Chunk detection rules:
#' \itemize{
#'   \item A chunk begins with \code{```{r ...}} and ends with \code{```}.
#'   \item A chunk is considered a figure chunk if its label begins with `"fig"`.
#'   \item Both label styles are supported:
#'   \itemize{
#'     \item Knitr inline: \code{```{r fig-plot}}
#'     \item Quarto hash-pipe: \code{#| label: fig-plot}
#'   }
#'   \item If both are present, the hash-pipe label takes precedence.
#'   \item Chunks with \code{eval: false} or \code{eval=FALSE} are ignored.
#'   \item A chunk labelled \code{save-figures} is always ignored.
#' }
#'
#' Duplicate labels:
#' \itemize{
#'   \item If duplicate figure labels are detected, the function stops
#'   with an informative error listing labels and line numbers.
#' }
#'
#' Output behavior:
#' \itemize{
#'   \item A single Quarto-native chunk is generated:
#'     \itemize{
#'       \item Uses \code{```{r}} with \code{#| label: save-figures}
#'       \item Creates \code{output_dir} if needed
#'       \item Reproduces each figure sequentially
#'     }
#'   \item Figures are named \code{Figure1}, \code{Figure2}, etc.
#'   \item Result is written to \code{output_file}
#'   \item A summary data frame is printed
#'   \item The generated chunk is returned invisibly as a character string
#' }
#'
#' Known limitations:
#' \itemize{
#'   \item Only the final plot in a chunk is saved
#'   \item Heuristic detection is used for distinguishing ggplot vs base plots
#' }
#'
#' @return Invisibly returns the generated chunk as a single character string.
#'   Prints a summary data frame mapping figure numbers to files.
#'
#' @examples
#' \dontrun{
#' extract_figure_chunks("analysis.qmd")
#' extract_figure_chunks(
#'   path = "report.Rmd",
#'   format = "tiff",
#'   output_file = "save-figures.txt",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
extract_figure_chunks <- function(
  path,
  format = "png",
  output_file = "save-figures.txt",
  output_dir = "figures",
  dpi = 300,
  default_width = 7,
  default_height = 5,
  overwrite = FALSE
) {
  if (!file.exists(path)) {
    stop("Input file does not exist: ", path)
  }

  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("rmd", "qmd")) {
    stop("File must have extension .Rmd or .qmd")
  }

  format <- tolower(format)
  if (!format %in% c("png", "tiff")) {
    stop("format must be 'png' or 'tiff'")
  }

  if (file.exists(output_file) && !overwrite) {
    stop("Output file already exists. Use overwrite = TRUE to replace it.")
  }

  lines <- readLines(path, warn = FALSE)

  fence_start <- grep("^```\\{r", lines)
  chunks <- list()

  for (start in fence_start) {
    end <- which(seq_along(lines) > start & grepl("^```\\s*$", lines))[1]
    if (is.na(end)) {
      next
    }

    chunk_lines <- lines[start:end]
    header <- chunk_lines[1]

    label_inline <- NA
    content <- sub("^```\\{r\\s*", "", header)
    content <- sub("\\}$", "", content)
    parts <- strsplit(content, ",")[[1]]
    parts <- trimws(parts)
    if (length(parts) > 0 && !grepl("=", parts[1])) {
      label_inline <- parts[1]
    }

    label_hash <- NA
    hash_lines <- grep("^#\\|", chunk_lines, value = TRUE)
    for (hl in hash_lines) {
      if (grepl("label:", hl)) {
        label_hash <- trimws(sub(".*label:\\s*", "", hl))
      }
    }

    label <- if (!is.na(label_hash)) label_hash else label_inline

    if (is.na(label) || !grepl("^fig", label)) {
      next
    }
    if (label == "save-figures") {
      next
    }
    if (any(grepl("eval\\s*[:=]\\s*FALSE", chunk_lines, ignore.case = TRUE))) {
      next
    }

    width <- default_width
    height <- default_height
    chunk_dpi <- dpi

    if (grepl("fig.width", header)) {
      width <- as.numeric(sub(".*fig.width\\s*=\\s*([0-9.]+).*", "\\1", header))
    }
    if (grepl("fig.height", header)) {
      height <- as.numeric(sub(
        ".*fig.height\\s*=\\s*([0-9.]+).*",
        "\\1",
        header
      ))
    }

    for (hl in hash_lines) {
      if (grepl("fig-width:", hl)) {
        width <- as.numeric(sub(".*fig-width:\\s*", "", hl))
      }
      if (grepl("fig-height:", hl)) {
        height <- as.numeric(sub(".*fig-height:\\s*", "", hl))
      }
      if (grepl("dpi:", hl)) {
        chunk_dpi <- as.numeric(sub(".*dpi:\\s*", "", hl))
      }
    }

    body <- chunk_lines[-c(1, length(chunk_lines))]
    body <- body[!grepl("^#\\|", body)]

    chunks[[length(chunks) + 1]] <- list(
      label = label,
      start_line = start,
      code = body,
      width = width,
      height = height,
      dpi = chunk_dpi
    )
  }

  if (length(chunks) == 0) {
    warning("No figure chunks found.")
    return(NULL)
  }

  labels <- sapply(chunks, `[[`, "label")
  dup <- labels[duplicated(labels)]
  if (length(dup) > 0) {
    dup_info <- unique(sapply(dup, function(d) {
      idx <- which(labels == d)
      paste0(
        d,
        " (lines ",
        paste(sapply(idx, function(i) chunks[[i]]$start_line), collapse = ", "),
        ")"
      )
    }))
    stop("Duplicate figure labels found:\n", paste(dup_info, collapse = "\n"))
  }

  output <- c(
    "```{r}",
    "#| label: save-figures"
  )

  output <- c(
    output,
    sprintf(
      'dir.create("%s", recursive = TRUE, showWarnings = FALSE)',
      output_dir
    )
  )

  summary <- data.frame(
    figure = integer(),
    file = character(),
    label = character(),
    width = numeric(),
    height = numeric(),
    dpi = numeric()
  )

  for (i in seq_along(chunks)) {
    ch <- chunks[[i]]
    file <- file.path(output_dir, paste0("Figure", i, ".", format))

    output <- c(output, sprintf("\n# ---- Figure %d: %s ----", i, ch$label))

    code <- ch$code
    code[length(code)] <- paste0(".fig <- ", code[length(code)])
    output <- c(output, code)

    if (format == "png") {
      output <- c(
        output,
        sprintf(
          'if (inherits(.fig, "ggplot")) {
  ggsave("%s", plot = .fig, width = %f, height = %f, units = "in", dpi = %d)
} else {
  png("%s", width = %f, height = %f, units = "in", res = %d)
  print(.fig)
  dev.off()
}',
          file,
          ch$width,
          ch$height,
          ch$dpi,
          file,
          ch$width,
          ch$height,
          ch$dpi
        )
      )
    } else {
      output <- c(
        output,
        sprintf(
          'if (inherits(.fig, "ggplot")) {
  ggsave("%s", plot = .fig, width = %f, height = %f, units = "in", dpi = %d, compression = "lzw")
} else {
  tiff("%s", width = %f, height = %f, units = "in", res = %d, compression = "lzw")
  print(.fig)
  dev.off()
}',
          file,
          ch$width,
          ch$height,
          ch$dpi,
          file,
          ch$width,
          ch$height,
          ch$dpi
        )
      )
    }

    summary[nrow(summary) + 1, ] <- list(
      i,
      file,
      ch$label,
      ch$width,
      ch$height,
      ch$dpi
    )
  }

  output <- c(output, "```")

  writeLines(output, output_file)

  message("Generated chunk written to: ", output_file)
  print(summary)

  invisible(paste(output, collapse = "\n"))
}

#' Extract figure chunks from a Quarto markdown file
#'
#' Finds all code chunks with `fig-` labels and `eval != FALSE`
#'
#' @param qmd_file path to the .qmd file
#' @return list of chunks with start_line, end_line, and content
#' @export
extract_complete_figure_chunks <- function(qmd_file) {
  lines <- readLines(qmd_file, warn = FALSE)
  chunk_starts <- which(stringr::str_detect(lines, "^```\\{.*\\}"))
  chunk_ends <- which(stringr::str_detect(lines, "^```$"))

  if (length(chunk_starts) != length(chunk_ends)) {
    stop("Mismatched code chunk delimiters in file")
  }

  figure_chunks <- list()
  for (i in seq_along(chunk_starts)) {
    start_line <- chunk_starts[i]
    end_line <- chunk_ends[i]
    chunk_content <- lines[start_line:end_line]
    has_fig_label <- any(stringr::str_detect(
      chunk_content,
      "#\\|\\s*label:\\s*fig-"
    ))
    if (has_fig_label) {
      eval_false <- any(stringr::str_detect(
        chunk_content,
        "#\\|\\s*eval:\\s*(FALSE|F)\\s*$"
      ))
      if (!eval_false) {
        figure_chunks[[length(figure_chunks) + 1]] <- list(
          start_line = start_line,
          end_line = end_line,
          content = chunk_content
        )
      }
    }
  }
  return(figure_chunks)
}

#' Write extracted figure chunks to a file
#'
#' @param chunks output from extract_figure_chunks
#' @param output_file path to write to
#' @export
write_figure_chunks <- function(chunks, output_file) {
  if (length(chunks) == 0) {
    cat("No figure chunks found.\n")
    return()
  }
  output_lines <- c()
  for (i in seq_along(chunks)) {
    chunk <- chunks[[i]]
    header <- paste0(
      "# Figure chunk ",
      i,
      " (lines ",
      chunk$start_line,
      "-",
      chunk$end_line,
      ")"
    )
    output_lines <- c(output_lines, header, chunk$content, "")
  }
  writeLines(output_lines, output_file)
  cat("Extracted", length(chunks), "figure chunks to", output_file, "\n")
}
