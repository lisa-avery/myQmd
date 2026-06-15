# quarto_tools.R
# Project tooling for Quarto/R Markdown reports: build a document-specific
# .bib from a master bibliography, extract figure chunks to a save script,
# and dump a function definition to a file.
#
# Suggests packages: rstudioapi, stringr, knitr, tools

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

# Code Export -------------------------------------------------------------

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
