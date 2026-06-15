# redcap.R
# Import and recode REDCap data exports using the accompanying data dictionary:
# applies dropdown/radio factor levels, condenses checkbox groups, and attaches
# cleaned variable labels.
#
# Suggests packages: readr, dplyr, tidyr, tibble, stringr, rlang, reportRmd

#' Parse a REDCap "Choices" string into codes and labels
#'
#' @param x A single "code, label | code, label | ..." string.
#' @return A data frame with `code` (numeric) and `label` (character).
#' @export
parse_code_label <- function(x) {
  # Split by pipes
  parts <- trimws(unlist(strsplit(x, "\\|")))

  # Split each part on the FIRST comma only
  code <- as.numeric(gsub("[^0-9]", "", sub(",.*", "", parts)))
  label <- trimws(sub("^[^,]*,", "", parts))

  data.frame(
    code = code,
    label = label,
    stringsAsFactors = FALSE
  )
}

#' Clean REDCap field labels for reporting
#'
#' Strips HTML, leading numbering, parenthetical text and trailing colons.
#'
#' @param z A character vector of raw field labels.
#' @return A character vector of cleaned labels.
#' @export
clean_field_labels <- function(z) {
  sapply(
    z,
    function(x) {
      if (is.na(x)) {
        return(NA)
      }
      x <- iconv(x, from = "", to = "UTF-8", sub = "")
      if (x == "(please specify):") {
        return("other")
      }
      x <- gsub("<.*?>", "", x)
      x <- gsub("^\\d+\\.\\s*", "", x)
      # Remove anything in parentheses (and the parentheses)
      x <- gsub("\\s*\\([^)]*\\)", "", x)
      # Remove trailing colon
      x <- gsub(":$", "", x)
      return(trimws(x))
    },
    USE.NAMES = FALSE
  )
}

#' Import and recode a REDCap data export
#'
#' Reads a REDCap data file and its data dictionary, converts dropdown/radio
#' fields to labelled factors, condenses checkbox groups into a single factor
#' (or an amalgamated `_all` variable when respondents may select more than
#' one option), and attaches cleaned variable labels via `reportRmd::set_labels`.
#'
#' @param data_path Path to the REDCap data CSV.
#' @param dict_path Path to the REDCap data dictionary CSV.
#' @return A labelled data frame.
#' @export
import_REDCap_data <- function(data_path, dict_path) {
  df <- readr::read_csv(data_path)
  dict <- readr::read_csv(dict_path)

  # Re-code all the drop-downs & radio buttons
  simple_recodes <- dict |>
    dplyr::filter(`Field Type` %in% c("dropdown", "radio")) |>
    dplyr::pull(`Variable / Field Name`)
  for (v in simple_recodes) {
    lbl_lookup <- parse_code_label(dict$`Choices, Calculations, OR Slider Labels`[
      dict$`Variable / Field Name` == v
    ])
    names(lbl_lookup)[1] <- v
    new_values <- df |> dplyr::select(!!rlang::sym(v)) |> dplyr::left_join(lbl_lookup)
    df[[v]] <- factor(new_values$label, levels = lbl_lookup$label)
  }

  # Recode the checkbox variables
  cb_recodes <- dict |>
    dplyr::filter(`Field Type` %in% c("checkbox")) |>
    dplyr::pull(`Variable / Field Name`)

  cb_labels <- cb_all_labels <- NULL
  if (length(cb_recodes) > 0) {
    for (v in cb_recodes) {
      checkbox_vars <- NULL
      lbl_lookup <- parse_code_label(dict$`Choices, Calculations, OR Slider Labels`[
        dict$`Variable / Field Name` == v
      ])
      for (i in lbl_lookup$code) {
        # BUGFIX: index the label by matching code, not by position. The
        # original `lbl_lookup$label[i]` assumed codes were 1..k and contiguous,
        # which silently mislabels checkboxes coded from 0 or with gaps.
        this_label <- lbl_lookup$label[match(i, lbl_lookup$code)]
        new_values <- dplyr::case_when(
          is.na(df[[paste0(v, "___", i)]]) ~ NA,
          df[[paste0(v, "___", i)]] == 0 ~ "",
          df[[paste0(v, "___", i)]] == 1 ~ this_label
        )
        new_values <- trimws(gsub("[(].*", "", new_values))
        checkbox_vars <- dplyr::bind_cols(checkbox_vars, new_values)
      }
      ordered_lbls <- trimws(gsub("[(].*", "", lbl_lookup$label))
      # Count number of entries per row
      non_empty_count <- rowSums(!is.na(checkbox_vars) & checkbox_vars != "")
      condensed_var <- apply(checkbox_vars, 1, function(x) {
        if (all(is.na(x))) {
          return(NA)
        }
        x <- trimws(x)
        paste0(x[!is.na(x) & x != ""], collapse = ", ")
      })
      condensed_lvls <- data.frame(condensed_var = condensed_var) |>
        dplyr::distinct(condensed_var) |>
        dplyr::filter(!is.na(condensed_var), condensed_var != "") |>
        dplyr::mutate(
          first_entry = stringr::str_extract(condensed_var, "^[^,]+") |>
            stringr::str_trim()
        ) |>
        dplyr::arrange(match(first_entry, ordered_lbls)) |>
        dplyr::pull(condensed_var)

      # Apply as factor
      condensed_var <- factor(condensed_var, levels = condensed_lvls)

      # If the maximum count is 1, replace the dummy columns with one factor
      if (max(non_empty_count, na.rm = TRUE) <= 1) {
        df[[v]] <- condensed_var
        df[, paste0(v, "___", lbl_lookup$code)] <- NULL
      } else {
        # Otherwise, add an amalgamated variable and record its label
        df <- df |>
          tibble::add_column(
            !!paste0(v, "_all") := condensed_var,
            .after = paste0(v, "___", max(lbl_lookup$code))
          )
        cb_all_labels <- dplyr::bind_rows(
          cb_all_labels,
          data.frame(v_name = paste0(v, "_all"), label = paste(v, "merged"))
        )
      }
    }

    # BUGFIX: removed a stray post-loop block that added a spurious "<v>_all2"
    # column using the final iteration's `v`/`condensed_var`/`lbl_lookup`
    # (which also errored when no checkbox variables were present).

    # Get the labels for checkbox variables
    cb_vars <- dict |>
      dplyr::filter(`Field Type` == "checkbox") |>
      dplyr::select(`Variable / Field Name`, `Choices, Calculations, OR Slider Labels`)
    cb_labels <- lapply(seq_len(nrow(cb_vars)), function(i) {
      x <- cb_vars$`Choices, Calculations, OR Slider Labels`[i]
      parse_code_label(x) |>
        dplyr::mutate(
          `Variable / Field Name` = cb_vars$`Variable / Field Name`[i],
          v_name = paste0(`Variable / Field Name`, "___", code),
          label = trimws(gsub("[(].*", "", label))
        )
    }) |>
      dplyr::bind_rows() |>
      dplyr::select(v_name, label)
  } # end checkbox recoding

  # Add variable labels
  v_labels <- dict |>
    dplyr::filter(`Field Type` != "descriptive") |>
    dplyr::select(v_name = `Variable / Field Name`, `Field Label`) |>
    dplyr::mutate(
      label = clean_field_labels(`Field Label`)
    ) |>
    dplyr::select(!`Field Label`) |>
    dplyr::mutate(
      label = dplyr::if_else(
        label == "other",
        gsub("_", " ", v_name),
        label
      )
    )

  v_labels <- dplyr::bind_rows(
    v_labels,
    cb_labels,
    cb_all_labels
  )
  df <- df |> reportRmd::set_labels(v_labels)

  return(df)
}
