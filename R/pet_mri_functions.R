#' Compare gold standard classification from two modalities
#' @param pet_values character vector of TP/TN/FP/FN for PET
#' @param mr_values character vector of TP/TN/FP/FN for MR
#' @export
compare_gold_standard <- function(pet_values, mr_values) {
  if (!length(pet_values) == length(mr_values)) {
    stop("vectors have different lengths, must be the same")
  }
  gs_pet <- dplyr::case_match(
    pet_values,
    "TP" ~ "Positive",
    "FN" ~ "Positive",
    "TN" ~ "Negative",
    "FP" ~ "Negative"
  )
  gs_mr <- dplyr::case_match(
    mr_values,
    "TP" ~ "Positive",
    "FN" ~ "Positive",
    "TN" ~ "Negative",
    "FP" ~ "Negative"
  )
  if (!all(gs_pet == gs_mr, na.rm = T)) {
    bad <- which(gs_pet != gs_mr)
    stop("Gold standard differs at: ", paste(bad, collapse = ","))
  }
  return(gs_pet)
}

#' Compare PET and MR result counts
#' @param pet_values character vector
#' @param mr_values character vector
#' @param tableOnly return data frame only
#' @export
compare_PET_MR_results <- function(pet_values, mr_values, tableOnly = FALSE) {
  tbl <- dplyr::full_join(
    pet_values |>
      janitor::tabyl() |>
      dplyr::select(!percent) |>
      dplyr::rename(result = pet_values) |>
      dplyr::mutate(Modality = "PET"),
    mr_values |>
      janitor::tabyl() |>
      dplyr::select(!percent) |>
      dplyr::rename(result = mr_values) |>
      dplyr::mutate(Modality = "MR")
  ) |>
    tidyr::pivot_wider(names_from = Modality, values_from = n) |>
    dplyr::mutate(
      result = factor(result, levels = c("TP", "TN", "FP", "FN", "NA"))
    ) |>
    dplyr::arrange(result)
  if (tableOnly) {
    return(tbl)
  }
  tbl |> reportRmd::outTable()
}

#' Compare diagnostic metrics between PET and MR with McNemar/Obuchowski test
#' @param pet_values character vector of TP/TN/FP/FN
#' @param mr_values character vector
#' @param cluster_id optional cluster IDs
#' @param n_boot optional bootstrap replicates
#' @param tableOnly return data frame only
#' @export
compare_PET_MR_metrics <- function(
    pet_values,
    mr_values,
    cluster_id = NULL,
    n_boot = NULL,
    tableOnly = FALSE
) {
  gold_standard_from_PET <- compare_gold_standard(pet_values, mr_values)
  total <- length(pet_values)
  tbl <- dplyr::bind_rows(
    compute_metrics_with_CI(pet_values, cluster = cluster_id) |>
      dplyr::mutate(Modality = "PET"),
    compute_metrics_with_CI(mr_values, cluster = cluster_id) |>
      dplyr::mutate(Modality = "MR")
  ) |>
    dplyr::rowwise() |>
    dplyr::mutate(est_ci = psthr(c(estimate, LB, UB))) |>
    dplyr::select(!c(estimate, LB, UB)) |>
    tidyr::pivot_wider(names_from = Modality, values_from = est_ci)
  
  if (!is.null(cluster_id)) {
    n_pet <- length(unique(cluster_id[!is.na(pet_values)]))
    n_mr <- length(unique(cluster_id[!is.na(mr_values)]))
  } else {
    n_pet <- length(na.omit(pet_values))
    n_mr <- length(na.omit(mr_values))
  }
  names(tbl) <- gsub("PET", paste0("PET (n=", n_pet, ")"), names(tbl))
  names(tbl) <- gsub("MR", paste0("MR (n=", n_mr, ")"), names(tbl))
  
  n_concordant <- sum(pet_values == mr_values, na.rm = TRUE)
  n_discordant <- sum(pet_values != mr_values, na.rm = TRUE)
  tbl <- tbl |>
    dplyr::add_row(metric = paste("Concordant cases:", n_concordant)) |>
    dplyr::add_row(metric = paste("Discordant cases:", n_discordant))
  
  if (n_discordant >= 5) {
    pet_positive <- factor(
      substr(pet_values, 2, 2),
      levels = c("P", "N"),
      labels = c("Positive", "Negative")
    )
    mr_positive <- factor(
      substr(mr_values, 2, 2),
      levels = c("P", "N"),
      labels = c("Positive", "Negative")
    )
    
    .add_test_row <- function(tbl, subset_label, gs_condition) {
      pv <- pet_positive[gold_standard_from_PET == gs_condition]
      mrv <- mr_positive[gold_standard_from_PET == gs_condition]
      disc <- sum(pv != mrv, na.rm = TRUE)
      if (disc < 5) {
        return(tbl)
      }
      
      if (!is.null(cluster_id)) {
        ids <- cluster_id[gold_standard_from_PET == gs_condition]
        df <- do.call(
          rbind,
          lapply(unique(cluster_id), function(x) {
            tab <- table(pv[ids == x], mrv[ids == x])
            data.frame(
              cluster = x,
              pv0_mrv0 = tab[1, 1],
              pv0_mrv1 = tab[1, 2],
              pv1_mrv0 = tab[2, 1],
              pv1_mrv1 = tab[2, 2]
            )
          })
        )
        ht <- clust.bin.pair::clust.bin.pair(
          df$pv0_mrv0,
          df$pv0_mrv1,
          df$pv1_mrv0,
          df$pv1_mrv1,
          method = "obuchowski"
        )
        test_name <- "Obuchowski ChiSq (p-value)"
      } else {
        ht <- mcnemar.test(table(pv, mrv))
        test_name <- "McNemar ChiSq (p-value)"
      }
      test_val <- paste0(rnd(ht$statistic, 1), " (", formatp(ht$p.value), ")")
      t_name <- grep("ChiSq", names(tbl), value = TRUE)
      if (length(t_name) > 0) {
        tbl <- tbl |>
          dplyr::add_row(
            metric = subset_label,
            !!!stats::setNames(list(test_val), t_name[1])
          )
      } else {
        tbl$diff_test <- NA
        tbl <- tbl |>
          dplyr::add_row(metric = subset_label, diff_test = test_val)
        names(tbl) <- gsub("diff_test", test_name, names(tbl))
      }
      tbl
    }
    
    tbl <- .add_test_row(tbl, "Difference in Sensitivty", "Positive")
    tbl <- .add_test_row(tbl, "Difference in Specificity", "Negative")
  }
  
  if (tableOnly) {
    return(tbl)
  }
  tbl |> reportRmd::outTable(digits = 3)
}
