#' Compare diagnostic test metrics for a paired study design
#'
#' Computes sensitivity, specificity, PPV, NPV (with confidence intervals)
#' for two diagnostic tests applied to the same subjects and tests for
#' differences between tests.
#'
#' PPV and NPV are compared using either the empirical Wald test from a
#' GEE logistic model with robust sandwich variance (Leisenring et al. 2000)
#' or the weighted generalized score statistic (Kosinski 2013).
#'
#' Note that the GEE method reports an interpretable OR, but may be unstable
#' at PPV/NPV near 0/1, while the WGS methods reports a chi-square like
#' statistic.
#'
#' Sensitivity is compared using McNemar's test (unclustered) or
#' Obuchowski's test for clustered paired data, applied to the subset of
#' diseased observations. Specificity is compared analogously on the
#' non-diseased subset. Both tests require at least 5 discordant
#' observations to be reported.
#'
#' @param y numeric vector of disease status (1 = disease, 0 = no disease).
#' @param x numeric vector of test results (1 = positive, 0 = negative).
#' @param z numeric vector indicating test (0 = test 1, 1 = test 2).
#' @param cluster_id optional vector of cluster/patient identifiers. When
#'   NULL, observations are paired by position within each test group.
#'   When provided, confidence intervals are cluster-adjusted and
#'   Obuchowski's test is used instead of McNemar's for sensitivity and
#'   specificity comparisons.
#' @param test_labels character vector of length 2 giving names for test 1
#'   (z = 0) and test 2 (z = 1). Defaults to \code{c("Test 1", "Test 2")}.
#' @param method character; \code{"gee"} (default) for the empirical Wald
#'   test from a GEE logistic model, or \code{"wgs"} for the weighted
#'   generalized score statistic of Kosinski (2013).
#' @param tableOnly logical; if TRUE return a data frame, otherwise pass
#'   through \code{reportRmd::outTable()} (default: FALSE).
#'
#' @return A data frame (or formatted table) comparing diagnostic metrics
#'   between the two tests, including point estimates with confidence
#'   intervals, concordance counts, and test statistics for PPV, NPV,
#'   sensitivity, and specificity comparisons.
#'
#' @references
#' Kosinski AS. A weighted generalized score statistic for comparison of
#' predictive values of diagnostic tests. \emph{Stat Med}.
#' 2013;32(6):964-977.
#'
#' Leisenring W, Alonzo T, Pepe MS. Comparisons of predictive values of
#' binary medical diagnostic tests for paired designs. \emph{Biometrics}.
#' 2000;56:345-351.
#'
#' McNemar, Q. (1947). Note on the sampling error of the difference between
#' correlated proportions or percentages. Psychometrika, 12(2), 153-157.
#'
#' Obuchowski NA. On the comparison of correlated proportions for
#' clustered data. \emph{Stat Med}. 1998;17:1495-1507.
#'
#' @export
compare_test_metrics <- function(
  y,
  x,
  z,
  cluster_id = NULL,
  test_labels = NULL,
  method = c("gee", "wgs"),
  tableOnly = FALSE
) {
  method <- match.arg(method)

  # Default test labels
  if (is.null(test_labels)) {
    test_labels <- c("Test 1", "Test 2")
  }

  # Validate inputs
  n <- length(y)
  stopifnot(
    "y, x, z must be the same length" = length(x) == n && length(z) == n,
    "z must contain only 0 and 1" = all(z %in% c(0, 1)),
    "y must contain only 0 and 1" = all(y %in% c(0, 1, NA)),
    "x must contain only 0 and 1" = all(x %in% c(0, 1, NA)),
    "test_labels must be length 2" = length(test_labels) == 2
  )

  # Create positional pairing when no cluster_id supplied
  if (is.null(cluster_id)) {
    n1 <- sum(z == 0)
    n2 <- sum(z == 1)
    stopifnot(
      "Equal observations per test required when cluster_id is NULL" = n1 == n2
    )
    cluster_id <- integer(n)
    cluster_id[z == 0] <- seq_len(n1)
    cluster_id[z == 1] <- seq_len(n2)
    is_clustered <- FALSE
  } else {
    stopifnot(
      "cluster_id must be the same length as y" = length(cluster_id) == n
    )
    is_clustered <- TRUE
  }

  # Split by test
  idx1 <- which(z == 0)
  idx2 <- which(z == 1)

  # Classify observations as TP/FP/FN/TN
  classify <- function(x, y) {
    dplyr::case_when(
      x == 1 & y == 1 ~ "TP",
      x == 1 & y == 0 ~ "FP",
      x == 0 & y == 1 ~ "FN",
      x == 0 & y == 0 ~ "TN",
      .default = NA_character_
    )
  }
  class1 <- classify(x[idx1], y[idx1])
  class2 <- classify(x[idx2], y[idx2])

  # Compute per-test metrics with CIs
  cl1 <- if (is_clustered) cluster_id[idx1] else NULL
  cl2 <- if (is_clustered) cluster_id[idx2] else NULL

  tbl <- dplyr::bind_rows(
    compute_metrics_with_CI(class1, cluster = cl1) |>
      dplyr::mutate(Modality = test_labels[1]),
    compute_metrics_with_CI(class2, cluster = cl2) |>
      dplyr::mutate(Modality = test_labels[2])
  ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      est_ci = dplyr::case_when(
        is.na(N) ~ psthr(c(estimate, LB, UB)),
        .default = as.character(N)
      )
    ) |>
    dplyr::select(!c(estimate, LB, UB, N)) |>
    tidyr::pivot_wider(names_from = Modality, values_from = est_ci)

  # Build column headers
  if (is_clustered) {
    n_t1_pts <- length(unique(cluster_id[idx1][!is.na(class1)]))
    n_t1_obs <- sum(!is.na(class1))
    n_t2_pts <- length(unique(cluster_id[idx2][!is.na(class2)]))
    n_t2_obs <- sum(!is.na(class2))
    t1_header <- paste0(
      test_labels[1],
      " (pt = ",
      n_t1_pts,
      ", obs = ",
      n_t1_obs,
      ")"
    )
    t2_header <- paste0(
      test_labels[2],
      " (pt = ",
      n_t2_pts,
      ", obs = ",
      n_t2_obs,
      ")"
    )
    concordant_label <- "Concordant observations:"
    discordant_label <- "Discordant observations:"
  } else {
    t1_header <- paste0(test_labels[1], " (n = ", sum(!is.na(class1)), ")")
    t2_header <- paste0(test_labels[2], " (n = ", sum(!is.na(class2)), ")")
    concordant_label <- "Concordant observations:"
    discordant_label <- "Discordant observations:"
  }
  names(tbl) <- gsub(test_labels[1], t1_header, names(tbl), fixed = TRUE)
  names(tbl) <- gsub(test_labels[2], t2_header, names(tbl), fixed = TRUE)

  # Concordance and discordance
  n_concordant <- sum(class1 == class2, na.rm = TRUE)
  n_discordant <- sum(class1 != class2, na.rm = TRUE)
  tbl <- tbl |>
    dplyr::add_row(metric = paste(concordant_label, n_concordant)) |>
    dplyr::add_row(metric = paste(discordant_label, n_discordant))

  # Test for difference in PPV and NPV
  if (method == "wgs") {
    wgs <- wgs_test(
      class1,
      class2,
      cluster_id = if (is_clustered) cluster_id[idx1] else NULL
    )
    tbl$`WGS (p-value)` <- NA_character_
    tbl$`WGS (p-value)`[which(tbl$metric == "PPV")] <- paste0(
      rnd(wgs$T_wgs[1], 1),
      " (",
      formatp(wgs$p_value[1]),
      ")"
    )
    tbl$`WGS (p-value)`[which(tbl$metric == "NPV")] <- paste0(
      rnd(wgs$T_wgs[2], 1),
      " (",
      formatp(wgs$p_value[2]),
      ")"
    )
  } else {
    # GEE approach
    gee_data <- data.frame(
      y = y,
      y0 = 1L - y,
      x = x,
      z = z,
      cluster_id = cluster_id
    ) |>
      dplyr::arrange(cluster_id) # required for correct estimation

    ppv_fit <- try(
      geepack::geeglm(
        y ~ z,
        id = cluster_id,
        data = subset(gee_data, x == 1),
        family = binomial(link = "logit"),
        corstr = "independence"
      ),
      silent = TRUE
    )

    npv_fit <- try(
      geepack::geeglm(
        y0 ~ z,
        id = cluster_id,
        data = subset(gee_data, x == 0),
        family = binomial(link = "logit"),
        corstr = "independence"
      ),
      silent = TRUE
    )

    if (!inherits(ppv_fit, "try-error") | !inherits(npv_fit, "try-error")) {
      tbl$`GEE OR (p-value)` <- NA
    }
    if (!inherits(ppv_fit, "try-error")) {
      ppv_test <- rm_mvsum(ppv_fit, tableOnly = TRUE, digits = 2)
      ppv_out <- paste0(
        trimws(gsub("[(].*", "", ppv_test$`OR(95%CI)`)),
        " (",
        ppv_test$`p-value`,
        ")"
      )
      tbl$`GEE OR (p-value)`[which(tbl$metric == "PPV")] <- ppv_out
    }
    if (!inherits(npv_fit, "try-error")) {
      npv_test <- rm_mvsum(npv_fit, tableOnly = TRUE, digits = 2)
      npv_out <- paste0(
        trimws(gsub("[(].*", "", npv_test$`OR(95%CI)`)),
        " (",
        npv_test$`p-value`,
        ")"
      )
      tbl$`GEE OR (p-value)`[which(tbl$metric == "NPV")] <- npv_out
    }
  }

  # McNemar / Obuchowski tests for sensitivity and specificity
  if (n_discordant >= 5) {
    t1_pos <- factor(
      x[idx1],
      levels = c(0, 1),
      labels = c("Negative", "Positive")
    )
    t2_pos <- factor(
      x[idx2],
      levels = c(0, 1),
      labels = c("Negative", "Positive")
    )
    y1 <- y[idx1]

    # Sensitivity: among diseased (y==1)
    diseased <- y1 == 1
    sens_discordant <- sum(t1_pos[diseased] != t2_pos[diseased], na.rm = TRUE)

    if (sens_discordant >= 5) {
      pv <- t1_pos[diseased]
      mrv <- t2_pos[diseased]
      if (is_clustered) {
        ids <- cluster_id[idx1][diseased]
        df <- do.call(
          rbind,
          lapply(unique(ids), function(id) {
            tab <- table(pv[ids == id], mrv[ids == id])
            data.frame(
              cluster = id,
              pv0_mrv0 = tab[1, 1],
              pv0_mrv1 = tab[1, 2],
              pv1_mrv0 = tab[2, 1],
              pv1_mrv1 = tab[2, 2]
            )
          })
        )
        htest_sensitivity <- clust.bin.pair::clust.bin.pair(
          df$pv0_mrv0,
          df$pv0_mrv1,
          df$pv1_mrv0,
          df$pv1_mrv1,
          method = "obuchowski"
        )
        test_name <- "Obuchowski ChiSq (p-value)"
      } else {
        htest_sensitivity <- mcnemar.test(table(pv, mrv))
        test_name <- "McNemar ChiSq (p-value)"
      }
      tbl$diff_test <- NA
      tbl$diff_test[which(tbl$metric == "Sensitivity")] <- paste0(
        rnd(htest_sensitivity$statistic, 1),
        " (",
        formatp(htest_sensitivity$p.value),
        ")"
      )
      names(tbl) <- gsub("diff_test", test_name, names(tbl))
    }

    # Specificity: among non-diseased (y==0)
    non_diseased <- y1 == 0
    spec_discordant <- sum(
      t1_pos[non_diseased] != t2_pos[non_diseased],
      na.rm = TRUE
    )

    if (spec_discordant >= 5) {
      pv <- t1_pos[non_diseased]
      mrv <- t2_pos[non_diseased]
      if (is_clustered) {
        ids <- cluster_id[idx1][non_diseased]
        df <- do.call(
          rbind,
          lapply(unique(ids), function(id) {
            tab <- table(pv[ids == id], mrv[ids == id])
            data.frame(
              cluster = id,
              pv0_mrv0 = tab[1, 1],
              pv0_mrv1 = tab[1, 2],
              pv1_mrv0 = tab[2, 1],
              pv1_mrv1 = tab[2, 2]
            )
          })
        )
        htest_specificity <- clust.bin.pair::clust.bin.pair(
          df$pv0_mrv0,
          df$pv0_mrv1,
          df$pv1_mrv0,
          df$pv1_mrv1,
          method = "obuchowski"
        )
        test_name <- "Obuchowski ChiSq (p-value)"
      } else {
        htest_specificity <- mcnemar.test(table(pv, mrv))
        test_name <- "McNemar ChiSq (p-value)"
      }
      t_name <- grep("ChiSq", names(tbl), value = TRUE)
      diff_test <- paste0(
        rnd(htest_specificity$statistic, 1),
        " (",
        formatp(htest_specificity$p.value),
        ")"
      )

      if (length(t_name) > 0) {
        tbl[[t_name[1]]][which(tbl$metric == "Specificity")] <- diff_test
      } else {
        tbl$diff_test <- NA
        tbl$diff_test[which(tbl$metric == "Specificity")] <- diff_test
        names(tbl) <- gsub("diff_test", test_name, names(tbl))
      }
    }
  }

  # Return
  if (tableOnly) {
    return(tbl)
  }
  tbl |> reportRmd::outTable(digits = 3)
}

compare_test_metrics(
  x = test_data$x,
  y = test_data$y,
  z = test_data$z,
  method = "wgs"
)
