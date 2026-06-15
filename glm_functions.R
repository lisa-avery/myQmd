# GLM Functions
# Model evaluation, ROC/AUC, all-possible-subsets, elastic net marker selection,
# Val-MI internal validation, and diagnostic comparison functions
#
# Depends on: R/utils.R (for rnd, niceNum, formatp, psthr, pstprn, nicename,
#             htest_dataframe, skewness, mad_winsorise, transform_marker)

# ROC / AUC

#' Fast Area Under the ROC Curve (AUC)
#'
#' @param probs A vector of probabilities/scores or a glm model object
#' @param class A vector denoting class membership (binary: 0 or 1)
#' @return Numeric AUC value
#' @export
fastAUC <- function(probs, class) {
  if (inherits(probs, "glm")) {
    class <- probs$y
    probs <- predict(probs, type = 'response')
  }
  x <- probs
  y <- class
  x1 = x[y == 1]
  n1 = length(x1)
  x2 = x[y == 0]
  n2 = length(x2)
  r = rank(c(x1, x2))
  auc = (sum(r[1:n1]) - n1 * (n1 + 1) / 2) / n1 / n2
  return(auc)
}

#' Compute ROC Statistics (TPR/FPR at thresholds)
#'
#' @param probs a vector of probabilities or scores
#' @param class a vector denoting class membership (binary)
#' @return data.frame with threshold, tpr, fpr
#' @export
fastROC <- function(probs, class) {
  p_ord <- order(probs, decreasing = F)
  probs_sorted <- probs[p_ord]
  cp <- c(
    -Inf,
    rowMeans(cbind(unique(probs_sorted)[-1], head(unique(probs_sorted), -1))),
    Inf
  )
  class_sorted <- class[p_ord]
  TPR = sapply(cp, function(x) {
    sum(class_sorted[probs_sorted > x]) / sum(class_sorted)
  })
  FPR = sapply(cp, function(x) {
    sum(1 - class_sorted[probs_sorted > x]) / sum(class_sorted == 0)
  })
  return(data.frame(threshold = cp, tpr = TPR, fpr = FPR))
}

#' Extract ROC Curve Data from Model or Predictions
#'
#' @param fitted_obj A glm object or a two-column data frame (predictions, observations)
#' @param digits Number of digits for AUC rounding
#' @return list with data (x, y, Youden, cutoff) and auc
#' @export
get_roc_data <- function(fitted_obj, digits = 2) {
  if (inherits(fitted_obj, 'glm')) {
    class_p <- predict(fitted_obj, type = 'response')
    obs = fitted_obj$y
    roc_obj <- fastROC(class_p, obs)
    auc <- fastAUC(class_p, obs)
  } else {
    roc_obj <- fastROC(fitted_obj[, 1], fitted_obj[, 2])
    auc <- fastAUC(fitted_obj[, 1], fitted_obj[, 2])
  }
  auc <- niceNum(auc, digits)
  df = data.frame(
    x = roc_obj$fpr,
    y = roc_obj$tpr,
    Youden = roc_obj$tpr - roc_obj$fpr,
    cutoff = roc_obj$threshold
  )
  return(list(data = df, auc = auc))
}

#' ROC Curve Plot
#'
#' @param fitted_obj glm object, two-column data frame, or list of glm objects
#' @param showAUC show AUC on plot
#' @param showCut show cut point
#' @param plotOnly return plot only (TRUE) or list with plot and AUC
#' @param title plot title
#' @param digits digits for AUC
#' @param model_names names for multiple ROC curves
#' @param fsize font size
#' @param lsize line size
#' @import ggplot2
#' @export
gg_roc <- function(
  fitted_obj,
  showAUC = T,
  showCut = F,
  plotOnly = T,
  title = NULL,
  digits = 2,
  model_names = NULL,
  fsize = 10,
  lsize = .8
) {
  if (inherits(fitted_obj, 'list')) {
    if (!is.null(model_names)) {
      if (length(model_names) != length(fitted_obj)) {
        stop('model_names and fitted_objects must have the same length')
      }
      names(fitted_obj) <- model_names
    }
    if (is.null(names(fitted_obj))) {
      names(fitted_obj) <- paste("Model", 1:length(fitted_obj))
    }
    obj_auc <- unlist(lapply(fitted_obj, function(x) fastAUC(x)))
    obj_ord <- order(obj_auc, decreasing = T)
    fitted_obj <- fitted_obj[obj_ord]
    roc_objects <- lapply(fitted_obj, function(x) get_roc_data(x, digits))
    roc_data <- lapply(seq_along(roc_objects), function(x) {
      z = roc_objects[[x]]$data
      z$model = names(roc_objects)[x]
      return(z)
    })
    df <- dplyr::bind_rows(roc_data) |> dplyr::arrange(x, y)
    model_lbl <- data.frame(
      model_nm = nicename(names(obj_auc)),
      model = names(obj_auc),
      auc = obj_auc
    ) |>
      dplyr::mutate(
        newName = paste0(nicename(model), " (AUC=", rnd(auc, digits), ")")
      )
    df <- dplyr::full_join(df, model_lbl)
    if (showAUC) {
      df$model <- df$newName
    } else {
      df$model <- df$model_nm
    }
    df <- df |>
      dplyr::mutate(model = forcats::fct_reorder(model, auc, .desc = T))
    p <- ggplot2::ggplot(
      data = df,
      ggplot2::aes(x = x, y = y, colour = model, linetype = model)
    )
    auc_txt <- NULL
  } else {
    roc_objects <- get_roc_data(fitted_obj, digits)
    cutpoint = roc_objects$data$cutoff[which.max(roc_objects$data$Youden)]
    df <- roc_objects$data |> dplyr::arrange(x, y)
    p = ggplot2::ggplot(data = df, ggplot2::aes(x = x, y = y))
    auc_txt <- paste("AUC =", roc_objects$auc)
  }
  p <- p +
    ggplot2::geom_line(size = lsize) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      text = ggplot2::element_text(size = fsize)
    ) +
    ggplot2::xlab("1-Specificity (FPR)") +
    ggplot2::ylab("Sensitivity (TPR)")
  if (showAUC) {
    if (!is.null(auc_txt)) {
      p <- p +
        ggplot2::annotate(
          geom = 'text',
          x = 1,
          y = .1,
          label = auc_txt,
          hjust = 'right'
        )
    } else {
      p <- p +
        ggplot2::theme(
          legend.position = c(.95, 0),
          legend.text = ggplot2::element_text(size = fsize),
          legend.justification = c("right", "bottom"),
          legend.box.just = "right",
          legend.margin = ggplot2::margin(2, 2, 2, 2),
          legend.background = ggplot2::element_rect(
            fill = "transparent",
            color = NA
          )
        )
    }
  }
  if (showCut & !inherits(fitted_obj, 'list')) {
    p = p +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = .5,
          y = .5,
          xend = x[which.max(Youden)],
          yend = y[which.max(Youden)]
        ),
        arrow = arrow(length = unit(0.03, "npc"))
      )
    p = p +
      ggplot2::annotate(
        geom = 'text',
        x = .5,
        y = .5,
        label = paste('cutpoint =', round(cutpoint, 2)),
        hjust = 0,
        vjust = 1
      )
  }
  if (!is.null(title)) {
    p = p +
      ggplot2::ggtitle(title) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 11))
  }
  if (plotOnly) return(p) else return(list(plot = p, auc = auc))
}


# Model Performance

#' Brier Score
#' @param probs predicted probabilities
#' @param class observed classes (0/1)
#' @export
BrierScore <- function(probs, class) {
  p_o <- na.omit(cbind(probs, class))
  n <- nrow(p_o)
  sum((p_o[, 1] - p_o[, 2])^2) / n
}

#' GLM Performance Statistics
#'
#' Computes classification metrics, AUC, Brier score, and optimism-adjusted AUC.
#' Supports Clopper-Pearson or bootstrap confidence intervals.
#'
#' @param glm_fit A fitted glm object with family = binomial
#' @param obs_pred A two-column data frame of observed and predicted values
#' @param newdata Optional new data for out-of-sample evaluation
#' @param cutoff Probability threshold (NULL for Youden-optimal)
#' @param stats "default", "classification", "all", or character vector of stat names
#' @param digits Number of decimal places
#' @param show_ci Compute confidence intervals
#' @param ci_method "clopper-pearson" or "bootstrap"
#' @param ci_width Confidence level (default 0.95)
#' @param bs Bootstrap replicates (default 1000)
#' @param for_plot If TRUE, returns ci_lower/ci_upper as separate columns
#' @export
glm_performance <- function(
  glm_fit = NULL,
  obs_pred = NULL,
  newdata = NULL,
  cutoff = NULL,
  stats = "default",
  digits = 2,
  show_ci = TRUE,
  ci_method = c("clopper-pearson", "bootstrap"),
  ci_width = 0.95,
  bs = 1000,
  for_plot = FALSE
) {
  ci_method <- match.arg(ci_method)
  alpha <- 1 - ci_width

  if (is.null(glm_fit) && is.null(obs_pred)) {
    stop("Please provide either glm_fit or obs_pred.")
  }
  if (!is.null(glm_fit) && !is.null(obs_pred)) {
    stop("Please provide either glm_fit or obs_pred, not both.")
  }

  has_model <- !is.null(glm_fit)
  has_newdata <- !is.null(newdata)

  # Resolve stats argument
  stat_presets <- list(
    default = c(
      "N",
      "Prevalence",
      "AUC",
      "Optimism-Adjusted AUC",
      "Brier Score"
    ),
    classification = c(
      "N",
      "Prevalence",
      "Accuracy",
      "AUC",
      "Optimism-Adjusted AUC",
      "Brier Score",
      "Sensitivity",
      "Specificity",
      "PPV",
      "NPV"
    ),
    all = c(
      "N",
      "Prevalence",
      "Accuracy",
      "AUC",
      "Optimism-Adjusted AUC",
      "Brier Score",
      "Cut-Point",
      "Youden Index",
      "Sensitivity",
      "Specificity",
      "PPV",
      "NPV"
    )
  )
  valid_stats <- stat_presets[["all"]]
  classification_stats <- c(
    "Sensitivity",
    "Specificity",
    "PPV",
    "NPV",
    "Accuracy"
  )

  if (length(stats) == 1 && stats %in% names(stat_presets)) {
    requested_stats <- stat_presets[[stats]]
    if (!is.null(newdata) || !is.null(cutoff)) {
      requested_stats <- union(
        requested_stats,
        stat_presets[["classification"]]
      )
    }
    if (has_newdata || !has_model) {
      requested_stats <- setdiff(requested_stats, "Optimism-Adjusted AUC")
    }
  } else {
    invalid <- setdiff(stats, valid_stats)
    if (length(invalid) > 0) {
      stop(
        "Invalid statistic name(s): ",
        paste(invalid, collapse = ", "),
        ". Valid options are: ",
        paste(valid_stats, collapse = ", ")
      )
    }
    requested_stats <- stats
  }

  needs_classification <- any(classification_stats %in% requested_stats)
  needs_youden <- "Youden Index" %in% requested_stats
  needs_cutpoint <- "Cut-Point" %in% requested_stats
  needs_cutpoint_calc <- needs_classification || needs_youden || needs_cutpoint
  needs_optimism <- "Optimism-Adjusted AUC" %in%
    requested_stats &&
    has_model &&
    !has_newdata

  # Extract obs, scores, probs
  if (has_model) {
    if (!inherits(glm_fit, "glm")) {
      stop("glm_fit must be a glm object.")
    }
    if (glm_fit$family$family != "binomial") {
      stop("glm_fit must be a binomial glm.")
    }
    if (has_newdata) {
      dv <- names(glm_fit$model)[1]
      obs <- newdata[[dv]]
      scores <- stats::predict(glm_fit, newdata = newdata)
      probs <- stats::predict(glm_fit, newdata = newdata, type = "response")
    } else {
      obs <- glm_fit$y
      scores <- stats::predict(glm_fit)
      probs <- stats::predict(glm_fit, type = "response")
    }
    is_miss <- is.na(obs) | is.na(scores) | is.na(probs)
    obs <- obs[!is_miss]
    scores <- scores[!is_miss]
    probs <- probs[!is_miss]
    m_auc <- fastAUC(scores, obs)
    brier <- sum((probs - obs)^2) / length(obs)
    poscat <- as.character(unique(glm_fit$model[, 1][glm_fit$y == 1]))
  } else {
    obs_pred <- stats::na.omit(obs_pred)
    obs <- obs_pred[[1]]
    pred_vals <- obs_pred[[2]]
    if (is.factor(obs)) {
      poscat <- levels(obs)[2]
      obs <- as.numeric(obs) - 1
    } else {
      poscat <- as.character(sort(unique(obs))[2])
    }
    if (all(sort(unique(pred_vals)) %in% c(0, 1))) {
      scores <- NULL
      m_auc <- NA
      brier <- NA
    } else {
      scores <- pred_vals
      probs <- pred_vals
      m_auc <- fastAUC(scores, obs)
      brier <- sum((probs - obs)^2) / length(obs)
    }
  }
  n <- length(obs)

  # Cutpoint
  if (needs_cutpoint_calc) {
    if (is.null(cutoff)) {
      if (!is.null(scores)) {
        roc_out <- fastROC(probs = scores, class = obs)
        optimal_cp <- roc_out$threshold[which.max(roc_out$tpr - roc_out$fpr)]
      } else {
        optimal_cp <- 0.5
      }
    } else {
      optimal_cp <- cutoff
    }
    pred_class <- if (!is.null(scores)) {
      ifelse(scores < optimal_cp, 0, 1)
    } else if (!has_model) {
      pred_vals
    }
  }

  # Classification metrics helper
  compute_classification <- function() {
    a <- sum(obs == 1 & pred_class == 1)
    b <- sum(obs == 0 & pred_class == 1)
    cc <- sum(obs == 1 & pred_class == 0)
    d <- sum(obs == 0 & pred_class == 0)
    metrics <- data.frame(
      statistic = c("Sensitivity", "Specificity", "PPV", "NPV", "Accuracy"),
      x = c(a, d, a, d, a + d),
      nn = c(a + cc, b + d, a + b, d + cc, n),
      stringsAsFactors = FALSE
    )
    metrics$value <- metrics$x / metrics$nn
    if (ci_method == "clopper-pearson") {
      cis <- t(mapply(
        function(x, nn) {
          if (nn > 0) {
            stats::binom.test(x, nn, conf.level = ci_width)$conf.int
          } else {
            c(NA, NA)
          }
        },
        metrics$x,
        metrics$nn
      ))
    } else {
      boot_mat <- replicate(bs, {
        idx <- sample(n, replace = TRUE)
        bo <- obs[idx]
        bp <- pred_class[idx]
        a <- sum(bo == 1 & bp == 1)
        b <- sum(bo == 0 & bp == 1)
        cc <- sum(bo == 1 & bp == 0)
        d <- sum(bo == 0 & bp == 0)
        c(
          a / (a + cc),
          d / (b + d),
          a / (a + b),
          d / (d + cc),
          (a + d) / (a + b + cc + d)
        )
      })
      cis <- cbind(
        apply(boot_mat, 1, stats::quantile, probs = alpha / 2, na.rm = TRUE),
        apply(boot_mat, 1, stats::quantile, probs = 1 - alpha / 2, na.rm = TRUE)
      )
    }
    metrics$ci_lower <- cis[, 1]
    metrics$ci_upper <- cis[, 2]
    metrics[, c("statistic", "value", "ci_lower", "ci_upper")]
  }

  compute_auc_ci <- function() {
    boot_auc <- replicate(bs, {
      idx <- sample(n, replace = TRUE)
      fastAUC(scores[idx], obs[idx])
    })
    stats::quantile(boot_auc, probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
  }

  # Build rows
  rows <- list()
  class_df <- NULL
  if ("N" %in% requested_stats) {
    rows <- c(
      rows,
      list(data.frame(statistic = "N", value = n, ci_lower = NA, ci_upper = NA))
    )
  }
  if ("Prevalence" %in% requested_stats) {
    prev_x <- sum(obs == 1)
    prev_ci <- stats::binom.test(prev_x, n, conf.level = ci_width)$conf.int
    rows <- c(
      rows,
      list(data.frame(
        statistic = "Prevalence",
        value = prev_x / n,
        ci_lower = prev_ci[1],
        ci_upper = prev_ci[2]
      ))
    )
  }
  if ("AUC" %in% requested_stats && !is.na(m_auc)) {
    auc_ci <- if (show_ci && !is.null(scores)) compute_auc_ci() else c(NA, NA)
    rows <- c(
      rows,
      list(data.frame(
        statistic = "AUC",
        value = m_auc,
        ci_lower = auc_ci[1],
        ci_upper = auc_ci[2]
      ))
    )
  }
  if (needs_optimism) {
    rows <- c(
      rows,
      list(data.frame(
        statistic = "Optimism-Adjusted AUC",
        value = compute_optimism_auc(glm_fit, bs),
        ci_lower = NA,
        ci_upper = NA
      ))
    )
  }
  if ("Brier Score" %in% requested_stats && !is.na(brier)) {
    rows <- c(
      rows,
      list(data.frame(
        statistic = "Brier Score",
        value = brier,
        ci_lower = NA,
        ci_upper = NA
      ))
    )
  }
  if (needs_cutpoint) {
    rows <- c(
      rows,
      list(data.frame(
        statistic = "Cut-Point",
        value = optimal_cp,
        ci_lower = NA,
        ci_upper = NA
      ))
    )
  }
  if (needs_youden || needs_classification) {
    class_df <- compute_classification()
  }
  if (needs_youden) {
    youden <- class_df$value[class_df$statistic == "Sensitivity"] +
      class_df$value[class_df$statistic == "Specificity"] -
      1
    rows <- c(
      rows,
      list(data.frame(
        statistic = "Youden Index",
        value = youden,
        ci_lower = NA,
        ci_upper = NA
      ))
    )
  }
  if (needs_classification) {
    rows <- c(
      rows,
      list(class_df[class_df$statistic %in% requested_stats, , drop = FALSE])
    )
  }

  # Assemble and format
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out <- out[
    match(intersect(requested_stats, out$statistic), out$statistic),
    ,
    drop = FALSE
  ]
  rownames(out) <- NULL
  if (interactive()) {
    message("Positive category: ", poscat)
  }
  out$value <- ifelse(
    out$statistic == "N",
    as.character(as.integer(out$value)),
    format(round(out$value, digits), nsmall = digits, trim = TRUE)
  )
  if (for_plot) {
    out$ci_lower <- round(as.numeric(out$ci_lower), digits)
    out$ci_upper <- round(as.numeric(out$ci_upper), digits)
  } else {
    out$ci <- ifelse(
      is.na(out$ci_lower),
      NA,
      paste0(
        "(",
        format(round(out$ci_lower, digits), nsmall = digits, trim = TRUE),
        ", ",
        format(round(out$ci_upper, digits), nsmall = digits, trim = TRUE),
        ")"
      )
    )
    out$ci_lower <- NULL
    out$ci_upper <- NULL
  }

  # Attributes
  ci_desc <- if (ci_method == "clopper-pearson") {
    "Exact binomial (Clopper-Pearson) confidence intervals"
  } else {
    paste0("Bootstrap confidence intervals (", bs, " replicates)")
  }
  cp_desc <- if (needs_cutpoint_calc) {
    paste0(
      "Classification metrics were computed at a ",
      if (is.null(cutoff)) "Youden-optimal" else "user-specified",
      " probability threshold of ",
      format(round(optimal_cp, digits), nsmall = digits),
      ". "
    )
  } else {
    ""
  }
  opt_desc <- if (needs_optimism) {
    paste0(
      "Optimism-adjusted AUC was estimated using Efron's enhanced bootstrap (",
      bs,
      " replicates) following Harrell (2015). "
    )
  } else {
    ""
  }
  attr(out, "method") <- paste0(
    "Model performance statistics for a binomial GLM. ",
    "The positive category was defined as '",
    poscat,
    "'. ",
    cp_desc,
    ci_desc,
    " were computed at the ",
    ci_width * 100,
    "% level. ",
    opt_desc,
    "AUC confidence intervals were obtained via ",
    bs,
    " bootstrap replicates."
  )
  attr(out, "biblio") <- c(
    clopper_pearson = "Clopper, C. J. & Pearson, E. S. (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. Biometrika, 26, 404-413.",
    harrell = "Harrell, F. E. (2015). Regression Modeling Strategies (2nd ed.). Springer."
  )
  if (has_model) {
    attr(out, "model") <- deparse(glm_fit$call)
  }
  return(out)
}

#' Optimism-adjusted AUC helper
#'
#' Computes the standard Efron optimism correction, plus the .632 and .632+
#' bootstrap estimates of AUC. The no-information rate is estimated from the
#' marginal frequencies of the observed outcome and predicted classes
#' (Efron & Tibshirani 1997).
#'
#' @param glm_fit A fitted glm object (family = binomial).
#' @param bs Number of bootstrap resamples.
#' @param return_optimism If TRUE, returns a detailed list instead of a scalar.
#' @return A scalar (optimism-corrected AUC) or, when return_optimism = TRUE,
#'   a list with apparent_auc, optimism, corrected_auc, auc_632, and
#'   auc_632plus.
#' @keywords internal
compute_optimism_auc <- function(glm_fit, bs, return_optimism = FALSE) {
  model_data <- glm_fit$model
  n <- nrow(model_data)
  apparent_auc <- fastAUC(stats::predict(glm_fit), glm_fit$y)

  # Storage for per-bootstrap quantities
  bs_auc_vec <- numeric(bs)
  orig_auc_vec <- numeric(bs)
  oob_auc_vec <- numeric(bs)

  for (b in seq_len(bs)) {
    idx <- sample(n, replace = TRUE)
    bs_data <- model_data[idx, ]
    bs_fit <- stats::update(glm_fit, . ~ ., data = bs_data)

    # AUC on bootstrap sample (resubstitution)
    bs_auc_vec[b] <- fastAUC(stats::predict(bs_fit), bs_fit$y)

    # AUC of bootstrap model on original data (for optimism)
    orig_auc_vec[b] <- fastAUC(
      stats::predict(bs_fit, newdata = model_data),
      glm_fit$y
    )

    # AUC on out-of-bag observations (for .632 / .632+)
    oob <- setdiff(seq_len(n), unique(idx))
    if (length(oob) > 1 && length(unique(glm_fit$y[oob])) == 2) {
      oob_auc_vec[b] <- fastAUC(
        stats::predict(bs_fit, newdata = model_data[oob, ]),
        glm_fit$y[oob]
      )
    } else {
      oob_auc_vec[b] <- NA
    }
  }

  # Standard optimism correction (Harrell)
  optimism <- mean(bs_auc_vec - orig_auc_vec)
  corrected_auc <- apparent_auc - optimism

  # .632 bootstrap (Efron 1983)
  mean_oob <- mean(oob_auc_vec, na.rm = TRUE)
  auc_632 <- 0.368 * apparent_auc + 0.632 * mean_oob

  # .632+ bootstrap (Efron & Tibshirani 1997)
  # No-information rate: AUC expected if predictions and outcomes are independent
  y <- glm_fit$y
  p_hat <- stats::predict(glm_fit, type = "response")
  gamma <- fastAUC(sample(p_hat), sample(y)) # permutation estimate

  # Relative overfitting rate
  R_hat <- ifelse(
    apparent_auc == mean_oob,
    0,
    (apparent_auc - mean_oob) / (apparent_auc - gamma)
  )
  R_hat <- max(0, min(1, R_hat)) # clamp to [0, 1]

  w <- 0.632 / (1 - 0.368 * R_hat)
  auc_632plus <- (1 - w) * apparent_auc + w * mean_oob

  if (return_optimism) {
    return(list(
      apparent_auc = apparent_auc,
      optimism = optimism,
      corrected_auc = corrected_auc,
      auc_632 = auc_632,
      auc_632plus = auc_632plus
    ))
  }
  corrected_auc
}

#' Efron's Optimism-Adjusted AUC
#'
#' Computes the apparent AUC along with three bootstrap-corrected estimates:
#' (1) standard optimism correction (Harrell), (2) .632 bootstrap (Efron 1983),
#' and (3) .632+ bootstrap (Efron & Tibshirani 1997).
#'
#' @param model output from glm with family=binomial
#' @param bs number of bootstrap samples
#' @return tibble with AUC, optimism, and adjusted AUC estimates
#' @export
efron_auc <- function(model, bs = 1000) {
  if (!inherits(model, "glm")) {
    stop("This function has been written for glm models only")
  }
  adj_auc <- compute_optimism_auc(model, bs, return_optimism = TRUE)
  corrected_auc <- tibble::tribble(
    ~Description                     , ~Value                           ,
    "Apparent AUC"                   , rnd(adj_auc$apparent_auc, 2)     ,
    "Bootstrapped Optimism Estimate" , rnd(max(0, adj_auc$optimism), 2) ,
    "Optimism-Adjusted AUC"          , rnd(adj_auc$corrected_auc, 2)    ,
    ".632 Bootstrap AUC"             , rnd(adj_auc$auc_632, 2)          ,
    ".632+ Bootstrap AUC"            , rnd(adj_auc$auc_632plus, 2)
  )

  method <- paste(
    "The area under the receiver operating characteristic curve (AUC) was used",
    "to quantify the model's ability to discriminate between positive and",
    "negative cases, where an AUC of 1.0 indicates perfect discrimination and",
    "0.5 indicates performance no better than chance. Because a model's",
    "performance on the same data used to build it tends to be",
    "over-optimistic, we applied three bootstrap correction methods using",
    paste0(bs, " resamples."),
    "\n\n",
    "First, the standard optimism correction [@Harrell2001] was used.",
    "In each bootstrap resample, a new model was fit and its AUC was computed",
    "both on the bootstrap sample and on the original data. The difference",
    "between these two values estimates how much the model's accuracy is",
    "inflated by overfitting. The average of this difference across all",
    "resamples (the 'optimism') was subtracted from the original AUC to",
    "produce the optimism-adjusted AUC.",
    "\n\n",
    "Second, the .632 bootstrap method [@Efron1983] was applied.",
    "Rather than testing each bootstrap model on the full original sample,",
    "this method evaluates each bootstrap model only on observations that",
    "were left out of that particular resample (out-of-bag observations).",
    "Since roughly 63.2% of observations appear in any given bootstrap",
    "sample, the remaining 36.8% provide a genuinely independent test set.",
    "The .632 estimate is a weighted average of the original AUC (weight",
    "0.368) and the average out-of-bag AUC (weight 0.632).",
    "\n\n",
    "Third, the .632+ bootstrap [@Efron1997] was applied as a refinement.",
    "The .632 method can still be slightly optimistic when the model is",
    "heavily overfitting, or overly pessimistic when the model is genuinely",
    "strong. The .632+ method addresses this by estimating how much the",
    "model has overfit relative to a 'no-information' baseline (i.e. the AUC",
    "expected if predictions and outcomes were unrelated). When overfitting",
    "is severe, the .632+ estimate shifts more weight toward the out-of-bag",
    "performance; when overfitting is minimal, it stays close to the .632",
    "estimate."
  )

  bibEntry <- list(
    "@Book{Harrell2001,
  Author = {Harrell, Frank E.},
  Title = {Regression Modeling Strategies: With Applications to Linear Models, Logistic Regression, and Survival Analysis},
  Year = {2001},
  Publisher = {Springer}
}",
    "@Article{Efron1983,
  Author = {Efron, Bradley},
  Title = {Estimating the error rate of a prediction rule: improvement on cross-validation},
  Journal = {Journal of the American Statistical Association},
  Year = {1983},
  Volume = {78},
  Number = {382},
  Pages = {316--331}
}",
    "@Article{Efron1997,
  Author = {Efron, Bradley and Tibshirani, Robert},
  Title = {Improvements on cross-validation: The .632+ bootstrap method},
  Journal = {Journal of the American Statistical Association},
  Year = {1997},
  Volume = {92},
  Number = {438},
  Pages = {548--560}
}"
  )

  return(list(
    corrected_auc = corrected_auc,
    method = method,
    bibEntry = bibEntry
  ))
}


# Model Diagnostics

#' Check Model Assumptions using DHARMa
#'
#' Supports single models and mice/mira objects.
#'
#' @param model A fitted model object or mira object
#' @param nsim Number of simulations
#' @param plot Logical, plot residuals
#' @export
check_model_assumptions <- function(model, nsim = 1000, plot = FALSE) {
  if (!requireNamespace("DHARMa", quietly = TRUE)) {
    stop(
      "The DHARMa package is required. Install with install.packages('DHARMa')"
    )
  }

  if (inherits(model, "mira")) {
    all_results <- list()
    for (i in seq_along(model$analyses)) {
      current_model <- model$analyses[[i]]
      sim_residuals <- DHARMa::simulateResiduals(current_model, n = nsim)
      tests_output <- list(
        uniformity = DHARMa::testUniformity(sim_residuals, plot = FALSE),
        dispersion = DHARMa::testDispersion(sim_residuals, plot = FALSE),
        outliers = DHARMa::testOutliers(sim_residuals, plot = FALSE)
      )
      results_df <- dplyr::bind_rows(
        lapply(tests_output, function(ht) htest_dataframe(ht)),
        .id = "Test"
      )
      results_df$imputation <- i
      all_results[[i]] <- results_df
    }
    combined_results <- dplyr::bind_rows(all_results)
    out <- combined_results |>
      dplyr::group_by(Test) |>
      dplyr::summarise(
        mean_statistic = mean(statistic, na.rm = TRUE),
        mean_p_value = mean(p.value, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        significant = mean_p_value < 0.05,
        interpretation = dplyr::case_when(
          Test == "uniformity" &
            significant ~ "Non-uniform residuals (model misspecification)",
          Test == "uniformity" & !significant ~ "Uniform residuals (good)",
          Test == "dispersion" & significant ~ "Over/under-dispersion detected",
          Test == "dispersion" & !significant ~ "No dispersion issues",
          Test == "outliers" & significant ~ "Outliers detected",
          Test == "outliers" & !significant ~ "No outlier issues",
          TRUE ~ "Unknown test"
        )
      )
  } else {
    sim_residuals <- DHARMa::simulateResiduals(model, n = nsim)
    tests_output <- list(
      uniformity = DHARMa::testUniformity(sim_residuals, plot = FALSE),
      dispersion = DHARMa::testDispersion(sim_residuals, plot = FALSE),
      outliers = DHARMa::testOutliers(sim_residuals, plot = FALSE)
    )
    out <- dplyr::bind_rows(
      lapply(tests_output, function(ht) htest_dataframe(ht)),
      .id = "Test"
    ) |>
      dplyr::transmute(
        Test,
        statistic,
        p.value,
        significant = p.value < 0.05,
        interpretation = dplyr::case_when(
          Test == "uniformity" &
            significant ~ "Non-uniform residuals (model misspecification)",
          Test == "uniformity" & !significant ~ "Uniform residuals (good)",
          Test == "dispersion" & significant ~ "Over/under-dispersion detected",
          Test == "dispersion" & !significant ~ "No dispersion issues",
          Test == "outliers" & significant ~ "Outliers detected",
          Test == "outliers" & !significant ~ "No outlier issues",
          TRUE ~ "Unknown test"
        )
      )
  }
  if (plot) {
    plot(sim_residuals)
  }
  return(out)
}

# Functions to Compare Predictive Models

#' Calculate Net Reclassification Improvement (NRI)
#'
#' Computes the categorical NRI to compare the predictive performance of two
#' models. NRI quantifies the improvement in reclassification from model 1 to
#' model 2. This function handles both probability inputs and categorical risk
#' group inputs.
#'
#' @param data Data frame containing the variables (optional if vectors provided
#'   directly)
#' @param obs_events Numeric vector of observed events (0 = non-event, 1 =
#'   event) or column name if data is provided
#' @param model1_pred Numeric vector of model 1 predictions. Can be:
#'                    - Predicted probabilities (values between 0 and 1)
#'                    - Risk categories (integer values like 1, 2, 3)
#'   Or column name if data is provided
#' @param model2_pred Numeric vector of model 2 predictions (same format as
#'   model1_pred) Or column name if data is provided
#' @param conf_level Numeric value for confidence interval level (default =
#'   0.95)
#' @param n_bootstrap Integer number of bootstrap samples for CI calculation
#'   (default = 1000, only used when method = "bootstrap")
#' @param method Character string specifying the method for p-value calculation:
#'   "asymptotic" (default) or "bootstrap"
#' @param format_cols Logical; if TRUE (default) rounds numeric output columns
#'
#' @return A list of class "nri_result" containing:
#'   \item{nri_summary}{Data frame with NRI statistics for overall, events, and non-events}
#'   \item{reclassification_table}{Overall cross-tabulation of risk categories}
#'   \item{reclassification_table_events}{Cross-tabulation for events only}
#'   \item{reclassification_table_nonevents}{Cross-tabulation for non-events only}
#'   \item{method}{Method used for inference}
#'   \item{conf_level}{Confidence level used}
#'
#' @examples
#' # Example from Pencina et al. (2008)
#' df <- data.frame(
#'   obs_events   = c(rep(1, 183), rep(0, 3081)),
#'   m1_prediction = c(rep(1, 54), rep(2, 105), rep(3, 24),
#'                     rep(1, 2101), rep(2, 882), rep(3, 98)),
#'   m2_prediction = c(rep(1, 43), rep(2, 105), rep(3, 35),
#'                     rep(1, 2108), rep(2, 870), rep(3, 103))
#' )
#'
#' result <- df |>
#'   calculate_nri(obs_events, m1_prediction, m2_prediction)
#'
#' result$nri_summary
#'
#' @references Pencina, M. J., D'Agostino Sr, R. B., D'Agostino Jr, R. B., &
#'   Vasan, R. S. (2008). Evaluating the added predictive ability of a new
#'   marker: from area under the ROC curve to reclassification and beyond.
#'   Statistics in Medicine, 27(2), 157-172.
#'
#' @export
calculate_nri <- function(
  data = NULL,
  obs_events,
  model1_pred,
  model2_pred,
  conf_level = 0.95,
  n_bootstrap = 1000,
  method = "asymptotic",
  format_cols = TRUE
) {
  # Validate method argument early, before any computation
  method <- match.arg(method, choices = c("asymptotic", "bootstrap"))
  if (method == "bootstrap") {
    stop("Bootstrap method is not yet implemented. Use method = 'asymptotic'.")
  }

  # Extract vectors from data frame if piped, otherwise use directly
  if (!is.null(data)) {
    obs_events_vec <- eval(substitute(obs_events), data)
    model1_pred_vec <- eval(substitute(model1_pred), data)
    model2_pred_vec <- eval(substitute(model2_pred), data)
  } else {
    obs_events_vec <- obs_events
    model1_pred_vec <- model1_pred
    model2_pred_vec <- model2_pred
  }

  # Convert two-level factor to 0/1, capturing levels before conversion
  if (is.factor(obs_events_vec)) {
    if (nlevels(obs_events_vec) != 2) {
      stop("obs_events factor must have exactly 2 levels.")
    }
    orig_levels <- levels(obs_events_vec)
    obs_events_vec <- as.numeric(obs_events_vec) - 1
    message(sprintf(
      "Converted factor to numeric: '%s' = 0 (non-event), '%s' = 1 (event).",
      orig_levels[1],
      orig_levels[2]
    ))
  }

  # Validate obs_events values
  if (!all(obs_events_vec %in% c(0, 1))) {
    stop(
      "obs_events must contain only 0 (non-event) or 1 (event), or be a two-level factor."
    )
  }

  # Check for missing values before any computation
  if (
    anyNA(obs_events_vec) || anyNA(model1_pred_vec) || anyNA(model2_pred_vec)
  ) {
    stop("Input vectors must not contain NA values.")
  }

  # Validate equal lengths
  if (
    length(obs_events_vec) != length(model1_pred_vec) ||
      length(obs_events_vec) != length(model2_pred_vec)
  ) {
    stop("All input vectors must have the same length.")
  }

  # Detect categorical vs probability inputs using floor comparison (float-safe)
  is_integer_valued <- function(x) all(x == floor(x))
  is_categorical <- is_integer_valued(model1_pred_vec) &&
    is_integer_valued(model2_pred_vec) &&
    min(model1_pred_vec) >= 1 &&
    min(model2_pred_vec) >= 1

  if (!is_categorical) {
    if (
      any(model1_pred_vec < 0 | model1_pred_vec > 1) ||
        any(model2_pred_vec < 0 | model2_pred_vec > 1)
    ) {
      stop(
        "Predicted probabilities must be between 0 and 1, or integer risk categories >= 1."
      )
    }
  }

  # Helper to count up/same/down reclassifications
  count_reclass <- function(cat1, cat2) {
    list(
      n_up = sum(cat2 > cat1),
      n_same = sum(cat2 == cat1),
      n_down = sum(cat2 < cat1)
    )
  }

  # Identify event and non-event indices
  events_idx <- which(obs_events_vec == 1)
  nonevents_idx <- which(obs_events_vec == 0)

  n_events <- length(events_idx)
  n_nonevents <- length(nonevents_idx)
  n_total <- n_events + n_nonevents

  # Count reclassifications for events and non-events
  ev <- count_reclass(model1_pred_vec[events_idx], model2_pred_vec[events_idx])
  nev <- count_reclass(
    model1_pred_vec[nonevents_idx],
    model2_pred_vec[nonevents_idx]
  )

  # Build reclassification tables for categorical inputs
  if (is_categorical) {
    all_levels <- sort(unique(c(model1_pred_vec, model2_pred_vec)))

    make_table <- function(idx) {
      table(
        Model1 = factor(model1_pred_vec[idx], levels = all_levels),
        Model2 = factor(model2_pred_vec[idx], levels = all_levels)
      )
    }

    reclass_table_all <- make_table(seq_along(obs_events_vec))
    reclass_table_events <- if (n_events > 0) make_table(events_idx) else NULL
    reclass_table_nonevents <- if (n_nonevents > 0) {
      make_table(nonevents_idx)
    } else {
      NULL
    }
  } else {
    reclass_table_all <- NULL
    reclass_table_events <- NULL
    reclass_table_nonevents <- NULL
  }

  # Calculate proportions
  p_up_events <- ev$n_up / n_events
  p_down_events <- ev$n_down / n_events
  p_up_nonevents <- nev$n_up / n_nonevents
  p_down_nonevents <- nev$n_down / n_nonevents

  # Calculate NRI components and standard errors
  nri_events <- p_up_events - p_down_events
  nri_nonevents <- p_down_nonevents - p_up_nonevents
  nri_overall <- nri_events + nri_nonevents

  se_events <- sqrt((p_up_events + p_down_events) / n_events)
  se_nonevents <- sqrt((p_up_nonevents + p_down_nonevents) / n_nonevents)
  se_overall <- sqrt(se_events^2 + se_nonevents^2)

  # Helper to compute z-stat, p-value, and CI for a given NRI and SE
  z_calc <- function(nri, se) {
    z_stat <- nri / se
    p_value <- 2 * pnorm(-abs(z_stat))
    z_alpha <- qnorm(1 - (1 - conf_level) / 2)
    data.frame(
      nri = nri,
      se = se,
      z = z_stat,
      p_value = p_value,
      ci_lower = nri - z_alpha * se,
      ci_upper = nri + z_alpha * se
    )
  }

  # Assemble summary data frame, binding counts and z-stats explicitly by group
  nri_summary <- rbind(
    cbind(
      data.frame(
        group = "Overall",
        n = n_total,
        n_up = ev$n_up + nev$n_up,
        n_same = ev$n_same + nev$n_same,
        n_down = ev$n_down + nev$n_down
      ),
      z_calc(nri_overall, se_overall)
    ),
    cbind(
      data.frame(
        group = "Events",
        n = n_events,
        n_up = ev$n_up,
        n_same = ev$n_same,
        n_down = ev$n_down
      ),
      z_calc(nri_events, se_events)
    ),
    cbind(
      data.frame(
        group = "Non-events",
        n = n_nonevents,
        n_up = nev$n_up,
        n_same = nev$n_same,
        n_down = nev$n_down
      ),
      z_calc(nri_nonevents, se_nonevents)
    )
  )

  # Round numeric output columns if requested
  if (format_cols) {
    nri_summary[, c("nri", "se", "ci_lower", "ci_upper")] <-
      round(nri_summary[, c("nri", "se", "ci_lower", "ci_upper")], 4)
    nri_summary$z <- round(nri_summary$z, 3)
    nri_summary$p_value <- round(nri_summary$p_value, 4)
  }

  rownames(nri_summary) <- NULL

  # Return structured result
  structure(
    list(
      nri_summary = nri_summary,
      reclassification_table = reclass_table_all,
      reclassification_table_events = reclass_table_events,
      reclassification_table_nonevents = reclass_table_nonevents,
      method = method,
      conf_level = conf_level
    ),
    class = "nri_result"
  )
}


#' Print method for NRI results
#'
#' @param x An object of class "nri_result"
#' @param ... Additional arguments (not used)
#'
#' @export
print.nri_result <- function(x, ...) {
  cat("Net Reclassification Improvement (NRI) Analysis\n")
  cat("================================================\n")
  cat(sprintf(
    "Method: %s | Confidence Level: %.0f%%\n\n",
    x$method,
    x$conf_level * 100
  ))

  # Build display-ready summary using column names that match nri_summary
  s <- x$nri_summary

  display <- data.frame(
    Group = s$group,
    N = s$n,
    NRI = s$nri,
    CI = sprintf("[%.4f, %.4f]", s$ci_lower, s$ci_upper),
    Z = s$z,
    P_value = ifelse(s$p_value < 0.001, "<0.001", sprintf("%.4f", s$p_value))
  )

  cat("NRI Summary:\n")
  print(display, row.names = FALSE)

  # Print reclassification counts
  cat("\nReclassification Counts:\n")
  cat("\n")
  for (i in seq_len(nrow(s))) {
    cat(sprintf(
      "%s:  Up = %d  |  Same = %d  |  Down = %d  (N = %d)\n",
      s$group[i],
      s$n_up[i],
      s$n_same[i],
      s$n_down[i],
      s$n[i]
    ))
  }

  # Print reclassification tables when available (categorical inputs only)
  if (!is.null(x$reclassification_table)) {
    cat(
      "\nReclassification Table — All subjects (Model 1 rows → Model 2 cols):\n"
    )
    print(x$reclassification_table)
  }

  if (!is.null(x$reclassification_table_events)) {
    cat("\nReclassification Table — Events:\n")
    print(x$reclassification_table_events)
  }

  if (!is.null(x$reclassification_table_nonevents)) {
    cat("\nReclassification Table — Non-events:\n")
    print(x$reclassification_table_nonevents)
  }

  invisible(x)
}


# Confusion Matrix -

#' Display a confusion matrix
#' @param glm_fit glm object
#' @param obs_pred two-column data frame (observed, predicted)
#' @param responseLabels character vector of response labels ordered 0,1
#' @export
confMatrix <- function(glm_fit, obs_pred, responseLabels = NULL) {
  if (!xor(missing(glm_fit), missing(obs_pred))) {
    stop('Please specify either glm_fit or obs_pred.')
  }
  if (!missing(glm_fit)) {
    if (inherits(glm_fit, 'glm')) {
      Observed = glm_fit$model[, 1]
      if (length(unique(Observed)) > 2) {
        stop('Only binary response supported.')
      }
      Predicted = ifelse(predict(glm_fit, type = 'response') < .5, 0, 1)
      if ("factor" %in% class(Observed)) {
        Predicted = factor(Predicted, levels = 0:1, labels = levels(Observed))
      }
    }
  } else {
    Observed = factor(obs_pred[[1]])
    Predicted = factor(obs_pred[[2]])
    if (!all(levels(Predicted) %in% levels(Observed))) {
      stop('Predicted levels must be present in observed data.')
    }
    if (length(levels(Observed)) != 2) {
      stop('Only two levels permitted.')
    }
    if (any(is.na(Observed)) | any(is.na(Predicted))) {
      stop('Missing values not permitted.')
    }
    Predicted = factor(Predicted, levels = levels(Observed))
  }
  tab <- as.data.frame.matrix(table(Observed, Predicted))
  names(tab) <- paste('Predicted', names(tab))
  tab <- cbind(Observed = rownames(tab), tab)
  rownames(tab) <- NULL
  return(tab)
}


# Standardised Coefficients

#' Standardised regression coefficients for lm
#' @param lm_fit an lm object
#' @export
lm.beta <- function(lm_fit) {
  if (!inherits(lm_fit, "lm")) {
    stop("lm_fit must be a linear regression model")
  }
  b <- summary(lm_fit)$coef[-1, 1]
  sx <- sapply(lm_fit$model[-1], sd)
  sy <- sd(lm_fit$model[[1]])
  data.frame(beta = b * sx / sy)
}

#' Standardised coefficients for lmer
#' @param mod an lmer object
#' @export
lm.beta.lmer <- function(mod) {
  b <- lme4::fixef(mod)[-1]
  sd.x <- apply(lme4::getME(mod, "X")[, -1], 2, sd)
  sd.y <- sd(lme4::getME(mod, "y"))
  data.frame(beta = b * sd.x / sd.y)
}


# All-Possible-Subsets -

#' Extract model predictor names
#' @param model fitted model
#' @export
getModelVars <- function(model) {
  attr(model$terms, "term.labels")
}

#' Model stats at Youden-optimal cutpoint
#' @keywords internal
modelStats <- function(
  glm_fit,
  obs_pred,
  probThreshold = NULL,
  newdata,
  returnCutPoint = FALSE
) {
  if (!missing(obs_pred)) {
    obs = na.omit(obs_pred)[, 1]
    pred = na.omit(obs_pred)[, 2]
    n <- nrow(na.omit(obs_pred))
    bs <- m_auc <- NA
  } else {
    if (missing(newdata)) {
      obs = glm_fit$y
      scores <- predict(glm_fit)
      probs <- predict(glm_fit, type = 'response')
    } else {
      dv <- names(glm_fit$model)[1]
      obs <- newdata[[dv]]
      scores <- predict(glm_fit, newdata = newdata)
      probs <- predict(glm_fit, newdata = newdata, type = 'response')
    }
    is_miss <- is.na(obs) | is.na(scores) | is.na(probs)
    obs <- obs[!is_miss]
    probs <- probs[!is_miss]
    scores <- scores[!is_miss]
    roc_out <- fastROC(probs = scores, class = obs)
    cp <- if (is.null(probThreshold)) {
      roc_out$threshold[which.max(roc_out$tpr - roc_out$fpr)]
    } else {
      probThreshold
    }
    m_auc <- fastAUC(probs = scores, class = obs)
    pred <- ifelse(scores < cp, 0, 1)
    n <- length(obs)
    bs <- sum((probs - obs)^2) / n
  }
  a = sum(obs == 1 & pred == 1, na.rm = T)
  b = sum(obs == 0 & pred == 1, na.rm = T)
  cc = sum(obs == 1 & pred == 0, na.rm = T)
  d = sum(obs == 0 & pred == 0, na.rm = T)
  rtn = data.frame(
    Prevalence = (a + cc) / (a + b + cc + d),
    Sensitivity = a / (a + cc),
    Specificity = d / (b + d),
    PPV = a / (a + b),
    NPV = d / (d + cc),
    Accuracy = (a + d) / (a + b + cc + d),
    AUC = m_auc,
    BrierScore = bs,
    Youden = a / (a + cc) + d / (b + d) - 1,
    N = n
  )
  if (returnCutPoint) {
    rtn <- cbind(rtn, cutPoint = cp)
  }
  return(rtn)
}

#' GLM summary statistics
#' @param model binomial glm
#' @param simple show compact output
#' @param digits rounding
#' @export
glm_stats <- function(model, simple = TRUE, digits = 2) {
  if (!inherits(model, 'glm')) {
    stop('This is for use with a glm model')
  }
  if (model$family$family != "binomial") {
    stop('This is for use with a binomial glm model')
  }
  ms <- modelStats(glm_fit = model)
  if (simple) {
    ms <- ms |> dplyr::select(N, Prevalence, Accuracy, AUC, BrierScore)
  } else {
    ms <- ms |>
      dplyr::select(
        N,
        Prevalence,
        Accuracy,
        AUC,
        BrierScore,
        dplyr::everything()
      )
  }
  ms_t <- data.frame(Statistic = names(ms), Value = unlist(ms))
  rownames(ms_t) <- NULL
  ms_t$Value <- c(rnd(ms_t$Value[1], 0), rnd(ms_t$Value[-1], digits))
  return(ms_t)
}

#' All possible subsets logistic regression
#' @param data data frame
#' @param dv dependent variable name
#' @param ivs independent variable names
#' @export
aps_lr <- function(data, dv, ivs) {
  k <- unlist(lapply(1:length(ivs), function(n) {
    combn(ivs, n, FUN = function(row) length(row))
  }))
  calls <- unlist(lapply(1:length(ivs), function(n) {
    combn(ivs, n, FUN = function(row) {
      paste0(dv, " ~ ", paste0(row, collapse = '+'))
    })
  }))
  models <- lapply(calls, function(x) {
    glm(formula = x, data = data, family = 'binomial')
  })
  m_stats <- dplyr::bind_rows(lapply(models, function(x) {
    data.frame(
      ivs = paste(names(x$coefficients)[-1], collapse = ","),
      k = length(names(x$coefficients)[-1]),
      N = nrow(x$model),
      AUC = fastAUC(probs = predict(x), class = x$y)
    )
  }))
  return(m_stats)
}

#' All possible subsets - select model maximising AUC
#' @param data data frame
#' @param dv dependent variable name
#' @param ivs independent variable names
#' @param maxk maximum number of predictors
#' @export
aps_AUC <- function(data, dv, ivs, maxk = 9) {
  aps_out <- aps_lr(data, dv, ivs)
  aps_best <- which.max(aps_out$AUC[aps_out$k <= maxk])
  best_model <- glm(
    paste(dv, '~', gsub(",", "+", aps_out$ivs[aps_best])),
    data = data,
    family = binomial
  )
  mstats <- modelStats(best_model)
  Features <- unlist(strsplit(aps_out$ivs[aps_best], ','))
  return(list(Performance = mstats, Features = Features))
}


# Elastic Net Marker Selection -

#' Biomarker Selection via Elastic Net with Optional MI and LOO Validation
#'
#' @param df data frame
#' @param outcome_var binary outcome column name
#' @param candidate_markers character vector of marker names (NULL = all non-outcome)
#' @param forced_var unpenalised variable (NULL for none)
#' @param n_imputations number of imputations (default 5)
#' @param k_mad MAD multiplier for winsorisation (default 4)
#' @param alpha elastic net mixing (default 0.5)
#' @param max_miss maximum missingness proportion (default 0.25)
#' @return glmnet_marker_selection object
#' @export
glmnet_marker_selection <- function(
  df,
  outcome_var,
  candidate_markers = NULL,
  forced_var = NULL,
  n_imputations = 5,
  k_mad = 4,
  alpha = 0.5,
  max_miss = 0.25
) {
  if (is.null(candidate_markers)) {
    candidate_markers <- setdiff(names(df), c(outcome_var, forced_var))
    if (interactive()) {
      message(
        "Using all non-outcome variables as candidates (",
        length(candidate_markers),
        " markers)"
      )
    }
  }

  has_forced_var <- function() {
    !is.null(forced_var) && !is.na(forced_var) && forced_var != ""
  }
  is_binary <- function(x) length(unique(x[!is.na(x)])) == 2

  # Input validation
  if (!outcome_var %in% names(df)) {
    stop("outcome_var '", outcome_var, "' not found in df.")
  }
  missing_markers <- setdiff(candidate_markers, names(df))
  if (length(missing_markers) > 0) {
    stop(
      "The following candidate_markers are not in df: ",
      paste(missing_markers, collapse = ", ")
    )
  }
  if (has_forced_var() && !forced_var %in% names(df)) {
    stop("forced_var '", forced_var, "' not found in df.")
  }
  if (max_miss < 0 || max_miss > 1) {
    stop("max_miss must be between 0 and 1.")
  }

  factor_markers <- candidate_markers[sapply(
    df[, candidate_markers, drop = FALSE],
    is.factor
  )]
  if (length(factor_markers) > 0) {
    stop(
      "Factor candidate markers not supported. Convert to numeric/dummy: ",
      paste(factor_markers, collapse = ", ")
    )
  }

  # Restrict dataset
  keep_vars <- unique(c(outcome_var, forced_var, candidate_markers))
  df <- df[, intersect(keep_vars, names(df)), drop = FALSE]
  df <- df[!is.na(df[[outcome_var]]), , drop = FALSE]
  n_obs <- nrow(df)

  # Coerce outcome
  y_raw <- df[[outcome_var]]
  if (is.factor(y_raw)) {
    poscat <- levels(y_raw)[2]
    df[[outcome_var]] <- as.numeric(y_raw) - 1
  } else {
    poscat <- as.character(sort(unique(y_raw))[2])
    df[[outcome_var]] <- as.numeric(as.character(y_raw))
  }
  unique_outcomes <- unique(df[[outcome_var]])
  if (length(unique_outcomes) != 2 || !all(unique_outcomes %in% c(0, 1))) {
    stop("outcome_var must be binary (0/1).")
  }
  n_events <- sum(df[[outcome_var]] == 1)
  n_nonevents <- sum(df[[outcome_var]] == 0)

  # Exclude high-missingness markers
  miss_props <- sapply(df[, candidate_markers, drop = FALSE], \(x) {
    mean(is.na(x))
  })
  high_miss_markers <- names(miss_props[miss_props > max_miss])
  high_miss_props <- miss_props[high_miss_markers]
  if (length(high_miss_markers) > 0) {
    message(
      "Excluding markers with >",
      max_miss * 100,
      "% missingness: ",
      paste0(high_miss_markers, " (", round(high_miss_props * 100, 1), "%)"),
      collapse = ", "
    )
    candidate_markers <- setdiff(candidate_markers, high_miss_markers)
  }
  if (length(candidate_markers) == 0) {
    stop(
      "No candidate markers remaining after excluding high-missingness columns."
    )
  }

  # Dummy code factor/character candidates
  zero_var <- character(0)
  factor_markers <- candidate_markers[
    sapply(df[, candidate_markers, drop = FALSE], \(x) {
      is.factor(x) || is.character(x)
    })
  ]
  if (length(factor_markers) > 0) {
    for (marker in factor_markers) {
      x <- df[[marker]]
      lvls <- if (is.factor(x)) levels(x) else sort(unique(x[!is.na(x)]))
      if (length(lvls) < 2) {
        zero_var <- c(zero_var, marker)
        candidate_markers <- setdiff(candidate_markers, marker)
        df[[marker]] <- NULL
        next
      }
      for (lvl in lvls[-1]) {
        new_name <- paste0(marker, "_", lvl)
        df[[new_name]] <- as.integer(x == lvl)
        candidate_markers <- c(candidate_markers, new_name)
      }
      df[[marker]] <- NULL
      candidate_markers <- setdiff(candidate_markers, marker)
    }
    if (interactive()) {
      message(
        "Dummy variables created for: ",
        paste(factor_markers, collapse = ", ")
      )
    }
  }

  # Drop zero-variance markers
  marker_vars <- sapply(df[, candidate_markers, drop = FALSE], \(x) {
    stats::var(x, na.rm = TRUE)
  })
  zero_var <- c(
    zero_var,
    names(marker_vars[marker_vars == 0 | is.na(marker_vars)])
  )
  if (length(zero_var) > 0) {
    message(
      "Dropping zero-variance markers: ",
      paste(zero_var, collapse = ", ")
    )
    candidate_markers <- setdiff(candidate_markers, zero_var)
  }
  if (length(candidate_markers) == 0) {
    stop("No candidate markers remaining after dropping zero-variance columns.")
  }

  # Sample size guard
  if (n_obs <= length(candidate_markers)) {
    stop(
      "Sample size (n = ",
      n_obs,
      ") must exceed number of candidate markers (p = ",
      length(candidate_markers),
      ")."
    )
  }
  epv <- min(n_events, n_nonevents) / length(candidate_markers)
  if (epv < 2) {
    message(
      "Very low events-per-variable ratio (",
      round(epv, 1),
      "). Interpret with caution."
    )
  }

  # Identify binary vs continuous
  binary_markers <- candidate_markers[sapply(
    df[, candidate_markers, drop = FALSE],
    is_binary
  )]
  continuous_markers <- setdiff(candidate_markers, binary_markers)
  if (length(binary_markers) > 0 && interactive()) {
    message(
      "Binary markers excluded from transformation/winsorisation: ",
      paste(binary_markers, collapse = ", ")
    )
  }

  # Transform continuous markers
  transformed_markers <- character(0)
  for (marker in continuous_markers) {
    result <- transform_marker(df[[marker]])
    if (result$transform != "none") {
      new_name <- paste0(result$transform, "_", marker)
      df[[new_name]] <- result$values
      df[[marker]] <- NULL
      candidate_markers[candidate_markers == marker] <- new_name
      continuous_markers[continuous_markers == marker] <- new_name
      transformed_markers <- c(transformed_markers, new_name)
    }
  }
  if (length(transformed_markers) > 0 && interactive()) {
    message(
      "Transformed markers: ",
      paste(transformed_markers, collapse = ", ")
    )
  }

  # Winsorise continuous markers
  modified_markers <- character(0)
  for (marker in continuous_markers) {
    x_orig <- df[[marker]]
    x_new <- mad_winsorise(x_orig, k = k_mad)
    if (!identical(x_orig, x_new)) {
      modified_markers <- c(modified_markers, marker)
      df[[marker]] <- x_new
    }
  }
  if (length(modified_markers) > 0 && interactive()) {
    message(
      "Outliers winsorised in: ",
      paste(modified_markers, collapse = ", ")
    )
  }

  # Multiple imputation
  predictor_cols <- if (has_forced_var()) {
    c(forced_var, candidate_markers)
  } else {
    candidate_markers
  }
  has_missing <- any(is.na(df[, predictor_cols, drop = FALSE]))
  if (has_missing) {
    ini <- mice::mice(df, maxit = 0)
    pred_matrix <- ini$predictorMatrix
    pred_matrix[outcome_var, ] <- 0
    pred_matrix[, outcome_var] <- 0
    imp <- mice::mice(df, m = n_imputations, predictorMatrix = pred_matrix)
    imputed_datasets <- purrr::map(1:n_imputations, \(m) mice::complete(imp, m))
    n_sets <- n_imputations
  } else {
    if (interactive()) {
      message("No missing data detected. Skipping multiple imputation.")
    }
    imputed_datasets <- list(df)
    n_sets <- 1
  }

  # Lambda selection
  select_lambda <- function(x, y, pf) {
    full_fit <- glmnet::glmnet(
      x,
      y,
      family = "binomial",
      alpha = alpha,
      penalty.factor = pf
    )
    n <- nrow(x)
    dev <- stats::deviance(full_fit)
    bic <- dev + log(n) * full_fit$df
    best_idx <- which.min(bic)
    best_lambda <- full_fit$lambda[best_idx]
    n_nonzero <- full_fit$df[best_idx]
    method_used <- "BIC"
    if (n_nonzero == 0) {
      warning("BIC selected an intercept-only model. Falling back to AIC.")
      aic <- dev + 2 * full_fit$df
      best_idx <- which.min(aic)
      best_lambda <- full_fit$lambda[best_idx]
      n_nonzero <- full_fit$df[best_idx]
      method_used <- "AIC"
      if (n_nonzero == 0) warning("AIC also selected an intercept-only model.")
    }
    list(lambda = best_lambda, n_nonzero = n_nonzero, method = method_used)
  }

  model_vars <- if (has_forced_var()) {
    c(forced_var, candidate_markers)
  } else {
    candidate_markers
  }

  # LOO elastic net
  results <- purrr::map(seq_along(imputed_datasets), \(m) {
    imp_data <- imputed_datasets[[m]]
    x <- stats::model.matrix(
      ~ . - 1,
      data = imp_data[, model_vars, drop = FALSE]
    )
    y <- imp_data[[outcome_var]]
    n <- nrow(x)
    pf <- if (!has_forced_var()) {
      rep(1, ncol(x))
    } else {
      ifelse(grepl(paste0("^", forced_var), colnames(x)), 0, 1)
    }
    lambda_result <- select_lambda(x, y, pf)
    best_lambda <- lambda_result$lambda

    loo_results <- purrr::map(1:n, \(i) {
      y_train <- y[-i]
      if (length(unique(y_train)) < 2) {
        return(list(
          pred_prob = mean(y_train),
          true_label = y[i],
          coefs = tibble::tibble(term = character(0), coefficient = numeric(0)),
          single_class = TRUE
        ))
      }
      fit <- tryCatch(
        glmnet::glmnet(
          x[-i, , drop = FALSE],
          y_train,
          family = "binomial",
          alpha = alpha,
          penalty.factor = pf,
          lambda = best_lambda
        ),
        error = function(e) NULL
      )
      if (is.null(fit)) {
        return(list(
          pred_prob = mean(y_train),
          true_label = y[i],
          coefs = tibble::tibble(term = character(0), coefficient = numeric(0)),
          single_class = FALSE
        ))
      }
      pred_prob <- stats::predict(
        fit,
        newx = x[i, , drop = FALSE],
        s = best_lambda,
        type = "response"
      ) |>
        as.numeric()
      coefs <- stats::coef(fit, s = best_lambda) |>
        as.matrix() |>
        as.data.frame() |>
        tibble::rownames_to_column("term") |>
        dplyr::rename(coefficient = s1) |>
        dplyr::filter(term != "(Intercept)")
      list(
        pred_prob = pred_prob,
        true_label = y[i],
        coefs = coefs,
        single_class = FALSE
      )
    })

    n_single_class <- sum(purrr::map_lgl(loo_results, \(r) {
      isTRUE(r$single_class)
    }))
    if (n_single_class > 0) {
      warning(
        "Imputation ",
        m,
        ": ",
        n_single_class,
        " LOO fold(s) had single-class training set."
      )
    }
    pred_probs <- purrr::map_dbl(loo_results, "pred_prob")
    true_labels <- purrr::map_dbl(loo_results, "true_label")
    auc_val <- fastAUC(pred_probs, true_labels)

    selection_freq <- purrr::map(loo_results, \(r) {
      if (nrow(r$coefs) == 0) {
        return(NULL)
      }
      r$coefs |>
        (\(d) {
          if (!has_forced_var()) {
            d
          } else {
            dplyr::filter(d, !grepl(paste0("^", forced_var), term))
          }
        })() |>
        dplyr::mutate(selected = coefficient != 0) |>
        dplyr::select(term, coefficient, selected)
    }) |>
      purrr::compact() |>
      purrr::list_rbind() |>
      dplyr::group_by(term) |>
      dplyr::summarise(
        selection_freq = mean(selected),
        median_coef = stats::median(coefficient),
        .groups = "drop"
      )

    list(
      imputation = m,
      auc = auc_val,
      best_lambda = best_lambda,
      n_nonzero = lambda_result$n_nonzero,
      lambda_method = lambda_result$method,
      selection_freq = selection_freq
    )
  })

  # Summarise
  auc_summary <- purrr::map(results, \(r) {
    tibble::tibble(
      imputation = r$imputation,
      auc = r$auc,
      lambda = r$best_lambda,
      lambda_method = r$lambda_method,
      n_nonzero = r$n_nonzero
    )
  }) |>
    purrr::list_rbind()
  if (n_sets == 1) {
    auc_summary <- auc_summary |> dplyr::select(-imputation)
  }

  marker_summary <- purrr::map(results, \(r) {
    r$selection_freq |> dplyr::mutate(imputation = r$imputation)
  }) |>
    purrr::list_rbind() |>
    dplyr::group_by(term) |>
    dplyr::summarise(
      mean_selection_freq = mean(selection_freq),
      sd_selection_freq = stats::sd(selection_freq),
      mean_median_coef = mean(median_coef),
      sd_median_coef = stats::sd(median_coef),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(mean_selection_freq))
  if (n_sets == 1) {
    marker_summary <- marker_summary |>
      dplyr::select(-sd_selection_freq, -sd_median_coef) |>
      dplyr::rename(
        selection_freq = mean_selection_freq,
        median_coef = mean_median_coef
      )
  }

  # Build return object with method text
  out <- list(
    auc_summary = auc_summary,
    marker_summary = marker_summary,
    zero_variance_markers = zero_var,
    high_missingness_markers = if (length(high_miss_markers) > 0) {
      stats::setNames(round(high_miss_props, 3), high_miss_markers)
    } else {
      character(0)
    },
    winsorised_markers = modified_markers,
    transformed_markers = transformed_markers,
    analysed_data = imputed_datasets
  )

  miss_desc <- if (length(high_miss_markers) > 0) {
    paste0(
      "Candidate markers with >",
      max_miss * 100,
      "% missing values were excluded (n=",
      length(high_miss_markers),
      "). "
    )
  } else {
    ""
  }
  zero_var_desc <- if (length(zero_var) > 0) {
    paste0("Zero-variance markers were dropped (n = ", length(zero_var), "). ")
  } else {
    ""
  }
  trans_desc <- if (length(transformed_markers) > 0) {
    "To reduce skewness, continuous markers were evaluated under identity, sqrt and log transforms; the one minimising absolute skewness was selected. "
  } else {
    ""
  }
  k_desc <- if (length(modified_markers) > 0) {
    paste0(
      "Outliers in continuous markers were winsorised at +/-",
      k_mad,
      " MAD from the median (",
      length(modified_markers),
      " marker(s)). "
    )
  } else {
    ""
  }
  poscat_desc <- if (poscat != "1") {
    paste0("The positive outcome category was '", poscat, "'. ")
  } else {
    ""
  }
  mi_desc <- if (has_missing) {
    paste0(
      "Residual missing values were imputed via mice (",
      n_imputations,
      " datasets); the outcome was excluded from the imputation model. "
    )
  } else {
    ""
  }
  agg_desc <- if (has_missing) {
    "Selection frequency and coefficients were averaged across imputed datasets. "
  } else {
    ""
  }
  forced_desc <- if (has_forced_var()) {
    paste0("The variable '", forced_var, "' was included unpenalised. ")
  } else {
    ""
  }
  lambda_methods_unique <- unique(purrr::map_chr(results, \(r) r$lambda_method))
  lambda_desc <- if (all(lambda_methods_unique == "BIC")) {
    "Lambda was selected by minimising BIC. "
  } else if (all(lambda_methods_unique == "AIC")) {
    "Lambda was selected by minimising AIC (BIC gave intercept-only). "
  } else {
    "Lambda was selected by BIC where possible, with AIC fallback. "
  }

  attr(out, "method") <- paste0(
    "Biomarker selection was performed using elastic net logistic regression (alpha = ",
    alpha,
    ") with LOO-CV. ",
    poscat_desc,
    miss_desc,
    zero_var_desc,
    trans_desc,
    k_desc,
    mi_desc,
    forced_desc,
    lambda_desc,
    "Variable importance was quantified by LOO selection frequency. ",
    agg_desc,
    "The sample comprised ",
    n_obs,
    " observations and ",
    length(candidate_markers),
    " candidate markers."
  )
  attr(out, "biblio") <- c(
    elastic_net = "Zou, H. & Hastie, T. (2005). Regularization and variable selection via the elastic net. JRSS-B, 67, 301-320.",
    bic = "Schwarz, G. (1978). Estimating the dimension of a model. Annals of Statistics, 6(2), 461-464."
  )
  if (has_missing) {
    attr(out, "biblio") <- c(
      attr(out, "biblio"),
      mice = "van Buuren, S. & Groothuis-Oudshoorn, K. (2011). mice: Multivariate Imputation by Chained Equations in R. JSS, 45(3), 1-67."
    )
  }

  class(out) <- c("glmnet_marker_selection", "list")
  return(out)
}

#' @export
print.glmnet_marker_selection <- function(x, ...) {
  cat("== AUC Summary ==\n")
  print(x$auc_summary, n = Inf)
  cat("\n== Marker Summary ==\n")
  print(x$marker_summary, n = Inf)
  cat("\n== Interpretation ==\n")
  cat(
    "selection_freq: proportion of LOO iterations with non-zero coefficient.\n"
  )
  cat("median_coef: median elastic net coefficient (log-odds scale).\n")
  if ("sd_selection_freq" %in% names(x$marker_summary)) {
    cat("sd_* columns: variability across imputed datasets.\n")
  }
  cat("\nUse attr(x, 'method') for a methods paragraph.\n")
  invisible(x)
}


# Val-MI Internal Validation ---

#' Optimism-Corrected AUC via Val-MI Strategy (Wahl et al. 2016)
#'
#' @param df data frame (with missing values)
#' @param outcome_var binary outcome column name
#' @param predictors character vector of predictor names
#' @param n_boot number of bootstrap replicates (default 200)
#' @param n_imp number of imputations per replicate (default 5)
#' @param method one of "0.632+", "0.632", "optimism", "oob"
#' @return val_mi_result object
#' @export
val_mi_auc <- function(
  df,
  outcome_var,
  predictors,
  n_boot = 200,
  n_imp = 5,
  method = c("0.632+", "0.632", "optimism", "oob")
) {
  method <- match.arg(method)
  n <- nrow(df)
  formula <- as.formula(paste(
    outcome_var,
    "~",
    paste(predictors, collapse = "+")
  ))

  impute_data <- function(data) {
    if (!any(is.na(data[, setdiff(names(data), outcome_var), drop = FALSE]))) {
      return(replicate(n_imp, data, simplify = FALSE))
    }
    ini <- mice::mice(data, maxit = 0)
    pred_matrix <- ini$predictorMatrix
    pred_matrix[outcome_var, ] <- 0
    pred_matrix[, outcome_var] <- 0
    imp <- mice::mice(
      data,
      m = n_imp,
      predictorMatrix = pred_matrix,
      printFlag = FALSE
    )
    purrr::map(1:n_imp, \(m) mice::complete(imp, m))
  }

  pooled_auc <- function(train_datasets, eval_datasets, eval_y) {
    pred_matrix <- sapply(seq_along(train_datasets), \(m) {
      fit <- tryCatch(
        stats::glm(
          formula,
          data = train_datasets[[m]],
          family = stats::binomial()
        ),
        error = function(e) NULL,
        warning = function(w) {
          suppressWarnings(stats::glm(
            formula,
            data = train_datasets[[m]],
            family = stats::binomial()
          ))
        }
      )
      if (is.null(fit)) {
        return(rep(NA, nrow(eval_datasets[[m]])))
      }
      stats::predict(fit, newdata = eval_datasets[[m]], type = "response")
    })
    pooled_preds <- rowMeans(pred_matrix, na.rm = TRUE)
    fastAUC(pooled_preds, eval_y)
  }

  if (interactive()) {
    message("Computing apparent performance...")
  }
  orig_datasets <- impute_data(df)
  y_orig <- orig_datasets[[1]][[outcome_var]]
  auc_apparent <- pooled_auc(orig_datasets, orig_datasets, y_orig)

  if (interactive()) {
    message("Running ", n_boot, " bootstrap replicates...")
  }
  boot_results <- purrr::map(1:n_boot, \(b) {
    if (interactive() && b %% 50 == 0) {
      message("  Bootstrap ", b, "/", n_boot)
    }
    idx <- sample(n, replace = TRUE)
    oob_idx <- setdiff(1:n, idx)
    if (length(oob_idx) < 2) {
      return(NULL)
    }
    y_oob <- df[[outcome_var]][oob_idx]
    if (length(unique(y_oob[!is.na(y_oob)])) < 2) {
      return(NULL)
    }
    bs_data <- df[idx, , drop = FALSE]
    oob_data <- df[oob_idx, , drop = FALSE]
    bs_datasets <- tryCatch(impute_data(bs_data), error = function(e) NULL)
    oob_datasets <- tryCatch(impute_data(oob_data), error = function(e) NULL)
    if (is.null(bs_datasets) || is.null(oob_datasets)) {
      return(NULL)
    }
    y_bs <- bs_datasets[[1]][[outcome_var]]
    auc_bs_bs <- tryCatch(
      pooled_auc(bs_datasets, bs_datasets, y_bs),
      error = function(e) NA
    )
    auc_bs_oob <- tryCatch(
      pooled_auc(bs_datasets, oob_datasets, y_oob),
      error = function(e) NA
    )
    auc_bs_orig <- tryCatch(
      pooled_auc(bs_datasets, orig_datasets, y_orig),
      error = function(e) NA
    )
    list(
      auc_bs_bs = auc_bs_bs,
      auc_bs_oob = auc_bs_oob,
      auc_bs_orig = auc_bs_orig
    )
  }) |>
    purrr::compact()

  if (length(boot_results) == 0) {
    stop("All bootstrap replicates failed.")
  }

  auc_bs_bs <- mean(purrr::map_dbl(boot_results, "auc_bs_bs"), na.rm = TRUE)
  auc_bs_oob <- mean(purrr::map_dbl(boot_results, "auc_bs_oob"), na.rm = TRUE)
  auc_bs_orig <- mean(purrr::map_dbl(boot_results, "auc_bs_orig"), na.rm = TRUE)

  optimism <- auc_bs_bs - auc_bs_orig
  auc_opt_corr <- auc_apparent - optimism
  auc_oob <- auc_bs_oob
  auc_0.632 <- 0.368 * auc_apparent + 0.632 * auc_oob
  R <- (auc_bs_oob - auc_apparent) / (0.5 - auc_apparent)
  R <- max(0, min(1, R))
  w <- 0.632 / (1 - 0.368 * R)
  w <- min(1, w)
  auc_0.632plus <- (1 - w) * auc_apparent + w * auc_oob

  auc_corrected <- switch(
    method,
    "optimism" = auc_opt_corr,
    "oob" = auc_oob,
    "0.632" = auc_0.632,
    "0.632+" = auc_0.632plus
  )

  n_successful <- length(boot_results)
  out <- list(
    apparent_auc = auc_apparent,
    corrected_auc = auc_corrected,
    method = method,
    all_estimates = list(
      optimism_corrected = auc_opt_corr,
      oob = auc_oob,
      `0.632` = auc_0.632,
      `0.632+` = auc_0.632plus
    ),
    components = list(
      auc_bs_bs = auc_bs_bs,
      auc_bs_oob = auc_bs_oob,
      auc_bs_orig = auc_bs_orig,
      optimism = optimism
    ),
    n_boot = n_boot,
    n_boot_successful = n_successful,
    n_imp = n_imp
  )

  method_name <- switch(
    method,
    "optimism" = "ordinary optimism-corrected",
    "oob" = "out-of-bag",
    "0.632" = "0.632",
    "0.632+" = "0.632+"
  )
  attr(out, "method") <- paste0(
    "Internal validation was conducted using Val-MI (Wahl et al., 2016), combining bootstrap ",
    "internal validation with multiple imputation. ",
    n_boot,
    " bootstrap replicates with ",
    n_imp,
    " imputations each. The ",
    method_name,
    " estimate was used. ",
    n_successful,
    " of ",
    n_boot,
    " replicates completed successfully."
  )
  attr(out, "biblio") <- c(
    val_mi = "Wahl, S. et al. (2016). Assessment of predictive performance in incomplete data by combining internal validation and multiple imputation. BMC Med Res Meth, 16, 144.",
    harrell = "Harrell, F. E. (2015). Regression Modeling Strategies (2nd ed.). Springer.",
    efron_632 = "Efron, B. & Tibshirani, R. (1997). The .632+ bootstrap method. JASA, 92(438), 548-560."
  )
  class(out) <- c("val_mi_result", "list")
  return(out)
}

#' @export
print.val_mi_result <- function(x, ...) {
  cat("== Val-MI Internal Validation ==\n\n")
  cat("Apparent AUC:              ", round(x$apparent_auc, 3), "\n")
  cat(
    "Corrected AUC (",
    x$method,
    "):  ",
    round(x$corrected_auc, 3),
    "\n\n",
    sep = ""
  )
  cat("All estimates:\n")
  cat(
    "  Optimism-corrected:      ",
    round(x$all_estimates$optimism_corrected, 3),
    "\n"
  )
  cat("  Out-of-bag:              ", round(x$all_estimates$oob, 3), "\n")
  cat("  0.632:                   ", round(x$all_estimates$`0.632`, 3), "\n")
  cat("  0.632+:                  ", round(x$all_estimates$`0.632+`, 3), "\n\n")
  cat("Bootstrap replicates:      ", x$n_boot_successful, "/", x$n_boot, "\n")
  cat("Imputations per replicate: ", x$n_imp, "\n\n")
  cat("Use attr(x, 'method') for a methods paragraph.\n")
  invisible(x)
}


# Diagnostic Comparison Functions --

#' Compute classification metrics with CIs from TP/TN/FP/FN vector
#'
#' Supports Clopper-Pearson (default) or cluster-robust GEE CIs.
#'
#' @param test_results character vector of "TP", "TN", "FP", "FN"
#' @param cluster optional cluster ID vector for GEE CIs
#' @param conf_level confidence level (default 0.95)
#' @export
compute_metrics_with_CI <- function(
  test_results,
  cluster = NULL,
  conf_level = 0.95
) {
  TP = sum(test_results == "TP", na.rm = TRUE)
  TN = sum(test_results == "TN", na.rm = TRUE)
  FP = sum(test_results == "FP", na.rm = TRUE)
  FN = sum(test_results == "FN", na.rm = TRUE)
  Total <- TP + TN + FP + FN
  alpha <- 1 - conf_level
  z <- qnorm(1 - alpha / 2)

  cp_ci <- function(x, n) {
    if (n == 0) {
      return(c(NA, NA))
    }
    binom.test(x, n, conf.level = conf_level)$conf.int
  }
  robust_ci <- function(x, n, cluster) {
    if (n == 0 || is.null(cluster)) {
      return(c(NA, NA))
    }
    y <- c(rep(1, x), rep(0, n - x))
    cl <- cluster[seq_len(n)]
    fit <- geepack::geeglm(
      y ~ 1,
      id = cl,
      family = binomial(link = "logit"),
      corstr = "independence"
    )
    est <- coef(fit)
    se <- sqrt(vcov(fit))
    plogis(est + c(-1, 1) * z * se)
  }

  metrics <- list(
    Prevalence = c(x = TP + FN, n = Total),
    Accuracy = c(x = TP + TN, n = Total),
    Precision = c(x = TP, n = TP + FP),
    Sensitivity = c(x = TP, n = TP + FN),
    Specificity = c(x = TN, n = TN + FP),
    PPV = c(x = TP, n = TP + FP),
    NPV = c(x = TN, n = TN + FN)
  )
  results <- do.call(
    rbind,
    lapply(names(metrics), function(m) {
      x <- metrics[[m]]["x"]
      n <- metrics[[m]]["n"]
      est <- ifelse(n == 0, NA, x / n)
      ci <- if (is.null(cluster)) cp_ci(x, n) else robust_ci(x, n, cluster)
      data.frame(metric = m, estimate = est, LB = ci[1], UB = ci[2])
    })
  )
  rownames(results) <- NULL
  if (interactive()) {
    if (is.null(cluster)) {
      message("Exact (Clopper-Pearson) ", conf_level * 100, "% CIs calculated.")
    } else {
      message("Cluster-robust (GEE) ", conf_level * 100, "% CIs calculated.")
    }
  }
  results
}

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


# Reporting Helpers

#' Table with mean(sd) by group with p-value and/or CI
#'
#' @param data dataframe
#' @param covs vector of variables
#' @param maincov grouping variable
#' @param colHeader column header name
#' @param digits rounding
#' @param showP show p-value
#' @param showCI show difference and CI
#' @param showMissing show missing counts
#' @param CIwidth confidence level
#' @param niceNames tidy variable names
#' @param tableOnly return data frame
#' @param unformattedp return raw p-values
#' @param padjust p-value adjustment method
#' @param caption table caption
#' @param reverse_difference reverse the direction of the difference
#' @export
rm_diffsum <- function(
  data,
  covs,
  maincov,
  colHeader = 'Variable',
  digits = 1,
  showP = TRUE,
  showCI = TRUE,
  showMissing = TRUE,
  CIwidth = .95,
  niceNames = TRUE,
  tableOnly = FALSE,
  unformattedp = FALSE,
  padjust = 'none',
  caption = NULL,
  reverse_difference = FALSE
) {
  if (inherits(data[[maincov]], 'factor')) {
    lvls <- levels(data[[maincov]])
  } else {
    lvls <- unique(data[[maincov]])
  }
  if (reverse_difference) {
    diff_lvls <- rev(lvls)
  } else {
    diff_lvls <- lvls
  }
  df <- lapply(lvls, function(x) subset(data, data[[maincov]] == x))
  names(df) <- lvls

  if (length(digits) < length(covs)) {
    if (length(digits) > 1) {
      warning(
        'digits should either be length 1 or length of covs, only the first element used.'
      )
    }
    digits = rep(digits[1], length(covs))
  }

  rws <- lapply(covs, function(x) {
    d <- digits[which(covs == x)]
    sumVar <- lapply(df, function(dat) {
      z <- dat[[x]]
      mn = mean(z, na.rm = T)
      sd = sd(z, na.rm = T)
      mean_sd = paste0(rnd(mn, d), " (", rnd(sd, d), ")")
      miss = sum(is.na(z))
      c(mean_sd, miss)
    })
    out <- do.call(c, sumVar)
    if (showCI) {
      if (length(lvls) == 2) {
        data$orderedGrp <- factor(data[[maincov]], levels = diff_lvls)
        ht <- try(
          t.test(data[[x]] ~ data$orderedGrp, conf.level = CIwidth),
          silent = T
        )
        if (!inherits(ht, 'try-error')) {
          difference <- pstprn(rnd(c(-diff(ht$estimate), ht$conf.int), d))
          p_value <- ht$p.value
        } else {
          difference <- NA
          p_value <- NA
        }
        out <- c(out, difference = difference, p_value = p_value)
      }
    }
    out
  })
  out <- data.frame(do.call(rbind, rws))
  out$p_value <- p.adjust(out$p_value, method = padjust)
  tab <- data.frame(out[, c(
    grep(".*1$", colnames(out)),
    grep("difference", colnames(out)),
    grep("p_value", colnames(out)),
    grep(".*2$", colnames(out))
  )])
  tab <- cbind(Covariate = covs, tab)
  ss <- table(data[[maincov]])
  lvlnm <- paste0(lvls, " (n=", ss, ") Mean (sd)")
  cnm <- c(
    colHeader,
    lvlnm,
    paste0("Difference (", 100 * CIwidth, "% CI)"),
    "p-value",
    paste0("N missing ", lvls)
  )
  names(tab) <- cnm
  if (showP & !unformattedp) {
    tab[["p-value"]] <- formatp(tab[["p-value"]])
  }
  if (niceNames) {
    tab[[colHeader]] <- nicename(tab[[colHeader]])
  }

  toShow <- unlist(sapply(c(colHeader, 'Mean'), function(x) {
    grep(x, names(tab))
  }))
  if (showCI) {
    toShow <- c(toShow, grep('Difference', names(tab)))
  }
  if (showP) {
    toShow <- c(toShow, grep('p-value', names(tab)))
  }
  if (showMissing) {
    toShow <- c(toShow, grep('missing', names(tab)))
  }

  if (tableOnly) {
    return(tab[, toShow])
  } else {
    reportRmd::outTable(tab[, toShow], caption = caption)
  }
}

#' Performance table for logistic regression
#'
#' @param glm_fit glm object
#' @param obs_pred two-column data frame
#' @param responseLabels response labels
#' @param cutoff probability cutoff
#' @param digits rounding
#' @param showCI show CIs
#' @param showN show denominators
#' @param bs bootstrap samples
#' @param allboot all CIs from bootstrap
#' @export
performancetable <- function(
  glm_fit,
  obs_pred,
  responseLabels = NULL,
  cutoff = 0.5,
  digits = 2,
  showCI = T,
  showN = FALSE,
  bs = 1000,
  allboot = T
) {
  if (!xor(missing(glm_fit), missing(obs_pred))) {
    stop('Please specify either glm_fit or obs_pred.')
  }
  if (!missing(glm_fit)) {
    if (inherits(glm_fit, 'glm')) {
      obs = glm_fit$model[, 1]
      if (length(unique(obs)) > 2) {
        stop('Only binary response supported.')
      }
      if ("factor" %in% class(obs)) {
        obs = as.numeric(obs) - 1
      }
      pred = ifelse(predict(glm_fit, type = 'response') < cutoff, 0, 1)
      poscat = as.character(unique(glm_fit$model[, 1][glm_fit$y == 1]))
    }
  } else {
    obs = factor(obs_pred[[1]])
    if (all(sort(unique(obs_pred[[2]])) %in% c(0, 1))) {
      pred = factor(obs_pred[[2]])
      score = NULL
    } else {
      score = obs_pred[[2]]
      pred <- factor(ifelse(score < cutoff, 0, 1))
    }
    if (!all(levels(pred) %in% levels(obs))) {
      stop('Predicted levels must be in observed.')
    }
    if (length(levels(obs)) != 2) {
      stop('Only two levels permitted.')
    }
    if (any(is.na(obs)) | any(is.na(pred))) {
      stop('Missing values not permitted.')
    }
    poscat = levels(obs)[2]
    obs_levels <- levels(obs)
    obs = as.numeric(factor(obs, levels = obs_levels)) - 1
    pred = as.numeric(factor(pred, levels = obs_levels)) - 1
  }
  a = sum(obs == 1 & pred == 1)
  b = sum(obs == 0 & pred == 1)
  cc = sum(obs == 1 & pred == 0)
  d = sum(obs == 0 & pred == 0)
  rtn = rbind(
    Prevalence = c((a + cc), (a + b + cc + d)),
    Sensitivity = c(a, (a + cc)),
    Specificity = c(d, (b + d)),
    PPV = c(a, (a + b)),
    NPV = c(d, (d + cc)),
    Accuracy = c((a + d), (a + b + cc + d))
  )
  colnames(rtn) <- c('x', 'n')
  rtn = cbind(rtn, p = rtn[, 'x'] / rtn[, 'n'])
  CI = apply(rtn, MARGIN = 1, function(x) {
    if (x[2] > 0) binom.test(x[1], x[2])$conf.int else c(NA, NA)
  })
  rtn <- cbind(rtn, t(CI))
  rtn = format(round(rtn, digits), nsmall = digits, trim = T)

  if (showCI) {
    CI = apply(rtn, MARGIN = 1, function(x) {
      paste0(x[3], " (", paste0(x[4:5], collapse = ", "), ")")
    })
    out = data.frame(Statistic = rownames(rtn), CI = CI)
    names(out)[2] = "Value (95%CI)"
  } else {
    out = data.frame(Statistic = rownames(rtn), Value = rtn[, 3])
  }

  if (!missing(glm_fit)) {
    auc <- fastAUC(predict(glm_fit), glm_fit$y)
    if (showCI) {
      bootstrap_auc <- sapply(1:bs, function(x) {
        data <- glm_fit$model
        bs_data <- data[sample(1:nrow(data), replace = TRUE), ]
        m_bs <- update(glm_fit, data = bs_data)
        fastAUC(predict(m_bs), m_bs$y)
      })
      auc_ci <- quantile(bootstrap_auc, c(.025, .975))
      out <- rbind(out, c('AUC', psthr(c(auc, auc_ci))))
    } else {
      out <- rbind(out, c('AUC', rnd(auc, 2)))
    }
  } else {
    if (!is.null(score)) {
      auc <- fastAUC(score, obs)
      out <- rbind(out, c('AUC', rnd(auc, 2)))
    }
  }
  out <- rbind(
    out,
    c('Sample Size', length(obs)),
    c("Positive Category", poscat)
  )
  rownames(out) <- NULL
  if (showN) {
    out <- cbind(
      out,
      x = c(as.numeric(rtn[, 'x']), NA, NA),
      n = c(as.numeric(rtn[, 'n']), NA, NA)
    )
  }
  attr(out, "description") <- paste(
    "Model performance metrics computed defining positive category as",
    poscat,
    ". CIs from Clopper-Pearson exact binomial. AUC CI from",
    bs,
    "bootstrap samples."
  )
  return(out)
}
