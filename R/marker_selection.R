# marker_selection.R
# Elastic-net biomarker selection with optional multiple imputation and LOO
# validation, plus the Val-MI bootstrap internal-validation strategy.
#
# Reconciled from glm_functions.R, glmnet_marker_selection.R and
# glmnet_marker_selection2.R: this is the most complete version (includes the
# factor-marker guard added in v2 and full namespacing).
#
# Depends on: format_utils.R (rnd, skewness, mad_winsorise, transform_marker)
# Suggests packages: glmnet, mice, purrr, tibble, dplyr

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
