#' Biomarker Selection via Elastic Net with Optional Multiple Imputation and LOO Validation
#'
#' Performs penalised logistic regression-based biomarker selection for binary outcomes,
#' combining leave-one-out cross-validation with optional multiple imputation to handle
#' missing data. Designed for variable selection stability assessment in small samples.
#'
#' @param df A data frame containing the outcome, candidate markers, and optionally a forced variable.
#' @param outcome_var Character string naming the binary outcome column.
#' @param candidate_markers Character vector of candidate biomarker column names. Must be numeric
#'   (continuous) variables. Binary variables are permitted but will not be transformed or winsorised.
#' @param forced_var Optional character string naming a variable to include unpenalised (e.g. "sex").
#'   Set to NULL (default) if no variable should be forced into the model.
#' @param n_imputations Integer. Number of multiply imputed datasets to generate (default 5).
#'   Ignored if no missing data are present.
#' @param k_mad Numeric. The number of median absolute deviations to use for outlier
#'   winsorisation (default 4).
#' @param alpha Numeric. The elastic net mixing parameter (default 0.5). 0 = ridge, 1 = lasso.
#' @param max_miss Numeric between 0 and 1. Maximum proportion of missing values permitted
#'   per candidate marker. Markers exceeding this threshold are excluded (default 0.25).
#' @param return_data Logical If TRUE (default) then the processed and imputed data frames are returned
#'
#' @return A named list with class \code{glmnet_marker_selection} containing:
#' \describe{
#'   \item{auc_summary}{A tibble reporting LOO-AUC, selected lambda, lambda selection method,
#'     and number of non-zero coefficients for each imputed (or single) dataset.}
#'   \item{marker_summary}{A tibble summarising variable selection stability and coefficient
#'     magnitude. Selection frequency is expressed as a proportion (0-1) of LOO iterations.}
#'   \item{zero_variance_markers}{Character vector of markers dropped for zero variance.}
#'   \item{high_missingness_markers}{Character vector of markers dropped for exceeding the
#'     missingness threshold, with the proportion missing as names.}
#'   \item{winsorised_markers}{Character vector of markers that were winsorised.}
#'   \item{transformed_markers}{Named character vector mapping original marker names to
#'     their transformation type ("log" or "sqrt").}
#' }
#'
#' The return object carries two attributes (access via \code{attr(result, "method")} and
#' \code{attr(result, "biblio")}):
#' \describe{
#'   \item{method}{A plain-text description of the analytical approach suitable for a methods section.}
#'   \item{biblio}{A named character vector of key references.}
#' }
#'
#' @details
#' The pipeline proceeds in the following order:
#' 1. Restrict to relevant variables and complete-outcome observations
#' 2. Exclude markers exceeding the missingness threshold
#' 3. Drop zero-variance markers
#' 4. Transform continuous (non-binary) markers to reduce skewness
#' 5. Winsorise continuous (non-binary) markers using MAD
#' 6. Multiple imputation (if residual missing data present)
#' 7. Elastic net LOO cross-validation
#'
#' **Reproducibility.** No internal seed is set. Users should call \code{set.seed()} before
#' running this function.
#'
#' @references
#' Zou, H. & Hastie, T. (2005). Regularization and variable selection via the elastic net.
#' \emph{Journal of the Royal Statistical Society: Series B}, 67, 301-320.
#'
#' van Buuren, S. & Groothuis-Oudshoorn, K. (2011). mice: Multivariate Imputation by Chained
#' Equations in R. \emph{Journal of Statistical Software}, 45(3), 1-67.
#'
#' Schwarz, G. (1978). Estimating the dimension of a model. \emph{The Annals of Statistics},
#' 6(2), 461-464.
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#' result <- glmnet_marker_selection(
#'   df = my_data,
#'   outcome_var = "disease",
#'   candidate_markers = c("marker1", "marker2", "marker3"),
#'   forced_var = "sex"
#' )
#' result$marker_summary
#' result$auc_summary
#' cat(attr(result, "method"))
#' }
#'
#' @export
glmnet_marker_selection <- function(
    df,
    outcome_var,
    candidate_markers = NULL,
    forced_var = NULL,
    n_imputations = 5,
    k_mad = 4,
    alpha = 0.5,
    max_miss = 0.25,
    return_data = TRUE
) {
  
  # Default candidate_markers to everything except outcome and forced var
  if (is.null(candidate_markers)) {
    candidate_markers <- setdiff(names(df), c(outcome_var, forced_var))
    if (interactive()) {
      message(
        "Using all non-outcome variables as candidates (",
        length(candidate_markers), " markers)"
      )
    }
  }
  
  has_forced_var <- function() {
    !is.null(forced_var) && !is.na(forced_var) && forced_var != ""
  }
  
  is_binary <- function(x) {
    vals <- unique(x[!is.na(x)])
    length(vals) == 2
  }
  
  
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
  
  # Restrict dataset
  keep_vars <- unique(c(outcome_var, forced_var, candidate_markers))
  df <- df[, intersect(keep_vars, names(df)), drop = FALSE]
  df <- df[!is.na(df[[outcome_var]]), , drop = FALSE]
  
  n_obs <- nrow(df)
  
  
  # Detect positive category and coerce outcome to numeric 0/1
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
    stop("outcome_var must be binary with exactly 2 unique values (coded 0/1).")
  }
  
  n_events <- sum(df[[outcome_var]] == 1)
  n_nonevents <- sum(df[[outcome_var]] == 0)
  
  
  # Exclude markers exceeding missingness threshold
  miss_props <- sapply(df[, candidate_markers, drop = FALSE], \(x) {
    mean(is.na(x))
  })
  high_miss_markers <- names(miss_props[miss_props > max_miss])
  high_miss_props <- miss_props[high_miss_markers]
  
  if (length(high_miss_markers) > 0) {
    message(
      "Excluding markers with >", max_miss * 100, "% missingness: ",
      paste0(high_miss_markers, " (", round(high_miss_props * 100, 1), "%)"),
      collapse = ", "
    )
    candidate_markers <- setdiff(candidate_markers, high_miss_markers)
  }
  
  if (length(candidate_markers) == 0) {
    stop("No candidate markers remaining after excluding high-missingness columns.")
  }
  
  # Dummy code variables
  zero_var <- character(0)
  factor_markers <- candidate_markers[
    sapply(df[, candidate_markers, drop = FALSE], \(x) is.factor(x) || is.character(x))
  ]
  
  if (length(factor_markers) > 0) {
    for (marker in factor_markers) {
      x <- df[[marker]]
      
      if (is.factor(x)) {
        lvls <- levels(x)
      } else {
        lvls <- sort(unique(x[!is.na(x)]))
      }
      
      # Track and drop single-level variables
      if (length(lvls) < 2) {
        zero_var <- c(zero_var, marker)
        candidate_markers <- setdiff(candidate_markers, marker)
        df[[marker]] <- NULL
        next
      }
      
      # First level is reference
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
  # Drop zero-variance candidate markers
  marker_vars <- sapply(df[, candidate_markers, drop = FALSE], \(x) {
    stats::var(x, na.rm = TRUE)
  })
  zero_var <- c(zero_var,names(marker_vars[marker_vars == 0 | is.na(marker_vars)]))
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
  
  # Events-per-variable warning
  epv <- min(n_events, n_nonevents) / length(candidate_markers)
  if (epv < 2) {
    message(
      "Very low events-per-variable ratio (", round(epv, 1),
      "). Results should be interpreted with extreme caution."
    )
  }
  
  # Identify binary vs continuous markers
  
  binary_markers <- candidate_markers[
    sapply(df[, candidate_markers, drop = FALSE], is_binary)
  ]
  continuous_markers <- setdiff(candidate_markers, binary_markers)
  
  if (length(binary_markers) > 0 && interactive()) {
    message(
      "Binary markers excluded from transformation/winsorisation: ",
      paste(binary_markers, collapse = ", ")
    )
  }
  
  
  # Transform continuous markers to reduce skewness
  transformed_markers <- character(0)
  for (marker in continuous_markers) {
    result <- transform_marker(df[[marker]])
    
    if (result$transform != "none") {
      new_name <- paste0(result$transform, "_", marker)
      df[[new_name]] <- result$values
      df[[marker]] <- NULL
      candidate_markers[candidate_markers == marker] <- new_name
      continuous_markers[continuous_markers == marker] <- new_name
      transformed_markers <- c(transformed_markers,new_name)
    }
  }
  if (length(transformed_markers) > 0 && interactive()) {
    message("The following markers were transformed: ", paste(transformed_markers, collapse = ", "))
  }
  
  
  # Winsorise continuous markers using MAD
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
    message("Outliers winsorised in: ", paste(modified_markers, collapse = ", "))
  }
  
  
  # Check for missing data
  predictor_cols <- if (has_forced_var()) {
    c(forced_var, candidate_markers)
  } else {
    candidate_markers
  }
  has_missing <- any(is.na(df[, predictor_cols, drop = FALSE]))
  
  
  # Build imputed datasets (or wrap complete data in a list)
  if (has_missing) {
    message("Running multiple imputations ...")
    ini <- mice::mice(df, maxit = 0)
    pred_matrix <- ini$predictorMatrix
    pred_matrix[outcome_var, ] <- 0
    pred_matrix[, outcome_var] <- 0
    imp <- mice::mice(df, m = n_imputations, predictorMatrix = pred_matrix, printFlag = FALSE)
    imputed_datasets <- purrr::map(1:n_imputations, \(m) mice::complete(imp, m))
    n_sets <- n_imputations
  } else {
    if (interactive()) {
      message("No missing data detected. Skipping multiple imputation.")
    }
    imputed_datasets <- list(df)
    n_sets <- 1
  }
  
  
  # Lambda selection helper
  message("Selecting lambda ...")
  select_lambda <- function(x, y, pf) {
    full_fit <- glmnet::glmnet(
      x, y,
      family = "binomial",
      alpha = alpha,
      penalty.factor = pf
    )
    n <- nrow(x)
    
    # Try BIC first
    dev <- stats::deviance(full_fit)
    bic <- dev + log(n) * full_fit$df
    best_idx <- which.min(bic)
    best_lambda <- full_fit$lambda[best_idx]
    n_nonzero <- full_fit$df[best_idx]
    method_used <- "BIC"
    
    # Fall back to AIC if BIC gives empty model
    if (n_nonzero == 0) {
      aic <- dev + 2 * full_fit$df
      best_idx <- which.min(aic)
      best_lambda <- full_fit$lambda[best_idx]
      n_nonzero <- full_fit$df[best_idx]
      method_used <- "AIC"
      
      if (n_nonzero == 0) {
        warning(
          "both BIC and AIC selected an intercept-only model. Results may not be informative."
        )
      }
    }
    
    list(lambda = best_lambda, n_nonzero = n_nonzero, method = method_used)
  }
  
  
  # Build model matrix columns (markers + forced var)
  
  
  model_vars <- if (has_forced_var()) {
    c(forced_var, candidate_markers)
  } else {
    candidate_markers
  }
  
  
  # Run LOO elastic net across datasets
  
  message("Running elastic net models ...")
  results <- purrr::map(seq_along(imputed_datasets), \(m) {
    imp_data <- imputed_datasets[[m]]
    
    # Prepare model matrix and response
    x <- stats::model.matrix(
      ~ . - 1,
      data = imp_data[, model_vars, drop = FALSE]
    )
    y <- imp_data[[outcome_var]]
    n <- nrow(x)
    
    # Build penalty factor
    pf <- if (!has_forced_var()) {
      rep(1, ncol(x))
    } else {
      ifelse(grepl(paste0("^", forced_var), colnames(x)), 0, 1)
    }
    
    # Select lambda
    lambda_result <- select_lambda(x, y, pf)
    best_lambda <- lambda_result$lambda
    
    # LOO iterations
    loo_results <- purrr::map(1:n, \(i) {
      y_train <- y[-i]
      
      # Handle single-class training set
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
    
    # Count single-class LOO folds
    n_single_class <- sum(
      purrr::map_lgl(loo_results, \(r) isTRUE(r$single_class))
    )
    if (n_single_class > 0) {
      warning(
        "Imputation ", m, ": ", n_single_class,
        " LOO fold(s) had a single-class training set. ",
        "Prevalence predictions were used for these folds."
      )
    }
    
    # Collect LOO predictions
    pred_probs <- purrr::map_dbl(loo_results, "pred_prob")
    true_labels <- purrr::map_dbl(loo_results, "true_label")
    
    # Compute AUC
    auc_val <- fastAUC(pred_probs, true_labels)
    
    # Compute selection frequency across LOO iterations
    selection_freq <- purrr::map(loo_results, \(r) {
      if (nrow(r$coefs) == 0) return(NULL)
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
  
  
  # Summarise AUC
  
  
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
  
  
  # Summarise selection frequency
  
  
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
  
  
  # Build return object
  
  
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
    processed_data = df,
    imputed_data = if (has_missing) imputed_datasets else NULL
  )
  
  
  # Method text components
  
  
  # Missingness exclusion description
  if (length(high_miss_markers) > 0) {
    miss_desc <- paste0(
      "Candidate markers with >", max_miss * 100, "% missing values were excluded ",
      "prior to analysis (n=", length(high_miss_markers), "). "
    )
  } else {
    miss_desc <- ""
  }
  
  # Zero variance description
  if (length(zero_var) > 0) {
    zero_var_desc <- paste0(
      "Candidate markers with zero variance were dropped (n = ",
      length(zero_var), "). "
    )
  } else {
    zero_var_desc <- ""
  }
  
  # Transformation description
  if (length(transformed_markers) > 0) {
    trans_desc <- paste0(
      "To reduce skewness, continuous candidate markers with non-negative values were ",
      "evaluated under identity, square-root and log transformations; the transformation ",
      "minimising the absolute skewness of the central 95% of values was selected for ",
      "each marker. ")
  } else {
    trans_desc <- ""
  }
  
  # Winsorisation description
  if (length(modified_markers) > 0) {
    k_desc <- paste0(
      "Outliers in continuous markers were winsorised at +/-", k_mad,
      " MAD (median absolute deviation) from the median, affecting ",
      length(modified_markers), " marker(s). "
    )
  } else {
    k_desc <- ""
  }
  
  # Positive category description
  if (poscat != "1") {
    poscat_desc <- paste0(
      "The positive outcome category was '", poscat, "'. "
    )
  } else {
    poscat_desc <- ""
  }
  
  # MI description
  if (has_missing) {
    mi_desc <- paste0(
      "Missing values among candidate markers were imputed using predictive ",
      "mean matching via the mice algorithm to generate ", n_imputations,
      " imputed datasets; the binary outcome was excluded from the imputation model ",
      "to prevent information leakage. "
    )
    agg_desc <- paste0(
      "Selection frequency and coefficient estimates were averaged across ",
      "imputed datasets. "
    )
  } else {
    mi_desc <- ""
    agg_desc <- ""
  }
  
  # Forced variable description
  forced_desc <- if (has_forced_var()) {
    paste0(
      "The variable '", forced_var,
      "' was included unpenalised in all models. "
    )
  } else {
    ""
  }
  
  # Lambda method description
  lambda_methods_unique <- unique(
    purrr::map_chr(results, \(r) r$lambda_method)
  )
  if (all(lambda_methods_unique == "BIC")) {
    lambda_desc <- paste0(
      "The regularisation parameter lambda was selected by minimising the Bayesian ",
      "Information Criterion (BIC) on the full dataset. "
    )
  } else if (all(lambda_methods_unique == "AIC")) {
    lambda_desc <- paste0(
      "The regularisation parameter lambda was selected by minimising the Akaike ",
      "Information Criterion (AIC) on the full dataset, as BIC selected an ",
      "intercept-only model. "
    )
  } else {
    lambda_desc <- paste0(
      "The regularisation parameter lambda was selected by minimising the BIC ",
      "where possible; for imputed datasets where BIC selected an intercept-only model, ",
      "the AIC was used as a fallback. "
    )
  }
  
  # Assemble method attribute
  attr(out, "method") <- paste0(
    "Biomarker selection was performed using elastic net logistic regression ",
    "(alpha = ", alpha, ") with leave-one-out cross-validation. ",
    poscat_desc,
    miss_desc,
    zero_var_desc,
    trans_desc,
    k_desc,
    mi_desc,
    forced_desc,
    lambda_desc,
    "For each LOO iteration, the model was refit on n-1 observations and a predicted ",
    "probability was obtained for the held-out case. Predictive discrimination was ",
    "assessed via the area under the receiver operating characteristic curve (AUC) ",
    "computed from the full vector of LOO predictions. ",
    "Variable importance was quantified by selection frequency: the proportion of LOO ",
    "iterations in which each candidate marker received a non-zero coefficient. ",
    agg_desc,
    "The sample comprised ", n_obs, " observations and ",
    length(candidate_markers), " candidate markers."
  )
  
  # Attach bibliography
  biblio <- c(
    elastic_net = paste0(
      "Zou, H. & Hastie, T. (2005). Regularization and variable selection via ",
      "the elastic net. Journal of the Royal Statistical Society: Series B, 67, 301-320."
    ),
    bic = paste0(
      "Schwarz, G. (1978). Estimating the dimension of a model. ",
      "The Annals of Statistics, 6(2), 461-464."
    )
  )
  if (has_missing) {
    biblio <- c(
      biblio,
      mice = paste0(
        "van Buuren, S. & Groothuis-Oudshoorn, K. (2011). mice: Multivariate ",
        "Imputation by Chained Equations in R. Journal of Statistical Software, ",
        "45(3), 1-67."
      )
    )
  }
  attr(out, "biblio") <- biblio
  
  # Set class for clean printing
  class(out) <- c("glmnet_marker_selection", "list")
  
  return(out)
}



#' Print method for glmnet_marker_selection objects
#' @param x A glmnet_marker_selection object.
#' @param ... Additional arguments (unused).
#' @export
#' Print method for glmnet_marker_selection objects
#' @param x A glmnet_marker_selection object.
#' @param ... Additional arguments (unused).
#' @export
print.glmnet_marker_selection <- function(x, ...) {
  cat("== AUC Summary ==\n")
  print(x$auc_summary, n = Inf)
  cat("\n== Marker Summary ==\n")
  print(x$marker_summary, n = Inf)
  cat("\n== Interpretation ==\n")
  cat(paste0(
    "selection_freq: proportion of LOO iterations where the marker had a non-zero\n",
    "  coefficient (1.0 = always selected, 0.0 = never selected).\n",
    "median_coef: median elastic net coefficient across LOO iterations (log-odds scale).\n",
    "  Positive = higher values associated with the positive outcome.\n"
  ))
  if ("sd_selection_freq" %in% names(x$marker_summary)) {
    cat("sd_* columns: variability across multiply imputed datasets.\n")
  }
  cat("\nUse attr(x, 'method') for a methods paragraph.\n")
  cat("Use attr(x, 'biblio') for references.\n")
  invisible(x)
}
