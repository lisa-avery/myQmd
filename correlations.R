# Correlation Functions & Efron's Kappa
# Correlation estimation, CI computation, GEE-based correlated performance,
# paired diagnostic comparison, and optimism-adjusted kappa for rpart models
#
# Depends on: R/utils.R (rnd, formatp, psthr)


# Correlation Estimation --------------------------------------------------

#' Spearman Correlation with Confidence Interval
#'
#' Computes the Spearman rank correlation and a Fisher-z based confidence
#' interval for a pair of numeric vectors.
#'
#' @param x,y Numeric vectors.
#' @param confidence_level Confidence level for the interval (default 0.95).
#' @return A one-row data frame with rho, lower_ci, upper_ci, p_value, and
#'   n_pairs.
#' @export
spearman_correlation_with_ci <- function(x, y, confidence_level = 0.95) {
  valid_indices <- which(!is.na(x) & !is.na(y))
  x_valid <- x[valid_indices]
  y_valid <- y[valid_indices]
  n <- length(x_valid)

  if (n < 3) {
    stop("Need at least 3 valid pairs to calculate Spearman correlation.")
  }

  spearman_test <- cor.test(x_valid, y_valid, method = "spearman", exact = FALSE)
  rho     <- spearman_test$estimate
  p_value <- spearman_test$p.value

  z    <- 0.5 * log((1 + rho) / (1 - rho))
  se_z <- 1 / sqrt(n - 3)

  alpha          <- 1 - confidence_level
  critical_value <- qnorm(1 - alpha / 2)

  lower_z <- z - critical_value * se_z
  upper_z <- z + critical_value * se_z

  lower_ci <- (exp(2 * lower_z) - 1) / (exp(2 * lower_z) + 1)
  upper_ci <- (exp(2 * upper_z) - 1) / (exp(2 * upper_z) + 1)

  data.frame(
    rho      = rho,
    lower_ci = lower_ci,
    upper_ci = upper_ci,
    p_value  = p_value,
    n_pairs  = n
  )
}


#' Correlation with CI (Pearson or Spearman)
#'
#' Computes a Pearson or Spearman correlation with confidence interval.
#' For Spearman correlations, CI is computed via the Bonett-Wright method
#' (Fisher-z with variance correction).
#'
#' @param x,y Numeric vectors.
#' @param method Character: "pearson" (default) or "spearman".
#' @param CIwidth Confidence level (default 0.95).
#' @param digits Number of decimal places (default 2).
#' @param as_char Logical; if TRUE returns a formatted string "est (LL, UL)".
#' @param showN Logical; if TRUE appends sample size N to the output.
#' @param pvalue Logical; if TRUE appends the formatted p-value.
#' @return A one-row data frame.
#' @export
correlationCI <- function(x, y, method = "pearson", CIwidth = 0.95,
                          digits = 2, as_char = FALSE, showN = FALSE,
                          pvalue = FALSE) {
  df_all <- data.frame(x = x, y = y)
  df     <- na.omit(df_all)
  n      <- nrow(df)

  if (n == 0) {
    rtn <- data.frame(rho = NA_character_)
  } else {
    if (nrow(df_all) != nrow(df)) {
      message(paste("There were", nrow(df_all) - nrow(df),
                    "observations removed with missing data."))
    }
    h <- cor.test(df$x, df$y, method = method, conf.level = CIwidth)

    if (method == "spearman") {
      rho   <- h$estimate
      names(rho) <- NULL
      alpha   <- 1 - CIwidth
      z_alpha <- abs(qnorm(alpha / 2))
      param_c <- (1 + rho^2 / 2)^(1 / 2)
      b  <- 3
      L1 <- 0.5 * (log(1 + rho) - log(1 - rho)) - (param_c * z_alpha) / sqrt(n - b)
      L2 <- 0.5 * (log(1 + rho) - log(1 - rho)) + (param_c * z_alpha) / sqrt(n - b)
      LL <- (exp(2 * L1) - 1) / (exp(2 * L1) + 1)
      UL <- (exp(2 * L2) - 1) / (exp(2 * L2) + 1)
    } else {
      LL <- h$conf.int[1]
      UL <- h$conf.int[2]
    }

    if (as_char) {
      rtn <- data.frame(
        rho = paste0(
          format(round(h$estimate, digits), nsmall = digits), " (",
          format(round(LL, digits), nsmall = digits), ", ",
          format(round(UL, digits), nsmall = digits), ")"
        )
      )
    } else {
      rtn <- data.frame(
        rho = rnd(h$estimate, digits),
        LL  = rnd(LL, digits),
        UL  = rnd(UL, digits)
      )
    }
    if (pvalue) rtn$p_value <- formatp(h$p.value)
    if (showN)  rtn$N <- n
  }

  names(rtn) <- gsub("rho", method, names(rtn))
  rownames(rtn) <- NULL
  return(rtn)
}


#' Format cor.test Output as Text
#'
#' Returns a character string "rho CI%(LL, UL)" from a cor.test result.
#'
#' @param corTest An object of class "htest" from cor.test.
#' @param digits Number of decimal places (default 2).
#' @param conf Confidence level used (default 0.95).
#' @return A character string.
#' @export
rm_cor <- function(corTest, digits = 2, conf = 0.95) {
  if (!inherits(corTest, "htest")) {
    stop("This function accepts the output of a call to cor.test")
  }
  paste0(
    rnd(corTest$estimate, digits = digits), " ",
    100 * conf, "%CI(",
    rnd(corTest$conf.int[1], digits = digits), ", ",
    rnd(corTest$conf.int[2], digits = digits), ")"
  )
}


#' Pairwise Correlations for All Variable Pairs
#'
#' Computes pairwise correlations for all column pairs in a data frame
#' with Fisher-z p-values and optional p-value adjustment.
#'
#' @param data A numeric data frame.
#' @param method Correlation method: "pearson" (default) or "spearman".
#' @param padj Method for p.adjust (default "none").
#' @return A data frame with v1, v2, rho, and adjusted p-value.
#' @export
corByVar <- function(data, method = "pearson", padj = "none") {
  rho_nn <- apply(combn(ncol(data), 2), 2, function(x) {
    rho <- cor(data[, x], method = method)[1, 2]
    n   <- sum(apply(data[, x], 1, function(r) any(!is.na(r))))
    data.frame(v1 = names(data)[x][1], v2 = names(data)[x][2], rho = rho, n = n)
  })
  rho_nn <- dplyr::bind_rows(rho_nn)
  rho_nn$fZ   <- abs(0.5 * log((1 + rho_nn$rho) / (1 - rho_nn$rho)) * sqrt(rho_nn$n - 3))
  rho_nn$pZ   <- 2 * pnorm(-rho_nn$fZ, lower.tail = TRUE)
  rho_nn$padj <- p.adjust(rho_nn$pZ, padj)
  attr(rho_nn, "p-adjust") <- padj
  rho_nn[, c("v1", "v2", "rho", "padj")]
}


# Correlated Performance (GEE) -------------------------------------------

#' Performance Statistics from a GEE Fit
#'
#' Extracts prevalence, sensitivity, and specificity with cluster-robust
#' standard errors from a GEE logistic regression (e.g. from geepack).
#' See Smith (1992) for methodology.
#'
#' @param gee_fit A GEE model fit (e.g. from geeglm).
#' @return A data frame with Statistic and CI columns.
#' @export
correlatedPerformance <- function(gee_fit) {
  y <- gee_fit$model$y
  prevalence  <- sum(y) / length(y)
  sensitivity <- exp(sum(gee_fit$coefficients)) / (1 + exp(sum(gee_fit$coefficients)))
  specificity <- 1 / (1 + exp(gee_fit$coefficients[1]))

  d1 <- rep(sensitivity * (1 - sensitivity), 2)
  d0 <- c(-specificity * (1 - specificity), 0)
  Vr <- vcov(gee_fit)

  se1 <- sqrt(d1 %*% Vr %*% d1)
  se0 <- sqrt(d0 %*% Vr %*% d0)

  out <- data.frame(
    Statistic = c("Prevalence", "Sensitivity", "Specificity"),
    Estimate  = c(prevalence, sensitivity, specificity),
    SE        = c(sqrt(prevalence * (1 - prevalence)), se1, se0)
  )
  out$LB <- out$Estimate - 1.96 * out$SE
  out$UB <- out$Estimate + 1.96 * out$SE

  prev <- binom.test(sum(y), length(y))
  out$LB[out$Statistic == "Prevalence"] <- prev$conf.int[1]
  out$UB[out$Statistic == "Prevalence"] <- prev$conf.int[2]

  out$CI <- paste0(rnd(out$Estimate), " (", rnd(out$LB), ", ", rnd(out$UB), ")")
  dplyr::select(out, Statistic, CI)
}


#' Compare Paired Diagnostic Performance (McNemar / Exact Binomial)
#'
#' Compares sensitivity and specificity between two diagnostic tests
#' using McNemar's test and the exact binomial test on discordant pairs.
#'
#' @param data Data frame containing test results and gold standard.
#' @param test1,test2 Column names of the two tests.
#' @param goldStandard Column name of the reference standard.
#' @param positiveCat The value denoting the positive category.
#' @param showMcNemar Logical; show McNemar's test results (default TRUE).
#' @param showExact Logical; show exact binomial test results (default TRUE).
#' @return A formatted data frame (via reportRmd::outTable).
#' @export
compare_performance <- function(data, test1, test2, goldStandard,
                                positiveCat, showMcNemar = TRUE,
                                showExact = TRUE) {

  .build_xtab <- function(class1, class2, lab_pos, lab_neg, test_name) {
    tab     <- data.frame(table(class1, class2))
    discord <- tab[tab$class1 != tab$class2, ]
    mtest   <- mcnemar.test(table(class1, class2))
    btest   <- binom.test(x = discord$Freq[1], n = sum(discord$Freq))

    xtab <- tab %>%
      dplyr::mutate(Freq = as.character(Freq)) %>%
      tidyr::pivot_wider(names_from = class2, values_from = Freq)

    xtab <- xtab %>%
      dplyr::add_row(
        class1 = "McNemar's ChiSq",
        !!lab_pos := rnd(mtest$statistic, 1),
        !!lab_neg := paste0("p=", formatp(mtest$p.value))
      ) %>%
      dplyr::add_row(
        class1 = "Exact test",
        !!lab_pos := NA,
        !!lab_neg := paste0("p=", formatp(btest$p.value))
      ) %>%
      dplyr::add_row(
        class1 = test_name,
        !!lab_pos := paste(test2, lab_pos),
        !!lab_neg := paste(test2, lab_neg),
        .before = 1
      )

    names(xtab) <- letters[1:3]
    list(xtab = xtab, p_value = btest$p.value)
  }

  # Sensitivity
  sens_data <- data[data[[goldStandard]] == positiveCat, ]
  sen1 <- factor(ifelse(sens_data[[test1]] == positiveCat, "TP", "FN"))
  sen2 <- factor(ifelse(sens_data[[test2]] == positiveCat, "TP", "FN"))
  sens_result <- .build_xtab(sen1, sen2, lab_pos = "FN", lab_neg = "TP",
                             test_name = test1)
  sens_xtab <- dplyr::add_row(sens_result$xtab, a = "Sensitivity", .before = 1)

  # Specificity
  spec_data <- data[data[[goldStandard]] != positiveCat, ]
  spec1 <- factor(ifelse(spec_data[[test1]] == positiveCat, "FP", "TN"))
  spec2 <- factor(ifelse(spec_data[[test2]] == positiveCat, "FP", "TN"))
  spec_result <- .build_xtab(spec1, spec2, lab_pos = "FP", lab_neg = "TN",
                             test_name = test1)
  spec_xtab <- dplyr::add_row(spec_result$xtab, a = "Specificity", .before = 1)

  out <- rbind(sens_xtab, spec_xtab)
  if (!showMcNemar) out <- out[!grepl("McNemar", out$a), ]
  if (!showExact)   out <- out[!grepl("Exact", out$a), ]

  to_bold <- which(is.na(out$b) & is.na(out$c))
  names(out) <- rep("", 3)

  reportRmd::outTable(out, to_bold = to_bold)
}


# Efron's Bootstrap Kappa -------------------------------------------------

#' Optimism-Adjusted Kappa for rpart Models
#'
#' Uses Efron's enhanced bootstrap (see Harrell, RMS pg 94) to compute
#' an optimism-adjusted kappa and accuracy for classification trees.
#'
#' @param model An rpart model object.
#' @param bs Number of bootstrap samples (default 1000).
#' @param bs_outer Number of outer bootstrap samples for double-bootstrap
#'   (default 100, not tested).
#' @param doubleBoot Logical; use double-bootstrap? (default FALSE, not tested).
#' @param weighted Logical; use quadratic weighted kappa (default TRUE).
#' @return A list with corrected_kappa (tibble), method description, and
#'   bibEntry references.
#' @importFrom psych cohen.kappa
#' @export
efron_kappa <- function(model, bs = 1000, bs_outer = 100,
                        doubleBoot = FALSE, weighted = TRUE) {
  if (!inherits(model, "rpart")) {
    stop("This function has been written for rpart models only")
  }

  N <- bs
  if (!doubleBoot) bs_outer <- 1
  which_kappa <- ifelse(weighted, "weighted kappa", "unweighted kappa")
  ylevels <- attr(model, "ylevels")

  kappa_dev <- suppressWarnings(
    psych::cohen.kappa(
      x = cbind(model$y, as.numeric(predict(model, type = "class")))
    )
  )

  mcall      <- model$call
  model_data <- eval(model$call$data)
  yind       <- which(unlist(lapply(model_data, function(x) {
    all(as.numeric(as.factor(x)) == model$y)
  })))
  yobs <- model_data[[yind]]
  n    <- length(yobs)

  acc_dev <- sum(model$y == as.numeric(predict(model, type = "class"))) / n
  opt_est <- acc_opt_est <- numeric(bs_outer)

  for (s in seq_len(bs_outer)) {
    bs_kappa   <- kappa_on_bs <- optimism     <- numeric(N)
    bs_acc     <- acc_on_bs   <- acc_optimism <- numeric(N)

    for (i in seq_len(N)) {
      bs_data <- model_data[sample(seq_len(nrow(model_data)), replace = TRUE), ]

      ht <- suppressWarnings(
        psych::cohen.kappa(
          x = cbind(
            as.numeric(factor(bs_data[[yind]], levels = ylevels)),
            as.numeric(predict(model, newdata = bs_data, type = "class"))
          )
        )
      )
      bs_kappa[i] <- ht$confid[which_kappa, 2]
      bs_acc[i]   <- sum(
        as.numeric(factor(bs_data[[yind]], levels = ylevels)) ==
          as.numeric(predict(model, newdata = bs_data, type = "class"))
      ) / n

      bs_fit <- update(model, . ~ ., data = bs_data)
      ht2 <- suppressWarnings(
        psych::cohen.kappa(
          x = cbind(
            model$y,
            as.numeric(predict(bs_fit, newdata = model_data, type = "class"))
          )
        )
      )
      kappa_on_bs[i] <- ht2$confid[which_kappa, 2]
      acc_on_bs[i]   <- sum(
        model$y == as.numeric(predict(bs_fit, newdata = model_data, type = "class"))
      ) / n

      optimism[i]     <- max(0, bs_kappa[i] - kappa_on_bs[i])
      acc_optimism[i] <- max(0, bs_acc[i] - acc_on_bs[i])
    }
    opt_est[s]     <- mean(optimism)
    acc_opt_est[s] <- mean(acc_optimism)
  }

  adj_kappa    <- ifelse(weighted, kappa_dev$weighted.kappa, kappa_dev$kappa) - opt_est
  adj_accuracy <- acc_dev - acc_opt_est
  acc_CI       <- psthr(quantile(bs_acc, c(0.5, 0.025, 0.975)))

  if (doubleBoot) {
    opt     <- psthr(quantile(opt_est, c(0.5, 0.025, 0.975)))
    opt_acc <- psthr(quantile(acc_opt_est, c(0.5, 0.025, 0.975)))
    adj     <- psthr(quantile(adj_kappa, c(0.5, 0.025, 0.975)))
    adj_acc <- psthr(quantile(adj_accuracy, c(0.5, 0.025, 0.975)))
  } else {
    opt     <- rnd(opt_est, 2)
    opt_acc <- rnd(acc_opt_est, 2)
    adj     <- rnd(adj_kappa, 2)
    adj_acc <- rnd(adj_accuracy, 2)
  }

  corrected_kappa <- tibble::tribble(
    ~Description,                         ~Value,
    "Kappa on full development sample",   psthr(kappa_dev$confid[which_kappa, c(2, 1, 3)]),
    "Optimism Estimate (Kappa)",          opt,
    "Optimism-Adjusted Kappa",            adj,
    "Accuracy on full development sample", acc_CI,
    "Optimism Estimate (Accuracy)",       opt_acc,
    "Optimism-Adjusted Accuracy",         adj_acc
  )
  names(corrected_kappa)[2] <- "Bootstrap Estimate (95% CI)"

  method <- paste(
    "Due to the small sample size Enfron's enhanced bootstrap validation was",
    "used to estimate the model performance following the method recommended",
    "by Harrell to adjust the Kappa measure of agreement for over-fitting",
    "[@Harrell2001] (pg94). We briefly explain this method. Bootstrapping",
    "re-sampling was used to derive confidence intervals for the model kappa.",
    "To adjust for over-fitting each bootstrap sample was also used to fit a",
    "new partitioning model, with different splits, the agreement of which was",
    "tested on the original data. The average kappa statistic of agreement of",
    "the bootstrap models on the original data was used as an estimate of the",
    "optimism. Finally, the optimism was subtracted from the final model",
    "accuracy to arrive at the 'optimism-adjusted' accuracy."
  )
  if (weighted) {
    method <- paste(method,
      "The quadratic weighted kappa was used in all calculations as",
      "recommended by Sim et al. [@Sim2005]"
    )
  } else {
    method <- paste(method,
      "Unweighted Kappa statistics were used in all calculations."
    )
  }

  bibEntry <- list(
    "@Book{Harrell2001,
  Author = {Harrell, Frank and Slaughter, James},
  Pages = {571},
  Title = {Regression Modeling Strategies : With Applications to Linear Models, Logistic Regression, and Survival Analysis},
  Year = {2001},
  Publisher = {Springer}
}",
    "@Article{Sim2005,
  Author = {Sim, Julius and Wright, Chris C.},
  Journal = {Physical Therapy},
  Pages = {257--268},
  Title = {The kappa statistic in reliability studies: Use, interpretation, and sample size requirements},
  Year = {2005},
  Number = {3},
  Volume = {85}
}",
    "@article{Landis1977,
  title={The measurement of observer agreement for categorical data},
  author={Landis, J. Richard and Koch, Gary G.},
  journal={Biometrics},
  volume={33},
  number={1},
  pages={159--174},
  year={1977}
}"
  )

  list(corrected_kappa = corrected_kappa, method = method, bibEntry = bibEntry)
}
