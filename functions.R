# Helper Functions ------------

#' Round Numbers with Trailing Zeros
#'
#' @description
#' Rounds numbers to a specified number of decimal places while preserving trailing zeros.
#' This function is useful for creating consistent numeric formatting in output tables.
#'
#' @param x Numeric vector to be rounded
#' @param digits Integer indicating the number of decimal places (default: 2)
#'
#' @return Character vector of the rounded numbers with trailing zeros preserved
#'
#' @examples
#' rnd(c(1.2345, 5.67, 9))  # Returns c("1.23", "5.67", "9.00")
#' rnd(c(1.2345, 5.67, 9), digits = 3)  # Returns c("1.235", "5.670", "9.000")
#'
#' @export
rnd <- function(x, digits = 2) {
  out <- sapply(x, function(x) {
    format(round(x, digits), nsmall = digits)
  })
  out <- unlist(out, use.names = FALSE)
  return(out)
}

#' Fast Area Under the ROC Curve (AUC) Calculation
#'
#' @description
#' Calculates the Area Under the ROC Curve (AUC) efficiently for binary classification.
#' Can accept either predictions and true classes or a glm model object.
#'
#' @param probs A vector of probabilities/scores or a glm model object
#' @param class A vector denoting class membership (must be binary: 0 or 1)
#'
#' @return Numeric value representing the AUC (between 0 and 1)
#'
#' @examples
#' # With probability vector and class vector
#' probs <- runif(100)
#' class <- sample(0:1, 100, replace = TRUE)
#' fastAUC(probs, class)
#'
#' # With a glm model
#' # model <- glm(y ~ x, family = binomial(), data = mydata)
#' # fastAUC(model)
#'
#' @export
fastAUC <- function(probs, class) {
  if (inherits(probs, "glm")) {
    class <- probs$y
    probs <- predict(probs, type = 'response')
  }
  x <- probs
  y <- class
  x1 = x[y == 1]; n1 = length(x1);
  x2 = x[y == 0]; n2 = length(x2);
  r = rank(c(x1, x2))
  auc = (sum(r[1:n1]) - n1 * (n1 + 1) / 2) / n1 / n2
  return(auc)
}

#' Extract ROC Curve Data from Model or Predictions
#'
#' @description
#' Extracts data needed to plot a ROC curve from either a glm model object or
#' predictions with observed classes. Also calculates AUC and Youden's index
#' for determining optimal cutoff points.
#'
#' @param fitted_obj Either a glm model object or a data frame with predictions and observations
#' @param digits Number of digits for rounding the AUC value (default: 2)
#'
#' @return A list containing:
#'   \item{data}{Data frame with columns: x (FPR), y (TPR), Youden (TPR-FPR), and cutoff}
#'   \item{auc}{AUC value formatted according to digit specification}
#'
#' @examples
#' # With a glm model
#' # model <- glm(y ~ x, family = binomial(), data = mydata)
#' # roc_data <- get_roc_data(model)
#'
#' # With predictions and observations
#' # preds_obs <- data.frame(predictions = runif(100), observed = sample(0:1, 100, replace = TRUE))
#' # roc_data <- get_roc_data(preds_obs)
#'
#' @export
get_roc_data <- function(fitted_obj, digits = 2) {
  if (inherits(fitted_obj, 'glm')) {
    class_p <- predict(fitted_obj, type = 'response')
    obs = fitted_obj$y
    roc_obj <- fastROC(class_p, obs)
    auc <- fastAUC(class_p, obs)
    #  if (missing(title)) title = as.character(fitted_obj$call)[2]
  } else {
    roc_obj <- fastROC(fitted_obj[, 1], fitted_obj[, 2])
    auc <- fastAUC(fitted_obj[, 1], fitted_obj[, 2])
    # if (missing(title)) title = NULL
  }
  auc <- niceNum(auc, digits)
  # TODO: Add different techniques for calculating cut points - see paper
  df = data.frame(x = roc_obj$fpr,
                  y = roc_obj$tpr,
                  Youden = roc_obj$tpr - roc_obj$fpr,
                  cutoff = roc_obj$threshold)
  return(list(data = df, auc = auc))
}

#' Numbers nicely formatted to string
#'
#' @param x numbers to format
#' @param digits number of digits to retain after decimal place
#'   checked for and retained.
#' @keywords helper
#'
#' @return Character vector with formatted numbers
#'
#' @examples
#' niceNum(c(1.2345, 5.67, 9))  # Returns c("1.23", "5.67", "9.00")
#'
#' @export
niceNum <- function(x, digits = 2) {
  rndx = sapply(x, function(x) {
    if (is.na(x)) return(x)
    if (is.null(x)) return(x)
    format(round(as.numeric(x), digits), nsmall = digits)
  })
  return(gsub(" ", "", rndx))
}


#' Convert Hypothesis Test Objects to Data Frames
#'
#' @description
#' Converts an object of class "htest" (returned by R's statistical test functions)
#' into a data frame for easier manipulation and integration with other analyses.
#'
#' @param htest_object An object of class "htest", typically returned by statistical test
#' functions like t.test(), wilcox.test(), cor.test(), etc.
#'
#' @return A data frame with one row containing all the components of the hypothesis test.
#' The columns include:
#' \itemize{
#'   \item method: The name of the statistical test
#'   \item statistic: The test statistic
#'   \item p.value: The p-value of the test
#'   \item parameter: Degrees of freedom or other parameters (if present)
#'   \item conf.int: Confidence intervals (if present, may be split into conf.int1, conf.int2)
#'   \item estimate: Point estimates (if present, may be split into multiple columns)
#'   \item alternative: The alternative hypothesis specification (if present)
#'   \item data.name: The name of the data used in the test (if present)
#'   \item null.value: The null hypothesis value (if present)
#' }
#'
#' @details
#' This function extracts all components from an "htest" object and arranges them into
#' a single-row data frame. For components that contain multiple values (like confidence
#' intervals or multiple estimates), the values are separated into different columns with
#' sequential numbering.
#'
#' The function is particularly useful when collecting results from multiple tests,
#' as the resulting data frames can be easily combined using dplyr::bind_rows() (but not rbind because the number of columns may differ)
#'
#' @examples
#' # Convert a t-test result to a data frame
#' t_result <- t.test(1:10, y = c(7:20))
#' t_df <- htest_dataframe(t_result)
#' print(t_df)
#'
#' # Convert a correlation test result
#' cor_result <- cor.test(1:10, 2:11, method = "spearman")
#' cor_df <- htest_dataframe(cor_result)
#' print(cor_df)
#'
#' # Combine multiple test results
#' tests_list <- list(
#'   t_test = t.test(1:10, y = c(7:20)),
#'   wilcox = wilcox.test(1:10, c(7:20))
#' )
#' results_df <- do.call(dplyr::bind_rows, lapply(tests_list, htest_dataframe))
#'
#' @export
htest_dataframe <- function(htest_object) {
  if (!inherits(htest_object, "htest")) stop("htest_object must be an object returned from a statistical test of class htest.")
  if (is.null(htest_object$method)) stop("htest_object must be an object returned from a statistical test of class htest with a methods component.")
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


# Model Evaluation Functions ------------
#' Compute the optimism of the observed AUC through bootstrapping
#'
#' @description
#' Examines the model AUC and uses Efron's method to calculate an optimism
#' estimate by which to correct AUC when internal validation is necessary.
#' This function performs bootstrap-based internal validation to correct for
#' optimism in the AUC statistic.
#'
#' @param model Output from a call to glm with family=binomial
#' @param bs Number of bootstrap samples to use (default: 1000)
#'
#' @return A tibble with three rows:
#' \itemize{
#'   \item Unadjusted AUC on sample
#'   \item Bootstrapped Optimism Estimate
#'   \item Bootstrapped Optimism-Adjusted AUC
#' }
#'
#' @details
#' Implements Efron's enhanced bootstrap method for estimating optimism in the
#' Area Under the ROC Curve. This adjustment is important for internal validation
#' when the same data is used for model development and evaluation.
#'
#' @references
#' Harrell, F. E., & Slaughter, J. C. (2001). Regression Modeling Strategies:
#' With Applications to Linear Models, Logistic Regression, and Survival Analysis. Springer.
#'
#' @examples
#' # With a logistic regression model
#' # fit <- glm(y ~ x1 + x2, family = binomial(), data = mydata)
#' # optimism_results <- efron_auc(fit, bs = 500)
#'
#' @export
efron_auc <- function(model, bs = 1000) {
  #Using Enfron's enhanced bootstrap (See RMS pg94 Harrell)
  if (!inherits(model, "glm")) stop("This function has been written for glm models only")
  N <- bs
  auc_dev = fastAUC(predict(model), model$y)
  mcall <- model$call
  model_data <- model$model
  bs_auc <- optimism <- bs_on_orig <- numeric(N)
  for (i in 1:N) {
    
    # Accuracy Index -auc across bootstraps
    bs_data <- model_data[sample(1:nrow(model_data), replace = T), ]
    bs_fit <- update(model, . ~ ., data = bs_data)
    bs_on_orig[i] <- fastAUC(predict(bs_fit, newdata = model_data), model$y)
    bs_auc[i] <- fastAUC(predict(bs_fit), bs_fit$y)
    # auc of predicting observed from bootstrap models
    #Optimism
    optimism[i] <- bs_auc[i] - bs_on_orig[i]
  }
  
  corrected_auc <- tribble(~Description, ~Value,
                           "Unadjusted AUC on sample", rnd(auc_dev),
                           "Bootstrapped Optimism Estimate", rnd(max(0, mean(optimism)), 2),
                           "Bootstrapped Optimism-Adjusted AUC", rnd(auc_dev - mean(optimism), 2))
  ref <- "@Book{Harrell2001,
  Author = {Harrell, Frank and Slaughter, James},
  Pages = {571},
  Title = {Regression Modeling Strategies : With Applications to Linear Models, Logistic Regression, and Survival Analysis},
  Year = {2001},
  Publisher = {Springer}
}"
  if (interactive()) cat(ref)
  return(corrected_auc)
  
}

#' Check Model Assumptions using DHARMa
#'
#' @description
#' Performs diagnostic tests on a model using the DHARMa package to check for violations
#' of model assumptions. Tests include uniformity of residuals, dispersion, and outliers.
#'
#' @param model A fitted model object compatible with DHARMa (glm, lm, glmm, etc.)
#' @param nsim Number of simulations for creating DHARMa residuals (default: 1000)
#'
#' @return A data frame containing the results of diagnostic tests:
#' \itemize{
#'   \item uniformity: Tests whether the residuals are uniformly distributed
#'   \item dispersion: Tests whether the model shows signs of over/underdispersion
#'   \item outliers: Tests for the presence of outliers in the model
#' }
#'
#' @details
#' This function requires the DHARMa package to be installed. It uses DHARMa's
#' simulation-based approach to create scaled residuals that are standardized to
#' a uniform distribution under the null hypothesis that the model is correctly specified.
#'
#' @examples
#' # Assuming a fitted model and DHARMa package installed:
#' # model <- glm(y ~ x, family = poisson(), data = mydata)
#' # assumption_tests <- check_model_assumptions(model, nsim = 500)
#'
#' @export
check_model_assumptions <- function(model, nsim = 1000) {
  # Check if DHARMa is installed and load it
  if (!requireNamespace("DHARMa", quietly = TRUE)) {
    stop("The DHARMa package is required. Please install it with install.packages('DHARMa')")
  }
  # Create DHARMa residuals
  sim_residuals <- DHARMa::simulateResiduals(model, n = nsim)
  tests_output <- list()
  tests_output$uniformity <- DHARMa::testUniformity(sim_residuals, plot = F)
  tests_output$dispersion <- DHARMa::testDispersion(sim_residuals, plot = F)
  tests_output$outliers <- DHARMa::testOutliers(sim_residuals, plot = F)
  out <- bind_rows(lapply(tests_output, function(ht) htest_dataframe(ht)), .id = "Test")
  return(out)
}


#' Receiving Operating Curve Plot
#'
#' @description
#' Creates a ggplot visualization of ROC curves from regression models or data frames
#' containing score and class data. Supports plotting multiple ROC curves for comparison.
#'
#' @param fitted_obj Either an object output from the glm function (must be from
#'   a logistic regression), a two-column dataframe in the format
#'   (RiskScore,Label), or a list of glm objects for multiple curves
#' @param showAUC Logical, should the AUC be plotted on the graph (default: TRUE)
#' @param showCut Logical, should the cut point be shown on the graph (default: FALSE)
#' @param plotOnly Logical, should both the plot and the AUC be returned (default: TRUE)
#' @param title Character string with graph title, defaults to the calling
#'   function, use title=NULL to leave empty
#' @param digits The number of digits to use for the AUC (default: 2)
#' @param model_names Character vector of names to label ROC curves with
#' @param fsize Default font size for all text elements (default: 10)
#' @param lsize Default line size for roc curves (default: 0.8)
#'
#' @return If plotOnly=TRUE, returns a ggplot object. If plotOnly=FALSE, returns a list
#'   containing the plot and AUC value(s).
#'
#' @details
#' TODO: Not robust - this needs to be updated to allow for missing data.
#'
#' @import ggplot2
#' @keywords plot
#'
#' @examples
#' # Single model ROC curve
#' # model <- glm(y ~ x, family = binomial(), data = mydata)
#' # roc_plot <- gg_roc(model, showAUC = TRUE)
#'
#' # Multiple model comparison
#' # models <- list(model1 = glm(y ~ x1, family = binomial(), data = mydata),
#' #                model2 = glm(y ~ x1 + x2, family = binomial(), data = mydata))
#' # roc_comparison <- gg_roc(models, showAUC = TRUE)
#'
#' @export
gg_roc <- function(fitted_obj, showAUC = T, showCut = F, plotOnly = T, title = NULL, digits = 2, model_names = NULL, fsize = 10, lsize = .8) {
  if (inherits(fitted_obj, 'list')) {
    if (!is.null(model_names)) {
      if (length(model_names) != length(fitted_obj)) stop('model_names and fitted_objects must have the same length')
      names(fitted_obj) <- model_names
    }
    if (is.null(names(fitted_obj))) names(fitted_obj) <- paste("Model", 1:length(fitted_obj))
    obj_auc <- unlist(lapply(fitted_obj, function(x) fastAUC(x)))
    obj_ord <- order(obj_auc, decreasing = T)
    fitted_obj <- fitted_obj[obj_ord]
    roc_objects <- lapply(fitted_obj, function(x) get_roc_data(x, digits))
    roc_data <- lapply(seq_along(roc_objects), function(x) {
      z = roc_objects[[x]]$data
      z$model = names(roc_objects)[x]
      return(z)
    })
    df <- bind_rows(roc_data) |>
      arrange(x, y)
    model_lbl <- data.frame(model_nm = nicename(names(obj_auc)),
                            model = names(obj_auc),
                            auc = obj_auc) |>
      mutate(newName = paste0(nicename(model), " (AUC=", rnd(auc, digits), ")"))
    df <- full_join(df, model_lbl)
    if (showAUC) {
      df$model <- df$newName
    } else df$model <- df$model_nm
    df <- df |>
      mutate(model = forcats::fct_reorder(model, auc, .desc = T))
    p <- ggplot(data = df,
                aes(x = x, y = y, colour = model, linetype = model))
    auc_txt <- NULL
    
  } else {
    roc_objects <- get_roc_data(fitted_obj, digits)
    cutpoint = roc_objects$data$cutoff[which.max(roc_objects$data$Youden)]
    df <- roc_objects$data |>
      arrange(x, y)
    p = ggplot(data = df,
               aes(x = x, y = y))
    auc_txt <- paste("AUC =", roc_objects$auc)
  }
  
  p <- p +
    geom_line(size = lsize) +
    theme_bw() +
    theme(legend.title = element_blank(),
          text = element_text(size = fsize)) +
    xlab("1-Specificity (FPR)") +
    ylab("Sensitivity (TPR)")
  if (showAUC) {
    if (!is.null(auc_txt)) {
      p <- p + annotate(geom = 'text', x = 1, y = .1, label = auc_txt, hjust = 'right')
    } else {
      p <- p + theme(
        legend.position = c(.95, 0),
        legend.text = element_text(size = fsize),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(2, 2, 2, 2),
        legend.background = element_rect(fill = "transparent", color = NA))
    }
  }
  if (showCut & !inherits(fitted_obj, 'list')) {
    p = p + geom_segment(
      aes(x = .5, y = .5, xend = x[which.max(Youden)], yend = y[which.max(Youden)]),
      arrow = arrow(length = unit(0.03, "npc")))
    p = p + annotate(geom = 'text', x = .5, y = .5, label = paste('cutpoint =', round(cutpoint, 2)), hjust = 0, vjust = 1)
  }
  if (!is.null(title)) {
    p = p + ggtitle(title) + theme(plot.title = element_text(size = 11))
  }
  if (plotOnly) {
    return(p)
  } else {
    return(list(plot = p, auc = auc))
  }
}

#' Compute ROC Statistics
#'
#' @description
#' Computes the true positive rate (sensitivity) and false positive rate (1-specificity)
#' for ROC curves at different probability thresholds.
#'
#' @param probs A vector of probabilities or scores, usually from predict
#' @param class A vector denoting class membership, must be binary (0 or 1)
#'
#' @return A data frame with columns:
#' \itemize{
#'   \item threshold: Cutoff values for classification
#'   \item tpr: True Positive Rate (Sensitivity)
#'   \item fpr: False Positive Rate (1-Specificity)
#' }
#'
#' @details
#' The function calculates sensitivity and 1-specificity for each unique score/probability
#' threshold. The returned cutpoints are mid-way between the scores to ensure no ambiguity
#' as to whether the true cut-point is < vs <= the value. The output agrees with that
#' provided by the pROC package.
#'
#' Not designed to be called on its own, does not contain error checking or
#' remove missing data.
#'
#' @examples
#' # Basic usage
#' probs <- runif(100)
#' class <- sample(0:1, 100, replace = TRUE)
#' roc_data <- fastROC(probs, class)
#'
#' @export
fastROC <- function(probs, class) {
  
  p_ord <- order(probs, decreasing = F)
  probs_sorted <- probs[p_ord]
  cp <- c(-Inf, rowMeans(cbind(unique(probs_sorted)[-1], head(unique(probs_sorted), -1))), Inf) # gives same thresholds as pROC
  class_sorted <- class[p_ord]
  TPR = sapply(cp, function(x) sum(class_sorted[probs_sorted > x]) / sum(class_sorted))
  FPR = sapply(cp, function(x) sum(1 - class_sorted[probs_sorted > x]) / sum(class_sorted == 0))
  return(data.frame(threshold = cp, tpr = TPR, fpr = FPR))
  
}
