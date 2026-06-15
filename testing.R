# Generate test data ----
set.seed(123)
n <- 100

test_data <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n),
  x4 = rnorm(n),
  x5 = rnorm(n)
)

# Generate binary outcome with known signal from x1 and x2 ----
lp <- 0.5 * test_data$x1 - 0.8 * test_data$x2 + 0.3 * test_data$x3
test_data$outcome <- rbinom(n, 1, plogis(lp))

out <- glmnet_marker_selection(
  test_data,
  outcome_var = "outcome",
  candidate_markers = setdiff(names(test_data), "outcome"),
  forced_var = NULL,
  n_imputations = 5
)

attr(out, "method")


test_fit <- glm(outcome ~ ., data = test_data, family = binomial)
glm_performance(test_fit, stats = "classification")

# Test datasets for glmnet_marker_selection
compute_optimism_auc(test_fit, bs = 100, T)

set.seed(123)
n <- 50


# Dataset 1: Outliers
# Clean data with injected outliers in some markers

test_outliers <- data.frame(
  outcome = rbinom(n, 1, 0.4),
  sex = rbinom(n, 1, 0.5),
  marker1 = rnorm(n, 10, 2),
  marker2 = rnorm(n, 5, 1),
  marker3 = rnorm(n, 20, 3),
  marker4 = rnorm(n, 8, 1.5),
  marker5 = rnorm(n, 15, 2)
)

# Inject outliers ----
test_outliers$marker1[c(1, 2)] <- c(50, -20)
test_outliers$marker3[c(5, 10, 15)] <- c(100, -30, 80)
test_outliers$marker5[48] <- 200

# Add some true signal ----
test_outliers$outcome <- ifelse(
  0.5 *
    scale(test_outliers$marker2) -
    0.3 * scale(test_outliers$marker4) +
    rnorm(n, 0, 1.5) >
    0,
  1,
  0
)

out <- glmnet_marker_selection(
  test_outliers,
  outcome_var = "outcome",
  forced_var = NULL,
  n_imputations = 5
)
out
attr(out, "method")
# Dataset 2: Skewed distributions
# Right-skewed markers (typical of biomarker data)

test_skewed <- data.frame(
  outcome = rbinom(n, 1, 0.4),
  sex = rbinom(n, 1, 0.5),
  marker1 = rlnorm(n, 2, 1), # heavily right-skewed
  marker2 = rgamma(n, shape = 1, rate = 0.5), # moderate skew
  marker3 = rexp(n, rate = 0.2), # exponential
  marker4 = rlnorm(n, 3, 0.5), # right-skewed, higher mean
  marker5 = rnorm(n, 10, 2) # symmetric (control)
)

# Some zeros for log-transform edge case ----
test_skewed$marker2[c(3, 7)] <- 0
test_skewed$marker3[12] <- 0

# Add true signal via marker1 and marker3 ----
lp <- 0.6 *
  scale(log(test_skewed$marker1)) -
  0.4 * scale(test_skewed$marker3) +
  rnorm(n, 0, 1.2)
test_skewed$outcome <- ifelse(lp > 0, 1, 0)

out <- glmnet_marker_selection(
  test_skewed,
  outcome_var = "outcome",
  forced_var = NULL,
  n_imputations = 5
)

out
attr(out, "method")

# Dataset 3: Missing data
# Various patterns of missingness (MCAR)

test_missing <- data.frame(
  outcome = rbinom(n, 1, 0.4),
  sex = rbinom(n, 1, 0.5),
  marker1 = rnorm(n, 10, 2),
  marker2 = rnorm(n, 5, 1),
  marker3 = rnorm(n, 20, 3),
  marker4 = rnorm(n, 8, 1.5),
  marker5 = rnorm(n, 15, 2),
  marker6 = rnorm(n, 12, 2) # this one will exceed missingness threshold
)

# Add true signal ----
lp <- 0.5 *
  scale(test_missing$marker1) +
  0.4 * scale(test_missing$marker2) -
  0.3 * scale(test_missing$marker5) +
  rnorm(n, 0, 1.2)
test_missing$outcome <- ifelse(lp > 0, 1, 0)

# Light missingness (~10%) in some markers ----
test_missing$marker1[sample(n, 5)] <- NA
test_missing$marker2[sample(n, 4)] <- NA
test_missing$marker3[sample(n, 6)] <- NA

# Moderate missingness (~20%) ----
test_missing$marker4[sample(n, 10)] <- NA

# Heavy missingness (~40%) — should be excluded by default ----
test_missing$marker6[sample(n, 20)] <- NA

out <- glmnet_marker_selection(
  test_missing,
  outcome_var = "outcome",
  forced_var = NULL,
  n_imputations = 5
)

out
attr(out, "method")


# Dataset 4: Everything combined
# Outliers + skew + missing data + a binary marker

test_mixed <- data.frame(
  outcome = rbinom(n, 1, 0.4),
  sex = rbinom(n, 1, 0.5),
  marker1 = rlnorm(n, 2, 1), # skewed
  marker2 = rnorm(n, 10, 2), # normal with outliers
  marker3 = rgamma(n, shape = 2, rate = 0.3), # skewed
  marker4 = rnorm(n, 5, 1), # normal with missing
  marker5 = rbinom(n, 1, 0.3), # binary marker
  marker6 = rexp(n, rate = 0.1), # skewed + outliers
  marker7 = rnorm(n, 20, 3), # heavy missingness — should be excluded
  marker8 = rep(1, n) # zero-variance — should be dropped
)

# Inject outliers ----
test_mixed$marker2[c(1, 2, 3)] <- c(50, -15, 45)
test_mixed$marker6[c(10, 20)] <- c(200, 150)

# Inject zeros for log edge case ----
test_mixed$marker3[c(5, 8)] <- 0

# Inject missing data ----
test_mixed$marker1[sample(n, 4)] <- NA # ~8%
test_mixed$marker4[sample(n, 8)] <- NA # ~16%
test_mixed$marker6[sample(n, 3)] <- NA # ~6%
test_mixed$marker7[sample(n, 18)] <- NA # ~36% — should trigger exclusion

# Add true signal via marker1, marker3 and marker5 ----
lp <- 0.5 *
  scale(log(test_mixed$marker1 + 1)) +
  0.4 * scale(test_mixed$marker3) +
  0.6 * test_mixed$marker5 -
  0.3 * scale(test_mixed$marker4) +
  rnorm(n, 0, 1)
test_mixed$outcome <- ifelse(lp > median(lp, na.rm = T), 1, 0)

out <- glmnet_marker_selection(
  test_mixed,
  outcome_var = "outcome",
  forced_var = NULL,
  n_imputations = 5
)

out
attr(out, "method")


val_mi_auc(
  df = out$processed_data,
  outcome_var = "outcome",
  predictors = c(
    "log_marker1",
    "log_marker4",
    "log_marker6",
    "marker5",
    "sex",
    "sqrt_marker3"
  ),
  method = "0.632+"
)

test_fit <- glm(
  outcome ~ log_marker1 + log_marker4 + sex + log_marker6,
  data = out$analysed_data,
  family = "binomial"
)

# ========================================================================
# Dataset 5: Character and factor variables
# ========================================================================

test_factors <- data.frame(
  outcome = rbinom(n, 1, 0.4),
  sex = factor(sample(c("M", "F"), n, replace = TRUE)),
  marker1 = rnorm(n, 10, 2),
  marker2 = rnorm(n, 5, 1),
  marker3 = factor(
    sample(c("low", "medium", "high"), n, replace = TRUE),
    levels = c("low", "medium", "high")
  ),
  marker4 = sample(c("A", "B", "C", "D"), n, replace = TRUE),
  marker5 = rnorm(n, 8, 1.5),
  marker6 = factor(rep("only_level", n)), # single level — should join zero_var
  stringsAsFactors = FALSE
)

# Add true signal ----
lp <- 0.5 *
  scale(test_factors$marker1) +
  0.4 * scale(test_factors$marker2) +
  0.6 * ifelse(test_factors$marker3 == "high", 1, 0) -
  0.3 * ifelse(test_factors$marker4 == "D", 1, 0) +
  rnorm(n, 0, 1)
test_factors$outcome <- ifelse(lp > median(lp), 1, 0)

test_outliers

rm(list = ls())
set.seed(123)
n <- 100

test_data <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n),
  x4 = rnorm(n),
  x5 = rnorm(n)
)

# Generate binary outcome with known signal from x1 and x2 ----
lp <- 0.5 * test_data$x1 - 0.8 * test_data$x2 + 0.3 * test_data$x3
test_data$outcome <- rbinom(n, 1, plogis(lp))
test_fit <- glm(outcome ~ ., data = test_data, family = binomial)
glm_performance(test_fit, stats = "classification")

# Test datasets for glmnet_marker_selection

compute_optimism_auc <- function(glm_fit, bs) {
  model_data <- glm_fit$model
  n <- nrow(model_data)
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

  return(list(
    apparent_auc = apparent_auc,
    optimism = optimism,
    corrected_auc = corrected_auc,
    auc_632 = auc_632,
    auc_632plus = auc_632plus
  ))
}
compute_optimism_auc(test_fit, bs = 100)


# Test Script for calculate_nri() ----
source("glm_functions.R")


# Pencina et al. (2008) categorical example ----
# Expected: NRI events ~0.12, non-events ~-0.008, overall ~0.11
df_cat <- data.frame(
  obs_events = c(rep(1, 183), rep(0, 3081)),
  m1_prediction = c(
    rep(1, 54),
    rep(2, 105),
    rep(3, 24),
    rep(1, 2101),
    rep(2, 882),
    rep(3, 98)
  ),
  m2_prediction = c(
    rep(1, 43),
    rep(2, 105),
    rep(3, 35),
    rep(1, 2108),
    rep(2, 870),
    rep(3, 103)
  )
)


df_cat |> calculate_nri(obs_events, m1_prediction, m2_prediction)


# Install and load the package
# install.packages("PredictABEL")
library(PredictABEL)

# specify dataset with outcome and predictor variables
data(ExampleData)
# specify column number of the outcome variable
cOutcome <- 2

# fit logistic regression models
# all steps needed to construct a logistic regression model are written in a function
# called 'ExampleModels', which is described on page 4-5
riskmodel1 <- ExampleModels()$riskModel1
riskmodel2 <- ExampleModels()$riskModel2

# obtain predicted risks
predRisk1 <- predict(riskmodel1, type = "response")
predRisk2 <- predRisk(riskmodel2, type = "response")
# specify cutoff values for risk categories
cutoff <- c(0, .5, 1)

# compute reclassification measures
reclassification(
  data = ExampleData,
  cOutcome = cOutcome,
  predrisk1 = predRisk1,
  predrisk2 = predRisk2,
  cutoff
)
