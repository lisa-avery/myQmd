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
