# Evaluation metrics. Macro-F1 follows the original definition: a class with no
# predicted or no actual members contributes an F1 of 0 instead of NA.

#' Per-class precision, recall and F1
#'
#' @param truth Factor of observed classes.
#' @param estimate Factor of predicted classes (same levels as `truth`).
#' @return A tibble with one row per class.
#' @export
per_class_metrics <- function(truth, estimate) {
  check_pair(truth, estimate)
  lev <- levels(truth)
  estimate <- factor(estimate, levels = lev)
  purrr::map_dfr(lev, function(l) {
    tp <- sum(truth == l & estimate == l)
    fp <- sum(truth != l & estimate == l)
    fn <- sum(truth == l & estimate != l)
    precision <- if (tp + fp == 0) 0 else tp / (tp + fp)
    recall <- if (tp + fn == 0) 0 else tp / (tp + fn)
    f1 <- if (precision + recall == 0) 0 else 2 * precision * recall / (precision + recall)
    tibble::tibble(class = l, precision = precision, recall = recall, f1 = f1, support = tp + fn)
  })
}

#' Macro-F1, Cohen's kappa and accuracy
#'
#' @inheritParams per_class_metrics
#' @return A named numeric vector: `macro_f1`, `kappa`, `accuracy`.
#' @export
classification_metrics <- function(truth, estimate) {
  check_pair(truth, estimate)
  estimate <- factor(estimate, levels = levels(truth))
  tab <- table(estimate, truth)
  n <- sum(tab)
  observed <- sum(diag(tab)) / n
  expected <- sum(rowSums(tab) * colSums(tab)) / n^2
  kappa <- if (expected == 1) 0 else (observed - expected) / (1 - expected)
  c(
    macro_f1 = mean(per_class_metrics(truth, estimate)$f1),
    kappa = kappa,
    accuracy = observed
  )
}

#' Stratified bootstrap confidence intervals for the test metrics
#'
#' Resamples within each true class (as in the original analysis) so every
#' replicate keeps the observed class proportions.
#'
#' @inheritParams per_class_metrics
#' @param times Number of bootstrap replicates.
#' @param seed Random seed.
#' @param level Confidence level.
#' @return A tibble with the point estimate and interval for each metric.
#' @export
bootstrap_ci <- function(truth, estimate, times = 1000, seed = 5003, level = 0.95) {
  check_pair(truth, estimate)
  if (times < 1) {
    abort_data("`times` must be at least 1.")
  }
  by_class <- split(seq_along(truth), truth)
  by_class <- by_class[lengths(by_class) > 0]
  boot <- withr::with_seed(seed, t(replicate(times, {
    i <- unlist(lapply(by_class, function(ix) ix[sample.int(length(ix), length(ix), replace = TRUE)]))
    classification_metrics(truth[i], estimate[i])
  })))
  alpha <- (1 - level) / 2
  point <- classification_metrics(truth, estimate)
  tibble::tibble(
    metric = names(point),
    estimate = unname(point),
    lower = unname(apply(boot, 2, stats::quantile, probs = alpha)),
    upper = unname(apply(boot, 2, stats::quantile, probs = 1 - alpha))
  )
}

#' Share of test rows whose exact predictor vector also appears in training
#'
#' A direct measure of split leakage: such rows are effectively memorised.
#'
#' @param train,test Frames from [model_frame()] (with `spec_group`).
#' @return A proportion between 0 and 1.
#' @export
twin_overlap <- function(train, test) {
  if (nrow(test) == 0) {
    return(0)
  }
  mean(test$spec_group %in% train$spec_group)
}

check_pair <- function(truth, estimate) {
  if (!is.factor(truth)) {
    abort_data("`truth` must be a factor.")
  }
  if (length(truth) != length(estimate)) {
    abort_data("`truth` and `estimate` must have the same length.")
  }
  if (length(truth) == 0) {
    abort_data("Cannot compute metrics on zero observations.")
  }
  invisible(TRUE)
}
