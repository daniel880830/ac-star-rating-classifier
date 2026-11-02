# Reproduce the original caret-based results end to end from the raw CSV.
#
# This keeps the original modelling code (caret + recipes + themis, random
# 80/20 split, 3-fold x 2 repeated CV, seeds 42 and 5003) and only swaps the
# hard-coded file paths for the package's cleaning pipeline.
#
# Usage: Rscript scripts/reproduce_original.R [--all-models]
#   --all-models  also fit the SVM-RBF, GLMnet, LDA and kNN comparison models.
#                 The linear SVM (caret "svmLinear3") needs LiblineaR, which is
#                 not a project dependency, so it is skipped.

suppressPackageStartupMessages({
  pkgload::load_all(quiet = TRUE)
  library(caret)
  library(recipes)
  library(themis)
})

args <- commandArgs(trailingOnly = TRUE)
all_models <- "--all-models" %in% args
settings <- load_settings(profile = "original")
started <- Sys.time()

# --- Data: raw CSV -> cleaning -> EDA filter -> top-23 hand-off ---------------
data <- build_dataset(settings$paths$raw_data)
df <- csv_roundtrip(dplyr::bind_cols(data[TARGET], data[ORIGINAL_TOP23]))
df <- df |>
  dplyr::filter(!is.na(c_star_mixed)) |>
  dplyr::mutate(star3 = cut(c_star_mixed,
    breaks = c(-Inf, 2.5, 4.0, Inf), labels = c("low", "mid", "high"),
    right = TRUE, include.lowest = TRUE
  ))

set.seed(42)
idx <- caret::createDataPartition(df$star3, p = 0.80, list = FALSE)
train <- droplevels(df[idx, ])
test <- droplevels(df[-idx, ])

# --- Metric used for model selection ----------------------------------------
macro_f1_kappa <- function(data, lev = NULL, model = NULL) {
  if (is.null(lev)) lev <- levels(factor(data$obs))
  keep <- stats::complete.cases(data$obs, data$pred)
  obs <- factor(data$obs[keep], levels = lev)
  pred <- factor(data$pred[keep], levels = lev)
  pr <- sapply(lev, function(l) {
    tp <- sum(obs == l & pred == l)
    fp <- sum(obs != l & pred == l)
    if ((tp + fp) == 0) 0 else tp / (tp + fp)
  })
  rc <- sapply(lev, function(l) {
    tp <- sum(obs == l & pred == l)
    fn <- sum(obs == l & pred != l)
    if ((tp + fn) == 0) 0 else tp / (tp + fn)
  })
  f1 <- mean(ifelse((pr + rc) == 0, 0, 2 * pr * rc / (pr + rc)))
  kappa <- as.numeric(caret::confusionMatrix(pred, obs)$overall["Kappa"])
  c(Mean_F1 = f1, Kappa = kappa, Accuracy = mean(obs == pred))
}

# --- Recipes (fold-internal preprocessing) -----------------------------------
base_rec <- recipe(star3 ~ ., data = train) |>
  step_rm(any_of(c("c_star_mixed", "ExpDate"))) |>
  step_zv(all_predictors())
common <- function(rec, one_hot) {
  rec |>
    step_novel(all_nominal_predictors(), new_level = "new") |>
    step_unknown(all_nominal_predictors(), new_level = "unknown") |>
    step_other(all_nominal_predictors(), threshold = 0.02, other = "other") |>
    step_dummy(all_nominal_predictors(), one_hot = one_hot)
}
rec_nonlinear <- common(base_rec, TRUE) |>
  step_zv(all_predictors()) |>
  step_impute_median(all_numeric_predictors()) |>
  step_smote(star3, over_ratio = 1.5, neighbors = 5) |>
  step_center(all_numeric_predictors()) |>
  step_scale(all_numeric_predictors())
rec_knn_pca <- common(base_rec, TRUE) |>
  step_zv(all_predictors()) |>
  step_impute_median(all_numeric_predictors()) |>
  step_smote(star3, over_ratio = 1.5, neighbors = 5) |>
  step_normalize(all_numeric_predictors()) |>
  step_pca(all_numeric_predictors(), threshold = 0.95)
rec_linear <- common(base_rec, FALSE) |>
  step_impute_median(all_numeric_predictors()) |>
  step_smote(star3, over_ratio = 1.5, neighbors = 5) |>
  step_corr(all_numeric_predictors(), threshold = 0.95) |>
  step_lincomb(all_predictors())

# --- Cross-validation and grids ---------------------------------------------
set.seed(42)
folds <- caret::createMultiFolds(train$star3, k = 3, times = 2)
ctrl <- caret::trainControl(
  method = "repeatedcv", index = folds, classProbs = TRUE,
  summaryFunction = macro_f1_kappa, savePredictions = "final", verboseIter = FALSE
)
grid_rf <- expand.grid(
  mtry = c(2, 4, 6, 8), splitrule = c("gini", "extratrees"), min.node.size = c(1, 3, 5)
)

fits <- list()
set.seed(42)
fits$RF <- train(rec_nonlinear,
  data = train, method = "ranger", trControl = ctrl, metric = "Mean_F1",
  tuneGrid = grid_rf, importance = "permutation", num.trees = 100
)
if (all_models) {
  set.seed(42)
  fits$SVMrbf <- train(rec_nonlinear,
    data = train, method = "svmRadial", trControl = ctrl, metric = "Mean_F1",
    tuneGrid = expand.grid(sigma = exp(seq(log(0.01), log(0.20), length = 5)), C = 2^(0:4))
  )
  set.seed(42)
  fits$GLMnet <- train(rec_linear,
    data = train, method = "glmnet", trControl = ctrl, metric = "Mean_F1",
    tuneGrid = expand.grid(alpha = seq(0, 1, by = 0.25), lambda = 10^seq(-3, 1, length = 15))
  )
  set.seed(42)
  fits$LDA <- train(rec_linear, data = train, method = "lda", trControl = ctrl, metric = "Mean_F1")
  set.seed(42)
  fits$kNN <- train(rec_knn_pca,
    data = train, method = "kknn", trControl = ctrl, metric = "Mean_F1",
    tuneGrid = expand.grid(kmax = seq(3, 41, 2), distance = 2, kernel = "triangular")
  )
}

cv_table <- purrr::imap_dfr(fits, function(fit, name) {
  r <- fit$resample
  tibble::tibble(
    model = name,
    cv_macro_f1_mean = mean(r$Mean_F1), cv_macro_f1_sd = stats::sd(r$Mean_F1),
    cv_kappa_mean = mean(r$Kappa), cv_kappa_sd = stats::sd(r$Kappa)
  )
})

# --- Test-set evaluation of the selected model (RF) ---------------------------
pred <- predict(fits$RF, newdata = test)
y <- droplevels(test$star3)
yh <- factor(pred, levels = levels(y))
point <- macro_f1_kappa(data.frame(obs = y, pred = yh), lev = levels(y))

set.seed(5003)
by_class <- split(seq_along(y), y)
boot <- t(replicate(1000, {
  i <- unlist(lapply(by_class, function(ix) sample(ix, length(ix), replace = TRUE)), use.names = FALSE)
  macro_f1_kappa(data.frame(obs = y[i], pred = yh[i]), lev = levels(y))
}))
ci <- apply(boot, 2, stats::quantile, probs = c(0.025, 0.975))

result <- list(
  description = "Original caret pipeline reproduced from the raw CSV",
  rows = list(total = nrow(df), train = nrow(train), test = nrow(test)),
  best_tune = as.list(fits$RF$bestTune),
  cv = cv_table,
  test = list(
    macro_f1 = unname(point["Mean_F1"]), kappa = unname(point["Kappa"]),
    accuracy = unname(point["Accuracy"]),
    ci95 = list(
      macro_f1 = unname(ci[, "Mean_F1"]), kappa = unname(ci[, "Kappa"]),
      accuracy = unname(ci[, "Accuracy"])
    )
  ),
  confusion = as.data.frame.matrix(table(predicted = yh, actual = y)),
  original_reported = list(macro_f1 = 0.953, kappa = 0.928, accuracy = 0.960),
  versions = list(
    R = R.version.string, caret = as.character(utils::packageVersion("caret")),
    recipes = as.character(utils::packageVersion("recipes")),
    themis = as.character(utils::packageVersion("themis")),
    ranger = as.character(utils::packageVersion("ranger"))
  ),
  runtime_seconds = round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1)
)

dir.create(settings$paths$reports_dir, showWarnings = FALSE, recursive = TRUE)
out <- file.path(settings$paths$reports_dir, "original_reproduction.json")
jsonlite::write_json(result, out, auto_unbox = TRUE, pretty = TRUE, digits = 6)
print(cv_table)
cat(sprintf(
  "\nTest  Macro-F1 %.3f [%.3f, %.3f]  Kappa %.3f  Accuracy %.3f  (original: 0.953 / 0.928 / 0.960)\n",
  point["Mean_F1"], ci[1, "Mean_F1"], ci[2, "Mean_F1"], point["Kappa"], point["Accuracy"]
))
cat("Wrote", out, "\n")
