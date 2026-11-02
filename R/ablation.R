# Leakage ablation: change one thing at a time and measure the test scores
# over several random splits, with the hyper-parameters held fixed.

#' Features the original |r| > 0.8 filter was meant to remove
#'
#' The original code computed the correlation of every numeric column with the
#' target's factor codes and stored the flagged names in `leak_features`, but
#' then never removed them. This recomputes that list on the given rows.
#'
#' @param data Output of [build_dataset()] (original column names).
#' @param threshold Absolute correlation threshold.
#' @return Character vector of flagged column names.
#' @export
intended_corr_filter <- function(data, threshold = 0.8) {
  data <- data[!is.na(data[[TARGET]]), , drop = FALSE]
  df <- data[setdiff(names(data), intersect(LEAKAGE_COLUMNS, names(data)))]
  num <- df[vapply(df, is.numeric, logical(1))]
  num[[TARGET]] <- NULL
  target_codes <- as.numeric(as.factor(data[[TARGET]]))
  r <- vapply(num, function(x) {
    if (sum(stats::complete.cases(x, target_codes)) < 3 || stats::sd(x, na.rm = TRUE) == 0) {
      return(NA_real_)
    }
    stats::cor(x, target_codes, use = "pairwise.complete.obs")
  }, numeric(1))
  names(r)[!is.na(r) & abs(r) > threshold]
}

#' Run the leakage ablation
#'
#' Experiments (each evaluated over `n_seeds` different train/test splits):
#' 1. `leaky_all_columns`: random split, every column left after EDA, including
#'    the sibling star ratings (the first analysis's setup).
#' 2. `original`: random split, the original 23 features.
#' 3. `group_split`: as 2, but rows with identical predictors stay together.
#' 4. `group_split_corr_filter`: as 3, also dropping the features the |r| > 0.8
#'    filter was meant to remove.
#'
#' @param settings From [load_settings()].
#' @param n_seeds Number of split seeds.
#' @param params Fixed forest parameters; defaults to the original model's
#'   best tune (mtry 6, min_n 1, gini).
#' @param data Optional pre-built output of [build_dataset()].
#' @return A list with per-run `results` and a `summary` tibble; also written
#'   to `reports/ablation.json`.
#' @export
run_ablation <- function(settings, n_seeds = 5, params = NULL, data = NULL) {
  data <- data %||% build_dataset(settings$paths$raw_data)
  # Best parameters of the reproduced original caret model (see scripts/reproduce_original.R).
  params <- params %||% list(mtry = 6, min_n = 1, splitrule = "gini")
  corr_flagged <- intended_corr_filter(data)
  all_cols <- setdiff(names(data), c(TARGET, "ExpDate"))
  experiments <- list(
    leaky_all_columns = list(features = all_cols, strategy = "random"),
    original = list(features = ORIGINAL_TOP23, strategy = "random"),
    group_split = list(features = ORIGINAL_TOP23, strategy = "group"),
    group_split_corr_filter = list(features = setdiff(ORIGINAL_TOP23, corr_flagged), strategy = "group")
  )
  seeds <- settings$seed + seq_len(n_seeds) - 1L
  results <- purrr::imap_dfr(experiments, function(exp, name) {
    frame <- model_frame_any(data, exp$features, settings$target$breaks)
    purrr::map_dfr(seeds, function(seed) {
      split <- split_data(frame, exp$strategy, settings$split$test_prop, seed)
      train <- rsample::training(split)
      test <- rsample::testing(split)
      spec <- rf_spec(settings$model$trees, params$mtry, params$min_n, params$splitrule)
      wf <- build_workflow(build_recipe(train, settings$smote, seed), spec)
      fit <- withr::with_seed(seed, parsnip::fit(wf, train))
      pred <- stats::predict(fit, test)$.pred_class
      m <- classification_metrics(test$star3, pred)
      log_info("{name} seed {seed}: macro-F1 {round(m[['macro_f1']], 3)}")
      tibble::tibble(
        experiment = name, seed = seed, n_features = length(exp$features),
        twin_overlap = twin_overlap(train, test),
        macro_f1 = m[["macro_f1"]], kappa = m[["kappa"]], accuracy = m[["accuracy"]]
      )
    })
  })
  summary <- results |>
    dplyr::group_by(.data$experiment) |>
    dplyr::summarise(
      n_features = dplyr::first(.data$n_features),
      twin_overlap = mean(.data$twin_overlap),
      macro_f1_mean = mean(.data$macro_f1), macro_f1_sd = stats::sd(.data$macro_f1),
      kappa_mean = mean(.data$kappa), accuracy_mean = mean(.data$accuracy),
      .groups = "drop"
    ) |>
    dplyr::arrange(match(.data$experiment, names(experiments)))
  out <- list(
    params = params, seeds = seeds, corr_filter_removes = corr_flagged,
    summary = summary, results = results
  )
  dir.create(settings$paths$reports_dir, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(out, file.path(settings$paths$reports_dir, "ablation.json"),
    auto_unbox = TRUE, pretty = TRUE, digits = 6
  )
  out
}

# Like model_frame(), but for an arbitrary feature list (categorical columns
# other than brand are kept as character).
model_frame_any <- function(data, features, breaks) {
  if (identical(features, ORIGINAL_TOP23)) {
    return(model_frame(data, features, breaks))
  }
  data <- data[!is.na(data[[TARGET]]), , drop = FALSE]
  out <- data[features]
  names(out) <- snake_case(features)
  out$star3 <- star_band(as.numeric(data[[TARGET]]), breaks)
  out$spec_group <- spec_group(out)
  out
}
