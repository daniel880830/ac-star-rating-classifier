# Splitting, tuning, training and persisting the model artifact.

#' Split the modelling frame into training and test sets
#'
#' @param frame Output of [model_frame()].
#' @param strategy `"random"` (stratified, as in the original analysis) or
#'   `"group"` (stratified, with rows sharing a `spec_group` kept together).
#' @param test_prop Share of rows (random) or groups (group) held out.
#' @param seed Random seed.
#' @return An `rsplit` object.
#' @export
split_data <- function(frame, strategy = "group", test_prop = 0.2, seed = 42) {
  withr::with_seed(seed, switch(strategy,
    random = rsample::initial_split(frame, prop = 1 - test_prop, strata = "star3"),
    group = group_split(frame, test_prop),
    abort_config("Unknown split strategy: ", strategy)
  ))
}

# Stratified group hold-out. Each group is assigned to the stratum of its most
# common class, and `test_prop` of the groups in every stratum go to the test
# set. (rsample::group_initial_split() with `strata` returned duplicated rows
# for this data, so the split is done explicitly.)
group_split <- function(frame, test_prop) {
  group_class <- tapply(as.character(frame$star3), frame$spec_group, function(x) names(which.max(table(x))))
  by_stratum <- split(names(group_class), unname(group_class))
  test_groups <- unlist(lapply(by_stratum, function(g) g[sample.int(length(g), round(length(g) * test_prop))]))
  in_test <- as.character(frame$spec_group) %in% test_groups
  rsample::make_splits(list(analysis = which(!in_test), assessment = which(in_test)), data = frame)
}

#' Cross-validation folds matching the split strategy
#'
#' @param train Training frame.
#' @inheritParams split_data
#' @param folds,repeats Number of folds and repeats.
#' @return An `rset`.
#' @export
make_resamples <- function(train, strategy = "group", folds = 3, repeats = 2, seed = 42) {
  withr::with_seed(seed, switch(strategy,
    random = rsample::vfold_cv(train, v = folds, repeats = repeats, strata = "star3"),
    group = rsample::group_vfold_cv(
      train,
      group = "spec_group", v = folds, repeats = repeats, strata = "star3"
    ),
    abort_config("Unknown split strategy: ", strategy)
  ))
}

#' Prepare the data and split for a settings profile
#'
#' @param settings From [load_settings()].
#' @param data Optional pre-built output of [build_dataset()].
#' @return A list with `frame`, `split`, `train` and `test`.
#' @export
prepare_data <- function(settings, data = NULL) {
  if (is.null(data)) {
    data <- build_dataset(settings$paths$raw_data)
  }
  frame <- model_frame(data, ORIGINAL_TOP23, settings$target$breaks)
  split <- split_data(frame, settings$split$strategy, settings$split$test_prop, settings$seed)
  list(frame = frame, split = split, train = rsample::training(split), test = rsample::testing(split))
}

#' Tune the random forest with cross-validation
#'
#' @param settings From [load_settings()].
#' @param prepared Optional output of [prepare_data()].
#' @return A list with the best parameters and the full CV results table.
#' @export
tune_model <- function(settings, prepared = NULL) {
  prepared <- prepared %||% prepare_data(settings)
  resamples <- make_resamples(
    prepared$train, settings$split$strategy, settings$cv$folds, settings$cv$repeats, settings$seed
  )
  wf <- build_workflow(build_recipe(prepared$train, settings$smote, settings$seed), rf_spec(settings$model$trees))
  grid <- tidyr::expand_grid(
    mtry = settings$model$mtry, min_n = settings$model$min_n, splitrule = settings$model$splitrule
  )
  log_info("Tuning {nrow(grid)} candidates on {nrow(resamples)} resamples ({settings$split$strategy} CV)")
  res <- withr::with_seed(settings$seed, tune::tune_grid(
    wf,
    resamples = resamples, grid = grid,
    metrics = yardstick::metric_set(yardstick::f_meas, yardstick::kap, yardstick::accuracy),
    control = tune::control_grid(verbose = FALSE)
  ))
  results <- tryCatch(tune::collect_metrics(res), error = function(e) {
    notes <- unique(tune::collect_notes(res)$note)
    abort_data(
      "Every tuning candidate failed. First error: ",
      if (length(notes) > 0) notes[1] else conditionMessage(e)
    )
  })
  best <- tune::select_best(res, metric = "f_meas")
  best_row <- dplyr::semi_join(results, best, by = c("mtry", "min_n", "splitrule"))
  list(
    best = as.list(best[c("mtry", "min_n", "splitrule")]),
    cv = summarise_cv(best_row),
    results = results
  )
}

summarise_cv <- function(rows) {
  pick <- function(metric) rows[rows$.metric == metric, , drop = FALSE]
  list(
    macro_f1 = list(mean = pick("f_meas")$mean, sd = pick("f_meas")$std_err * sqrt(pick("f_meas")$n)),
    kappa = list(mean = pick("kap")$mean, sd = pick("kap")$std_err * sqrt(pick("kap")$n)),
    accuracy = list(mean = pick("accuracy")$mean, sd = pick("accuracy")$std_err * sqrt(pick("accuracy")$n)),
    n_resamples = pick("f_meas")$n
  )
}

#' Fit the final workflow and evaluate it on the held-out test set
#'
#' @param settings From [load_settings()].
#' @param params List with `mtry`, `min_n`, `splitrule`. When `NULL`, the
#'   tuning report for this profile is used, and tuning runs first if missing.
#' @param prepared Optional output of [prepare_data()].
#' @param save If `TRUE`, write the model artifact and the metrics report.
#' @return The model artifact (a list of class `acstar_model`).
#' @export
train_model <- function(settings, params = NULL, prepared = NULL, save = TRUE) {
  started <- Sys.time()
  prepared <- prepared %||% prepare_data(settings)
  tuning <- NULL
  if (is.null(params)) {
    tuning <- read_tuning(settings) %||% {
      tuned <- tune_model(settings, prepared)
      if (save) write_tuning(settings, tuned)
      tuned
    }
    params <- tuning$best
  }
  spec <- rf_spec(settings$model$trees, params$mtry, params$min_n, params$splitrule)
  wf <- build_workflow(build_recipe(prepared$train, settings$smote, settings$seed), spec)
  fitted <- withr::with_seed(settings$seed, parsnip::fit(wf, data = prepared$train))

  test_pred <- stats::predict(fitted, prepared$test)$.pred_class
  evaluation <- list(
    metrics = as.list(classification_metrics(prepared$test$star3, test_pred)),
    ci95 = bootstrap_ci(prepared$test$star3, test_pred),
    per_class = per_class_metrics(prepared$test$star3, test_pred),
    confusion = as.data.frame.matrix(table(predicted = test_pred, actual = prepared$test$star3)),
    twin_overlap = twin_overlap(prepared$train, prepared$test)
  )
  model <- structure(list(
    workflow = butcher::butcher(fitted),
    metadata = list(
      schema_version = 1L,
      profile = settings$profile,
      features = snake_case(ORIGINAL_TOP23),
      feature_labels = ORIGINAL_TOP23,
      classes = levels(prepared$train$star3),
      breaks = settings$target$breaks,
      split_strategy = settings$split$strategy,
      params = params,
      cv = tuning$cv,
      test = evaluation,
      n_train = nrow(prepared$train),
      n_test = nrow(prepared$test),
      trained_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      versions = package_versions(),
      train_seconds = round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1)
    )
  ), class = "acstar_model")
  if (save) {
    save_model(model, settings$paths$model)
    write_metrics_report(settings, model)
  }
  model
}

#' Save a model artifact
#'
#' @param model An `acstar_model`.
#' @param path Destination `.rds` path.
#' @return `path`, invisibly.
#' @export
save_model <- function(model, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(model, path, compress = "xz")
  log_info("Saved model to {path} ({round(file.size(path) / 2^20, 2)} MiB)")
  invisible(path)
}

tuning_path <- function(settings) {
  file.path(settings$paths$reports_dir, paste0("tuning_", settings$profile, ".json"))
}

write_tuning <- function(settings, tuned) {
  dir.create(settings$paths$reports_dir, recursive = TRUE, showWarnings = FALSE)
  out <- list(best = tuned$best, cv = tuned$cv, results = tuned$results)
  jsonlite::write_json(out, tuning_path(settings), auto_unbox = TRUE, pretty = TRUE, digits = 6)
  invisible(tuning_path(settings))
}

read_tuning <- function(settings) {
  path <- tuning_path(settings)
  if (!file.exists(path)) {
    return(NULL)
  }
  tuned <- jsonlite::read_json(path, simplifyVector = TRUE)
  needed <- c("mtry", "min_n", "splitrule")
  if (!all(needed %in% names(tuned$best))) {
    abort_config("Tuning report ", path, " is missing best parameters; rerun `tune`.")
  }
  tuned
}

write_metrics_report <- function(settings, model) {
  dir.create(settings$paths$reports_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(settings$paths$reports_dir, paste0("metrics_", settings$profile, ".json"))
  jsonlite::write_json(model$metadata, path, auto_unbox = TRUE, pretty = TRUE, digits = 6)
  invisible(path)
}

package_versions <- function() {
  pk <- c("recipes", "parsnip", "workflows", "themis", "ranger", "tune", "rsample")
  c(list(R = paste(R.version$major, R.version$minor, sep = ".")), stats::setNames(
    lapply(pk, function(p) as.character(utils::packageVersion(p))), pk
  ))
}

log_info <- function(msg, .envir = parent.frame()) {
  logger::log_info(msg, namespace = "acstar", .topenv = .envir)
}
