# Loading the saved artifact and predicting.

#' Load and check a model artifact
#'
#' @param path Path to an `.rds` file written by [train_model()].
#' @return An `acstar_model`.
#' @export
load_model <- function(path) {
  if (!file.exists(path)) {
    abort_model("Model file not found: ", path, ". Run `train` first.")
  }
  model <- tryCatch(readRDS(path), error = function(e) {
    abort_model("Model file is unreadable or corrupt: ", path, " (", conditionMessage(e), ")")
  })
  if (!inherits(model, "acstar_model") || is.null(model$workflow) || is.null(model$metadata$features)) {
    abort_model("File is not an acstar model artifact: ", path)
  }
  if (!identical(model$metadata$schema_version, 1L)) {
    abort_model("Unsupported model schema version: ", format(model$metadata$schema_version))
  }
  # readRDS() does not load the namespaces that define the artifact's S3
  # methods. In an installed package nothing else loads them either, so
  # predict() would find no method for the workflow.
  for (pkg in MODEL_NAMESPACES) loadNamespace(pkg)
  model
}

MODEL_NAMESPACES <- c("workflows", "parsnip", "recipes", "themis", "ranger")

#' Predict star bands with class probabilities
#'
#' @param model An `acstar_model`.
#' @param newdata A data frame of features (snake_case or original names).
#' @return A tibble with `star_band` and one `prob_<class>` column per class.
#' @export
predict_star <- function(model, newdata) {
  if (nrow(newdata) == 0) {
    abort_data("No rows to predict.")
  }
  features <- prepare_features(newdata, model$metadata$feature_labels)
  probs <- stats::predict(model$workflow, features, type = "prob")
  classes <- model$metadata$classes
  prob_cols <- paste0(".pred_", classes)
  best <- classes[max.col(as.matrix(probs[prob_cols]), ties.method = "first")]
  out <- tibble::tibble(star_band = best)
  for (k in seq_along(classes)) {
    out[[paste0("prob_", classes[k])]] <- round(probs[[prob_cols[k]]], 4)
  }
  out
}

#' Model description for the `/model/info` endpoint
#'
#' @param model An `acstar_model`.
#' @return A list of metadata safe to serialise.
#' @export
model_info <- function(model) {
  md <- model$metadata
  list(
    profile = md$profile,
    trained_at = md$trained_at,
    classes = md$classes,
    star_band_breaks = md$breaks,
    split_strategy = md$split_strategy,
    params = md$params,
    test_metrics = md$test$metrics,
    n_train = md$n_train,
    n_test = md$n_test,
    features = FEATURE_SCHEMA[c("name", "type", "min", "max", "nullable", "description")],
    versions = md$versions
  )
}

abort_model <- function(...) {
  rlang::abort(paste0(...), class = "acstar_model_error")
}
