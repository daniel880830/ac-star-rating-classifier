# Preprocessing recipe and model specification, combined into one workflow so
# the saved artifact applies identical preprocessing at training and inference.

#' Build the preprocessing recipe
#'
#' A port of the original "nonlinear" recipe: novel/unknown/rare-level
#' handling, one-hot encoding, median imputation, SMOTE (training only), then
#' centring and scaling. `spec_group` is kept as an id column for grouped
#' resampling and is not required at prediction time.
#'
#' @param train Training frame from [model_frame()].
#' @param smote List with `over_ratio` and `neighbors`.
#' @param seed Seed for SMOTE. themis otherwise draws one when the recipe is
#'   built, which makes otherwise identical fits differ.
#' @return An untrained `recipe`.
#' @export
build_recipe <- function(train, smote = list(over_ratio = 1.5, neighbors = 5), seed = 42) {
  rec <- recipes::recipe(star3 ~ ., data = train)
  if ("spec_group" %in% names(train)) {
    rec <- rec |>
      recipes::update_role("spec_group", new_role = "id") |>
      recipes::update_role_requirements("id", bake = FALSE)
  }
  rec |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_novel(recipes::all_nominal_predictors(), new_level = "new") |>
    recipes::step_unknown(recipes::all_nominal_predictors(), new_level = "unknown") |>
    recipes::step_other(recipes::all_nominal_predictors(), threshold = 0.02, other = "other") |>
    recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = TRUE) |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_impute_median(recipes::all_numeric_predictors()) |>
    themis::step_smote("star3", over_ratio = smote$over_ratio, neighbors = smote$neighbors, seed = seed) |>
    recipes::step_center(recipes::all_numeric_predictors()) |>
    recipes::step_scale(recipes::all_numeric_predictors())
}

#' Random-forest specification (ranger engine)
#'
#' @param trees Number of trees.
#' @param mtry,min_n,splitrule Fixed values, or `tune::tune()` placeholders.
#' @param threads Number of ranger threads. Results are reproducible for a
#'   fixed seed regardless of the thread count.
#' @return A parsnip model specification.
#' @export
rf_spec <- function(trees = 500, mtry = tune::tune(), min_n = tune::tune(),
                    splitrule = tune::tune(), threads = default_threads()) {
  # Inject the values so tune() placeholders stay visible to the tune package.
  parsnip::rand_forest(trees = !!trees, mtry = !!mtry, min_n = !!min_n) |>
    parsnip::set_engine(
      "ranger",
      splitrule = !!splitrule, importance = "permutation", num.threads = threads
    ) |>
    parsnip::set_mode("classification")
}

#' Combine recipe and model into a workflow
#'
#' @param recipe From [build_recipe()].
#' @param spec From [rf_spec()].
#' @return A `workflow`.
#' @export
build_workflow <- function(recipe, spec) {
  workflows::workflow() |>
    workflows::add_recipe(recipe) |>
    workflows::add_model(spec)
}

default_threads <- function() {
  max(1L, parallel::detectCores(logical = TRUE) - 1L, na.rm = TRUE)
}
