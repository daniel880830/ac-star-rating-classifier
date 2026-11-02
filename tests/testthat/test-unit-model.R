test_that("a single-row prediction sees the same feature width as training", {
  model <- fixture_model()
  wf <- model$workflow
  rec <- workflows::extract_recipe(wf)
  one <- prepare_features(model_frame(make_dataset(5, twins = 0))[1, ])
  many <- prepare_features(model_frame(make_dataset(50, seed = 9, twins = 0)))
  width_one <- ncol(recipes::bake(rec, one))
  width_many <- ncol(recipes::bake(rec, many))
  expect_equal(width_one, width_many)
  expect_equal(nrow(predict_star(model, one)), 1)
})

test_that("predictions handle unseen brands and missing optional values", {
  model <- fixture_model()
  rec <- example_record()
  rec$brand <- "A BRAND NEVER SEEN"
  rec$residential_hspf_hot <- NA
  pred <- predict_star(model, tibble::as_tibble(rec))
  expect_true(pred$star_band %in% c("low", "mid", "high"))
  expect_equal(sum(unlist(pred[c("prob_low", "prob_mid", "prob_high")])), 1, tolerance = 1e-3)
  expect_error(predict_star(model, tibble::as_tibble(rec)[0, ]), class = "acstar_data_error")
})

test_that("training is reproducible for a fixed seed", {
  dir <- withr::local_tempdir()
  settings <- validate_settings(test_settings(dir))
  prepared <- prepare_data(settings, data = make_dataset())
  params <- list(mtry = 3, min_n = 2, splitrule = "extratrees")
  a <- train_model(settings, params = params, prepared = prepared, save = FALSE)
  b <- train_model(settings, params = params, prepared = prepared, save = FALSE)
  newdata <- prepared$test
  expect_identical(predict_star(a, newdata), predict_star(b, newdata))
  expect_identical(a$metadata$test$metrics, b$metadata$test$metrics)
})

test_that("the artifact round-trips through disk and records metadata", {
  model <- fixture_model()
  path <- withr::local_tempfile(fileext = ".rds")
  save_model(model, path)
  loaded <- load_model(path)
  expect_equal(loaded$metadata$features, snake_case(ORIGINAL_TOP23))
  expect_equal(loaded$metadata$classes, c("low", "mid", "high"))
  expect_equal(loaded$metadata$test$twin_overlap, 0)
  newdata <- prepare_features(model_frame(make_dataset(10, seed = 4, twins = 0)))
  expect_identical(predict_star(loaded, newdata), predict_star(model, newdata))
  info <- model_info(loaded)
  expect_named(info$test_metrics, c("macro_f1", "kappa", "accuracy"))
})

test_that("load_model rejects missing, corrupt and foreign files", {
  expect_error(load_model(tempfile()), "not found", class = "acstar_model_error")
  corrupt <- withr::local_tempfile(fileext = ".rds")
  writeLines("not an rds file", corrupt)
  expect_error(load_model(corrupt), "corrupt", class = "acstar_model_error")
  foreign <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(a = 1), foreign)
  expect_error(load_model(foreign), "not an acstar model", class = "acstar_model_error")
  old <- fixture_model()
  old$metadata$schema_version <- 99L
  outdated <- withr::local_tempfile(fileext = ".rds")
  saveRDS(old, outdated)
  expect_error(load_model(outdated), "schema version", class = "acstar_model_error")
})

test_that("tuning picks a candidate from the grid and train reuses the saved result", {
  dir <- withr::local_tempdir()
  grid <- list(trees = 15, mtry = c(2, 4), min_n = 3, splitrule = "gini")
  settings <- validate_settings(test_settings(dir, model = grid))
  readr::write_csv(make_raw(), settings$paths$raw_data, na = "")
  tuned <- tune_model(settings)
  expect_true(tuned$best$mtry %in% c(2, 4))
  expect_gt(tuned$cv$macro_f1$mean, 0)
  write_tuning(settings, tuned)
  model <- train_model(settings)
  expect_true(file.exists(settings$paths$model))
  expect_true(file.exists(file.path(settings$paths$reports_dir, "metrics_default.json")))
  expect_equal(model$metadata$params$mtry, tuned$best$mtry)
})

test_that("tuning explains why it fails when every candidate errors", {
  dir <- withr::local_tempdir()
  # Too few rows per fold for SMOTE's neighbours: every fit fails.
  settings <- validate_settings(test_settings(dir, smote = list(over_ratio = 1, neighbors = 50)))
  prepared <- prepare_data(settings, data = make_dataset(80, twins = 0))
  expect_error(
    suppressWarnings(tune_model(settings, prepared)), "Every tuning candidate failed",
    class = "acstar_data_error"
  )
})

test_that("a tuning report without parameters is an error", {
  dir <- withr::local_tempdir()
  settings <- validate_settings(test_settings(dir))
  dir.create(settings$paths$reports_dir)
  jsonlite::write_json(list(best = list(mtry = 2)), tuning_path(settings), auto_unbox = TRUE)
  expect_error(read_tuning(settings), class = "acstar_config_error")
})
