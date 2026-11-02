state_with_model <- function(max_batch_size = 5) {
  path <- withr::local_tempfile(fileext = ".rds", .local_envir = parent.frame())
  save_model(fixture_model(), path)
  api_state(path, max_batch_size)
}

state_without_model <- function() {
  suppressMessages(api_state(tempfile(fileext = ".rds")))
}

json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))

test_that("health reports whether the model is loaded", {
  expect_true(handle_health(state_with_model())$body$model_loaded)
  down <- handle_health(state_without_model())
  expect_equal(down$status, 200L)
  expect_false(down$body$model_loaded)
  expect_match(down$body$model_error, "not found")
})

test_that("model endpoints return 503 when no model is loaded", {
  state <- state_without_model()
  expect_equal(handle_model_info(state)$status, 503L)
  expect_equal(handle_predict(state, json(valid_record()))$status, 503L)
  expect_equal(handle_predict_batch(state, json(list(instances = list(valid_record()))))$status, 503L)
})

test_that("model info describes the artifact", {
  res <- handle_model_info(state_with_model())
  expect_equal(res$status, 200L)
  expect_equal(res$body$classes, c("low", "mid", "high"))
  expect_equal(nrow(res$body$features), 23)
})

test_that("single prediction returns a band and probabilities", {
  res <- handle_predict(state_with_model(), json(valid_record()))
  expect_equal(res$status, 200L)
  expect_true(res$body$star_band %in% c("low", "mid", "high"))
  expect_named(res$body$probabilities, c("low", "mid", "high"))
})

test_that("bad single requests get 400 or 422", {
  state <- state_with_model()
  expect_equal(handle_predict(state, "{not json")$status, 400L)
  expect_equal(handle_predict(state, "")$status, 422L)
  expect_equal(handle_predict(state, NULL)$status, 422L)
  bad <- handle_predict(state, json(c(valid_record(), list(extra = 1))))
  expect_equal(bad$status, 422L)
  expect_match(unlist(bad$body$detail), "unexpected field")
  expect_equal(handle_predict(state, "[1, 2]")$status, 422L)
})

test_that("batch prediction enforces shape, emptiness and size limits", {
  state <- state_with_model(max_batch_size = 3)
  ok <- handle_predict_batch(state, json(list(instances = rep(list(valid_record()), 3))))
  expect_equal(ok$status, 200L)
  expect_equal(ok$body$count, 3)
  expect_equal(handle_predict_batch(state, json(list(instances = list())))$status, 422L)
  too_big <- handle_predict_batch(state, json(list(instances = rep(list(valid_record()), 4))))
  expect_equal(too_big$status, 413L)
  expect_equal(handle_predict_batch(state, json(valid_record()))$status, 422L)
  expect_equal(handle_predict_batch(state, json(list(instances = valid_record())))$status, 422L)
  expect_equal(handle_predict_batch(state, "{")$status, 400L)
  mixed <- handle_predict_batch(state, json(list(instances = list(valid_record(), list(brand = "x")))))
  expect_equal(mixed$status, 422L)
  expect_match(unlist(mixed$body$detail)[1], "^record 2")
})

test_that("the router exposes every route and the OpenAPI request schemas", {
  pr <- build_api(state_with_model())
  paths <- vapply(pr$endpoints[[1]], function(e) e$path, character(1))
  expect_setequal(paths, c("/", "/health", "/model/info", "/predict", "/predict/batch"))
  spec <- pr$getApiSpec()
  schema <- spec$paths$`/predict`$post$requestBody$content$`application/json`$schema
  expect_false(schema$additionalProperties)
  expect_length(schema$required, 23)
})
