# Starts the real plumber server in a background R process and calls it over
# HTTP, covering routing, status codes, serialisation and the docs redirect.

skip_if_not_installed("callr")
skip_if_not_installed("httr2")

free_port <- function() {
  for (port in sample(20000:40000, 20)) {
    con <- tryCatch(suppressWarnings(socketConnection("127.0.0.1", port, timeout = 0.2, open = "r")),
      error = function(e) NULL
    )
    if (is.null(con)) {
      return(port)
    }
    close(con)
  }
  skip("No free port found")
}

start_server <- function(model_path, port, max_batch_size = 3) {
  pkg_root <- normalizePath(testthat::test_path("..", ".."))
  proc <- callr::r_bg(function(pkg_root, model_path, port, max_batch_size) {
    if (file.exists(file.path(pkg_root, "DESCRIPTION")) && requireNamespace("pkgload", quietly = TRUE)) {
      pkgload::load_all(pkg_root, quiet = TRUE)
    } else {
      library(acstar)
    }
    logger::log_threshold(logger::WARN)
    state <- acstar::api_state(model_path, max_batch_size)
    plumber::pr_run(acstar::build_api(state), host = "127.0.0.1", port = port, docs = TRUE, quiet = TRUE)
  }, args = list(pkg_root, model_path, port, max_batch_size), supervise = TRUE)
  base <- paste0("http://127.0.0.1:", port)
  for (i in 1:120) {
    ok <- tryCatch(httr2::request(paste0(base, "/health")) |> httr2::req_perform() |> httr2::resp_status(),
      error = function(e) NA
    )
    if (identical(ok, 200L)) {
      return(list(proc = proc, base = base))
    }
    if (!proc$is_alive()) {
      stop("Server process died: ", proc$read_all_error())
    }
    Sys.sleep(0.5)
  }
  proc$kill()
  stop("Server did not start in time")
}

call_api <- function(base, path, body = NULL) {
  req <- httr2::request(paste0(base, path)) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_options(followlocation = 0L)
  if (!is.null(body)) {
    req <- httr2::req_body_raw(req, body, type = "application/json")
  }
  httr2::req_perform(req)
}

to_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))

test_that("the running API serves every endpoint with the right status codes", {
  model_path <- withr::local_tempfile(fileext = ".rds")
  save_model(fixture_model(), model_path)
  server <- start_server(model_path, free_port())
  withr::defer(server$proc$kill())
  base <- server$base

  root <- call_api(base, "/")
  expect_equal(httr2::resp_status(root), 302L)
  expect_equal(httr2::resp_header(root, "Location"), "/__docs__/")
  expect_equal(httr2::resp_status(call_api(base, "/__docs__/")), 200L)

  health <- httr2::resp_body_json(call_api(base, "/health"))
  expect_true(health$model_loaded)
  expect_equal(httr2::resp_status(call_api(base, "/model/info")), 200L)

  one <- call_api(base, "/predict", to_json(valid_record()))
  expect_equal(httr2::resp_status(one), 200L)
  expect_true(httr2::resp_body_json(one)$star_band %in% c("low", "mid", "high"))

  expect_equal(httr2::resp_status(call_api(base, "/predict", to_json(c(valid_record(), list(x = 1))))), 422L)
  expect_equal(httr2::resp_status(call_api(base, "/predict", "{oops")), 400L)

  batch <- to_json(list(instances = rep(list(valid_record()), 2)))
  expect_equal(httr2::resp_body_json(call_api(base, "/predict/batch", batch))$count, 2)
  too_big <- to_json(list(instances = rep(list(valid_record()), 4)))
  expect_equal(httr2::resp_status(call_api(base, "/predict/batch", too_big)), 413L)
  expect_equal(httr2::resp_status(call_api(base, "/predict/batch", to_json(list(instances = list())))), 422L)
  expect_equal(httr2::resp_status(call_api(base, "/nope")), 404L)
})

test_that("the API starts without a model and answers 503", {
  server <- start_server(tempfile(fileext = ".rds"), free_port())
  withr::defer(server$proc$kill())
  expect_false(httr2::resp_body_json(call_api(server$base, "/health"))$model_loaded)
  expect_equal(httr2::resp_status(call_api(server$base, "/predict", to_json(valid_record()))), 503L)
})
