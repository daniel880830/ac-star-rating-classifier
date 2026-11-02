# REST API (plumber). Handlers are plain functions returning
# `list(status, body)` so they can be unit-tested without a running server;
# `build_api()` wires them into a plumber router.

#' Create the API state
#'
#' @param model_path Path to the model artifact. If loading fails the API still
#'   starts and prediction endpoints answer 503.
#' @param max_batch_size Largest accepted batch.
#' @return An environment holding `model`, `model_error` and `max_batch_size`.
#' @export
api_state <- function(model_path, max_batch_size = 1000) {
  state <- new.env(parent = emptyenv())
  state$max_batch_size <- max_batch_size
  state$model <- NULL
  state$model_error <- NULL
  tryCatch(
    state$model <- load_model(model_path),
    acstar_model_error = function(e) {
      state$model_error <- conditionMessage(e)
      logger::log_warn(namespace = "acstar", "Model not loaded: {conditionMessage(e)}")
    }
  )
  state
}

handle_health <- function(state) {
  list(status = 200L, body = list(
    status = "ok",
    model_loaded = !is.null(state$model),
    model_error = state$model_error
  ))
}

handle_model_info <- function(state) {
  if (is.null(state$model)) {
    return(unavailable(state))
  }
  list(status = 200L, body = model_info(state$model))
}

handle_predict <- function(state, body_text) {
  if (is.null(state$model)) {
    return(unavailable(state))
  }
  parsed <- parse_json_body(body_text)
  if (!is.null(parsed$error)) {
    return(parsed$error)
  }
  run_prediction(state, list(parsed$value), single = TRUE)
}

handle_predict_batch <- function(state, body_text) {
  if (is.null(state$model)) {
    return(unavailable(state))
  }
  parsed <- parse_json_body(body_text)
  if (!is.null(parsed$error)) {
    return(parsed$error)
  }
  body <- parsed$value
  is_batch <- is.list(body) && identical(names(body), "instances") &&
    is.list(body$instances) && is.null(names(body$instances))
  if (!is_batch) {
    return(unprocessable("Body must be an object with a single field 'instances' holding an array of records."))
  }
  n <- length(body$instances)
  if (n == 0) {
    return(unprocessable("'instances' must contain at least one record."))
  }
  if (n > state$max_batch_size) {
    return(list(status = 413L, body = list(
      error = "payload_too_large",
      detail = sprintf("Batch of %d records exceeds the limit of %d.", n, state$max_batch_size)
    )))
  }
  run_prediction(state, body$instances, single = FALSE)
}

run_prediction <- function(state, records, single) {
  checked <- validate_records(records)
  if (!checked$ok) {
    return(unprocessable(checked$errors))
  }
  pred <- predict_star(state$model, checked$data)
  rows <- lapply(seq_len(nrow(pred)), function(i) {
    list(
      star_band = pred$star_band[i],
      probabilities = as.list(unlist(pred[i, grepl("^prob_", names(pred))]))
    )
  })
  rows <- lapply(rows, function(r) {
    names(r$probabilities) <- sub("^prob_", "", names(r$probabilities))
    r
  })
  body <- if (single) rows[[1]] else list(predictions = rows, count = length(rows))
  list(status = 200L, body = body)
}

parse_json_body <- function(body_text) {
  if (is.null(body_text) || !nzchar(trimws(body_text))) {
    return(list(error = unprocessable("Request body is empty; send a JSON object.")))
  }
  value <- tryCatch(
    jsonlite::parse_json(body_text, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(value)) {
    return(list(error = list(status = 400L, body = list(error = "invalid_json", detail = "Body is not valid JSON."))))
  }
  list(value = value)
}

unprocessable <- function(detail) {
  list(status = 422L, body = list(error = "validation_error", detail = as.list(detail)))
}

unavailable <- function(state) {
  list(status = 503L, body = list(
    error = "model_unavailable",
    detail = state$model_error %||% "Model is not loaded."
  ))
}

#' Build the plumber router
#'
#' @param state From [api_state()].
#' @return A plumber router.
#' @export
build_api <- function(state) {
  respond <- function(res, result) {
    res$status <- result$status
    result$body
  }
  body_text <- function(req) {
    raw <- req$bodyRaw
    if (is.null(raw) || length(raw) == 0) "" else rawToChar(raw)
  }
  json <- plumber::serializer_unboxed_json(null = "null", na = "null")
  # Pass request bodies through untouched: plumber's JSON parser turns
  # malformed JSON into a 500, whereas the handlers answer 400/422 themselves.
  if (!"acstar_raw" %in% plumber::registered_parsers()) {
    plumber::register_parser("acstar_raw", function(...) function(value, ...) value, regex = ".*", verbose = FALSE)
  }
  pr <- plumber::pr() |>
    plumber::pr_get("/", function(res) {
      res$status <- 302L
      res$setHeader("Location", "/__docs__/")
      list()
    }, serializer = json) |>
    plumber::pr_get("/health", function(res) respond(res, handle_health(state)), serializer = json) |>
    plumber::pr_get("/model/info", function(res) respond(res, handle_model_info(state)), serializer = json) |>
    plumber::pr_post("/predict", function(req, res) {
      respond(res, handle_predict(state, body_text(req)))
    }, serializer = json, parsers = "acstar_raw") |>
    plumber::pr_post("/predict/batch", function(req, res) {
      respond(res, handle_predict_batch(state, body_text(req)))
    }, serializer = json, parsers = "acstar_raw") |>
    plumber::pr_hook("postroute", function(req, res) {
      logger::log_info(namespace = "acstar", "{req$REQUEST_METHOD} {req$PATH_INFO} -> {res$status}")
    }) |>
    plumber::pr_set_api_spec(api_spec)
  pr
}

#' Start the API server
#'
#' @param settings From [load_settings()].
#' @return Does not return while the server runs.
#' @export
run_api <- function(settings) {
  state <- api_state(settings$paths$model, settings$api$max_batch_size)
  logger::log_info(
    namespace = "acstar",
    "Serving on http://{settings$api$host}:{settings$api$port} (docs at /__docs__/)"
  )
  plumber::pr_run(build_api(state), host = settings$api$host, port = settings$api$port, docs = TRUE, quiet = TRUE)
}

# OpenAPI request bodies so /__docs__/ shows the expected payloads.
api_spec <- function(spec) {
  props <- stats::setNames(lapply(seq_len(nrow(FEATURE_SCHEMA)), function(i) {
    f <- FEATURE_SCHEMA[i, ]
    p <- list(type = f$type, description = f$description, nullable = f$nullable)
    if (f$type == "number") {
      p$minimum <- f$min
      p$maximum <- f$max
    }
    p
  }), FEATURE_SCHEMA$name)
  record <- list(
    type = "object", properties = props, required = as.list(FEATURE_SCHEMA$name),
    additionalProperties = FALSE, example = example_record()
  )
  body <- function(schema) list(required = TRUE, content = list(`application/json` = list(schema = schema)))
  spec$info$title <- "AC star-rating API"
  spec$info$description <- "Predicts the cooling energy-efficiency band (low / mid / high) of an air conditioner."
  spec$paths$`/predict`$post$requestBody <- body(record)
  spec$paths$`/predict/batch`$post$requestBody <- body(list(
    type = "object", required = list("instances"), additionalProperties = FALSE,
    properties = list(instances = list(type = "array", minItems = 1, items = record))
  ))
  spec
}

#' A valid example request record
#'
#' @return A named list with one value per field of the input schema.
#' @export
example_record <- function() {
  list(
    brand = "MITSUBISHI ELECTRIC", rated_aeer = 4.2, eertestavg = 4.3,
    c_power_inp_rated = 0.6, residential_hspf_hot = 5.6, commercial_hspf_hot = 5.4,
    pia_inoperative_power = 2.5, commercial_hspf_mixed = 4.6, rated_acop = 4.4,
    commercial_hspf_cold = 3.9, coptestavg = 4.5, residential_hspf_cold = 4.1,
    residential_hspf_mixed = 4.9, rated_total_cool_capacity_w = 2500,
    c_total_cool_rated = 2.5, t1_half_cap_power_rated = 240,
    commercial_thec_hot = 120, residential_thec_hot = 180, h_power_inp_rated = 0.7,
    low_temp_cooling_half_cap_power_rated = 220, residential_thec_mixed = 900,
    commercial_thec_mixed = 450, h1_half_cap_power_rated = 300
  )
}
