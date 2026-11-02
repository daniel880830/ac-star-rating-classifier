# Measure API latency and throughput against a real server process.
#
# Starts the API on a free local port with models/model.rds, then:
#   - 300 sequential single-record POST /predict calls (after 20 warm-up calls)
#   - 20 POST /predict/batch calls with 100 records each
# and writes reports/benchmark.json with the hardware and software used.
#
# Usage: Rscript scripts/benchmark.R [model_path]

suppressPackageStartupMessages(pkgload::load_all(quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
model_path <- if (length(args) > 0) args[1] else "models/model.rds"
stopifnot(file.exists(model_path))
port <- 18765L
root <- normalizePath(".")

server <- callr::r_bg(function(root, model_path, port) {
  pkgload::load_all(root, quiet = TRUE)
  logger::log_threshold(logger::WARN, namespace = "acstar")
  logger::log_threshold(logger::WARN)
  plumber::pr_run(build_api(api_state(model_path)), host = "127.0.0.1", port = port, docs = FALSE, quiet = TRUE)
}, args = list(root, model_path, port), supervise = TRUE)
on.exit(server$kill(), add = TRUE)

base <- paste0("http://127.0.0.1:", port)
for (i in 1:120) {
  ok <- tryCatch(httr2::resp_status(httr2::req_perform(httr2::request(paste0(base, "/health")))), error = function(e) NA)
  if (identical(ok, 200L)) break
  Sys.sleep(0.5)
}

post <- function(path, body) {
  req <- httr2::request(paste0(base, path)) |> httr2::req_body_raw(body, type = "application/json")
  t0 <- Sys.time()
  resp <- httr2::req_perform(req)
  stopifnot(httr2::resp_status(resp) == 200L)
  as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
}

one <- as.character(jsonlite::toJSON(example_record(), auto_unbox = TRUE))
batch <- as.character(jsonlite::toJSON(list(instances = rep(list(example_record()), 100)), auto_unbox = TRUE))

invisible(replicate(20, post("/predict", one)))
single_ms <- replicate(300, post("/predict", one))
batch_ms <- replicate(20, post("/predict/batch", batch))

cpu <- tryCatch(
  if (.Platform$OS.type == "windows") {
    trimws(system2("powershell", c("-NoProfile", "-Command", "(Get-CimInstance Win32_Processor).Name"), stdout = TRUE)[1])
  } else {
    sub(".*: ", "", grep("model name", readLines("/proc/cpuinfo"), value = TRUE)[1])
  },
  error = function(e) NA_character_
)

q <- function(x, p) round(unname(stats::quantile(x, p)), 1)
result <- list(
  single = list(n = length(single_ms), p50_ms = q(single_ms, 0.5), p95_ms = q(single_ms, 0.95),
                p99_ms = q(single_ms, 0.99), requests_per_second = round(1000 / mean(single_ms), 1)),
  batch_100 = list(n = length(batch_ms), p50_ms = q(batch_ms, 0.5), p95_ms = q(batch_ms, 0.95),
                   records_per_second = round(100 * 1000 / mean(batch_ms), 0)),
  artifact_mib = round(file.size(model_path) / 2^20, 2),
  environment = list(
    os = utils::sessionInfo()$running, cpu = cpu, logical_cores = parallel::detectCores(),
    r = R.version.string, plumber = as.character(utils::packageVersion("plumber")),
    ranger = as.character(utils::packageVersion("ranger"))
  ),
  measured_at = format(Sys.time(), "%Y-%m-%d")
)
jsonlite::write_json(result, "reports/benchmark.json", auto_unbox = TRUE, pretty = TRUE)
str(result)
