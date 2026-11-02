# Settings: config/config.yml profiles, overridden by APP_* environment variables
# (optionally loaded from a .env file).

# Environment variables that override a setting, mapped to the setting's path.
ENV_OVERRIDES <- list(
  APP_DATA_PATH = c("paths", "raw_data"),
  APP_MODEL_PATH = c("paths", "model"),
  APP_REPORTS_DIR = c("paths", "reports_dir"),
  APP_HOST = c("api", "host"),
  APP_PORT = c("api", "port"),
  APP_MAX_BATCH_SIZE = c("api", "max_batch_size"),
  APP_SEED = "seed",
  APP_LOG_LEVEL = "log_level"
)

LOG_LEVELS <- c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")

#' Load a .env file without overriding variables that are already set
#'
#' Lines look like `KEY=value`; blank lines and `#` comments are ignored and
#' values may be wrapped in single or double quotes. Variables already present
#' in the environment win, mirroring how most dotenv loaders behave.
#'
#' @param path Path to the file. A missing file is not an error.
#' @return Invisibly, the names of the variables that were set.
#' @export
load_dotenv <- function(path = ".env") {
  if (!file.exists(path)) {
    return(invisible(character()))
  }
  lines <- trimws(readLines(path, warn = FALSE))
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
  set <- character()
  for (line in lines) {
    m <- regmatches(line, regexec("^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*(.*)$", line))[[1]]
    if (length(m) != 3) {
      warning("Ignoring malformed line in ", path, ": ", line, call. = FALSE)
      next
    }
    value <- sub("^\"(.*)\"$", "\\1", sub("^'(.*)'$", "\\1", m[3]))
    if (!nzchar(Sys.getenv(m[2]))) {
      do.call(Sys.setenv, stats::setNames(list(value), m[2]))
      set <- c(set, m[2])
    }
  }
  invisible(set)
}

#' Load and validate settings
#'
#' @param profile Profile in the YAML file (`default` or `original`).
#' @param config_file Path to the YAML settings file.
#' @param env_file Optional `.env` file loaded before reading overrides.
#' @return A validated, nested list of settings.
#' @export
load_settings <- function(profile = Sys.getenv("APP_PROFILE", "default"),
                          config_file = Sys.getenv("APP_CONFIG", "config/config.yml"),
                          env_file = ".env") {
  load_dotenv(env_file)
  if (!file.exists(config_file)) {
    abort_config("Settings file not found: ", config_file)
  }
  profiles <- names(yaml::read_yaml(config_file))
  if (!profile %in% profiles) {
    abort_config("Unknown profile '", profile, "'. Available: ", paste(profiles, collapse = ", "))
  }
  cfg <- config::get(config = profile, file = config_file, use_parent = FALSE)
  for (var in names(ENV_OVERRIDES)) {
    value <- Sys.getenv(var)
    if (nzchar(value)) {
      cfg[[ENV_OVERRIDES[[var]]]] <- value
    }
  }
  cfg$profile <- profile
  validate_settings(cfg)
}

#' Validate and coerce a settings list
#'
#' @param cfg Nested settings list.
#' @return `cfg` with numeric fields coerced to the right type.
#' @export
validate_settings <- function(cfg) {
  cfg$seed <- as_whole(cfg$seed, "seed")
  cfg$api$port <- as_whole(cfg$api$port, "api.port", min = 1, max = 65535)
  cfg$api$max_batch_size <- as_whole(cfg$api$max_batch_size, "api.max_batch_size", min = 1)
  cfg$cv$folds <- as_whole(cfg$cv$folds, "cv.folds", min = 2)
  cfg$cv$repeats <- as_whole(cfg$cv$repeats, "cv.repeats", min = 1)
  cfg$model$trees <- as_whole(cfg$model$trees, "model.trees", min = 1)
  cfg$smote$neighbors <- as_whole(cfg$smote$neighbors, "smote.neighbors", min = 1)
  cfg$model$mtry <- vapply(cfg$model$mtry, as_whole, numeric(1), name = "model.mtry", min = 1)
  cfg$model$min_n <- vapply(cfg$model$min_n, as_whole, numeric(1), name = "model.min_n", min = 1)

  prop <- suppressWarnings(as.numeric(cfg$split$test_prop))
  if (length(prop) != 1 || is.na(prop) || prop <= 0 || prop >= 1) {
    abort_config("split.test_prop must be a number strictly between 0 and 1.")
  }
  cfg$split$test_prop <- prop
  if (!identical(length(cfg$split$strategy), 1L) || !cfg$split$strategy %in% c("random", "group")) {
    abort_config("split.strategy must be 'random' or 'group'.")
  }
  over <- suppressWarnings(as.numeric(cfg$smote$over_ratio))
  if (length(over) != 1 || is.na(over) || over <= 0) {
    abort_config("smote.over_ratio must be a positive number.")
  }
  cfg$smote$over_ratio <- over
  breaks <- suppressWarnings(as.numeric(unlist(cfg$target$breaks)))
  if (length(breaks) != 2 || anyNA(breaks) || breaks[1] >= breaks[2]) {
    abort_config("target.breaks must be two increasing numbers.")
  }
  cfg$target$breaks <- breaks
  if (!all(cfg$model$splitrule %in% c("gini", "extratrees", "hellinger"))) {
    abort_config("model.splitrule must use gini, extratrees or hellinger.")
  }
  cfg$log_level <- toupper(cfg$log_level)
  if (!cfg$log_level %in% LOG_LEVELS) {
    abort_config("log_level must be one of ", paste(LOG_LEVELS, collapse = ", "), ".")
  }
  for (p in c("raw_data", "model", "reports_dir")) {
    if (!is.character(cfg$paths[[p]]) || !nzchar(cfg$paths[[p]])) {
      abort_config("paths.", p, " must be a non-empty string.")
    }
  }
  cfg
}

as_whole <- function(x, name, min = -Inf, max = Inf) {
  value <- suppressWarnings(as.numeric(x))
  if (length(value) != 1 || is.na(value) || value != round(value) || value < min || value > max) {
    abort_config(name, " must be a whole number in [", min, ", ", max, "], got: ", format(x))
  }
  value
}

abort_config <- function(...) {
  rlang::abort(paste0(...), class = "acstar_config_error")
}
