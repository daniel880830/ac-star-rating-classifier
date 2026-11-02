# Command line interface: `Rscript exec/acstar.R <command> [options]`.

CLI_USAGE <- "Usage: acstar <command> [options]

Commands:
  tune       Cross-validate the hyper-parameter grid and save the best values
  train      Fit the final model (tunes first if needed) and save the artifact
  ablation   Measure how each leakage fix changes the test scores
  serve      Start the REST API
  help       Show this message

Options:
  --profile NAME   Settings profile: default (leakage-aware) or original
  --host HOST      API host (serve)
  --port PORT      API port (serve)
  --seeds N        Number of split seeds (ablation, default 5)
"

#' Run the command line interface
#'
#' @param args Character vector of arguments (without the script name).
#' @return An exit status (0 on success), invisibly.
#' @export
cli_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  parsed <- parse_cli_args(args)
  if (!is.null(parsed$error)) {
    message(parsed$error, "\n\n", CLI_USAGE)
    return(invisible(2L))
  }
  if (parsed$command %in% c("help", "--help", "-h")) {
    cat(CLI_USAGE)
    return(invisible(0L))
  }
  if (!is.null(parsed$options$host)) Sys.setenv(APP_HOST = parsed$options$host)
  if (!is.null(parsed$options$port)) Sys.setenv(APP_PORT = parsed$options$port)
  status <- tryCatch(
    {
      settings <- load_settings(profile = parsed$options$profile %||% Sys.getenv("APP_PROFILE", "default"))
      logger::log_threshold(settings$log_level)
      run_command(parsed$command, settings, parsed$options)
      0L
    },
    acstar_config_error = function(e) cli_fail("Configuration error: ", conditionMessage(e)),
    acstar_data_error = function(e) cli_fail("Data error: ", conditionMessage(e)),
    acstar_model_error = function(e) cli_fail("Model error: ", conditionMessage(e))
  )
  invisible(status)
}

run_command <- function(command, settings, options) {
  switch(command,
    tune = {
      tuned <- tune_model(settings)
      # Keep side effects out of log calls: logger skips evaluating messages
      # below the threshold.
      written <- write_tuning(settings, tuned)
      logger::log_info(namespace = "acstar", sprintf(
        "Best: mtry=%s min_n=%s splitrule=%s; CV macro-F1 %.3f",
        tuned$best$mtry, tuned$best$min_n, tuned$best$splitrule, tuned$cv$macro_f1$mean
      ))
      logger::log_info(namespace = "acstar", paste("Wrote", written))
    },
    train = {
      model <- train_model(settings)
      m <- model$metadata$test$metrics
      logger::log_info(namespace = "acstar", sprintf(
        "Test macro-F1 %.3f, kappa %.3f, accuracy %.3f (%s split, %d test rows)",
        m$macro_f1, m$kappa, m$accuracy, model$metadata$split_strategy, model$metadata$n_test
      ))
    },
    ablation = {
      seeds <- as.integer(options$seeds %||% 5L)
      result <- run_ablation(settings, n_seeds = seeds)
      print(result$summary, n = Inf, width = Inf)
    },
    serve = run_api(settings)
  )
}

parse_cli_args <- function(args) {
  commands <- c("tune", "train", "ablation", "serve", "help", "--help", "-h")
  if (length(args) == 0) {
    return(list(error = "No command given."))
  }
  command <- args[1]
  if (!command %in% commands) {
    return(list(error = paste0("Unknown command: ", command)))
  }
  rest <- args[-1]
  allowed <- c("--profile", "--host", "--port", "--seeds")
  options <- list()
  i <- 1
  while (i <= length(rest)) {
    key <- rest[i]
    if (!key %in% allowed) {
      return(list(error = paste0("Unknown option: ", key)))
    }
    if (i == length(rest) || startsWith(rest[i + 1], "--")) {
      return(list(error = paste0("Option ", key, " needs a value.")))
    }
    options[[sub("^--", "", key)]] <- rest[i + 1]
    i <- i + 2
  }
  list(command = command, options = options)
}

cli_fail <- function(...) {
  logger::log_error(namespace = "acstar", paste0(...))
  1L
}
