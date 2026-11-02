test_that("argument parsing accepts known commands and options", {
  p <- parse_cli_args(c("train", "--profile", "original"))
  expect_null(p$error)
  expect_equal(p$command, "train")
  expect_equal(p$options$profile, "original")
  expect_equal(parse_cli_args(c("serve", "--host", "0.0.0.0", "--port", "9000"))$options$port, "9000")
})

test_that("argument parsing reports mistakes", {
  expect_match(parse_cli_args(character())$error, "No command")
  expect_match(parse_cli_args("deploy")$error, "Unknown command")
  expect_match(parse_cli_args(c("train", "--fast", "yes"))$error, "Unknown option")
  expect_match(parse_cli_args(c("train", "--profile"))$error, "needs a value")
  expect_match(parse_cli_args(c("train", "--profile", "--port"))$error, "needs a value")
})

test_that("cli_main returns exit codes", {
  expect_output(expect_equal(cli_main("help"), 0L), "Usage")
  expect_message(expect_equal(cli_main("deploy"), 2L), "Unknown command")
  withr::local_envvar(APP_CONFIG = tempfile(fileext = ".yml"))
  expect_equal(suppressMessages(cli_main(c("train"))), 1L)
})

test_that("cli train and tune run end to end on a small configuration", {
  dir <- withr::local_tempdir()
  cfg <- file.path(dir, "config.yml")
  small <- test_settings(dir, model = list(trees = 15, mtry = 3, min_n = 3, splitrule = "gini"))
  yaml::write_yaml(list(default = small), cfg)
  readr::write_csv(make_raw(), file.path(dir, "raw.csv"), na = "")
  withr::local_envvar(APP_CONFIG = cfg)
  expect_equal(suppressMessages(cli_main("tune")), 0L)
  expect_true(file.exists(file.path(dir, "reports", "tuning_default.json")))
  expect_equal(suppressMessages(cli_main("train")), 0L)
  expect_true(file.exists(file.path(dir, "model.rds")))
})
