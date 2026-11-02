test_that("load_dotenv sets missing variables, keeps existing ones and strips quotes", {
  path <- withr::local_tempfile(lines = c(
    "# comment", "", "APP_TEST_A=one", "APP_TEST_B = \"two words\"", "APP_TEST_C='three'"
  ))
  withr::local_envvar(APP_TEST_A = NA, APP_TEST_B = "already", APP_TEST_C = NA)
  set <- load_dotenv(path)
  expect_setequal(set, c("APP_TEST_A", "APP_TEST_C"))
  expect_equal(Sys.getenv("APP_TEST_A"), "one")
  expect_equal(Sys.getenv("APP_TEST_B"), "already")
  expect_equal(Sys.getenv("APP_TEST_C"), "three")
})

test_that("load_dotenv ignores a missing file and warns on malformed lines", {
  expect_length(load_dotenv(tempfile()), 0)
  path <- withr::local_tempfile(lines = c("not a pair", "APP_TEST_D=4"))
  withr::local_envvar(APP_TEST_D = NA)
  expect_warning(load_dotenv(path), "malformed")
  expect_equal(Sys.getenv("APP_TEST_D"), "4")
})

config_file <- function() testthat::test_path("..", "..", "config", "config.yml")

test_that("profiles load and inherit from default", {
  default <- load_settings("default", config_file(), env_file = tempfile())
  original <- load_settings("original", config_file(), env_file = tempfile())
  expect_equal(default$split$strategy, "group")
  expect_equal(original$split$strategy, "random")
  expect_equal(original$model$trees, 100)
  expect_equal(original$seed, default$seed)
  expect_equal(original$profile, "original")
})

test_that("APP_ environment variables override the file", {
  withr::local_envvar(APP_PORT = "9123", APP_MODEL_PATH = "elsewhere.rds", APP_LOG_LEVEL = "debug")
  s <- load_settings("default", config_file(), env_file = tempfile())
  expect_identical(s$api$port, 9123)
  expect_equal(s$paths$model, "elsewhere.rds")
  expect_equal(s$log_level, "DEBUG")
})

test_that("unknown profile and missing file are configuration errors", {
  expect_error(load_settings("nope", config_file(), env_file = tempfile()), class = "acstar_config_error")
  expect_error(load_settings("default", tempfile(), env_file = tempfile()), class = "acstar_config_error")
})

test_that("invalid values are rejected", {
  base <- load_settings("default", config_file(), env_file = tempfile())
  bad <- list(
    list(api = list(port = 0)), list(api = list(port = "abc")), list(api = list(max_batch_size = 0)),
    list(split = list(test_prop = 1.5)), list(split = list(strategy = "stratified")),
    list(target = list(breaks = c(4, 2.5))), list(log_level = "LOUD"), list(model = list(mtry = 2.5)),
    list(cv = list(folds = 1)), list(smote = list(over_ratio = -1)), list(paths = list(model = "")),
    list(model = list(splitrule = "variance")), list(seed = "x")
  )
  for (override in bad) {
    expect_error(validate_settings(utils::modifyList(base, override)), class = "acstar_config_error")
  }
})
