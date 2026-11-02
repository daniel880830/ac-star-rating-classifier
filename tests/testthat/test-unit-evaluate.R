lv <- c("low", "mid", "high")
f <- function(x) factor(x, levels = lv)

test_that("perfect predictions score 1 on every metric", {
  y <- f(c("low", "mid", "high", "low"))
  m <- classification_metrics(y, y)
  expect_equal(unname(m), c(1, 1, 1))
})

test_that("a class never predicted contributes F1 = 0 (original definition)", {
  truth <- f(c("low", "low", "mid", "high"))
  pred <- f(c("low", "low", "mid", "mid"))
  pc <- per_class_metrics(truth, pred)
  expect_equal(pc$f1[pc$class == "high"], 0)
  expect_equal(pc$support, c(2, 1, 1))
  expect_equal(classification_metrics(truth, pred)[["macro_f1"]], mean(c(1, 2 / 3, 0)))
})

test_that("kappa matches a hand-computed value", {
  truth <- f(c("low", "low", "mid", "mid"))
  pred <- f(c("low", "mid", "mid", "mid"))
  # observed 0.75, expected (1*2 + 3*2) / 16 = 0.5 -> kappa 0.5
  expect_equal(classification_metrics(truth, pred)[["kappa"]], 0.5)
})

test_that("bootstrap CIs are reproducible and bracket the point estimate", {
  y <- f(rep(lv, c(40, 30, 10)))
  yh <- y
  yh[c(1, 45, 75)] <- f(c("mid", "low", "mid"))
  a <- bootstrap_ci(y, yh, times = 200, seed = 1)
  b <- bootstrap_ci(y, yh, times = 200, seed = 1)
  expect_identical(a, b)
  expect_true(all(a$lower <= a$estimate & a$estimate <= a$upper))
  expect_error(bootstrap_ci(y, yh, times = 0), class = "acstar_data_error")
})

test_that("metric inputs are checked", {
  expect_error(classification_metrics(c("low"), f("low")), class = "acstar_data_error")
  expect_error(classification_metrics(f(c("low", "mid")), f("low")), class = "acstar_data_error")
  expect_error(classification_metrics(f(character()), f(character())), class = "acstar_data_error")
})

test_that("twin_overlap is the share of test rows with a training twin", {
  train <- tibble::tibble(spec_group = c(1, 2, 3))
  test <- tibble::tibble(spec_group = c(3, 4, 5, 3))
  expect_equal(twin_overlap(train, test), 0.5)
  expect_equal(twin_overlap(train, test[0, ]), 0)
})
