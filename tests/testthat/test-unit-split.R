frame <- model_frame(make_dataset(200, twins = 40))

test_that("group split never puts twins on both sides", {
  split <- split_data(frame, "group", test_prop = 0.25, seed = 7)
  train <- rsample::training(split)
  test <- rsample::testing(split)
  expect_length(intersect(train$spec_group, test$spec_group), 0)
  expect_equal(twin_overlap(train, test), 0)
  expect_equal(nrow(train) + nrow(test), nrow(frame))
})

test_that("random split leaks twins across the split (the original method)", {
  split <- split_data(frame, "random", test_prop = 0.25, seed = 7)
  expect_gt(twin_overlap(rsample::training(split), rsample::testing(split)), 0)
})

test_that("splits are reproducible for a fixed seed", {
  a <- split_data(frame, "group", seed = 3)
  b <- split_data(frame, "group", seed = 3)
  expect_identical(rsample::complement(a), rsample::complement(b))
})

test_that("group resamples keep every group inside a single fold", {
  train <- rsample::training(split_data(frame, "group", seed = 1))
  folds <- make_resamples(train, "group", folds = 3, repeats = 2, seed = 1)
  expect_equal(nrow(folds), 6)
  for (s in folds$splits) {
    expect_length(intersect(rsample::analysis(s)$spec_group, rsample::assessment(s)$spec_group), 0)
  }
  expect_equal(nrow(make_resamples(train, "random", folds = 2, repeats = 1)), 2)
})

test_that("unknown strategies are rejected", {
  expect_error(split_data(frame, "temporal"), class = "acstar_config_error")
  expect_error(make_resamples(frame, "temporal"), class = "acstar_config_error")
})
