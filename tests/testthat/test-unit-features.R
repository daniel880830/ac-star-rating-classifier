test_that("snake_case normalises column names", {
  expect_equal(
    snake_case(c("Rated AEER", "C-Power_Inp_Rated", "Low temp cooling half cap power rated", "-x-")),
    c("rated_aeer", "c_power_inp_rated", "low_temp_cooling_half_cap_power_rated", "x")
  )
})

test_that("prepare_features accepts original or snake_case names and normalises values", {
  data <- make_dataset(10, twins = 0)
  from_original <- prepare_features(data)
  expect_equal(names(from_original), snake_case(ORIGINAL_TOP23))
  expect_identical(prepare_features(from_original), from_original)
  data$Brand[1] <- "  daikin "
  expect_equal(prepare_features(data)$brand[1], "DAIKIN")
  expect_true(all(vapply(from_original[-1], is.double, logical(1))))
})

test_that("prepare_features rejects missing columns", {
  data <- make_dataset(5, twins = 0)
  data$`Rated AEER` <- NULL
  expect_error(prepare_features(data), "rated_aeer", class = "acstar_data_error")
})

test_that("spec_group gives exact twins the same id", {
  frame <- model_frame(make_dataset(50, twins = 10))
  expect_equal(nrow(frame), 60)
  expect_equal(length(unique(frame$spec_group)), 50)
  key <- do.call(paste, frame[setdiff(names(frame), c("brand", "star3", "spec_group"))])
  expect_equal(frame$spec_group[51:60], frame$spec_group[match(key[51:60], key[1:50])])
  expect_error(spec_group(tibble::tibble(a = "x")), class = "acstar_data_error")
})

test_that("model_frame drops rows without a target", {
  data <- make_dataset(20, twins = 0)
  data$c_star_mixed[1:3] <- NA
  expect_equal(nrow(model_frame(data)), 17)
  data$c_star_mixed <- NULL
  expect_error(model_frame(data), class = "acstar_data_error")
})
