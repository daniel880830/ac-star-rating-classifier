test_that("a valid record passes and is normalised", {
  rec <- valid_record()
  rec$brand <- " daikin"
  out <- validate_records(list(rec))
  expect_true(out$ok)
  expect_equal(out$data$brand, "DAIKIN")
  expect_equal(ncol(out$data), nrow(FEATURE_SCHEMA))
})

test_that("nullable fields accept null, required fields do not", {
  rec <- valid_record()
  rec["residential_hspf_hot"] <- list(NULL)
  expect_true(validate_records(list(rec))$ok)
  rec["rated_aeer"] <- list(NULL)
  out <- validate_records(list(rec))
  expect_false(out$ok)
  expect_match(out$errors, "rated_aeer: must not be null")
})

test_that("extra, missing, mistyped and out-of-range fields are reported", {
  cases <- list(
    extra = c(valid_record(), list(colour = "red")),
    missing = valid_record()[-2],
    string_for_number = utils::modifyList(valid_record(), list(rated_aeer = "4.2")),
    boolean = utils::modifyList(valid_record(), list(rated_aeer = TRUE)),
    too_large = utils::modifyList(valid_record(), list(rated_aeer = 99)),
    negative = utils::modifyList(valid_record(), list(c_power_inp_rated = -1)),
    array = utils::modifyList(valid_record(), list(rated_aeer = list(1, 2))),
    empty_brand = utils::modifyList(valid_record(), list(brand = "  ")),
    long_brand = utils::modifyList(valid_record(), list(brand = strrep("x", 101))),
    numeric_brand = utils::modifyList(valid_record(), list(brand = 5))
  )
  patterns <- c(
    "unexpected field", "missing field", "finite number", "finite number", "between",
    "between", "single value", "1-100 characters", "1-100 characters", "must be a string"
  )
  for (i in seq_along(cases)) {
    out <- validate_records(list(cases[[i]]))
    expect_false(out$ok, label = names(cases)[i])
    expect_match(paste(out$errors, collapse = " "), patterns[i], label = names(cases)[i])
  }
})

test_that("non-object records and record indices are reported", {
  out <- validate_records(list(valid_record(), list(1, 2), "text"))
  expect_false(out$ok)
  expect_match(out$errors[1], "^record 2: must be a JSON object")
  expect_match(out$errors[2], "^record 3: must be a JSON object")
})
