test_that("read_raw reports missing, empty and incomplete files", {
  expect_error(read_raw(tempfile(fileext = ".csv")), "not found", class = "acstar_data_error")
  empty <- withr::local_tempfile(fileext = ".csv")
  file.create(empty)
  expect_error(read_raw(empty), "empty", class = "acstar_data_error")
  partial <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(c_star_mixed = 1, Brand = "X"), partial)
  expect_error(read_raw(partial), "missing required columns", class = "acstar_data_error")
})

test_that("clean_raw applies the original cleaning steps", {
  raw <- read_raw(write_raw_csv(make_raw()))
  cleaned <- clean_raw(raw)
  expect_false(anyNA(cleaned$c_star_mixed))
  expect_equal(nrow(cleaned), nrow(raw) - 2) # one missing target, one outlier row
  expect_true(all(cleaned$`C-Dehumid_Rated` <= 1000, na.rm = TRUE))
  expect_false("Country" %in% names(cleaned))
  expect_true(all(c("Country_China", "Country_Thailand", "Sold_in_Fiji") %in% names(cleaned)))
  # Step 13 upper-cases every character column, including the new indicators.
  expect_true(all(cleaned$Country_China %in% c("YES", "NO")))
  expect_true(all(is.na(cleaned$Refrigerant) | cleaned$Refrigerant %in% c("R32", "R410A")))
  cooling_only_heat <- cleaned$`H-Power_Inp_Rated`[cleaned$Type == "COOLING ONLY"]
  expect_false(any(cooling_only_heat == 0, na.rm = TRUE))
  expect_true(all(cleaned$Brand == toupper(cleaned$Brand)))
})

test_that("the original phase merge is a no-op; the fixed version merges", {
  raw <- read_raw(write_raw_csv(make_raw()))
  expect_true("THREE-PHASE" %in% clean_raw(raw)$Phase)
  expect_false("THREE-PHASE" %in% clean_raw(raw, fix_phase_bug = TRUE)$Phase)
})

test_that("multi-label expansion pools rare labels and leaves NA rows as NA", {
  df <- tibble::tibble(id = 1:5, Country = c("A,B", "A", "C", NA, "A, B,D"))
  out <- expand_multilabel(df, "Country", "Country_", top_n = 2)
  expect_setequal(setdiff(names(out), "id"), c("Country_A", "Country_B", "Country_Other"))
  expect_equal(out$Country_A, c("Yes", "Yes", "No", NA, "Yes"))
  expect_equal(out$Country_Other, c("No", "No", "Yes", NA, "Yes"))
})

test_that("negative efficiency values become NA", {
  raw <- make_raw()
  raw$`Residential TCSPF_mixed` <- c(-1, rep(4, nrow(raw) - 1))
  cleaned <- clean_raw(read_raw(write_raw_csv(raw)))
  expect_false(any(cleaned$`Residential TCSPF_mixed` < 0, na.rm = TRUE))
})

test_that("star_band uses right-closed bands and validates input", {
  expect_equal(
    as.character(star_band(c(0, 2.5, 2.6, 4.0, 4.5, 7))),
    c("low", "low", "mid", "mid", "high", "high")
  )
  expect_error(star_band("3"), class = "acstar_data_error")
  expect_error(star_band(1, breaks = c(4, 2)), class = "acstar_data_error")
  expect_error(star_band(1, breaks = 3), class = "acstar_data_error")
})

test_that("eda_decisions flags sparse, binary and dominated columns", {
  df <- tibble::tibble(
    c_star_mixed = factor(rep(1:2, 10)),
    sparse = c(1, rep(NA, 19)),
    binary = rep(c("A", "B"), 10),
    dominated = c(rep("A", 17), "B", "C", "D"),
    useful = rep(c("A", "B", "C", "D"), 5),
    numeric = 1:20
  )
  d <- eda_decisions(df)
  expect_equal(d$feature[d$drop], c("sparse", "binary", "dominated"))
})

test_that("build_dataset removes portable units and keeps the target numeric", {
  data <- build_dataset(write_raw_csv(make_raw()))
  expect_true(is.numeric(data$c_star_mixed))
  expect_false("PORTABLE" %in% data$Configuration2)
  expect_true(all(c("Brand", "Rated AEER") %in% names(data)))
})
