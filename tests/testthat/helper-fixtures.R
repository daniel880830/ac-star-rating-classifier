# Synthetic fixtures: small, fast, and shaped like the real data.

# A modelling table with the original feature names and a numeric target in
# which the star rating mostly follows rated AEER.
make_dataset <- function(n = 240, seed = 1, twins = 20) {
  withr::with_seed(seed, {
    aeer <- stats::runif(n, 2.8, 5.6)
    # Roughly 50% low, 25% mid, 25% high, so SMOTE has enough minority rows.
    stars <- pmin(7, pmax(0, round((aeer - 2.8) * 4 + stats::rnorm(n, 0, 0.5)) / 2))
    df <- tibble::tibble(
      c_star_mixed = stars,
      Brand = sample(c("Daikin", "Fujitsu", "Mitsubishi Electric", "LG", "Samsung"), n, replace = TRUE),
      `Rated AEER` = aeer,
      EERtestAvg = aeer + stats::rnorm(n, 0, 0.1),
      `C-Power_Inp_Rated` = stats::runif(n, 0.4, 20),
      `Residential HSPF_hot` = stats::runif(n, 3, 6),
      `Commercial HSPF_hot` = stats::runif(n, 3, 6),
      `PIA inoperative power` = stats::runif(n, 0.5, 30),
      `Commercial HSPF_mixed` = stats::runif(n, 3, 6),
      `Rated ACOP` = aeer + stats::rnorm(n, 0.2, 0.2),
      `Commercial HSPF_cold` = stats::runif(n, 2, 5),
      COPtestAvg = aeer + stats::rnorm(n, 0.3, 0.2),
      `Residential HSPF_cold` = stats::runif(n, 2, 5),
      `Residential HSPF_mixed` = stats::runif(n, 3, 6),
      `Rated Total Cool Capacity W` = round(stats::runif(n, 2000, 20000)),
      `C-Total Cool Rated` = 0,
      T1_half_cap_power_rated = stats::runif(n, 100, 3000),
      `Commercial thec_hot` = stats::runif(n, 50, 1500),
      `Residential thec_hot` = stats::runif(n, 50, 2500),
      `H-Power_Inp_Rated` = stats::runif(n, 0.4, 20),
      `Low temp cooling half cap power rated` = stats::runif(n, 100, 3000),
      `Residential thec_mixed` = stats::runif(n, 100, 9000),
      `Commercial thec_mixed` = stats::runif(n, 100, 5000),
      H1_half_cap_power_rated = stats::runif(n, 100, 3000)
    )
    df$`C-Total Cool Rated` <- df$`Rated Total Cool Capacity W` / 1000
    df$`Residential HSPF_hot`[sample(n, n %/% 10)] <- NA
    if (twins > 0) {
      df <- dplyr::bind_rows(df, df[sample(n, twins), ])
    }
    df
  })
}

# A raw-format table: the modelling columns plus the columns the cleaning
# steps touch, with the raw data's quirks ("N/A", "-", mixed case, ...).
make_raw <- function(n = 240, seed = 1) {
  df <- make_dataset(n, seed, twins = 0)
  withr::with_seed(seed, {
    df$Type <- sample(c("Reverse Cycle", "Cooling Only"), n, replace = TRUE, prob = c(0.7, 0.3))
    df$Phase <- sample(c("Single", "Three", "Three-phase"), n, replace = TRUE, prob = c(0.6, 0.3, 0.1))
    df$Configuration2 <- sample(c("Single Split System", "Packaged", "vrf", "Portable"), n, replace = TRUE)
    df$Country <- sample(c("China", "Thailand", "China, Thailand", "Japan", "Malaysia,Japan"), n, replace = TRUE)
    df$Sold_in <- sample(c("Australia,New Zealand", "Australia", "Australia,Fiji,New Zealand"), n, replace = TRUE)
    df$`C-Dehumid_Rated` <- as.character(round(stats::runif(n, 0.5, 3), 2))
    df$Refrigerant <- sample(c("R32", "R410A", "N/A", "-"), n, replace = TRUE)
  })
  df$`C-Dehumid_Rated`[1] <- "1500"
  df$`H-Power_Inp_Rated`[which(df$Type == "Cooling Only")[1:3]] <- 0
  df$`C-Power_Inp_Rated`[2] <- 22952
  df$c_star_mixed[3] <- NA
  df
}

write_raw_csv <- function(df, dir = withr::local_tempdir(.local_envir = parent.frame())) {
  path <- file.path(dir, "raw.csv")
  readr::write_csv(df, path, na = "")
  path
}

# Settings for fast tests, pointing every path into a temporary directory.
test_settings <- function(dir, profile = "default", ...) {
  cfg <- list(
    seed = 42, log_level = "ERROR", profile = profile,
    paths = list(
      raw_data = file.path(dir, "raw.csv"), model = file.path(dir, "model.rds"),
      reports_dir = file.path(dir, "reports")
    ),
    target = list(breaks = c(2.5, 4.0)),
    split = list(strategy = if (profile == "original") "random" else "group", test_prop = 0.25),
    cv = list(folds = 2, repeats = 1),
    smote = list(over_ratio = 1, neighbors = 3),
    model = list(trees = 25, mtry = 4, min_n = 3, splitrule = "gini"),
    api = list(host = "127.0.0.1", port = 8000, max_batch_size = 5)
  )
  utils::modifyList(cfg, list(...))
}

# A fitted model artifact on synthetic data, cached for the test session.
fixture_model <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      dir <- tempfile("acstar-model-")
      dir.create(dir)
      settings <- validate_settings(test_settings(dir))
      prepared <- prepare_data(settings, data = make_dataset())
      cache <<- train_model(settings,
        params = list(mtry = 4, min_n = 3, splitrule = "gini"),
        prepared = prepared, save = FALSE
      )
    }
    cache
  }
})

valid_record <- function() example_record()
