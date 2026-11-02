# Feature definitions shared by training, evaluation and inference.

# The 23 predictors used by the original model, in the original column order.
# Note: this hand-written list differs from the output of the original feature
# selection step (it adds Brand and the two "thec_mixed" columns, and omits
# T1_half_cap_cooling_cap_rated and the two "thec_cold" columns).
ORIGINAL_TOP23 <- c(
  "Brand", "Rated AEER", "EERtestAvg", "C-Power_Inp_Rated", "Residential HSPF_hot",
  "Commercial HSPF_hot", "PIA inoperative power", "Commercial HSPF_mixed",
  "Rated ACOP", "Commercial HSPF_cold", "COPtestAvg", "Residential HSPF_cold",
  "Residential HSPF_mixed", "Rated Total Cool Capacity W", "C-Total Cool Rated",
  "T1_half_cap_power_rated", "Commercial thec_hot", "Residential thec_hot",
  "H-Power_Inp_Rated", "Low temp cooling half cap power rated",
  "Residential thec_mixed", "Commercial thec_mixed", "H1_half_cap_power_rated"
)

# Columns computed with the same formula (or from the same seasonal model) as
# the target. Keeping any of them lets a model read the label off the inputs.
LEAKAGE_COLUMNS <- c(
  "c_star_cold", "c_star_hot", "h_star_cold", "h_star_mixed", "h_star_hot",
  "Residential TCSPF_cold", "Residential TCSPF_mixed", "Residential TCSPF_hot",
  "Commercial TCSPF_cold", "Commercial TCSPF_mixed", "Commercial TCSPF_hot",
  "Residential tcec_cold", "Residential tcec_mixed", "Residential tcec_hot",
  "Commercial tcec_cold", "Commercial tcec_mixed", "Commercial tcec_hot",
  "Star2010_Cool", "Star2010_Heat", "sri2010_cool", "sri2010_heat"
)

#' Convert column names to snake_case
#'
#' @param x Character vector of column names.
#' @return Lower-case names with every run of non-alphanumeric characters
#'   replaced by a single underscore.
#' @export
snake_case <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  gsub("^_|_$", "", x)
}

#' Prepare model inputs from a cleaned table or from API records
#'
#' This is the single place where raw column values are normalised, so the
#' exact same transformation runs at training and at inference time.
#'
#' @param df A data frame containing the original-named feature columns
#'   (training) or the snake_case feature columns (inference).
#' @param features Original feature names.
#' @return A tibble with snake_case feature columns in a fixed order. `brand`
#'   is upper-cased and trimmed; numeric columns are coerced to double.
#' @export
prepare_features <- function(df, features = ORIGINAL_TOP23) {
  snake <- snake_case(features)
  if (all(features %in% names(df))) {
    df <- df[features]
    names(df) <- snake
  }
  missing <- setdiff(snake, names(df))
  if (length(missing) > 0) {
    abort_data("Missing feature columns: ", paste(missing, collapse = ", "))
  }
  out <- tibble::as_tibble(df[snake])
  for (nm in snake) {
    if (nm == "brand") {
      out[[nm]] <- toupper(trimws(as.character(out[[nm]])))
    } else {
      out[[nm]] <- as.double(out[[nm]])
    }
  }
  out
}

#' Build the modelling frame: features plus the banded target
#'
#' @param data Output of [build_dataset()].
#' @param features Original feature names.
#' @param breaks Star-band cut points passed to [star_band()].
#' @return A tibble with snake_case features, `star3` and a `spec_group` id.
#' @export
model_frame <- function(data, features = ORIGINAL_TOP23, breaks = c(2.5, 4.0)) {
  if (!TARGET %in% names(data)) {
    abort_data("Data has no `", TARGET, "` column.")
  }
  data <- data[!is.na(data[[TARGET]]), , drop = FALSE]
  out <- prepare_features(data, features)
  out$star3 <- star_band(as.numeric(data[[TARGET]]), breaks)
  out$spec_group <- spec_group(out)
  out
}

#' Identify rows that share an identical predictor vector
#'
#' Many products are registered several times (different model numbers for
#' the same indoor/outdoor pairing, or rebadged units), producing rows whose
#' predictors are exactly equal. Grouping on the numeric predictors keeps such
#' twins on the same side of a train/test split.
#'
#' @param df A frame from [prepare_features()].
#' @return An integer group id per row.
#' @export
spec_group <- function(df) {
  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  numeric_cols <- setdiff(numeric_cols, c("star3", "spec_group"))
  if (length(numeric_cols) == 0) {
    abort_data("No numeric columns to build specification groups from.")
  }
  key <- do.call(paste, c(lapply(df[numeric_cols], function(x) format(x, digits = 10)), sep = "|"))
  as.integer(factor(key, levels = unique(key)))
}
