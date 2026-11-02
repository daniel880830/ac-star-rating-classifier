# Data loading and cleaning.
#
# `clean_raw()` and `eda_filter()` are a faithful port of the original R Markdown
# cleaning (13 steps) and EDA column-pruning logic. They intentionally keep the
# original behaviour, including known quirks, so the original results can be
# reproduced; the quirks are documented in the README and covered by tests.

# Columns removed in cleaning step 3 (all-NA, administrative, image, or 0/NA only).
COLUMNS_TO_DELETE <- c(
  "avg_pwr_standby_mode", "Setting_heat", "eermepslev", "TestedOutputEER",
  "TestedOutputCOP", "PL_EERMEPS", "PL_COPMEPS", "GrandDate", "star2000_cool",
  "star2000_heat", "Family Name", "Representative Brand URL", "Product Website",
  "Model_No", "Registration Number", "Submit_ID", "SubmitStatus",
  "Star Image Large", "Star Image Small", "Rated cool power input with water",
  "Rated cool cap with water W", "Rated cool cap with water kW",
  "Residential tcec_cold with water", "Residential tcec_mixed with water",
  "Residential tcec_hot with water"
)

# Character columns that hold numbers (cleaning step 5).
COLUMNS_TO_NUMERIC <- c(
  "C-Dehumid_Rated", "C-Power_Inp_Rated", "C-Sens_Cool_Rated", "C-Total Cool Rated",
  "Depth", "H2_COP", "H2_HeatPwrCapacity", "H2_HeatPwrInput", "Height",
  "H-Power_Inp_Rated", "H-Total Heat Rated", "EERtestAvg", "COPtestAvg",
  "Setting_cool", "Pnoc", "Pnoh", "VSCP_EER50", "VSCP_COP50", "AnnualOutputEER",
  "AnnualOutputCOP", "sri2010_cool", "sri2010_heat", "Width", "EER",
  "Rated Total Cool Capacity W", "Rated cooling power input kW",
  "T1_half_cap_power_rated", "T1_half_cap_cooling_cap_rated",
  "T1_min_cap_power_rated", "T1_min_cap_cooling_cap_rated",
  "Low temp cooling full cap power rated", "Low temp cooling full cap rated",
  "Low temp cooling half cap power rated", "Low temp cooling half cap rated",
  "Low temp cooling min cap power rated", "Low temp cooling min cap rated",
  "Rated Heating Capacity watts", "Rated heating power input kW",
  "H1_half_cap_power_rated", "H1_half_cap_heat_cap_rated",
  "H1_min_cap_power_rated", "H1_min_cap_heat_cap_rated",
  "H2_ext_cap_power_rated", "H2_ext_cap_heat_cap_rated",
  "H2_full_cap_power_rated", "H2_full_cap_heat_cap_rated",
  "H2_half_cap_power_rated", "H2_half_cap_heat_cap_rated",
  "H2_min_cap_power_rated", "H2_min_cap_heat_cap_rated",
  "H3_ext_cap_power_rated", "H3_ext_cap_heat_cap_rated",
  "H3_full_cap_power_rated", "H3_full_cap_heat_cap_rated",
  "H3_half_cap_power_rated", "H3_half_cap_heat_cap_rated",
  "indoor_sound_level", "outdoor_sound_level",
  "Residential TCSPF_cold", "Residential TCSPF_mixed", "Residential TCSPF_hot",
  "Commercial TCSPF_cold", "Commercial TCSPF_mixed", "Commercial TCSPF_hot",
  "Residential tcec_cold", "Residential tcec_mixed", "Residential tcec_hot",
  "Commercial tcec_cold", "Commercial tcec_mixed", "Commercial tcec_hot",
  "Residential HSPF_cold", "Residential HSPF_mixed", "Residential HSPF_hot",
  "Commercial HSPF_cold", "Commercial HSPF_mixed", "Commercial HSPF_hot",
  "Residential thec_cold", "Residential thec_mixed", "Residential thec_hot",
  "Commercial thec_cold", "Commercial thec_mixed", "Commercial thec_hot",
  "PIA inoperative power", "Rated AEER", "Rated ACOP"
)

# Categorical columns (cleaning step 5).
COLUMNS_TO_FACTOR <- c(
  "ApplStandard", "MEPSComp", "N-Standard", "Brand", "Configuration1",
  "Configuration2", "Configuration2-unitmount", "Configuration3_Sink",
  "Configuration3_Source", "Country", "indoorType", "Invert", "Star2010_Cool",
  "Star2010_Heat", "outdoortype", "Phase", "Refrigerant", "Sold_in", "Type",
  "Product Class", "Demand Response Capability", "Demand Response 1",
  "Demand Response 2", "Demand Response 4", "Demand Response 5",
  "Demand Response 6", "Demand Response 7", "PartNumber", "Availability Status",
  "Variable Output Compressor", "Have T1 half cap results",
  "Have T1 min cap results", "Have low temp cool full cap results",
  "Have low temp cool half cap results", "Have low temp cool min cap results",
  "Have H1 min cap results", "Have H2 extended mode results",
  "Have H2 full mode results", "Have H2 half capacity results",
  "Have H2 min capacity results", "Have H3 extended mode results",
  "Have H3 full mode results", "Have H3 half capacity results",
  "c_star_cold", "c_star_mixed", "c_star_hot", "h_star_cold", "h_star_mixed",
  "h_star_hot", "Outdoor unit only", "Have_water_tank", "Is variable speed",
  "Type variable output", "Var output compressor",
  "Variable output rated as fixed sp"
)

# Heating columns that should be NA (not 0) for cooling-only units (step 10).
HEAT_COLUMNS <- c(
  "H-Power_Inp_Rated", "H-Total Heat Rated", "COPtestAvg",
  "Rated Heating Capacity watts", "Residential thec_cold",
  "Residential thec_mixed", "Residential thec_hot", "h_star_cold",
  "h_star_mixed", "h_star_hot", "Rated ACOP"
)

# Columns where negative values are physically impossible (step 12).
NON_NEGATIVE_COLUMNS <- c(
  "Residential TCSPF_cold", "Residential TCSPF_mixed", "Residential TCSPF_hot",
  "Commercial TCSPF_cold", "Commercial TCSPF_mixed", "Commercial TCSPF_hot",
  "Residential tcec_cold", "Residential tcec_mixed", "Residential tcec_hot",
  "Commercial tcec_cold", "Commercial tcec_mixed", "Commercial tcec_hot"
)

# Strings the EDA stage treated as missing when re-reading the cleaned CSV.
EDA_NA_STRINGS <- c("", "NA", "N/A", "n/a", "-", "--", "null", "Null", "NULL")

TARGET <- "c_star_mixed"

#' Read the raw Energy Rating CSV
#'
#' @param path Path to the raw CSV file.
#' @return A tibble with the raw columns (types guessed by readr, as in the
#'   original analysis).
#' @export
read_raw <- function(path) {
  if (!file.exists(path)) {
    abort_data("Raw data file not found: ", path)
  }
  if (file.size(path) == 0) {
    abort_data("Raw data file is empty: ", path)
  }
  df <- read_csv_logged(path)
  required <- c(TARGET, "Country", "Sold_in", "Type", "Phase", "Configuration2")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    abort_data("Raw data is missing required columns: ", paste(missing, collapse = ", "))
  }
  df
}

#' Clean the raw data (port of the original 13 cleaning steps)
#'
#' @param df Raw tibble from [read_raw()].
#' @param fix_phase_bug If `TRUE`, also merge `"Three-phase"` into `"Three"`.
#'   The original code compared against `"Three-Phase"` (capital P), which never
#'   matches the raw data, so the merge silently did nothing. Defaults to the
#'   original behaviour.
#' @return The cleaned tibble, with all categorical columns upper-cased.
#' @export
clean_raw <- function(df, fix_phase_bug = FALSE) {
  # Step 1: "N/A" and "-" mean missing.
  df <- dplyr::mutate(df, dplyr::across(
    dplyr::where(is.character),
    ~ dplyr::na_if(dplyr::na_if(.x, "N/A"), "-")
  ))
  # Step 3: drop unusable columns. Step 4: drop rows without a target.
  df <- dplyr::select(df, -dplyr::any_of(COLUMNS_TO_DELETE))
  df <- dplyr::filter(df, !is.na(.data[[TARGET]]))
  # Step 5: fix column types.
  df <- dplyr::mutate(
    df,
    dplyr::across(dplyr::any_of(COLUMNS_TO_NUMERIC), ~ suppressWarnings(as.numeric(.x))),
    dplyr::across(dplyr::any_of(COLUMNS_TO_FACTOR), factor)
  )
  # Step 6: some dehumidification values were recorded in W instead of kW.
  df <- dplyr::mutate(df, `C-Dehumid_Rated` = dplyr::if_else(
    .data$`C-Dehumid_Rated` > 1000, .data$`C-Dehumid_Rated` / 1000, .data$`C-Dehumid_Rated`
  ))
  # Steps 7-8: multi-label country columns become Yes/No indicator columns.
  df <- expand_multilabel(df, "Country", "Country_", top_n = 7)
  df <- expand_multilabel(df, "Sold_in", "Sold_in_")
  # Step 9: drop duplicated columns and two extreme outlier rows.
  df <- dplyr::select(df, -dplyr::any_of(c("Rated cooling power input kW", "Rated heating power input kW")))
  df <- dplyr::filter(
    df,
    .data$`C-Power_Inp_Rated` != 22952 | is.na(.data$`C-Power_Inp_Rated`),
    .data$`H-Power_Inp_Rated` != 19242 | is.na(.data$`H-Power_Inp_Rated`)
  )
  # Step 10: heating values of cooling-only units are not applicable.
  df <- dplyr::mutate(
    df,
    dplyr::across(dplyr::any_of(HEAT_COLUMNS), ~ as.numeric(as.character(.x))),
    dplyr::across(
      dplyr::any_of(HEAT_COLUMNS),
      ~ dplyr::if_else(.data$Type == "Cooling Only" & .x == 0, NA_real_, .x)
    )
  )
  # Step 11: merge equivalent phase labels.
  three_phase <- if (fix_phase_bug) c("Three-Phase", "Three-phase") else "Three-Phase"
  df <- dplyr::mutate(df, Phase = replace(.data$Phase, .data$Phase %in% three_phase, "Three"))
  # Step 12: negative efficiency or energy values are invalid.
  df <- dplyr::mutate(df, dplyr::across(
    dplyr::any_of(NON_NEGATIVE_COLUMNS), ~ dplyr::if_else(.x < 0, NA_real_, .x)
  ))
  # Step 13: harmonise capitalisation of all categorical columns.
  dplyr::mutate(df, dplyr::across(dplyr::where(~ is.character(.x) || is.factor(.x)), toupper))
}

#' Turn a comma-separated multi-label column into Yes/No indicator columns
#'
#' Rows whose column is missing get `NA` in every indicator, matching the
#' original join-based implementation.
#'
#' @param df A tibble.
#' @param column Name of the multi-label column (removed from the output).
#' @param prefix Prefix for the new indicator columns.
#' @param top_n If not `NULL`, keep the `top_n` most frequent labels (ties
#'   included) and pool the rest into `"Other"`.
#' @return `df` without `column` and with one indicator column per label.
#' @keywords internal
expand_multilabel <- function(df, column, prefix, top_n = NULL) {
  long <- df |>
    dplyr::mutate(.row_id = dplyr::row_number()) |>
    dplyr::select(".row_id", value = dplyr::all_of(column)) |>
    tidyr::separate_rows("value", sep = ",") |>
    dplyr::mutate(value = trimws(.data$value)) |>
    dplyr::filter(.data$value != "")
  if (!is.null(top_n)) {
    counts <- dplyr::count(long, .data$value, name = "n")
    keep <- dplyr::slice_max(counts, .data$n, n = top_n, with_ties = TRUE)$value
    long <- dplyr::mutate(long, value = dplyr::if_else(.data$value %in% keep, .data$value, "Other"))
  }
  wide <- long |>
    dplyr::distinct(.data$.row_id, .data$value) |>
    dplyr::mutate(flag = "Yes") |>
    tidyr::pivot_wider(
      id_cols = ".row_id", names_from = "value", names_prefix = prefix,
      values_from = "flag", values_fill = "No"
    )
  df |>
    dplyr::select(-dplyr::all_of(column)) |>
    dplyr::mutate(.row_id = dplyr::row_number()) |>
    dplyr::left_join(wide, by = ".row_id") |>
    dplyr::select(-".row_id")
}

#' Re-infer column types the way the original CSV round trip did
#'
#' The original analysis wrote each stage to CSV and read it back, which
#' re-guesses every column type (for example factor star ratings become
#' numeric and all-NA columns become logical). Later stages depend on those
#' types, so the round trip is reproduced in memory.
#'
#' @param df A tibble.
#' @param na Strings to treat as missing when re-reading.
#' @return The re-typed tibble.
#' @keywords internal
csv_roundtrip <- function(df, na = c("", "NA")) {
  read_csv_logged(I(readr::format_csv(df)), na = na)
}

# readr::read_csv() with type guessing, as in the original analysis. Cells that
# do not fit the guessed type become NA (on the raw file: three GrandDate
# values and three "N/A" capacities); they are logged instead of raising a
# generic warning.
read_csv_logged <- function(file, na = c("", "NA")) {
  df <- withCallingHandlers(
    readr::read_csv(file, na = na, show_col_types = FALSE, progress = FALSE),
    warning = function(w) {
      if (grepl("parsing issues", conditionMessage(w), fixed = TRUE)) invokeRestart("muffleWarning")
    }
  )
  probs <- readr::problems(df)
  if (nrow(probs) > 0) {
    logger::log_debug(namespace = "acstar", sprintf(
      "%d cell(s) did not match the guessed column type and were read as NA (%s)",
      nrow(probs), paste(unique(names(df)[probs$col]), collapse = ", ")
    ))
  }
  df
}

#' Summarise missingness and cardinality of every column
#'
#' @param df A tibble.
#' @return A tibble with one row per column.
#' @keywords internal
column_profile <- function(df) {
  tibble::tibble(
    feature = names(df),
    dtype = vapply(df, function(x) class(x)[1], character(1)),
    n_unique = vapply(df, function(x) dplyr::n_distinct(x, na.rm = TRUE), integer(1)),
    missing_pct = vapply(df, function(x) round(100 * mean(is.na(x)), 2), numeric(1))
  )
}

#' Decide which columns the EDA stage drops
#'
#' A column is dropped when it is at least 90% missing, is categorical with at
#' most two levels, or is categorical with one level covering at least 80% of
#' the non-missing rows.
#'
#' @param df The cleaned, re-typed tibble (target already a factor).
#' @return A tibble of column decisions with a logical `drop` column.
#' @export
eda_decisions <- function(df) {
  prof <- column_profile(df)
  is_cat <- prof$dtype %in% c("character", "factor")
  dominant <- vapply(prof$feature, function(nm) {
    x <- df[[nm]]
    x <- x[!is.na(x)]
    length(x) > 0 && max(prop.table(table(x))) >= 0.8
  }, logical(1))
  prof$flag_missing90 <- prof$missing_pct >= 90
  prof$flag_low_card <- is_cat & prof$n_unique <= 2
  prof$flag_rare_levels <- is_cat & dominant
  prof$drop <- prof$feature != TARGET &
    (prof$flag_missing90 | prof$flag_low_card | prof$flag_rare_levels)
  prof
}

#' Apply the EDA stage: drop flagged columns and portable units
#'
#' Portable units are always rated 0 stars under a separate labelling rule, so
#' the original analysis removed them to avoid a rule-based shortcut.
#'
#' @param cleaned Output of [clean_raw()].
#' @return The modelling table (target numeric, as in the original CSV hand-off).
#' @export
eda_filter <- function(cleaned) {
  df <- csv_roundtrip(cleaned, na = EDA_NA_STRINGS)
  df[[TARGET]] <- as.factor(df[[TARGET]])
  decisions <- eda_decisions(df)
  df <- df |>
    dplyr::mutate(Configuration2 = stringr::str_trim(.data$Configuration2)) |>
    dplyr::filter(tolower(.data$Configuration2) != "portable") |>
    dplyr::select(-dplyr::all_of(decisions$feature[decisions$drop]))
  csv_roundtrip(df)
}

#' Build the modelling table from the raw CSV
#'
#' @param path Path to the raw CSV.
#' @param fix_phase_bug Passed to [clean_raw()].
#' @return The modelling table produced by [eda_filter()].
#' @export
build_dataset <- function(path, fix_phase_bug = FALSE) {
  eda_filter(clean_raw(read_raw(path), fix_phase_bug = fix_phase_bug))
}

#' Band the numeric star rating into low / mid / high
#'
#' Uses right-closed intervals like the original `cut()` call, so with the
#' default breaks a 4.0-star unit is "mid" (the original appendix described
#' high as ">= 4.0", which the code did not implement).
#'
#' @param stars Numeric star ratings.
#' @param breaks Two increasing cut points.
#' @return A factor with levels `low`, `mid`, `high`.
#' @export
star_band <- function(stars, breaks = c(2.5, 4.0)) {
  if (!is.numeric(stars)) {
    abort_data("Star ratings must be numeric.")
  }
  if (length(breaks) != 2 || is.unsorted(breaks, strictly = TRUE)) {
    abort_data("`breaks` must be two strictly increasing numbers.")
  }
  cut(stars, breaks = c(-Inf, breaks, Inf), labels = c("low", "mid", "high"), right = TRUE)
}

abort_data <- function(...) {
  rlang::abort(paste0(...), class = "acstar_data_error")
}
