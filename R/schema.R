# Input schema for inference: one entry per model feature.
#
# Bounds are physical sanity limits wide enough to admit every training value
# (some heating-season columns contain implausibly large values in the source
# data). `nullable` features had missing values in training and are imputed by
# the recipe; the others must be supplied.

FEATURE_SCHEMA <- tibble::tribble(
  ~name, ~type, ~min, ~max, ~nullable, ~description,
  "brand", "string", NA, NA, FALSE, "Brand name (case-insensitive)",
  "rated_aeer", "number", 0, 15, FALSE, "Rated annual energy efficiency ratio (AEER)",
  "eertestavg", "number", 0, 15, FALSE, "Average tested EER",
  "c_power_inp_rated", "number", 0, 200, FALSE, "Rated cooling power input (kW)",
  "residential_hspf_hot", "number", 0, 5000, TRUE, "Residential HSPF, hot zone",
  "commercial_hspf_hot", "number", 0, 5000, TRUE, "Commercial HSPF, hot zone",
  "pia_inoperative_power", "number", 0, 5000, FALSE, "Inoperative (standby) power (W)",
  "commercial_hspf_mixed", "number", 0, 5000, TRUE, "Commercial HSPF, mixed zone",
  "rated_acop", "number", 0, 15, TRUE, "Rated annual coefficient of performance (ACOP)",
  "commercial_hspf_cold", "number", -10, 5000, TRUE, "Commercial HSPF, cold zone",
  "coptestavg", "number", 0, 15, TRUE, "Average tested COP",
  "residential_hspf_cold", "number", 0, 5000, TRUE, "Residential HSPF, cold zone",
  "residential_hspf_mixed", "number", 0, 5000, TRUE, "Residential HSPF, mixed zone",
  "rated_total_cool_capacity_w", "number", 0, 1e6, FALSE, "Rated total cooling capacity (W)",
  "c_total_cool_rated", "number", 0, 1000, FALSE, "Rated total cooling capacity (kW)",
  "t1_half_cap_power_rated", "number", 0, 2e5, TRUE, "T1 half-capacity power input (W)",
  "commercial_thec_hot", "number", 0, 2e5, TRUE, "Commercial heating energy, hot zone (kWh)",
  "residential_thec_hot", "number", 0, 2e5, TRUE, "Residential heating energy, hot zone (kWh)",
  "h_power_inp_rated", "number", 0, 200, TRUE, "Rated heating power input (kW)",
  "low_temp_cooling_half_cap_power_rated", "number", 0, 2e5, TRUE, "Low-temperature half-capacity cooling power (W)",
  "residential_thec_mixed", "number", 0, 2e5, TRUE, "Residential heating energy, mixed zone (kWh)",
  "commercial_thec_mixed", "number", 0, 2e5, TRUE, "Commercial heating energy, mixed zone (kWh)",
  "h1_half_cap_power_rated", "number", 0, 2e5, TRUE, "H1 half-capacity heating power input (W)"
)

MAX_BRAND_CHARS <- 100

#' Validate inference records against the feature schema
#'
#' Each record must be a JSON object (named list) with exactly the schema
#' fields: unknown fields, missing fields, wrong types, out-of-range numbers
#' and nulls in required fields are all reported.
#'
#' @param records A list of named lists (parsed JSON objects).
#' @param schema The feature schema.
#' @return A list with `ok` (logical), `errors` (character vector, each prefixed
#'   with the record index) and `data` (a tibble when `ok`).
#' @export
validate_records <- function(records, schema = FEATURE_SCHEMA) {
  errors <- character()
  for (i in seq_along(records)) {
    errors <- c(errors, sprintf("record %d: %s", i, validate_record(records[[i]], schema)))
  }
  if (length(errors) > 0) {
    return(list(ok = FALSE, errors = errors, data = NULL))
  }
  data <- purrr::map_dfr(records, function(r) {
    tibble::as_tibble(lapply(r[schema$name], function(v) if (is.null(v)) NA else v))
  })
  list(ok = TRUE, errors = character(), data = prepare_features(data, schema$name))
}

validate_record <- function(record, schema) {
  if (!is.list(record) || is.null(names(record)) || any(!nzchar(names(record)))) {
    return("must be a JSON object")
  }
  errs <- character()
  extra <- setdiff(names(record), schema$name)
  if (length(extra) > 0) {
    errs <- c(errs, paste0("unexpected field(s): ", paste(extra, collapse = ", ")))
  }
  missing <- setdiff(schema$name, names(record))
  if (length(missing) > 0) {
    errs <- c(errs, paste0("missing field(s): ", paste(missing, collapse = ", ")))
  }
  for (j in which(schema$name %in% names(record))) {
    errs <- c(errs, check_value(record[[schema$name[j]]], schema[j, ]))
  }
  errs
}

check_value <- function(value, field) {
  name <- field$name
  if (is.null(value)) {
    return(if (field$nullable) character() else paste0(name, ": must not be null"))
  }
  if (length(value) != 1 || is.list(value)) {
    return(paste0(name, ": must be a single value"))
  }
  if (field$type == "string") {
    if (!is.character(value)) {
      return(paste0(name, ": must be a string"))
    }
    if (!nzchar(trimws(value)) || nchar(value) > MAX_BRAND_CHARS) {
      return(paste0(name, ": must be 1-", MAX_BRAND_CHARS, " characters"))
    }
    return(character())
  }
  if (!is.numeric(value) || is.na(value) || !is.finite(value)) {
    return(paste0(name, ": must be a finite number"))
  }
  if (value < field$min || value > field$max) {
    return(sprintf("%s: must be between %s and %s", name, format(field$min), format(field$max)))
  }
  character()
}
