# Line coverage of R/ by the test suite.
#
# covr::package_coverage() installs the package first, which on Windows needs
# Rtools (`make`). file_coverage() instruments the sources in-process instead,
# so it works with a plain R installation. Code that only runs inside the API
# server subprocess (integration tests) is not counted.
#
# Usage: Rscript scripts/coverage.R [--html]

suppressPackageStartupMessages(library(testthat))
for (pkg in c("dplyr", "rlang")) loadNamespace(pkg)

sources <- list.files("R", pattern = "\\.R$", full.names = TRUE)
tests <- c(
  "tests/testthat/helper-fixtures.R",
  list.files("tests/testthat", pattern = "^test-.*\\.R$", full.names = TRUE)
)
logger::log_threshold(logger::FATAL)
cov <- covr::file_coverage(sources, tests, parent_env = globalenv())

by_file <- covr::tally_coverage(cov, by = "line")
by_file <- stats::aggregate(value ~ filename, by_file, function(v) round(100 * mean(v > 0), 1))
names(by_file)[2] <- "line_coverage_pct"
print(by_file[order(by_file$line_coverage_pct), ], row.names = FALSE)
cat(sprintf("\nTOTAL LINE COVERAGE: %.1f%%\n", covr::percent_coverage(cov)))
if ("--html" %in% commandArgs(trailingOnly = TRUE)) {
  covr::report(cov, file = "reports/coverage.html", browse = FALSE)
}
