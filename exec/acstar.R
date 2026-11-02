#!/usr/bin/env Rscript
# CLI entry point: Rscript exec/acstar.R <command> [options]
# From a source checkout the package is loaded with pkgload; inside the Docker
# image (or after installing the package) the installed copy is used.
if (file.exists("DESCRIPTION") && requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(quiet = TRUE, export_all = FALSE)
} else {
  library(acstar)
}
quit(status = acstar::cli_main(commandArgs(trailingOnly = TRUE)), save = "no")
