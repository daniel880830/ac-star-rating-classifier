# Keep every package inside the project library: no shared renv cache, no
# sandbox copy of the system library under the user profile, and no external
# library (renv's default for package projects).
Sys.setenv(
  RENV_CONFIG_CACHE_ENABLED = "FALSE",
  RENV_CONFIG_SANDBOX_ENABLED = "FALSE",
  RENV_PATHS_LIBRARY = file.path(getwd(), "renv", "library")
)
source("renv/activate.R")
