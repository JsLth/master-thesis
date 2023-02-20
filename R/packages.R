library(easypackages)

pkgs <- c(
  # General
  "tidyverse", "data.table", "dtplyr", "easystats",
  
  # I/O
  "httr2", "jsonlite", "ows4R", "readr", "arrow", "sfarrow", "osmextract",
  
  # Data cleaning
  "emoji", "lubridate", "zoo", "xml2", "randomForest", "broom", "broom.mixed",
  "tidytext", "vctrs",
  
  # Debugging and convenience
  "cli", "callr", "processx", "magrittr", "rlang", "retry", "R.utils",
  
  # Text semantics
  "stringr", "quanteda", "quanteda.textstats", "quanteda.textplots",
  "quanteda.textmodels", "LSX",
  
  # Geoprocessing
  "sf", "sp",  "stars", "starsExtra", "gstat",
  
  # Spatial statistics
  "spdep", "GWmodel", "lme4", "merTools", "nortest",
  
  # Visualization
  "tmap", "ggpubr", "Cairo", "ggrastr", "ggrepel", "qqplotr",
  
  # LaTeX
  "flextable", "ftExtra","modelsummary",
  
  # Parallelization
  "furrr", "progressr"
)

if (!all(pkgs %in% names(sessionInfo()$otherPkgs))) {
  cli::cli_progress_step(
    msg = "Attaching packages...",
    msg_done = "Successfully attached all packages.",
    msg_failed = "Could not attach packages."
  )
  
  suppressPackageStartupMessages(packages(pkgs, prompt = FALSE))
  
  cli::cli_progress_done()
}
