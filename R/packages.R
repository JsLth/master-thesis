library(easypackages)

pkgs <- c(
  # General
  "tidyverse", "data.table", "dtplyr", "easystats",
  
  # I/O
  "httr2", "jsonlite", "ows4R", "readr", "arrow", "sfarrow", "osmextract",
  
  # Data cleaning
  "emoji", "lubridate", "zoo", "xml2", "randomForest", "broom", "broom.mixed",
  
  # Debugging and convenience
  "cli", "callr", "processx", "bench", "magrittr", "rlang", "retry", "R.utils",
  
  # Text semantics
  "stringr", "quanteda", "quanteda.textstats", "quanteda.textplots",
  "quanteda.textmodels", "LSX", "rsvd",
  
  # Geoprocessing
  "sf", "sp",  "stars", "starsExtra",
  
  # Spatial statistics
  "spdep", "spgwr", "GWmodel", "lme4", "merTools", "lmerTest",
  
  # Visualization
  "tmap", "ggpubr", "ggnewscale", "Cairo", "patchwork", "gridExtra",
  "latex2exp", "extrafont", "classInt", "RColorBrewer", "pheatmap",
  
  # LaTeX
  "kableExtra", "stargazer", "modelsummary", "huxtable", "flextable",
  
  # Parallelization
  "furrr", "multidplyr", "progressr"
)

if (!all(pkgs %in% names(sessionInfo()$otherPkgs))) {
  cli::cli_progress_step(
    msg = "Attaching packages...",
    msg_done = "Successfully attached all packages.",
    msg_failed = "Could not attach packages."
  )
  
  suppressMessages(packages(pkgs, prompt = FALSE))
  
  cli::cli_progress_done()
}
