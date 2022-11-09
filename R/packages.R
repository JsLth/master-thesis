library(easypackages)

pkgs <- c(
  # General
  "tidyverse", "data.table", "dtplyr",
  
  # I/O
  "httr2", "jsonlite", "ows4R", "readr", "arrow", "sfarrow",
  
  # Data cleaning
  "emoji", "lubridate", "zoo",
  
  # Debugging and convenience
  "cli", "callr", "bench", "magrittr", "rlang", "retry",
  
  # Text semantics
  "stringr", "quanteda", "quanteda.textstats", "quanteda.textplots",
  "quanteda.textmodels", "LSX",
  
  # Geoprocessing
  "sf", "sp", "spdep", "spgwr", "mgwrsar", "geostan", "gwrr", "GWmodel",
  
  # Presentation
  "tmap", "ggpubr", "texreg", "ggnewscale", "kableExtra", "stargazer", "Cairo",
  "patchwork", "gridExtra", "latex2exp", "extrafont"
)

if (!all(pkgs %in% names(sessionInfo()$otherPkgs))) {
  cli::cli_progress_step(
    msg = "Attaching packages...",
    msg_done = "Successfully attached all packages.",
    msg_failed = "Could not attack all packages."
  )
  
  suppressMessages(packages(pkgs, prompt = FALSE))
  
  cli::cli_progress_done()
}
