source("~/Masterarbeit/R/packages.R")
source("~/Masterarbeit/R/lists.R")
source("~/Masterarbeit/R/read_context.R")

# Collect IÖR data ----
if (askYesNo("Read IÖR data? (takes about ten minutes)")) {
  ind_n <- length(ioer_sel)
  ioer_data <- vector("list", length = ind_n)
  names(ioer_data) <- names(ioer_sel)
  cli::cli_progress_bar(total = ind_n)
  
  # Read all INKAR indicators
  for (ind in names(ioer_sel)) {
    cli::cli_progress_update(status = paste("Collecting indicator", ind))
    ioer_data[[ind]] <- read_ioer(ioer_sel[ind])
  }
  
  # Set baseline dataset
  ioer_data_join <- ioer_data[[1]] %>% cleanup_ioer()
  
  # Join all other datasets with baseline dataset
  for (.data in tail(ioer_data, -1)) {
    ioer_data_join <- ioer_data_join %>%
      left_join(cleanup_ioer(.data, include_gen = FALSE), by = "ags")
  }
  
  write_feather(ioer_data_join, "data/context/ioer.feather")
}

# Collect INKAR data ----
if (askYesNo("Read INKAR data? (takes about an hour)")) {
  db <- read_inkar_db()
  ind_n <- length(inkar_sel)
  inkar_data <- vector("list", length = ind_n)
  names(inkar_data) <- names(inkar_sel)
  cli::cli_progress_bar(total = ind_n)
  
  for (ind in names(inkar_sel)) {
    cli::cli_progress_update(status = paste("Collecting indicator", ind))
    inkar_data[[ind]] <- read_inkar(inkar_sel[ind], obj = db)
  }
  
  inkar_data_join <- inkar_data[[1]] %>% cleanup_inkar()
  
  for (.data in tail(inkar_data, -1)) {
    inkar_data_join <- inkar_data_join %>%
      left_join(cleanup_inkar(.data, include_gen = FALSE), by = "ags")
  }
  
  write_feather(inkar_data_join, "data/context/inkar.feather")
}

context_data <- left_join(
  inkar_data_join %>% select(-gen),
  ioer_data_join %>% select(-gen),
  by = "ags"
)

write_feather(context_data, "data/context/context.feather")
