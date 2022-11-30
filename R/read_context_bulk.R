source("R/packages.R")
source("R/globals.R")
source("R/read_context.R")

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
} else {
  ioer_data_join <- read_feather("data/context/ioer.feather")
}

# Collect INKAR data ----
if (askYesNo("Read INKAR data? (takes about fifteen minutes)")) {
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
  
  inkar_data_join$unter_30 <- inkar_data_join$unter_30 +
    inkar_data_join$unter_25 +
    inkar_data_join$unter_18 +
    inkar_data_join$unter_6
  inkar_data_join[c("unter_25", "unter_18", "unter_6")] <- NULL
  
  write_feather(inkar_data_join, "data/context/inkar.feather")
} else {
  inkar_data_join <- read_feather("data/context/inkar.feather")
}

if (askYesNo("Read OSM data?")) {
  osmd <- osmextract::oe_read(
    "data/context/germany-latest.osm.pbf",
    layer = "multipolygons",
    query = paste(
      "SELECT amenity,leisure,office,shop,geometry FROM 'multipolygons'",
      "WHERE", nested_list_to_sql(osm_features)
    )
  ) %>%
    mutate(scenes = coalesce(amenity, leisure, office, shop)) %>%
    select(scenes)
  
  bounds_needed <- FALSE
  if (bounds_needed) {
    kreise <- retry::retry(
      get_admin("Kreis"),
      upon = "error",
      when = "InitializeSecurityContext|Connection",
      max_tries = 3
    )
    
    # Fix non-unique names
    kreise <- kreise %>%
      group_by(gen) %>%
      mutate(ags = ags, gen = make_kreis_unique(gen)) %>%
      ungroup()
  }
  
  osmd <- aggregate(
    st_transform(osmd, 3035),
    kreise,
    FUN = length,
    join = st_contains
  )
  
  amenity_data_join <- st_join(osmd, kreise["ags"], st_equals) %>%
    st_drop_geometry()
  write_feather(amenity_data_join, "data/context/amenities.feather")
} else {
  amenity_data_join <- read_feather("data/context/amenities.feather")
}

context_data <- left_join(
  inkar_data_join %>% select(-gen),
  ioer_data_join %>% select(-gen),
  by = "ags"
) %>%
  left_join(amenity_data_join, by = "ags") %>%
  select(c(ags, names(var_sel)))

write_feather(context_data, "data/context/context.feather")
