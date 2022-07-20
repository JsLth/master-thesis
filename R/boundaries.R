### This script collects boundary data of Germany's administrative regions by
### connecting to the 1:250,000 administrative boundary WFS of the 
### Bundesamt für Kartographie und Geodäsie.

source("~/Masterarbeit/R/packages.R")

# Connect to WFS
wfs <- WFSClient$new("https://sgx.geodatenzentrum.de/wfs_vg250", serviceVersion = "2.0.0")

# List feature types
wfs$capabilities$getFeatureTypes(pretty = TRUE)

# Extract German state borders
germany <- wfs$getFeatures("vg250:vg250_sta") %>%
  st_cast("GEOMETRYCOLLECTION") %>%
  st_collection_extract("POLYGON") %>%
  st_combine() %>%
  st_transform(4326) %>%
  st_make_valid()

saveRDS(germany, "data/germany.rds")
