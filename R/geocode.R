source("~/Masterarbeit/R/photon.R")
source("~/Masterarbeit/R/datatable.R")
source("~/Masterarbeit/R/utils.R")

# Start the local photon server
if (!photon_running()) {
  start_photon()
}

# Detect tweet packages that are not yet geocoded and return the first one
to_geocode <- not_geocoded_yet()[1]

# Geocode and save to data/geo
geocode_day(to_geocode, size = 3, lang = "en")
