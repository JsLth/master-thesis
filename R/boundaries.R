source("~/Masterarbeit/R/packages.R")

#' Get administrative boundaries of Germany
#' 
#' @description Connects to the "Verwaltungsgrenzen 1:250.000" WFS of the BKG
#' and retrieves geodata from it.
#' 
#' @param what Which administrative level to extract. Possible values are
#' `Staat`, `Bundesland`, `Regierungsbezirk`, `Verwaltungsgebiete`, `Grenzlinien`,
#' `Kreis`, `Gemeinden`, and `Gemeindepunkte`
get_admin <- function(what) {
  wfs <- WFSClient$new("https://sgx.geodatenzentrum.de/wfs_vg250", serviceVersion = "2.0.0")
  
  types <- wfs$capabilities$getFeatureTypes(pretty = TRUE)
  what <- types[types$title == what, "name"]
  
  # Geometries come as "MULTISURFACE" which is not fully supported by sf
  admin <- wfs$getFeatures(what) %>%
    st_cast("GEOMETRYCOLLECTION") %>%
    st_collection_extract("POLYGON") %>%
    set_rownames(NULL)
  
  # Country boundaries are not valid off-the-shelf and are divided in the
  # Northern part. 
  if (what == "Staat") {
    admin <- admin %>%
      st_make_valid() %>%
      st_combine()
  }
  
  admin
}