source("~/Masterarbeit/R/packages.R")

#' Get administrative boundaries of Germany
#' 
#' @description Connects to the "Verwaltungsgrenzen 1:250.000" WFS of the BKG
#' and retrieves geodata from it.
#' 
#' @param what Which administrative level to extract. Possible values are
#' `Staat`, `Bundesland`, `Regierungsbezirk`, `Verwaltungsgebiete`, `Grenzlinien`,
#' `Kreis`, `Gemeinden`, and `Gemeindepunkte`
get_admin <- function(what, crs = 3035) {
  wfs <- WFSClient$new("https://sgx.geodatenzentrum.de/wfs_vg250", serviceVersion = "2.0.0")
  
  types <- wfs$capabilities$getFeatureTypes(pretty = TRUE)
  type <- types[types$title == what, "name"]
  
  # Geometries come as "MULTISURFACE" which is not fully supported by sf
  admin <- wfs$getFeatures(type) %>%
    st_cast("GEOMETRYCOLLECTION") %>%
    st_collection_extract("POLYGON") %>%
    set_rownames(NULL) %>%
    st_transform(crs)
  
  if (what == "Staat") {
    # Country boundaries are not valid off-the-shelf and are divided in the
    # Northern part. 
    admin <- admin %>%
      st_make_valid() %>%
      st_combine()
  } else if (what == "Kreis") {
    # Kreise are returned as multiple polygons. Union these to a single one
    admin <- admin %>%
      group_by(ags) %>%
      summarise(gen = unique(gen))
  }
  
  admin
}
