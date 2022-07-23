#' Geocode with Pelias
#' 
#' @description Sends a forward geocoding request to a local Pelias instance
#' listening to port 4000.
#' 
#' @param text Character vector containing toponyms or addresses to be geocoded
#' @param id Vector of identifiers used to join results with tweet dataset.
#' @param size Number of results to return. Defaults to 1.
#' @param sources Sources to inluce in the response. Can be one of
#' `osm`, `oa`, `gn` or `wof`. Pelias will only search within data from the
#' specified sources.
#' @param layers Geographic levels to include in the response. Can be one of
#' `address`, `venue`, `neighbourhood`, `locality`, `borough`, `localadmin`,
#' `country`, `macrocountry`, `region`, `macroregion`, `country`, `coarse`,
#' `postalcode`. Pelias will only search within the specified geographic
#' levels.
#' @param focus_point Point (lon, lat) that is used to prioritize results, i.e.
#' places closer to the focus point are prioritized in the response.
#' @param boundary_rect Boundary box (xmin, ymin, xmax, ymax) to narrow down the
#' search to a specific area.
#' @param boundary_circle Boundary circle (lon, lat, radius) to narrow down the
#' search to a specific area.
#' @param boundary_gid Who's on first administrative parent code to narrow down
#' the search to a specific area.
#' @param boundary_country ISO 3166-1 Country code to narrow down the search to
#' a specific area.
pelias_geocode <- function(
  text,
  id,
  size = NULL,
  sources = c(),
  layers = c(),
  focus_point = c(),
  boundary_rect = c(),
  boundary_circle = c(),
  boundary_gid = NULL,
  boundary_country = NULL
) {
  args <- as.list(environment())
  large_admin <- c("macrocounty", "region", "macroregion", "country", "coarse", "postalcode")
  
  # Some pre-filtering
  # - Emojis are removed because Pelias does not recognize them
  # - Only numbers are removed because they cannot be places
  # - Some people identify themselves as world citizens. That's not a place in Germany.
  text[
    emoji::emoji_detect(text) |
    str_detect(text, "^[:digit:]+$") |
    str_detect(text, regex("welt|erde|planet|hier", ignore_case = TRUE))
  ] <- ""
  
  cli_progress_bar(
    name = "Geocoding",
    format = "{cli::pb_name} {cli::pb_bar} {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}",
    total = length(text)
  )
  
  map_dfr(seq_along(text), function(i) {
    cli_progress_update(.envir = parent.frame(3))
    
    # Get response from Pelias
    if (nchar(text[i])) {
      args$text <- text[i]
      args$id <- NULL
      res <- try(do.call(pelias_search, args))
      if (inherits(res, "try-error")) browser()
      feat <- res$features
    } else feat <- list()
    
    if (!length(feat) || feat$properties.layer %in% large_admin) {
      return(data.table(text = text[i], id = id[i]))
    }

    # Subset and link dataset
    feat <- feat %>%
      select(
        geometry.coordinates,
        properties.layer,
        properties.name,
        properties.confidence
     ) %>%
      bind_cols(text = text[i], id = id[i])

    # Unnest coordinates
    names(feat$geometry.coordinates[[1]]) <- c("x", "y")
    feat %>%
      unnest_wider(col = geometry.coordinates) %>%
      as.data.table()
  }) %>% lazy_dt()
}


pelias_search <- function(
  text,
  focus_point = c(),
  boundary_rect = c(),
  boundary_circle = c(),
  boundary_gid = NULL,
  sources = c(),
  layers = c(),
  boundary_country = NULL,
  size = NULL
) {
  res <- httr2::request("http://localhost:4000/") %>%
    req_url_path_append("v1/search") %>%
    req_url_query(
      text = text,
      focus.point.lat = focus_point[2],
      focus.point.lon = focus_point[1],
      boundary.rect.min_lon = boundary_rect[1],
      boundary.rect.max_lon = boundary_rect[3],
      boundary.rect.min_lat = boundary_rect[2],
      boundary.rect.max_lat = boundary_rect[4],
      boundary.circle.lat = boundary_circle[2],
      boundary.circle.lon = boundary_circle[1],
      boundary.circle.radius = boundary_circle[3],
      boundary.gid = boundary_gid,
      sources = do.call(paste, c(as.list(sources), sep = ",")),
      layers = do.call(paste, c(as.list(layers), sep = ",")),
      boundary_country = boundary_country,
      size = size
    ) %>%
    req_error(is_error = function(x) FALSE) %>%
    req_perform()
  
  body <- resp_body_json(res, simplifyVector = TRUE, flatten = TRUE)
  
  if (res$status_code != 200L) {
    msg <- unlist(body$geocoding$errors)
    cli_abort(c("Pelias returned an error:", "x" = msg))
  } else body
}


pelias_ready <- function() {
  httr2::request("http://localhost:4000/status") %>%
    httr2::req_perform() %>%
    httr2::resp_body_string() %>%
    identical("status: ok")
}