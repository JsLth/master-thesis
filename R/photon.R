source("~/Masterarbeit/R/packages.R")
source("~/Masterarbeit/R/lists.R")
source("~/Masterarbeit/R/boundaries.R")

if (!exists("photon")) {
  photon <- new.env()
}

start_photon <- function(path = "./photon", min_ram = 6, max_ram = 12) {
  exec <- grep("photon\\-.+\\.jar", dir(path), value = TRUE)
  
  if (!length(exec)) {
    cli::cli_abort("Photon executable not found in the given {.var path}.")
  }
  
  path <- normalizePath(path, winslash = "/")

  cmd <- c(
    "-d64", sprintf("-Xms%sg", min_ram), sprintf("-Xmx%sg", max_ram),
    "-jar", exec
  )
  
  proc <- processx::process$new(
    command = "java",
    args = cmd,
    stdout = "|",
    stderr = "|",
    wd = path
  )
  
  cli::cli_progress_step(
    msg = "Starting photon...",
    msg_done = "Photon is now running.",
    msg_failed = "Photon could not be started."
  )
  
  out <- ""
  while (!grepl("ES cluster is now ready", out, fixed = TRUE)) {
    out <- proc$read_output()
  }
  
  assign("proc", proc, envir = photon)
  invisible(proc)
}


stop_photon <- function(proc = NULL) {
  if (is.null(proc)) {
    proc <- get("proc", envir = photon)
  }
  
  if (proc$is_alive()) {
    proc <- proc$interrupt()
  }
  
  proc
}


photon_running <- function(proc = NULL) {
  if (is.null(proc)) {
    proc <- get0("proc", envir = photon)
  }
  
  if (is.environment(proc)) {
    proc$is_alive()
  } else {
    FALSE
  }
}


geocode <- function(text, limit = 3, lang = "en") {
  httr2::request("http://localhost:2322/") %>%
    httr2::req_method("GET") %>%
    httr2::req_error(is_error = \(r) FALSE) %>%
    httr2::req_url_path("api") %>%
    httr2::req_url_query(q = text, limit = limit, lang = lang) %>%
    httr2::req_perform() %>%
    httr2::resp_body_string(encoding = "UTF-8") %>%
    sf::st_read(as_tibble = TRUE, quiet = TRUE, drivers = "geojson")
}


#' Converts arcdegrees to decimal coordinates
#' Takes in a character vector, matches all numbers and potential cardinal
#' directions (N, E) and then converts the resulting numerics to decimal
#' coordinates.
#' Inspired by https://gist.github.com/valentinitnelav/ea94fea68227e05c453e13c4f7b7716b,
#' but completely rewritten
#' 
#' Example: 
#' 
#' Input  : "51° 2' N , 6° 59' O"
#' Output : x         y
#'      1   51.03333  6.983333
degree_to_decimal <- function(coords) {
  stringr::str_match_all(coords, "[[:digit:]NOELBr]+") %>%
    purrr::map_dfr(function(chr) {
      chr <- c(chr)
      if (!length(chr) %% 2) {
        dms <- list(chr[1:(length(chr)/2)], chr[(length(chr) / 2 + 1):length(chr)])
        lon_regex <- paste(c("N", "Br"), collapse = "|")
        lat_regex <- paste(c("E", "O", "L"), collapse = "|")
        
        vapply(c(lon_regex, lat_regex), function(regex) {
          dms %>%
            magrittr::extract(purrr::map_lgl(dms, ~any(str_detect(., regex)))) %>%
            magrittr::extract2(1) %>%
            stringr::str_replace(paste(regex, collapse = "|"), "") %>%
            magrittr::extract(nchar(.) > 0) %>%
            as.numeric() %>%
            { .[1] + .[2] / 60 + ifelse(length(.) > 2, .[3] / 3600, 0) }
        }, numeric(1), USE.NAMES = FALSE) %>%
          as.list() %>%
          magrittr::set_names(c("x", "y")) %>%
          do.call(data.frame, .)
      } else data.frame(x = NA_real_, y = NA_real_)
    })
}


batch_geocode <- function(text, id, size = 3, lang = "en") {
  excluded <- c("house", "state", "country")
  
  # Some pre-filtering
  # - Emojis are removed because Photon does not recognize them
  # - Only numbers are removed because they cannot be places
  # - Some people identify themselves as world citizens
  text[
    emoji::emoji_detect(text) |
      stringr::str_detect(text, "^[:digit:]+$") |
      stringr::str_detect(text, regex("welt|erde|planet|hier", ignore_case = TRUE))
  ] <- ""
  
  cli::cli_progress_bar(
    name = "Geocoding",
    format = "{cli::pb_name} {cli::pb_bar} {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}",
    total = length(text)
  )
  
  gcd <- purrr::map_dfr(seq_along(text), function(i) {
    cli::cli_progress_update(.envir = parent.frame(3))

    # Some locations are given directly as coordinates. Parse them and use them
    # as geometries.
    is_degree <- grepl("(?=.*°)(?=.*')", text[[i]], perl = TRUE)
    if (is_degree) {
      geometry <- try(text[[i]] %>%
        degree_to_decimal() %>%
        as.numeric() %>%
        st_point() %>%
        st_sfc())
    } else {
      geometry <- st_sfc(st_point())
    }

    cor <- st_sf(
      name = NA_character_,
      country = NA_character_,
      type = NA_character_,
      osm_key = NA_character_,
      geometry = geometry,
      crs = 4326
    )

    if (nzchar(text[[i]])) {
      res <- geocode(text[[i]], limit = size, lang = lang)
    } else {
      res <- cor
    }
    
    if (!nrow(res)) {
      res <- cor
    }
    
    # In some cases the `name` column is missing
    if (!has_name(res, "name")) {
      res$name <- NA_character_
    }
    
    res <- res %>%
      filter(
        ifelse(has_name(res, "country"), country == "Germany", TRUE),
        ifelse(has_name(res, "type"), type %in% allowed_levels, TRUE),
        ifelse(has_name(res, "osm_key"), osm_key %in% allowed_keys, TRUE)
      ) %>%
      select(name, type, geometry)
    
    if (!length(res)) {
      tibble(text = text[[i]], id = id[[i]])
    } else {
      res[1, ]
    }
  })
  
  res <- bind_cols(id = id, gcd) %>%
    as_tibble() %>%
    st_as_sf()
}


geocode_day <- function(file, size = 3, lang = "en") {
  if (is.na(file)) {
    cli::cli_abort("All collected tweets are already geocoded.")
  }
  
  ymd <- file %>%
    remove_file_ext() %>%
    as_date(format = "%y%m%d") %>%
    format(format = "%Y-%m-%d")
  
  data_path <- "data/tweets"
  geo_path <- "data/geo"

  if (!exists("ger")) {
    ger <- get_admin("Staat") %>%
      st_geometry() %>%
      st_make_valid() %>%
      st_transform(4326)
  }
  
  tweets <- file.path(data_path, file) %>%
    twitter_to_dt(filter = TRUE) %>%
    as.data.table()
  
  cli::cli_alert_info("Geocoding tweets for {ymd}.")
  
  geo <- batch_geocode(
    tweets$profile_place,
    id = tweets$id,
    size = size,
    lang = lang
  )
  
  geo <- geo %>%
    filter(
      !sf::st_is_empty(geometry),
      sf::st_within(geometry, ger, sparse = FALSE)
    )
  
  geo_file <- file.path(geo_path, paste0(remove_file_ext(file), "_geo.rds"))
  readr::write_rds(geo, geo_file)
  cli_alert_success("All tweets for {ymd} were geocoded.")
  geo
}


read_geo <- function() {
  files <- collected_geo()
  
  cli_progress_bar(
    status = "Reading and binding geocoded tweet datasets",
    format = "{pb_status} {pb_bar} {cli::pb_current}/{cli::pb_total} | ETA: {pb_eta}",
    total = length(files)
  )
  
  dt <- map_dfr(files, function(f) {
    cli::cli_progress_update(.envir = parent.frame(3))
    readRDS(f)
  })
  
  lazy_dt(dt)
}
