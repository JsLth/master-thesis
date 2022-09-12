source("~/Masterarbeit/R/packages.R")

photon <- new.env()

start_photon <- function(path = "./photon", min_ram = 6, max_ram = 12) {
  exec <- grep("photon\\-.+\\.jar", dir(path), value = TRUE)
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
    msg = "Starting photon instance...",
    msg_done = "Photon instance is now running.",
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


batch_geocode <- function(text, id, size, lang) {
  excluded <- c("house", "state", "country")
  is_degree <- grepl("(?=.*°)(?=.*')", text, perl = TRUE)
  parsed_lonlat <- degree_to_decimal(text[is_degree])
  text[is_degree] <- ""
  
  # Some pre-filtering
  # - Emojis are removed because Pelias does not recognize them
  # - Only numbers are removed because they cannot be places
  # - Some people identify themselves as world citizens. That's not a place in Germany.
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
    
    if (nzchar(text[[i]])) {
      res <- geocode(text[[i]], limit = size, lang = lang)
    } else {
      res <- tibble::tibble()
    }
    
    if (!length(res) || res$type %in% c("state", "country")) {
      return(tibble::tibble(text = text[[i]], id = id[[i]]))
    }
  })
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