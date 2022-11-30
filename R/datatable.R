source("R/packages.R")
source("R/globals.R")
source("R/utils.R")
source("R/photon.R")

twitter_to_dt <- function(file, filter = TRUE) {
  day <- readr::read_rds(file)
  out <- day %>%
    cli_progress_along() %>%
    map_dfr(function(i) {
      if (is.null(day[[i]]$status)) {
        data <- day[[i]]$data
        users <- day[[i]]$includes$users
        geo <- day[[i]]$includes$places
        
        if (!is.null(data)) {
          day[[i]]$data %>%
            left_join(users, by = c("author_id" = "id")) %>%
            when(!is.null(geo) ~ left_join(., geo, by = c("geo.place_id" = "id")), ~.) %>%
            as.data.table()
        }
      }
    })

  if (filter) {
    has_geo <- !is.na(out$location) | !is.na(out$geo.place_id)
    is_not_automated <- out$source %in% allowed_sources
    is_german <- out$country_code == "DE" | is.na(out$country_code)
    out %>%
      filter(has_geo & is_not_automated) %>%
      mutate(place_name = name.y) %>%
      dplyr::select(
        text, id, author_id, geo.place_id, source, created_at.x, location,
        place_name, place_type, geo.bbox
      ) %>%
      as_tibble() %>%
      set_colnames(c(
        "text", "id", "author_id", "place_id", "source", "tweet_time",
        "profile_place", "tweet_place", "place_type", "bbox")
      ) %>%
      unnest_wider(col = bbox, names_sep = "", names_repair = function(x) {
          inset(x, str_detect(x, fixed("bbox")), c("xmin", "ymin", "xmax", "ymax"))
      }) %>%
      mutate(
        source = as_factor(source),
        tweet_time = as_datetime(tweet_time),
        place_type = as_factor(place_type)
      ) %>%
      lazy_dt(name = "filter(DT, has_geo & is_not_automated)")
  } else {
    lazy_dt(out)
  }
}


path_to_dt <- function(filter = TRUE, geocoded = TRUE) {
  files <- collected_dates()
  
  cli_progress_bar(
    status = "Reading and binding tweet datasets",
    format = "{pb_status} {pb_bar} {cli::pb_current}/{cli::pb_total} | ETA: {pb_eta}",
    total = length(files)
  )
  
  dt <- map_dfr(files, function(f) {
    cli::cli_progress_update(.envir = parent.frame(3))
    as.data.table(twitter_to_dt(f, filter = filter))
  })
  
  if (geocoded) {
    cli::cli_progress_step("Reading and joining geocoded data")
    geo <- read_geo()
    dt <- right_join(dt, geo, by = "id")
  }
  
  lazy_dt(dt, name = "filter(DT, has_geo & is_not_automated)")
}
