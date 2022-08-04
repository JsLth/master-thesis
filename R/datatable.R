source("~/Masterarbeit/R/packages.R")
source("~/Masterarbeit/R/dictionaries.R")

twitter_to_dt <- function(res, filter = TRUE) {
  out <- res %>%
    cli_progress_along() %>%
    map_dfr(function(i) {
      if (is.null(res[[i]]$status)) {
        data <- res[[i]]$data
        users <- res[[i]]$includes$users
        geo <- res[[i]]$includes$places
        
        if (!is.null(data)) {
          res[[i]]$data %>%
            left_join(users, by = c("author_id" = "id")) %>%
            when(!is.null(geo) ~ left_join(., geo, by = c("geo.place_id" = "id")), ~.) %>%
            as.data.table()
        }
      }
    })

  if (filter) {
    #cli::cli_alert_info("Filtering data...")
    has_geo <- !is.na(out$location) | !is.na(out$geo.place_id))
    is_not_automated <- out$source %in% allowed_sources
    is_german <- out$country_code == "DE" | is.na(out$country_code)
    out %>%
      filter(has_geo & is_not_automated) %>%
      mutate(place_name = name.y) %>%
      select(text, id, author_id, geo.place_id, source, created_at.x, location, place_name, place_type, geo.bbox) %>%
      as_tibble() %>%
      set_colnames(c("text", "id", "author_id", "place_id", "source", "tweet_time", "profile_place", "tweet_place", "place_type", "bbox")) %>%
      unnest_wider(col = bbox, names_sep = "", names_repair = function(x) {
          inset(x, str_detect(x, fixed("bbox")), c("xmin", "ymin", "xmax", "ymax"))
      }) %>%
      mutate(
        source = as_factor(source),
        tweet_time = as_datetime(tweet_time),
        place_type = as_factor(place_type)
      ) %>%
      lazy_dt(name = "filter(DT, has_geo & is_not_automated)")
  } else lazy_dt(out)
}


path_to_dt <- function(filter = TRUE) {
  data_path <- normalizePath("data/tweets", "/")
  files <- file.path(data_path, dir(data_path))
  
  cli_progress_bar(
    status = "Reading and binding tweet datasets",
    format = "{pb_status} {pb_bar} {cli::pb_current}/{cli::pb_total} | ETA: {pb_eta}",
    total = length(files)
  )
  
  dt <- map_dfr(files, function(f) {
    cli::cli_progress_update(.envir = parent.frame(3))
    day <- readRDS(f)
    as.data.table(twitter_to_dt(day, filter = filter))
  })
  
  lazy_dt(dt, name = "filter(DT, has_geo & is_not_automated)")
}


fix_files <- function() {
  data_path <- normalizePath("data/tweets", "/")
  files <- file.path(data_path, dir(data_path))
  files <- files[grepl("_[0-9]", files)]
  days <- unique(str_match(files, "[0-9]{6}"))
  
  for (day in days) {
    day_files <- files[str_detect(files, day)]
    tw <- lapply(day_files, readRDS)
    if (length(tw) > 1) {
      browser()
      tw <- c(tw[[1]][1:length(tw[[1]]) - 1], tw[[2]][length(tw[[1]]):length(tw[[2]])])
    }
    next
    saveRDS(tw, file = gsub("_[0-9]", "", day_files[1]))
    browser()
  }
}