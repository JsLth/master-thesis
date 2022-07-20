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
    has_geo <- !is.na(out$location) | when(out, "geo.place_id" %in% colnames(out) ~ !is.na(out$geo.place_id), ~FALSE)
    is_not_automated <- out$source %in% allowed_sources
    out %>%
      filter(has_geo & is_not_automated) %>%
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