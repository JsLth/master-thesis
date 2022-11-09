### This file reads in the Climate Change Twitter Dataset, filters and hydrates
### it. Also included is a function to convert the CCTD dataset to an sf
### object and save it as a feather file.

source("~/Masterarbeit/R/packages.R")


## Hydrate tweets ----
lookup_tweets <- function(ids, ...) {
  no_ids <- length(ids)
  iter <- seq(1, length(no_ids), 100)
  contents <- lapply(iter, function(i) {
    start <- iter[i]
    if (i < length(no_ids)) {
      end <- iter[i + 1] - 1
    } else end <- length(no_ids)
    
    res <- v2_lookup_tweets(ids[seq(start, end)], ...)
  })
}


v2_lookup_tweets <- function(
  ids,
  tweet.fields = NULL,
  expansions = NULL,
  media.fields = NULL,
  place.fields = NULL,
  poll.fields = NULL,
  user.fields = NULL
) {
  bearer_token <- Sys.getenv("TWIT_BEARER")
  
  params <- list(
    ids = ids,
    expansions = paste(expansions, collapse = ","),
    media.fields = paste(media.fields, collapse = ","),
    place.fields = paste(place.fields, collapse = ","),
    poll.fields = paste(poll.fields, collapse = ","),
    tweet.fields = paste(tweet.fields, collapse = ","),
    user.fields = paste(user.fields, collapse = ",")
  )
  params <- params[!sapply(params, is.null)]
  
  request("https://api.twitter.com/2/tweets") %>%
    req_method("GET") %>%
    req_auth_bearer_token(bearer_token) %>%
    list() %>%
    c(.req = ., params) %>%
    do.call(req_url_query, .) %>%
    req_error(is_error = function(res) FALSE) %>%
    req_perform()
}


## Read in CCTD dataset ----
read_german_cctd <- function(force = FALSE) {
  file <- "data/cctd_de_sf.feather"
  
  if (!file.exists(file) || force) {
    cctd <- read_csv(
      "data/The Climate Change Twitter Dataset.csv",
      col_select = c("created_at", "id", "lng", "lat", "topic", "stance")
    )
    
    germany <- get_admin("Staat") %>%
      st_transform(4326) %>%
      st_make_valid()
    
    # Filter out non-geotagged tweets and tweets outside of Germany
    cctd <- cctd %>%
      filter(!is.na(cctd$lat)) %>%
      st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
      filter(as.logical(st_contains(germany, ., sparse = FALSE))) %>%
      filter(lang == "de") %>%
      mutate(id = as.character(id))
    
    sfarrow::st_write_feather(cctd, file)
    cctd
  } else {
    sfarrow::st_read_feather(file)
  }
}


## Format hydration results ----
format_lookup <- function(tweets) {
  tweets <- tweets %>%
    cli_progress_along() %>%
    map_dfr(function(i) {
      tweets[[i]]$data %>%
        map_dfr(as.data.frame) %>%
        select(text, id, lang, author_id) %>%
        mutate(id = as.numeric(id))
    })
  
  tweets %>%
    filter(lang %in% "de") %>%
    left_join(read_german_cctd(), by = "id") %>%
    st_as_sf() %>%
    lazy_dt()
}


