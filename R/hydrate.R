### This file reads in the Climate Change Twitter Dataset, filters it
### and hydrates it using twarc. Another dataset could have been used 
### (https://doi.org/10.7910/DVN/5QCCUU), but the Tweet limit of the elevated
### access does not suffice to handle 40 million tweets.

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
read_german_cctd <- function() {
  cctd <- read_csv(
    "data/The Climate Change Twitter Dataset.csv",
    col_select = c("created_at", "id", "lng", "lat", "topic", "stance")
  )
  
  # Filter out non-geotagged tweets and tweets outside of Germany
  cctd %>%
    filter(!is.na(cctd$lat)) %>%
    st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
    filter(as.logical(st_contains(germany, ., sparse = FALSE)))
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


cctd_hydrated <- v2_lookup_tweets(
  ids = cctd$id,
  expansion = c("geo.place_id", "author_id"),
  place.fields = c("full_name", "id", "contained_within", "country_code", "geo", "name", "place_type"),
  user.fields = c("location", "created_at", "description"),
  tweet.fields = c("author_id", "created_at", "geo", "lang", "source")
)

cctd_hydrated <- format_lookup(cctd_hydrated)


hydrated_tweets <- v2_lookup_tweets(
  ids = ids,
  expansion = c("geo.place_id", "author_id"),
  place.fields = c("full_name", "id", "contained_within", "country_code", "geo", "name", "place_type"),
  user.fields = c("location", "created_at", "description"),
  tweet.fields = c("author_id", "created_at", "geo", "lang", "source")
)

