### This script is used on a daily basis to collect tweets for days of the past
### week that have not yet been collected. It is seperated from other code files
### to seperate evaluated from unevaluated code (i.e. other code file creates
### the function while this code file executes the function).

source("~/Masterarbeit/R/search_tweets.R")
source("~/Masterarbeit/R/dictionaries.R")

search_day <- free_dates()[1]

tweets <- collect_tweets(
  query = sprintf("(%s) lang:de -is:verified", paste(keywords, collapse = " OR ")),
  day = search_day,
  expansion = c("geo.place_id", "author_id"),
  place.fields = c("full_name", "id", "contained_within", "country_code", "geo", "name", "place_type"),
  user.fields = c("location", "created_at", "description"),
  tweet.fields = c("author_id", "created_at", "geo", "lang", "source")
)