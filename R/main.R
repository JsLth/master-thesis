source("~/Masterarbeit/R/packages.R")
source("~/Masterarbeit/R/dictionaries.R")
source("~/Masterarbeit/R/datatable.R")
#source("~/Masterarbeit/R/hydrate.R")
source("~/Masterarbeit/R/boundaries.R")
source("~/Masterarbeit/R/tf_idf.R")
source("~/Masterarbeit/R/search_tweets.R")

# Collect tweets daily using collect_tweets.R

# Read all collected tweets and filter duplicates and retweets
tweets <- path_to_dt() %>%
  as.data.table()

# Create a copy that only contains unique tweet texts, i.e. all non-distinct
# retweets are excluded. This is done to prevent a semantic bias in document
# scaling because 50 retweets of a single tweet would amplify each co-occurrence
# between the original and the retweets by a factor of 50. Nonetheless, retweets
# are an important indicator of attitude, which is why the OG dataset will be
# rejoined with the full dataset after text scaling.
tweets_only_og <- data.table(
  text = dtext <- unique(tweets$text),
  did = seq_along(dtext)
)

# Create corpus from tweet data
tw_corpus <- tweets_only_og %>%
  extract(!grepl("RT", tweets_only_og$text, fixed = TRUE)) %>%
  corpus(docid_field = "id", text_field = "text")
#tw_corpus <- corpus(tweets$text, docnames = tweets$id)

tw_dfm <- tw_corpus %>%
  tokens(
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE,
    remove_url = TRUE,
    remove_separators = FALSE,
    split_hyphens = FALSE,
    split_tags = TRUE
  ) %>%
  tokens_remove(stopwords("de", source = "marimo")) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 5) %>%
  dfm_select(c(" ", "\\n"), valuetype = "regex", selection = "remove") %>%
  dfm_wordstem(language = "german")


# LSX model
lsx_model <- textmodel_lss(tw_dfm, seeds = as.seedwords(seed))

tweets_only_og$polarity <- predict(lsx_model, newdata = tw_dfm)
polarity <- data.frame(
  features = tweets_only_og$polarity,
  weight = tweets_only_og$polarity
)