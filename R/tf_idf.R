source("~/Masterarbeit/R/packages.R")

calculate_tfidf <- function(tweets, what) {
  corpus(tweets) %>%
    tokens(
      remove_punct = TRUE,
      remove_symbols = TRUE,
      remove_numbers = TRUE,
      remove_url = TRUE,
      remove_separators = TRUE,
      split_hyphens = TRUE,
      split_tags = TRUE
    ) %>%
    dfm() %>%
    dfm_remove(stopwords("de")) %>%
    dfm_tfidf() %>%
    colSums() %>%
    sort()
}