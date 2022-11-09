source("~/Masterarbeit/R/packages.R")

tfidf <- function(tweets, n) {
  if (!inherits(tweets, "dfm")) {
    tweets <- corpus(tweets) %>%
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
      dfm_remove(stopwords("de"))
  }
  tweets %>%
    dfm_tfidf() %>%
    colSums() %>%
    sort(decreasing = TRUE) %>%
    select(1:n)
}


lss_loocv <- function(model) {
  seeds <- model$seeds
  seedwords <- names(seeds)
  seed_weights <- model$beta[seedwords] ##############
  adj_seed_weights <- list()
  seed_errors <- list()
  err <- 0
  
  for (i in cli_progress_along(seedwords)) {
    seed <- seedwords[i]
    ref <- seed_weights[seed]
    if (is.na(ref)) {
      seed_weights[[i]] <- NA
      adj_seed_weights[[i]] <- NA
      seed_errors[[i]] <- NA
      next
    }
    adj_model <- suppressMessages(lsx_model %>%
      update(seeds = seeds[!names(seeds) %in% seed], evaluate = FALSE) %>%
      deparse() %>%
      trimws() %>%
      paste(collapse = "") %>%
      paste0("LSX:::", .) %>%
      str2lang() %>%
      eval())
    adj_seed_weights[[seed]] <- adj_model$beta[seed]
    seed_errors[[seed]] <- (ref - adj_seed_weights[[seed]])^2
    err <- err + seed_errors[[seed]]
  }
  
  seeds <- recode(seeds, `-1` = "polemic", `1` = "hegemonic")
  df <- data.frame(
    seedword = unlist(seedwords),
    position = unlist(seeds),
    reference = round(unlist(seed_weights), 5),
    estimate = round(unlist(adj_seed_weights), 5),
    error = round(unlist(seed_errors), 5),
    row.names = NULL
  )
  
  capture.output(
    latex <- stargazer(df, summary = FALSE, rownames = FALSE, digits = NA, digits.extra = 5) %>%
      str_replace_all(fixed("$$"), "$") %>%
      str_replace_all(fixed("$0$"), "$0.00000$") %>%
      str_replace_all(fixed("$-$"), "$")
  )
  
  list(
    df = df,
    latex = paste(latex, collapse = "\n"),
    err = unname(err)
  )
}


plot_docscores <- function(pred,
                           n = 1000,
                           seed = 123,
                           fatten = 3,
                           alpha = 0.3,
                           lwd = 0.3,
                           na.rm = TRUE) {
  fit <- pred$fit
  se <- pred$se.fit
  idx <- seq_along(fit)
  set.seed(seed)
  s <- sample(idx, n)
  fit <- fit[s]
  se <- se[s]
  
  results <- data.frame(
    x = seq_along(s),
    fit = fit,
    lower = fit - 1.96 * se,
    upper = fit + 1.96 * se
  )
  
  ggplot(data = results, aes(x = reorder(x, fit), y = fit)) +
    coord_flip() +
    geom_pointrange(aes(ymin = lower, ymax = upper), lwd = 0.3, alpha = 0.5, fatten = 3, na.rm = TRUE) +
    scale_x_discrete(labels = NULL, breaks = NULL) +
    xlab("Tweets") +
    ylab("Polarity") +
    theme_bw()
}