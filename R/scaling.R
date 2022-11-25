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


textplot_terms.textmodel_lss <- function(x, highlighted = NULL, max_words = 10000) {
  
  max_words <- check_integer(max_words, min_len = 1, max_len = 1, min = 1)
  
  if (is.null(highlighted))
    highlighted <- character()
  if (is.dictionary(highlighted)) {
    separator <- meta(highlighted, field = "separator", type = "object")
    valuetype <- meta(highlighted, field = "valuetype", type = "object")
    concatenator <- x$concatenator
    highlighted <- unlist(highlighted, use.names = FALSE)
    if (!nzchar(separator) && !is.null(concatenator)) # for backward compatibility
      highlighted <- stri_replace_all_fixed(highlighted, separator, concatenator)
  } else {
    highlighted <- unlist(highlighted, use.names = FALSE)
    valuetype <- "glob"
  }
  words_hl <- quanteda::pattern2fixed(
    highlighted,
    types = names(x$beta),
    valuetype = valuetype,
    case_insensitive = TRUE
  )
  
  # fix for a bug before v1.1.4
  x$frequency <- x$frequency[names(x$beta)]
  
  beta <- frequency <- word <- NULL
  temp <- data.frame(word = names(x$beta), beta = x$beta, frequency = log(x$frequency),
                     stringsAsFactors = FALSE)
  is_hl <- temp$word %in% unlist(words_hl, use.names = FALSE)
  is_sm <- temp$word %in% sample(temp$word, min(length(temp$word), max_words))
  temp_black <- subset(temp, is_hl)
  temp_gray <- subset(temp, !is_hl & is_sm)
  ggplot(data = temp_gray, aes(x = beta, y = frequency, label = word)) +
    geom_text(aes(alpha = abs(beta) * frequency), colour = "grey70") +
    labs(x = "Polarity", y = "Frequency (log)") +
    scale_alpha(range = c(0.1, 1)) +
    theme_bw() +
    theme(panel.grid= element_blank(),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          legend.position="none") +
    ggrepel::geom_text_repel(data = temp_black, aes(x = beta, y = frequency, label = word),
                    segment.size = 0.25, colour = "black") +
    geom_point(data = temp_black, aes(x = beta, y = frequency), cex = 0.7, colour = "black")
  
}


plot_multiple_density <- function(dfm, seeds, k, engine = "rsvd", seed = 1111) {
  if (length(seeds[[1]]) != length(seeds[[2]])) {
    cli_abort("Both seedword directions must be of equal length.")
  }
  
  pred <- map(cli_progress_along(seeds[[1]]), function(i) {
    si <- map(1:2, ~seeds[[.x]][1:i])
    set.seed(seed)
    suppressMessages(m <- textmodel_lss(
      tw_dfm,
      seeds = as.seedwords(si),
      auto_weight = TRUE,
      k = k,
      cache = TRUE,
      engine = engine
    ))
    
    pred <- predict(
      m,
      newdata = dfm,
      rescaling = TRUE,
      min_n = 4
    )
  })
  
  pl <- ggplot()
  for (i in seq_along(pred)) {
    p <- pred[[i]]
    best_fit <- i == length(pred)
    cl <- ifelse(best_fit, "black", "grey")
    sz <- ifelse(best_fit, 1, 0.5)
    
    pl <- pl +
      geom_density(
        data = data.frame(fit = p),
        aes(x = fit),
        na.rm = TRUE,
        size = sz,
        color = cl
      )
  }
  
  pl +
    geom_function(fun = dnorm, args = list(mean = 0, sd = 1), size = 1, color = "green") +
    theme_bw() +
    labs(x = "Polarity estimate", y = "Density") +
    scale_x_continuous(expand = c(0, 0))
}