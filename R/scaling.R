source("R/packages.R")

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


validate_lss <- function(model) {
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
    adj_model <- suppressMessages(model %>%
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
  
  vcols <- c("seedword", "reference", "estimate", "error")
  fmt <- cbind(
    df[df$position == "hegemonic", vcols],
    df[df$position == "polemic", vcols]
  ) %>%
    set_colnames(str_to_title(colnames(.))) %>%
    set_colnames(paste(c(rep("Hegemonic", 4), rep("Polemic", 4)), names(.), sep = "."))
  fmt_unique <- set_colnames(fmt, make.unique(names(fmt)))
  fmt_names <- set_names(as.list(names(fmt)), names(fmt_unique))
  fmt_header <- 
  fmt <- flextable(fmt_unique) %>%
    set_header_labels(values = fmt_names) %>%
    ftExtra::span_header() %>%
    theme_booktabs()
  
  list(
    df = df,
    latex = fmt,
    err = unname(err)
  )
}


plot_docscores <- function(pred,
                           n = 1000,
                           seed = 123,
                           fatten = 3,
                           alpha = 0.3,
                           lwd = 0.3,
                           na.rm = TRUE,
                           zoom = NULL) {
  fit <- pred$fit
  se <- pred$se.fit
  idx <- seq_along(fit)
  set.seed(seed)
  s <- sample(idx, n)
  fit <- fit[s]
  se <- se[s]
  
  results <- data.frame(
    x = reorder(seq_along(s), fit),
    fit = fit,
    lower = fit - 1.96 * se,
    upper = fit + 1.96 * se
  )
  
  ggplot(data = results, aes(x = x, y = fit)) +
    coord_flip(ylim = zoom) +
    geom_point2(color = "black", size = 1.3) +
    ggrastr::rasterise(geom_linerange(aes(ymin = lower, ymax = upper), lwd = 0.2, alpha = 0.6, na.rm = TRUE), device = "cairo", dpi = 1000) +
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
    ggrastr::rasterise(geom_text(aes(alpha = abs(beta) * frequency), colour = "grey70", size = 3), dpi = 1000) +
    labs(x = "Polarity", y = "Frequency (log)") +
    scale_alpha(range = c(0.1, 1)) +
    theme_bw() +
    theme(panel.grid= element_blank(),
          axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
          legend.position="none") +
    ggrepel::geom_text_repel(data = temp_black, aes(x = beta, y = frequency, label = word),
                    segment.size = 0.25, colour = "black", size = 3) +
    geom_point(data = temp_black, aes(x = beta, y = frequency), cex = 0.7, colour = "black")
  
}


marginal_seedword_improvement <- function(dfm, seeds, k, engine = "rsvd", seed = 1111) {
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
    
    p <- predict(
      m,
      newdata = dfm,
      rescaling = TRUE,
      min_n = 4
    )
    
    list(
      fit = p,
      norm = nortest::ad.test(p)$statistic
    )
  })
  
  pd <- ggplot()
  for (i in seq_along(pred)) {
    p <- pred[[i]]$fit
    best_fit <- i == length(pred)
    cl <- ifelse(best_fit, "black", "grey")
    sz <- ifelse(best_fit, 1, 0.2)
    al <- ifelse(best_fit, 1, 0.5)
    
    pd <- pd +
      geom_density(
        data = data.frame(fit = p),
        aes(x = fit),
        na.rm = TRUE,
        size = sz,
        color = cl,
        alpha = al
      )
  }

  pd <- pd +
    geom_function(fun = dnorm, args = list(mean = 0, sd = 1), size = 1, color = "green") +
    theme_bw() +
    labs(x = "Polarity estimate", y = "Density") +
    scale_x_continuous(expand = c(0, 0))
  
  n <- sapply(pred, "[[", 2)
  n <- data.frame(x = seq_along(n) * 2, norm = n)
  
  list(
    density = pd,
    normality = n
  )
}
