remove_file_ext <- function(file) {
  ext <- paste0(".", tools::file_ext(file))
  stringr::str_replace(file, fixed(ext), "")
}


check_date <- function(date) {
  date <- as_datetime(date) %>%
    floor_date(unit = "days") %>%
    add(dseconds(1)) %>%
    interval(end = ceiling_date(., unit = "days"))
  
  tweets <- dir("data/tweets") %>%
    remove_file_ext() %>%
    unique() %>%
    as_datetime(format = "%y%m%d")
  
  !any(vapply(tweets, `%within%`, FUN.VALUE = logical(1), date))
}


free_dates <- function() {
  lapply(1:6, function(days) {
    date <- floor_date(now(), unit = "days") - ddays(days)
    if (check_date(date)) date else NULL
  }) %>%
    unlist() %>%
    as_datetime(tz = Sys.timezone()) %>%
    format("%Y-%m-%d") %>% 
    rev()
}


collected_dates <- function() {
  normalizePath("data/tweets", "/") %>%
    file.path(dir(.))
}


collected_geo <- function() {
  geo_path <- normalizePath("data/geo", "/")
  collected <- collected_dates() %>%
    basename() %>%
    remove_file_ext() %>%
    paste0("_geo.rds") %>%
    file.path(geo_path, .)
  
  collected[file.exists(collected)]
}


not_geocoded_yet <- function() {
  collected <- collected_dates() %>%
    basename()
  geo <- collected_geo() %>%
    basename() %>%
    stringr::str_replace(fixed("_geo"), "")
  
  setdiff(collected, geo)
}


make_kreis_unique <- function(place) {
  df <- kreise[kreise$gen %in% place, ]
  
  if (nrow(df) > 1) {
    area <- st_area(df)
    ind <- which(min(area) == area)
    df$gen[ind] <- paste("Landkreis", df$gen[ind])
  }
  df$gen
}


find_keywords_in_tweets <- function(texts, keywords) {
  map_dbl(keywords, function(x) {
    if (nzchar(x)) {
      x <- str_replace_all(x, fixed(", "), "|")
      n <- sum(str_detect_all(texts, x))
      if (nchar(n) == 5) round(n, -3) else round(n, -2)
    } else {
      0
    }
  })
}


# Sorts tweets alphabetically, groups them by unique text substring and adopts
# the earliest text (i.e. the original)
fix_retweet_texts <- function(tweets) {
  ftweets <- tweets %>%
    st_drop_geometry() %>%
    dplyr::select(text, tweet_time, id) %>%
    mutate(text_cut = str_sub(.$text, 1, 80)) %>%
    group_by(text_cut)
  ftweets <- group_modify(ftweets, function(.x, g) {
    if (nrow(.x) > 1) {
      nt <- .x[which(min(.x$tweet_time) == .x$tweet_time), ][["text"]][1]
      .x[["text"]] <- nt
    }
    .x
  }) %>%
    ungroup() %>%
    dplyr::select(-text_cut, -tweet_time)
  
  left_join(tweets %>% rename(oldtext = text), ftweets, by = "id") %>%
    as_tibble() %>%
    relocate(text, .before = oldtext) %>%
    st_as_sf()
}


clear_memory <- function() {
  obj <- c(
    "cohesion_plot", "t", "pred", "tmap_stats", "tmap_polarity", "tmap_counts",
    "kreise_polarity", "kreise_authors", "kreise_counts", "is_retweet",
    "ts_plot", "tweets_ts", "lsx_model", "lsx_model_irlba", "lsx_model_rsvd",
    "lsx_model_svds", "model", "tw_corpus", "tweets", "tweets_only_og",
    "tw_dfm"
  )
  
  suppressWarnings(rm(list = obj, envir = .GlobalEnv))
  gc()
}


#' Riffle-merges two vectors, possibly of different lengths
#'
#' Takes two vectors and interleaves the elements.  If one vector is longer than
#' the other, it appends on the tail of the longer vector to the output vector.
#' @param a First vector
#' @param b Second vector
#' @return Interleaved vector as described above.
#' @author Matt Pettis
riffle <- function(a, b) {
  len_a <- length(a)
  len_b <- length(b)
  len_comm <- pmin(len_a, len_b)
  len_tail <- abs(len_a - len_b)
  
  if (len_a < 1) stop("First vector has length less than 1")
  if (len_b < 1) stop("Second vector has length less than 1")
  
  riffle_common <- c(rbind(a[1:len_comm], b[1:len_comm]))
  
  if (len_tail == 0) return(riffle_common)
  
  if (len_a > len_b) {
    return(c(riffle_common, a[(len_comm + 1):len_a]))
  } else {
    return(c(riffle_common, b[(len_comm + 1):len_b]))
  }
}


pvalue_stars <- function(x, p, cutoff = c(0.1, 0.05, 0.005)) {
  if (!missing(x)) {
    dplyr::case_when(
      p < cutoff[3] ~ paste0(x, "***"),
      p < cutoff[2] ~ paste0(x, "**"),
      p < cutoff[1] ~ paste0(x, "*"),
      TRUE ~ as.character(x)
    )
  } else {
    dplyr::case_when(
      p < cutoff[3] ~ "***",
      p < cutoff[2] ~ "**",
      p < cutoff[1] ~ "*",
      TRUE ~ NA_character_
    )
  }
}


plot_ci <- function(model, level = 0.95, cutoff = c(0.1, 0.05, 0.005), bottom = TRUE, left = TRUE) {
  summ <- summary(model)
  z.value <- qnorm((1 + level) / 2)
  se <- summ$coefficients[, "Std. Error"]
  coef <- summ$coefficients[, "Estimate"]
  var_names <- sapply(rownames(summ$coefficients), function(x) {
    if (x %in% names(sel_eng)) {
      sel_eng[[x]]
    } else x
  })
  
  ci <- tibble(
    variable = factor(var_names, levels = var_names),
    coef = coef,
    lower = coef - z.value * se,
    upper = coef + z.value * se,
    sig = pvalue_stars(p = summ$coefficients[, "Pr(>|t|)"])
  )
  
  mar <- global_model$coefficients %>%
    range() %>%
    as.list() %>%
    do.call(subtract, .) %>%
    abs() %>%
    {. - (0.9 * .)}
  
  ggplot(tail(ci, -1), aes(x = variable)) +
    geom_point(aes(y = coef), size = 2) +
    geom_errorbar(aes(y = coef, ymin = lower, ymax = upper), width = 0.25) +
    geom_hline(yintercept = 0) +
    geom_text(aes(y = upper + mar, label = sig), na.rm = TRUE) +
    theme_bw() +
    labs(y = if (left) "Coefficient" else NULL, x = NULL) +
    scale_y_continuous(limits = c(-0.15, 0.2), labels = if (left) \(x) x else NULL) +
    scale_x_discrete(labels = if (bottom) \(x) x else NULL) +
    theme(
      axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1),
      
    )
}


lmer_callback <- function(newout, proc) {
  if (str_detect(newout, "^ ?[0-9]+:")) {
    newout <- paste0("Iteration", str_split(newout, ":")[[1]][1])
    assign("iteration", newout, envir = .GlobalEnv)
    cli_progress_update(.envir = .GlobalEnv)
  } else if (str_detect(newout, "iteration:")) {
    assign("iteration", newout, envir = .GlobalEnv)
    cli_progress_update(.envir = .GlobalEnv)
  }
}