###############
# TODO:
# - search for solutions to aggregate tweets by author to possibly reduce "publication bias"
# - try without lemmatizing (see Watanabe 2021)
# - find new seedwords
# - make illustrative kernel function plots
###############


source("~/Masterarbeit/R/packages.R")
source("~/Masterarbeit/R/lists.R")
source("~/Masterarbeit/R/datatable.R")
source("~/Masterarbeit/R/boundaries.R")
source("~/Masterarbeit/R/scaling.R")
source("~/Masterarbeit/R/search_tweets.R")
source("~/Masterarbeit/R/read_context.R")
source("~/Masterarbeit/R/gwr.R")
source("~/Masterarbeit/R/esda.R")
source("~/Masterarbeit/R/utils.R")

options(quanteda_verbose = TRUE)
options(rgdal_show_exportToProj4_warnings = "none")

# Read all collected tweets and filter duplicates and retweets ----
cli_progress_step("Reading data")
compile_tweets <- TRUE

if (compile_tweets) {
  tweets <- path_to_dt(filter = TRUE, geocoded = TRUE) %>%
    as_tibble() %>%
    st_as_sf() %>%
    st_transform(3035)
  st_write_feather(tweets, "data/tweets_cmp.feather")
} else {
  tweets <- st_read_feather("data/tweets_cmp.feather")
}

# Create a copy that only contains unique tweet texts, i.e. all non-distinct
# retweets are excluded. This is done to prevent a semantic bias in document
# scaling because 50 retweets of a single tweet would amplify each co-occurrence
# between the original and the retweets by a factor of 50. Nonetheless, retweets
# are an important indicator of attitude, which is why the OG dataset will be
# rejoined with the full dataset after text scaling.
cli_progress_step("Removing duplicates")
tweets$text <- tweets$text %>%
  str_remove(regex("^RT ")) %>%
  str_remove_all("@[A-Za-z0-9_]+:?\\s") # remove @
tweets <- fix_retweet_texts(tweets) # exchange retweet texts with originals
tweets_stable <- tweets

tweets_only_og <- tibble(text = dtext <- unique(tweets$text), did = seq_along(dtext))

# Create corpus from tweet data ----
cli_progress_step("Creating dfm")
tw_corpus <- corpus(tweets_only_og, docid_field = "did", text_field = "text")

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
  dfm_trim(min_termfreq = 50, min_docfreq = 5) %>%
  dfm_select(c(" ", "\\n"), valuetype = "regex", selection = "remove") %>%
  dfm_wordstem(language = "german")


# LSX model ----
cli_progress_step("Fitting scaling model")
lsx_model <- textmodel_lss(
  tw_dfm,
  seeds = as.seedwords(seed),
  auto_weight = TRUE,
  k = 500,
  cache = TRUE
)

cohesion_plot <- LSX::cohesion(lsx_model) %>%
  ggplot(aes(x = k, y = smoothed)) +
  geom_line() +
  geom_point(aes(x = k, y = raw), alpha = 0.2) +
  geom_vline(xintercept = 370) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  ylab("Cohesion") +
  theme(plot.margin = margin(0.2, 0.5, 0.2, 0.2, "cm"))
ggsave("plots/cohesion.pdf", cohesion_plot, device = cairo_pdf)

lsx_model <- textmodel_lss(
  tw_dfm,
  seeds = as.seedwords(seed),
  auto_weight = TRUE,
  k = 255,
  cache = TRUE,
)

term_plot <- LSX::textplot_terms(lsx_model, highlighted = seed)
ggsave("plots/term_plot.pdf", term_plot, device = cairo_pdf)

cli_progress_step("Applying scaling model")
pred <- predict(
  lsx_model,
  newdata = tw_dfm,
  se_fit = TRUE,
  density = TRUE,
  rescaling = TRUE,
  min_n = 5
)

docscores_plot <- plot_docscores(pred, n = 1000, seed = 123)
ggsave("plots/doc_plot.pdf", docscores_plot, device = cairo_pdf)

# Combine unique tweets with prediction results ----
cli_progress_step("Joining with original tweet data")
tweets_only_og <- tweets_only_og %>%
  mutate(
    polarity = pred$fit,
    se = pred$se
  )

# Join unique tweets with original tweet dataset ----
tweets <- right_join(tweets_only_og, tweets, by = "text") %>%
  as_tibble() %>%
  st_as_sf()

# Plot time series
tweets_ts <- tweets %>%
  arrange(tweet_time) %>%
  bind_cols(authors_agg = cumsum(duplicated(tweets$author_id))) %>%
  st_drop_geometry() %>%
  group_by(tweet_day = floor_date(tweet_time, "1 hour")) %>%
  summarise(polarity = mean(polarity, na.rm = TRUE),
            se = mean(se, na.rm = TRUE),
            authors_agg = sum(authors_agg)) %>%
  mutate(polarity = rollmean(polarity, k = 7, align = "center", fill = 0),
         se = rollmean(se, k = 7, align = "center", fill = 0))
ggplot(tweets_ts, aes(x = tweet_day, y = polarity)) +
  geom_line(size = 1) +
  geom_line(aes(x = tweet_day, y = polarity + se), lty = "dashed") +
  geom_line(aes(x = tweet_day, y = polarity - se), lty = "dashed") +
  scale_x_datetime(expand = c(0, 0)) +
  theme_bw()

tweets_ts <- tweets %>%
  arrange(tweet_time) %>%
  bind_cols(authors_agg = duplicated(tweets$author_id) %>%
              not() %>%
              as.numeric() %>%
              cumsum()) %>%
  st_drop_geometry() %>%
  mutate(tweet_day = as_date(tweet_time)) %>%
  select(polarity, tweet_day, authors_agg) %>%
  group_by(tweet_day) %>%
  summarise(polarity = mean(polarity, na.rm = TRUE), authors = max(authors_agg)) %>%
  mutate(authors = authors / max(authors))
ts_plot <- ggplot(tweets_ts, aes(x = tweet_day, y = polarity)) +
  #geom_line(size = 1) +
  geom_smooth(span = 0.1, size = 1) +
  geom_smooth(method = "lm", size = 0.5, se = FALSE, color = "black") +
  scale_x_date("Collection time", expand = c(0, 0)) +
  scale_y_continuous("Polarity", limits = c(-1, 1)) +
  theme_bw() +
  ggplot(tweets_ts, aes(x = tweet_day, y = authors)) +
  geom_line(size = 1) +
  scale_x_date("Collection time", expand = c(0, 0)) +
  scale_y_continuous("Cum. share of authors", expand = c(0, 0)) +
  theme_bw() +
  plot_layout(nrow = 2) +
  plot_annotation(tag_levels = "a")
ggsave("plots/timeseries.pdf", ts_plot, device = cairo_pdf)

# Get Kreis boundaries ----
cli_progress_step("Downloading boundaries")
kreise <- retry::retry(
  get_admin("Kreis"),
  upon = "error",
  when = "InitializeSecurityContext|Connection",
  max_tries = 3
)

# Fix non-unique names
kreise <- kreise %>%
  group_by(gen) %>%
  mutate(ags = ags, gen = make_kreis_unique(gen)) %>%
  ungroup()

# Aggregate data ----
cli_progress_step("Aggregating polarity scores")
kreise_polarity <- aggregate(
  tweets["polarity"],
  by = kreise,
  FUN = mean,
  join = st_contains,
  na.rm = TRUE
)

cli_progress_step("Aggregating counts")
kreise_counts <- aggregate(
  tweets["id"],
  by = kreise,
  FUN = length,
  join = st_contains
)

kreise_authors <- aggregate(
  tweets["author_id"],
  by = kreise,
  FUN = function(kr) length(unique(kr)),
  join = st_contains
)

kreise_polarity <- kreise_polarity %>%
  bind_cols(counts = kreise_counts$id,
            authors = kreise_authors$author_id,
            place = kreise$gen,
            ags = kreise$ags) %>%
  as_tibble() %>%
  st_as_sf()

tmap_polarity <- tm_shape(kreise_polarity) +
  tm_borders() +
  tm_fill(col = "polarity", palette = "RdYlBu", style = "cont", midpoint = NA, title = "Polarity estimates") +
  tm_layout(frame = FALSE, legend.position = c("left", "bottom"))
tmap_counts <- tm_shape(kreise_polarity) +
  tm_borders() +
  tm_fill(col = "authors", palette = "PuBu", style = "log10", title = "Number of authors") +
  tm_scale_bar(position = c("RIGHT", "BOTTOM"), text.size = 0.5) +
  tm_compass(position = c(0.8, 0.08), size = 2.5) +
  tm_layout(legend.format = "scientific", legend.height = 0.15, frame = FALSE, legend.position = c("left", "bottom"))
tmap_stats <- tmap_arrange(tmap_polarity, tmap_counts, ncol = 2, asp = 0.5)
tmap_save(tmap_stats, filename = "plots/polarity_map.pdf", device = cairo_pdf)

# Prepare local model ----
context <- read_feather("data/context/context.feather")
kplot <- plot_kernels()
ggsave("plots/kplot.pdf", kplot, device = cairo_pdf)

# Prepare context variables and join them with polarity scores
kreise_polarity_context <- kreise_polarity %>%
  left_join(context, by = "ags") %>%
  select(where(~!all(is.na(.x)))) %>%
  mutate(across(.fns = function(x) { # impute with global mean
    if (any(is.na(x))) {
      x[is.na(x)] <- mean(x, na.rm = TRUE)
    }
    x
  }))

# Construct a formula for regression modelling
formula <- paste("polarity ~", paste(paste0("scale(", names(var_sel), ")"), collapse = " + ")) %>%
  as.formula()

global_model <- lm(formula, kreise_polarity_context)
kreise_residuals <- cbind(kreise, residuals(global_model))

# Create neighborhood structure and spatial weight matrix
kreise_nb <- poly2nb(
  st_geometry(kreise),
  row.names = kreise$gen,
  queen = TRUE
)

# Apply S coding to account for edge effects of variance and activate zero
# policy because some islands don't have neighbors
kreise_listw <- nb2listw(kreise_nb, style = "B", zero.policy = TRUE)

global_specs <- sapply(names(var_sel), function(x) {
  m <- spdep::moran.test(kreise_polarity_context[[x]], kreise_listw, zero.policy = TRUE)
  c(m$estimate, p = round(m$p.value, 5))
}) %>%
  t() %>%
  as.data.frame()

# Convert to sp object
kreise_polarity_context_sp <- as_Spatial(kreise_polarity_context)

msel <- model_selection(
  formula,
  data = kreise_polarity_context,
  diagnostic = "AIC",
  table_diagnostic = "all"
)

approach <- unname(msel$solution["approach"])
adaptive <- unname(msel$solution["type"] == "adaptive")
kernel <- unname(msel$solution["func"])

# Multi-scale model ----
invisible(capture.output(
  msgwr_model <- gwr.multiscale(
    formula,
    data = kreise_polarity_context_sp,
    kernel = kernel,
    adaptive = adaptive,
    criterion = "dCVR"
  )
))


# Basic model ----
bw <- bw.gwr(
  formula,
  data = kreise_polarity_context_sp,
  approach = approach,
  kernel = kernel,
  adaptive = adaptive
)

gwr_model <- gwr.basic(
  formula,
  data = kreise_polarity_context_sp,
  bw = bw,
  kernel = kernel,
  adaptive = adaptive,
  cv = TRUE
)
