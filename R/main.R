### MAIN WORKFLOW
### At this point this code should not be sourced directly
### because it will take a LONG time and is likely to crash
### somewhere

###############
# TODO:
# - search for solutions to aggregate tweets by author to possibly reduce "publication bias"
# - try without lemmatizing (see Watanabe 2021)
# - find new seedwords
# - make illustrative kernel function plots
# - filter out low-polarity terms
# - compute VIF, Morans' I and non-stationarity (using Monte Carlo) for each variable
# - look into bootstrap GWR for improving inference
###############

# Load packages, lists and utility functions
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

# Set up verbosity
options(quanteda_verbose = TRUE)
options(rgdal_show_exportToProj4_warnings = "none")

# Set up parallelization
workers <- round(availableCores() * 0.8, 0)
plan(multisession, workers = workers)


###############################################################################
## Data preparation ----                                                     ##
############################################################################### 

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


cli_progress_step("Pre-process text data")
tweets$text <- tweets$text %>%
  str_remove(regex("^RT ")) %>% # remove retweet indicators
  str_remove_all("@[A-Za-z0-9_]+:?\\s") # remove response indicators

# Retweet texts are capped. To enable matching retweets with their originals,
# retweet texts are exchanged with original texts if found.
tweets <- fix_retweet_texts(tweets)
tweets_stable <- tweets

# Create a copy that only contains unique tweet texts, i.e. all non-distinct
# tweets are excluded. This is done to prevent a semantic bias in document
# scaling because 50 retweets of a single tweet would amplify each co-occurrence
# between the original and the retweets by a factor of 50. Nonetheless, retweets
# are an important indicator of attitude, which is why the OG dataset will be
# rejoined with the full dataset after text scaling.
tweets_only_og <- tibble(text = dtext <- unique(tweets$text), did = seq_along(dtext))

# Create corpus from tweet data
cli_progress_step("Creating dfm")
tw_corpus <- corpus(tweets_only_og, docid_field = "did", text_field = "text")

# Create document-feature matrix
tw_tokens <- tw_corpus %>%
  tokens(
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE,
    remove_url = TRUE,
    remove_separators = FALSE,
    split_hyphens = FALSE,
    split_tags = TRUE
  ) %>%
  tokens_remove(stopwords("de", source = "marimo"))
tw_dfm <- tw_tokens %>%
  dfm() %>%
  dfm_trim(min_termfreq = 50, min_docfreq = 5) %>%
  dfm_select(c(" ", "\\n"), valuetype = "regex", selection = "remove")



###############################################################################
## Semantic scaling ----                                                     ##
############################################################################### 

cli_progress_step("Fitting scaling model")
set.seed(1111)
lsx_model <- textmodel_lss(
  tw_dfm,
  seeds = as.seedwords(as.list(seed)),
  auto_weight = TRUE,
  k = 500,
  cache = TRUE,
  engine = "rsvd",
)

k <- 304

# Plot cohesion statistic over k
cohesion_plot <- LSX::cohesion(lsx_model) %>%
  ggplot(aes(x = k, y = smoothed)) +
  geom_line() +
  geom_point(aes(x = k, y = raw), alpha = 0.2) +
  geom_vline(xintercept = k) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  ylab("Cohesion") +
  theme(plot.margin = margin(0.2, 0.5, 0.2, 0.2, "cm"))
ggsave("plots/cohesion.pdf", cohesion_plot, device = cairo_pdf)

set.seed(1111)
lsx_model <- textmodel_lss(
  tw_dfm,
  seeds = as.seedwords(as.list(seed)),
  auto_weight = TRUE,
  k = k,
  cache = TRUE,
  engine = "rsvd"
)

pheatmap(
  lsx_model$similarity,
  treeheight_col = 0,
  treeheight_row = 0,
  fontsize = 7,
  color = RColorBrewer::brewer.pal(11, "RdBu"),
  breaks = seq(-1, 1, 0.2)
)

# Plot term polarity
term_plot <- textplot_terms.textmodel_lss(lsx_model, highlighted = seed)
ggsave("plots/term_plot.pdf", term_plot, device = cairo_pdf)

# Leave-one-out validation approach for seedword selection
term_validation <- lss_loocv(lsx_model)
cbind(
  term_validation$df[term_validation$df$position == "hegemonic", c("seedword", "reference", "estimate", "error")],
  term_validation$df[term_validation$df$position == "polemic", c("seedword", "reference", "estimate", "error")]
) %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    col.names = str_to_title(colnames(.)),
    label = "seedwords",
    caption = "Seedwords used as reference for semantic scaling together with their validation results. The reference column refers to the weight assigned to a keyword when included in the model. The estimate column shows the prediction of a seedword when excluded from the model. The error measures the squared distance between both values."
  ) %>%
  add_header_above(c("Hegemonic" = 4, "Polemic" = 4)) %>%
  column_spec(4) %>%
  kable_paper()

# Compute predictions for documents
cli_progress_step("Applying scaling model")
pred <- predict(
  lsx_model,
  newdata = tw_dfm,
  se_fit = TRUE,
  density = FALSE,
  rescaling = TRUE,
  min_n = 4
)

density_plot <- ggplot(data.frame(fit = pred$fit)) +
  geom_density(aes(x = fit), na.rm = TRUE, size = 1, color = "black", fill = "black", alpha = 0.5) +
  geom_function(fun = dnorm, args = list(mean = 0, sd = 0.77), size = 1, color = "red") +
  geom_function(fun = dnorm, args = list(mean = 0, sd = 1), size = 1, color = "green") +
  geom_function(fun = dcauchy, args = list(location = 0, scale = 0.62), size = 1, color = "blue") +
  theme_bw() +
  labs(x = "Polarity estimate", y = "Density") +
  scale_x_continuous(expand = c(0, 0))
ggsave("plots/density.pdf", density_plot, device = cairo_pdf)

# Plot document polarity
docscores_plot <- plot_docscores(pred, n = 1000, seed = 123)
ggsave("plots/doc_plot.pdf", docscores_plot, device = cairo_pdf)

# Combine unique tweets with prediction results
cli_progress_step("Joining with original tweet data")
tweets_only_og <- tweets_only_og %>%
  mutate(
    polarity = pred$fit,
    se = pred$se
  )

# Join unique tweets with original tweet dataset
tweets_pred <- right_join(tweets_only_og, tweets[c("text", "id", "author_id", "tweet_time")], by = "text") %>%
  as_tibble() %>%
  st_as_sf()

# Plot time series
tweets_ts <- tweets_pred %>%
  arrange(tweet_time) %>%
  bind_cols(authors_agg = duplicated(tweets_pred$author_id) %>%
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
  geom_smooth(method = "loess", span = 0.1, size = 1) +
  geom_smooth(method = "lm", size = 0.5, se = FALSE, color = "black") +
  scale_x_date(expand = c(0, 0)) +
  scale_y_continuous("Polarity", limits = c(-1, 1)) +
  theme_bw() +
  ggplot(tweets_ts, aes(x = tweet_day, y = authors)) +
  geom_line(size = 1) +
  scale_x_date("Collection time", expand = c(0, 0)) +
  scale_y_continuous("Cum. share of authors", expand = c(0, 0)) +
  theme_bw() +
  plot_layout(nrow = 2) +
  plot_annotation(tag_levels = "1", tag_prefix = "(", tag_suffix = ")")
ggsave("plots/timeseries.pdf", ts_plot, device = cairo_pdf)




###############################################################################
## Spatial analysis ----                                                     ##
############################################################################### 

# Download county boundaries
cli_progress_step("Downloading boundaries")
bounds_needed <- FALSE

if (bounds_needed) {
  kreise <- retry::retry(
    get_admin("Kreis"),
    upon = "error",
    when = "InitializeSecurityContext|Connection",
    max_tries = 3
  )
  
  states <- retry::retry(
    get_admin("Bundesland"),
    upon = "error",
    when = "InitializeSecurityContext|Connection",
    max_tries = 3
  )
  
  # Fix non-unique names
  kreise <- kreise %>%
    group_by(gen) %>%
    mutate(ags = ags, gen = make_kreis_unique(gen)) %>%
    ungroup()
}


tweets_pred_25 <- tweets_pred[abs(tweets_pred$polarity) >= 0.25,]
tweets_pred_50 <- tweets_pred[abs(tweets_pred$polarity) >= 0.5,]
tweets_pred_100 <- tweets_pred[abs(tweets_pred$polarity) >= 1,]


# Aggregate polarity scores by authors to create a single polarity score
# for each author
# This takes a lot of time (~1-2 hours depending on number of cores used)
tweets_by_author <- tweets_pred %>%
  as_tibble() %>%
  group_by(author_id) %>%
  group_split()
with_progress({
  p <- progressor(steps = length(unique(tweets_pred$author_id)))
  tweets_by_author <- tweets_by_author %>%
    future_map_dfr(function(x) {
      p()
      tibble(
        author = unique(x$author_id),
        polarity = mean(x$polarity, na.rm = TRUE),
        se = mean(x$se, na.rm = TRUE),
        geometry = x$geometry[1]
      )
    }) %>%
    st_as_sf() %>%
    st_set_crs(3035)
})


tweets_by_author_25 <- tweets_pred_25 %>%
  as_tibble() %>%
  group_by(author_id) %>%
  group_split()
with_progress({
  p <- progressor(steps = length(unique(tweets_pred_25$author_id)))
  tweets_by_author_25 <- tweets_by_author_25 %>%
    future_map_dfr(function(x) {
      p()
      tibble(
        author = unique(x$author_id),
        polarity = mean(x$polarity, na.rm = TRUE),
        se = mean(x$se, na.rm = TRUE),
        geometry = x$geometry[1]
      )
    }) %>%
    st_as_sf() %>%
    st_set_crs(3035)
})


tweets_by_author_50 <- tweets_pred_50 %>%
  as_tibble() %>%
  group_by(author_id) %>%
  group_split()
with_progress({
  p <- progressor(steps = length(unique(tweets_pred_50$author_id)))
  tweets_by_author_50 <- tweets_by_author_50 %>%
    future_map_dfr(function(x) {
      p()
      tibble(
        author = unique(x$author_id),
        polarity = mean(x$polarity, na.rm = TRUE),
        se = mean(x$se, na.rm = TRUE),
        geometry = x$geometry[1]
      )
    }) %>%
    st_as_sf() %>%
    st_set_crs(3035)
})


tweets_by_author_100 <- tweets_pred_100 %>%
  as_tibble() %>%
  group_by(author_id) %>%
  group_split()
with_progress({
  p <- progressor(steps = length(unique(tweets_pred_100$author_id)))
  tweets_by_author_100 <- tweets_by_author_100 %>%
    future_map_dfr(function(x) {
      p()
      tibble(
        author = unique(x$author_id),
        polarity = mean(x$polarity, na.rm = TRUE),
        se = mean(x$se, na.rm = TRUE),
        geometry = x$geometry[1]
      )
    }) %>%
    st_as_sf() %>%
    st_set_crs(3035)
})

# Aggregate data ----
cli_progress_step("Aggregating polarity scores")
kreise_polarity <- aggregate(
  tweets_by_author["polarity"],
  by = kreise,
  FUN = mean,
  join = st_contains,
  na.rm = TRUE
)

kreise_polarity_25 <- aggregate(
  tweets_by_author_25["polarity"],
  by = kreise,
  FUN = mean,
  join = st_contains,
  na.rm = TRUE
)

kreise_polarity_50 <- aggregate(
  tweets_by_author_50["polarity"],
  by = kreise,
  FUN = mean,
  join = st_contains,
  na.rm = TRUE
)

kreise_polarity_100 <- aggregate(
  tweets_by_author_100["polarity"],
  by = kreise,
  FUN = mean,
  join = st_contains,
  na.rm = TRUE
)

cli_progress_step("Aggregating counts")
kreise_authors <- aggregate(
  tweets_by_author["author"],
  by = kreise,
  FUN = length,
  join = st_contains
)

kreise_se <- aggregate(
  tweets_by_author["se"],
  by = kreise,
  FUN = mean,
  join = st_contains,
  na.rm = TRUE
)

kreise_polarity <- kreise_polarity %>%
  bind_cols(polarity_25 = kreise_polarity_25$polarity,
            polarity_50 = kreise_polarity_50$polarity,
            polarity_100 = kreise_polarity_100$polarity,
            authors = kreise_authors$author,
            place = kreise$gen,
            ags = kreise$ags,
            se = kreise_se$se) %>%
  as_tibble() %>%
  st_as_sf()

tmap_polarity <- tm_shape(kreise_polarity) +
  tm_borders() +
  tm_fill(col = "polarity", palette = "RdYlBu", style = "order", midpoint = 0, title = "Polarity estimates") +
  tm_layout(frame = FALSE, legend.position = c("left", "bottom"))
tmap_counts <- tm_shape(kreise_polarity) +
  tm_borders() +
  tm_fill(col = "authors", palette = "PuBu", style = "log10", title = "Number of authors") +
  tm_scale_bar(position = c("RIGHT", "BOTTOM"), text.size = 0.5) +
  tm_compass(position = c(0.8, 0.08), size = 2.5) +
  tm_layout(legend.format = "scientific", legend.height = 0.15, frame = FALSE, legend.position = c("left", "bottom"))
tmap_stats <- tmap_arrange(tmap_polarity, tmap_counts, ncol = 2, asp = 0.5)
tmap_save(tmap_stats, filename = "plots/polarity_map.pdf", device = cairo_pdf)

tmap_se <- tm_shape(kreise_polarity) +
  tm_borders() +
  tm_fill(col = "se", palette = "OrRd", style = "order", midpoint = 0, title = "Std. error") +
  tm_scale_bar(position = c("RIGHT", "BOTTOM"), text.size = 0.5) +
  tm_compass(position = c(0.8, 0.08), size = 2.5) +
  tm_layout(frame = FALSE, legend.position = c("left", "bottom"))
tmap_save(tmap_se, filename = "plots/polarity_se_map.pdf", device = cairo_pdf)

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

kreise_polarity_varsel <- kreise_polarity_context[names(var_sel)]
ind_maps <- tm_shape(kreise_polarity_varsel) +
  tm_fill(
    names(var_sel),
    style = "order",
    title = sel_eng[names(var_sel)]
  ) +
  tm_layout(frame = FALSE, legend.position = c("LEFT", "TOP"), scale = 0.8) +
  tm_facets(ncol = 4)
tmap_save(ind_maps, "plots/ind_maps.pdf", device = cairo_pdf)

kreise_polarity_context <- kreise_polarity_context %>%
  as_tibble()

kreise_polarity_context[
  !names(kreise_polarity_context) %in% c(
    "polarity", "polarity_25", "polarity_50", "polarity_100", "authors",
    "place", "ags", "se", "geometry"
  )
] <-
  kreise_polarity_context %>%
  select(-c(
    polarity, polarity_25, polarity_50, polarity_100, authors,
    place, ags, se, geometry)
  ) %>%
  scale() %>%
  as_tibble()

kreise_polarity_context <- kreise_polarity_context %>%
  st_as_sf()

# Multi-level model ----
cli_alert_info("Linking district-level contextual variables to individual tweet authors")
authors_context <- st_join(tweets_by_author, kreise_polarity_context[c("ags", names(var_sel))], st_within)
authors_context_25 <- st_join(tweets_by_author_25, kreise_polarity_context[c("ags", names(var_sel))], st_within)
authors_context_50 <- st_join(tweets_by_author_50, kreise_polarity_context[c("ags", names(var_sel))], st_within)
authors_context_100 <- st_join(tweets_by_author_100, kreise_polarity_context[c("ags", names(var_sel))], st_within)

formula_lmer <- as.formula(paste(
  "polarity ~",
  paste(paste(names(var_sel), "+ (1 +", names(var_sel), "| ags)" ), collapse = " + ")
))
formula_lmer_25 <- as.formula(paste(
  "polarity_25 ~",
  paste(paste(names(var_sel), "+ (1 +", names(var_sel), "| ags)" ), collapse = " + ")
))
formula_lmer_50 <- as.formula(paste(
  "polarity_50 ~",
  paste(paste(names(var_sel), "+ (1 +", names(var_sel), "| ags)" ), collapse = " + ")
))
formula_lmer_100 <- as.formula(paste(
  "polarity_100 ~",
  paste(paste(names(var_sel), "+ (1 +", names(var_sel), "| ags)" ), collapse = " + ")
))

lmer_control <- lmerControl(
  calc.derivs = FALSE,
  optCtrl = list(algorithm = "NLOPT_LN_BOBYQA", iprint = 3)
)
iteration <- ""
cli_alert_info("Starting multilevel modelling: Polarity")
cli_progress_message("  {iteration}")
mlmodel <- callr::r(
  function(form, data, ...) {
    lme4::lmer(form, data = data, verbose = 1, ...)
  },
  args = list(form = formula_lmer, data = authors_context, control = lmer_control),
  callback = lmer_callback,
  spinner = TRUE
)



coords <- kreise %>%
  st_geometry() %>%
  st_centroid() %>%
  st_coordinates()

# Construct a formula for regression modelling
formula <- paste("polarity ~", paste(names(var_sel), collapse = " + ")) %>%
  as.formula()
formula_25 <- paste("polarity_25 ~", paste(names(var_sel), collapse = " + ")) %>%
  as.formula()
formula_50 <- paste("polarity_50 ~", paste(names(var_sel), collapse = " + ")) %>%
  as.formula()
formula_100 <- paste("polarity_100 ~", paste(names(var_sel), collapse = " + ")) %>%
  as.formula()

global_model <- lm(formula, kreise_polarity_context)
global_model_25 <- lm(formula_25, kreise_polarity_context)
global_model_50 <- lm(formula_50, kreise_polarity_context)
global_model_100 <- lm(formula_100, kreise_polarity_context)

# LaTeX output for global model
stargazer(
  global_model, global_model_25, global_model_50, global_model_100,
  type = "latex",
  style = "all2",
  dep.var.labels = c("Polarity", "Polarity >= 0.25", "Polarity >= 0.5", "Polarity >= 1"),
  align = TRUE,
  covariate.labels = sel_eng[names(var_sel)],
  df = FALSE,
  ci = TRUE,
  font.size = "footnotesize",
  add.lines = list(
    c("AIC", round(AIC(global_model), 2), round(AIC(global_model_25), 2), round(AIC(global_model_50), 2), round(AIC(global_model_100), 2)),
    c("AICc", round(AICc(global_model), 2), round(AICc(global_model_25), 2), round(AICc(global_model_50), 2), round(AICc(global_model_100), 2)),
    c("BIC", round(BIC(global_model), 2), round(BIC(global_model_25), 2), round(BIC(global_model_50), 2), round(BIC(global_model_100), 2))
  ),
  label = "tab:global",
  title = "Global OLS regression results taking into account (1) all polarity estimates, (2) polarity estimates >= 0.5, and (3) polarity estimates >= 1"
) %>%
  paste(collapse = "\n") %>%
  str_remove_all("\\(p = 0\\.0+\\)") %>%
  cat()

ci_plot <- plot_ci(global_model, bottom = FALSE) +
  plot_ci(global_model_25, left = FALSE, bottom = FALSE) +
  plot_ci(global_model_50) +
  plot_ci(global_model_100, left = FALSE) +
  patchwork::plot_annotation(tag_levels = 1, tag_prefix = "(", tag_suffix = ")") +
  patchwork::plot_layout(ncol = 2)
ggsave("plots/ci_plot.pdf", ci_plot, device = cairo_pdf)

kreise_residuals <- cbind(kreise, residuals(global_model_25))

# Create neighborhood structure and spatial weight matrix
kreise_nb <- poly2nb(
  st_geometry(kreise),
  row.names = kreise$gen,
  queen = TRUE
)

# Apply S coding to account for edge effects of variance and activate zero
# policy because some islands don't have neighbors
kreise_listw <- nb2listw(kreise_nb, style = "B", zero.policy = TRUE)

global_specs <- sapply(c("polarity", names(var_sel)), function(x) {
  m <- moran.test(kreise_polarity_context[[x]], kreise_listw, zero.policy = TRUE)
  est <- pvalue_stars(round(m$estimate["Moran I statistic"], 5), m$p.value)

}) %>%
  bind_cols("Moran's I" = ., VIF = c("", round(car::vif(global_model), 2)))

# Convert to sp object
kreise_polarity_context_sp <- as_Spatial(st_as_sf(kreise_polarity_context))

msel <- model_selection(
  formula_25,
  data = kreise_polarity_context,
  diagnostic = "AIC",
  table_diagnostic = "all"
)

approach <- toupper(unname(msel$solution["approach"]))
adaptive <- unname(msel$solution["type"] == "adaptive")
kernel <- tolower(unname(msel$solution["func"]))

# Multi-scale model ----
invisible(captureOutput(
  msgwr_model <- gwr.multiscale(
    formula_25,
    data = kreise_polarity_context_sp,
    kernel = kernel,
    adaptive = adaptive,
    criterion = "dCVR"
  )
))

names(msgwr_model)[5] <- "bw"
row.names(msgwr_model$bw) <- NULL
msgwr_sf <- st_as_sf(msgwr_model$SDF)
msgwr_model$bw <- msgwr_model$bw[nrow(msgwr_model$bw), ]
names(msgwr_model$bw) <- c("Intercept", names(var_sel))
global_specs <- bind_cols(
  Variable = c("Intercept", sel_eng[names(var_sel)]),
  global_specs,
  Bandwidth = paste0(round(msgwr_model$bw, 0), ifelse(adaptive, "", "m"))
)

stargazer(
  global_specs,
  type = "latex",
  title = "Global diagnostics for spatial auto-correlation, multicollinearity and non-stationarity",
  label = "tab:diagnostics",
  summary = FALSE,
  notes = "* p < 0.1, ** p < 0.05, *** p < 0.01",
  rownames = FALSE,
  notes.align = "r"
) %>%
  paste(collapse = "\n") %>%
  str_replace_all(fixed("\\textasteriskcentered "), "*") %>%
  str_replace_all(regex("\\{(\\*)"), "{$^{*") %>%
  str_replace_all(regex(" (\\*)"), "$^{*") %>%
  str_replace_all(regex("[0-9](\\*)"), "$^{*") %>%
  str_replace_all(fixed("* "), "*}$") %>%
  str_replace_all(fixed("cccc"), "lccc") %>%
  str_replace_all(fixed("<"), "$<$") %>%
  cat()

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


# Plot results of MSGWR
eco_plot <- plot_gwr(
  msgwr_model,
  col = names(var_sel)[1:4],
  style = "mennis4",
  scale = 0.7
)
tmap_save(eco_plot, "plots/tmap_eco_msgwr.pdf", device = cairo_pdf, outer.margins = 0)
 
social_plot <- plot_gwr(
  msgwr_model,
  col = names(var_sel)[5:8],
  style = "mennis4",
  scale = 0.7
)
tmap_save(social_plot, "plots/tmap_social_msgwr.pdf", device = cairo_pdf, outer.margins = 0)

inst_plot <- plot_gwr(
  msgwr_model,
  col = names(var_sel)[9:12],
  style = "mennis4",
  scale = 0.7
)
tmap_save(inst_plot, "plots/tmap_inst_msgwr.pdf", device = cairo_pdf, outer.margins = 0)

env_plot <- plot_gwr(
  msgwr_model,
  col = names(var_sel)[13:16],
  style = "mennis4",
  scale = 0.7
)
tmap_save(env_plot, "plots/tmap_env_msgwr.pdf", device = cairo_pdf, outer.margins = 0)
