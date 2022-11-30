# MAIN WORKFLOW
# At this point this code should not be sourced directly
# because it will take way more than a full work day to process
# and is likely to crash somewhere

#
# TODO:
# - search for solutions to aggregate tweets by author to possibly reduce sampling bias
# - try without lemmatizing (see Watanabe 2021)
# - find new seedwords
# - make illustrative kernel function plots
# - filter out low-polarity terms
# - compute VIF, Morans' I and non-stationarity (using Monte Carlo) for each variable
# - look into bootstrap GWR for improving inference
# - Remove primsek? Moderate VIF
# - Remove random structure of non-varying variables?
# - Create a cool network plot to make R file structure neater :)
#

# Load packages, lists and utility functions
source("R/packages.R")
source("R/globals.R")
source("R/gather.R")
source("R/boundaries.R")
source("R/scaling.R")
source("R/search_tweets.R")
source("R/read_context.R")
source("R/gwr.R")
source("R/effects.R")
source("R/utils.R")

# Set up verbosity
options(quanteda_verbose = TRUE)
options(rgdal_show_exportToProj4_warnings = "none")

# Set up parallelization
workers <- round(availableCores() * 0.8, 0)
plan(multisession, workers = workers)

# Control randomness
rseed <- 1111


#### Tweet preparation ----                                                     

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

freq <- textstat_frequency(tw_dfm) %>%
  filter(feature %in% str_remove_all(keywords, "#"))

freq <- sapply(keywords_df$Query.parameter, function(key) {
  key <- strsplit(key, ", ")[[1]] %>%
    str_remove_all("#")
  sum(freq[freq$feature %in% key, ]$frequency)
}, USE.NAMES = FALSE)
keywords_df$Volume <- {round(freq / sum(freq), 4) * 100} %>% paste("%")

keywords_df %>%
  kbl(
    caption = "Environmental domains based on the planetary boundary framework by \textcite{Steffen2015} and their associated keywords in the Twitter discourse. The volume column represents the share of each domain out of all domains.",
    label = "domains",
    format = "latex",
    col.names = c("Domain", "Query parameters", "Volume"),
    align = "l",
    booktabs = TRUE,
    row.names = FALSE
  ) %>%
  kable_paper() %>%
  str_replace_all("tabular", "tabularx") %>%
  str_replace_all("lll", "lXl") %>%
  str_replace_all(fixed("[t]"), "{\\textwidth}") %>%
  cat()


#### Semantic scaling ----                                                     

cli_progress_step("Fitting scaling model")
set.seed(rseed)
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

set.seed(rseed)
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

# Plot density function
dn <- plot_multiple_density(tw_dfm, seed, k = k, engine = "rsvd", seed = rseed)
ggsave("plots/density.pdf", dn$density, device = cairo_pdf)

# Plot normality test
ad_plot <- ggplot(dn$normality) +
  geom_smooth(aes(x, y = norm), size = 1, color = "black") +
  geom_point(aes(x, y = norm), alpha = 0.2) +
  labs(x = "Number of seedwords", y = "Anderson-Darling normality statistic") +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw()
ggsave("plots/normality_plot.pdf", ad_plot, device = cairo_pdf)

# Plot document polarity
docscores_plot <- plot_docscores(pred, n = 1000, seed = rseed, zoom = c(-7.5, 7.5))
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
  dplyr::select(polarity, tweet_day, authors_agg) %>%
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




#### Data aggregation ----                                                     

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
  
  coords <- kreise %>%
    st_geometry() %>%
    st_centroid() %>%
    st_coordinates()
}


tweets_pred_25 <- tweets_pred[abs(tweets_pred$polarity) >= 0.25,]
tweets_pred_50 <- tweets_pred[abs(tweets_pred$polarity) >= 0.5,]
tweets_pred_100 <- tweets_pred[abs(tweets_pred$polarity) >= 1,]


# Aggregate polarity scores by authors to create a single polarity score
# for each author
# This takes a lot of time (~1-2 hours depending on number of cores used)
# and strains the CPU
tweets_by_author <- tweets_pred %>%
  as_tibble() %>%
  group_by(author_id) %>%
  group_split()
with_progress({
  p <- progressor(steps = length(unique(tweets_pred$author_id)))
  tweets_by_author <- tweets_by_author %>%
    future_map_dfr(aggregate_author) %>%
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
    future_map_dfr(aggregate_author) %>%
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
    future_map_dfr(aggregate_author) %>%
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
    future_map_dfr(aggregate_author) %>%
    st_as_sf() %>%
    st_set_crs(3035)
})


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

# Create baseline grid
grid <- starsExtra::make_grid(tweets_by_author, 2500)

# Interpolate polarity based on baseline grid
polarity_grid <- gstat::idw(
  polarity ~ 1,
  location = na.omit(tweets_by_author[!abs(tweets_by_author$polarity) > 3.5, ]),
  newdata = grid,
  idp = 1.5
) %>% st_crop(kreise)
polarity_25_grid <- gstat::idw(
  polarity ~ 1,
  location = na.omit(tweets_by_author_25),
  newdata = grid
) %>% st_crop(kreise)
polarity_50_grid <- gstat::idw(
  polarity ~ 1,
  location = na.omit(tweets_by_author_50),
  newdata = grid
) %>% st_crop(kreise)
polarity_100_grid <- gstat::idw(
  polarity ~ 1,
  location = na.omit(tweets_by_author_100),
  newdata = grid
) %>% st_crop(kreise)

# Map spatial distribution and distribution of counts
tmap_polarity1 <- tm_shape(kreise) +
  tm_borders() +
  tm_shape(polarity_grid) +
  tm_raster(col = "var1.pred", palette = "RdBu", style = "order", labels = c("Negative", "Mixed", "Positive"), midpoint = 0, title = "Polarity estimates") +
  tm_layout(title.fontface = "bold", legend.format = "scientific", legend.height = 0.15, frame = FALSE, legend.position = c("left", "bottom"))
tmap_polarity2 <- tm_shape(kreise) +
  tm_borders() +
  tm_shape(kreise_polarity) +
  tm_fill(col = "polarity", palette = "RdBu", style = "order", midpoint = 0, title = "Polarity estimates") +
  tm_layout(title.fontface = "bold", legend.format = "scientific", legend.height = 0.15, frame = FALSE, legend.position = c("left", "bottom"))
tmap_counts <- tm_shape(kreise) +
  tm_borders() +
  tm_shape(kreise_polarity) +
  tm_fill(col = "authors", palette = "Blues", style = "log10", title = "Number of authors") +
  tm_scale_bar(position = c("RIGHT", "BOTTOM"), text.size = 0.5) +
  tm_compass(position = c(0.8, 0.08), size = 2.5) +
  tm_layout(title.fontface = "bold", legend.format = "scientific", legend.height = 0.15, frame = FALSE, legend.position = c("left", "bottom"))
tmap_stats <- tmap_arrange(tmap_polarity1, tmap_polarity2, tmap_counts, ncol = 3, asp = 0.5)
tmap_save(tmap_stats, filename = "plots/polarity_map.pdf", device = cairo_pdf)

# Map standard errors of polarity scores
tmap_se <- tm_shape(kreise_polarity) +
  tm_borders() +
  tm_fill(col = "se", palette = "OrRd", style = "order", midpoint = 0, title = "Std. error") +
  tm_scale_bar(position = c("RIGHT", "BOTTOM"), text.size = 0.5) +
  tm_compass(position = c(0.8, 0.08), size = 2.5) +
  tm_layout(frame = FALSE, legend.position = c("left", "bottom"))
tmap_save(tmap_se, filename = "plots/polarity_se_map.pdf", device = cairo_pdf)



## Context variable preparation ----

# Read context data generated by read_context_bulk.R
context <- read_feather("data/context/context.feather")

# Plot kernel illustration
kplot <- plot_kernels()
ggsave("plots/kplot.pdf", kplot, device = cairo_pdf)

# Prepare context variables and join them with polarity scores
kreise_polarity_context <- kreise_polarity %>%
  left_join(context, by = "ags") %>%
  dplyr::select(where(~!all(is.na(.x)))) %>%
  mutate(across(.fns = randomForest::na.roughfix)) # impute NA by median

# Map distribution of context variables
kreise_polarity_varsel <- kreise_polarity_context[names(var_sel)]
ind_maps <- tm_shape(kreise) +
  tm_borders() +
  tm_shape(kreise_polarity_varsel) +
  tm_fill(
    names(var_sel),
    style = "order",
    palette = "RdBu",
    midpoint = 0,
    title = sel_eng[names(var_sel)]
  ) +
  tm_layout(frame = FALSE, legend.position = c("LEFT", "TOP"), scale = 0.75) +
  tm_facets(ncol = 4)
tmap_save(ind_maps, "plots/ind_maps.pdf", device = cairo_pdf)

# Center and scale context variables
kreise_polarity_context <- datawizard::standardize(kreise_polarity_context)



## Multi-level model ----

cli_alert_info("Linking district-level contextual variables to individual tweet authors")
authors_context <- st_join(tweets_by_author, kreise_polarity_context[c("ags", names(var_sel))], st_within)
authors_context_25 <- st_join(tweets_by_author_25, kreise_polarity_context[c("ags", names(var_sel))], st_within)
authors_context_50 <- st_join(tweets_by_author_50, kreise_polarity_context[c("ags", names(var_sel))], st_within)
authors_context_100 <- st_join(tweets_by_author_100, kreise_polarity_context[c("ags", names(var_sel))], st_within)

# Construct formula containing fixed and random intercepts/slopes depending
# on effects
formula_lmer <- construct_mer_formula(
  polarity                = "dependent",
  industriequote          = c(intercept = "fixed",  slope = "fixed"),
  kreative_klasse         = c(intercept = "fixed",  slope = "fixed"),
  akademiker              = c(intercept = "random", slope = "random"),
  erwerbstätige_primsek   = c(intercept = "random", slope = "random"),
  unter_30                = c(intercept = "random", slope = "random"),
  lebenserwartung         = c(intercept = "fixed",  slope = "fixed"),
  pkwdichte               = c(intercept = "random", slope = "random"),
  scenes                  = c(intercept = "random", slope = "random"),
  stimmenanteile_afd      = c(intercept = "random", slope = "random"),
  neuinanspruchnahme      = c(intercept = "random", slope = "random"),
  städtebauförderung_kurz = c(intercept = "random", slope = "random"),
  sachinvestitionen       = c(intercept = "fixed",  slope = "fixed"),
  naturschutz             = c(intercept = "random", slope = "random"),
  windkraft_pro_10000     = c(intercept = "random", slope = "random"),
  überschwemmungsgefahr   = c(intercept = "random", slope = "random"),
  erholungsfläche         = c(intercept = "random", slope = "random"),
  grp = "ags",
  data = authors_context
)

# Improve performance by selecting a fast model optimizer
lmer_control <- lmerControl(
  calc.derivs = FALSE,
  optCtrl = list(algorithm = "NLOPT_LN_BOBYQA", iprint = 3)
)

# Fit multilevel models (takes around 300-1500 iterations each, ~40 minutes)
iteration <- ""
cli_alert_info("Starting multilevel modelling: Polarity")
cli_progress_message("  {iteration}")
mlmodel <- callr::r(
  call_lmer,
  args = list(form = formula_lmer, data = authors_context, control = lmer_control),
  callback = lmer_callback,
  spinner = TRUE
)
mlmodel_25 <- callr::r(
  call_lmer,
  args = list(form = formula_lmer, data = authors_context_25, control = lmer_control),
  callback = lmer_callback,
  spinner = TRUE
)
mlmodel_50 <- callr::r(
  call_lmer,
  args = list(form = formula_lmer, data = authors_context_50, control = lmer_control),
  callback = lmer_callback,
  spinner = TRUE
)
mlmodel_100 <- callr::r(
  call_lmer,
  args = list(form = formula_lmer, data = authors_context_100, control = lmer_control),
  callback = lmer_callback,
  spinner = TRUE
)

# Plot exploratory effect ranges
mlsim <- REsim(mlmodel_100, seed = rseed)
all_caterpillars <- plotREsim(mlsim, sd = TRUE)
ggsave("plots/all_er.pdf", all_caterpillars, device = cairo_pdf, height = 30)

# Plot random effects of selected terms using adjusted function from effects.R
good_caterpillars <- list()
good_caterpillars[[1]] <- plotREsim(mlsim, facet = list(groupFctr = "ags", term = "(Intercept)"), sd = FALSE)
good_caterpillars[[2]] <- plotREsim(mlsim, facet = list(groupFctr = "ags", term = "akademiker"), sd = FALSE)
good_caterpillars[[3]] <- plotREsim(mlsim, facet = list(groupFctr = "ags", term = "scenes"), sd = FALSE)
good_caterpillars[[4]] <- plotREsim(mlsim, facet = list(groupFctr = "ags", term = "städtebauförderung_kurz"), sd = FALSE)
good_caterpillars[[5]] <- plotREsim(mlsim, facet = list(groupFctr = "ags", term = "unter_30"), sd = FALSE)
good_caterpillars[[6]] <- plotREsim(mlsim, facet = list(groupFctr = "ags", term = "windkraft_pro_10000"), sd = FALSE)
good_caterpillars <- arrangeGrob(grobs = good_caterpillars, ncol = 3, left = "Effect range", bottom = "Districts")
ggsave("plots/effect_ranges.pdf", good_caterpillars, device = cairo_pdf)

effect_plot <- plot_ci(mlmodel, bottom = FALSE) +
  plot_ci(mlmodel_25, bottom = FALSE, left = FALSE) +
  plot_ci(mlmodel_50) +
  plot_ci(mlmodel_100, left = FALSE) +
  plot_annotation(tag_levels = 1, tag_prefix = "(", tag_suffix = ")")
ggsave("plots/ci_plot.pdf", effect_plot, device = cairo_pdf, width = 12)

mlsimsf <- mlsim %>%
  filter(term %in% c("akademiker", "scenes",
                   "städtebauförderung_kurz", "unter_30", "windkraft_pro_10000")) %>%
  left_join(kreise, by = c("groupID" = "ags")) %>%
  st_as_sf() %>%
  mutate(term = recode_values(term, as.list(setNames(names(sel_eng), sel_eng))))

mlmap <- tm_shape(kreise) +
  tm_borders() +
  tm_shape(mlsimsf) +
  tm_fill(col = "mean", palette = "PRGn", style = "jenks", midpoint = 0, title = "Estimate") +
  tm_facets(by = "term", ncol = 3) +
  tm_layout(asp = 0.8, outer.margins = c(-0.35, 0, -0.35, -0.15))
tmap_save(mlmap, "plots/mlmap.pdf", device = cairo_pdf)

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
