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
# - Remove graduate? Moderate VIF
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
options(modelsummary_stars_note = TRUE)
options(modelsummary_get = "all")

# Set up parallelization
workers <- round(availableCores() * 0.8, 0)
plan(multisession, workers = workers)


# Create LaTeX table for package overview
# Table A.1
data.frame(
  Software = c(
    "R Programming Language", "Photon", "\\texttt{Tidyverse}", "\\texttt{httr2}",
    "\\texttt{sf}", "\\texttt{quanteda}", "\\texttt{LSX}", "\\texttt{rsvd}",
    "\\texttt{lme4}", "\\texttt{GWmodel}"
  ),
  Use = c(
    "Statistical software and programming environment that is used for all tasks concerning data collection, manipulation, processing and analysis.",
    "Java-based geocoder that is based on OpenStreetMap data. Used to geocode the entirety of collected tweets.",
    "R framework for general data manipulation. Used in all steps of data analysis.",
    "R Package to build, test and repeatedly perform calls to both Photon and the Twitter API",
    "R package for GIS-based analyses that is build upon classic geospatial libraries like GDAL and S2. Used for all manipulation and analyses of geographical data.",
    "R framework for the analysis of textual data. Used for preparation, cleaning and exploration of document-feature matrices.",
    "R package that implements \\posscite{Watanabe2021} Latent Semantic Scaling.",
    "R package that implements \\posscite{Halko2011} randomized matrix decomposition.",
    "R package for fitting and analyzing mixed-effect models",
    "R package that implements geographically weighted models. Used for model selection and implementation."
  ),
  Citation = c(
    "\\cite{R2022}",
    "\\cite{Komoot2022}",
    "\\cite{Wickham2019}",
    "\\cite{Wickham2022}",
    "\\cite{Pebesma2018}",
    "\\cite{Benoit2018}",
    "\\cite{Watanabe2022}",
    "\\cite{Erichson2019}",
    "\\cite{Bates2015}",
    "\\cite{Gollini2015}"
  )
) %>%
  kbl(
    format = "latex",
    escape = FALSE,
    booktabs = TRUE,
    align = "l",
    caption = "List of key software packages that are applied in this work",
    label = "software"
  ) %>%
  kable_paper() %>%
  str_replace_all("tabular", "tabularx") %>%
  str_replace(fixed("[t]{lll}"), "{\\textwidth}{lXl}") %>%
  cat(file = "plots/software.tex")


#### Tweet preparation ----                                                     

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

# Create table for environmental domains + volumes
# Table 3.1
keywords_df %>%
  kbl(
    caption = "Environmental domains based on the planetary boundary framework by \\textcite{Steffen2015} and their associated keywords in the Twitter discourse. The volume column represents the share of each domain out of all domains.",
    caption.short = "Environmental domains and their associated keywords",
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
  str_replace(fixed("{table}"), "{table}[!htb]") %>%
  str_replace_all(fixed("[t]"), "{\\textwidth}") %>%
  cat(file = "plots/domains.tex")


#### Semantic scaling ----                                                     

set.seed(rseed)
lsx_model <- textmodel_lss(
  tw_dfm,
  seeds = as.seedwords(as.list(seed)),
  auto_weight = TRUE,
  k = 500,
  cache = TRUE,
  engine = "rsvd",
)

k <- 299

# Plot cohesion statistic over k
# Table A.3
cohesion_plot <- LSX::cohesion(lsx_model) %>%
  ggplot(aes(x = k, y = smoothed)) +
  geom_line(na.rm = TRUE) +
  geom_point(aes(x = k, y = raw), alpha = 0.2, na.rm = TRUE) +
  geom_vline(xintercept = k) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  ylab("Cohesion") +
  theme(plot.margin = ggplot2::margin(0.2, 0.5, 0.2, 0.2, "cm"))
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
  fontsize = 6,
  color = RColorBrewer::brewer.pal(11, "RdBu"),
  breaks = seq(-1, 1, 0.2)
)

# Plot term polarity
# Figure 4.2
term_plot <- textplot_terms.textmodel_lss(lsx_model, highlighted = seed, max_words = 10000)
ggsave("plots/term_plot.pdf", term_plot, device = cairo_pdf)

# Leave-one-out validation approach for seedword selection
# Table 3.2
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
  kable_paper() %>%
  str_replace(fixed("\\centering"), "\\centering\n\\scriptsize") %>%
  cat(file = "plots/term_validation.tex")

# Compute predictions for documents
pred <- predict(
  lsx_model,
  newdata = tw_dfm,
  se_fit = TRUE,
  density = FALSE,
  rescaling = TRUE,
  min_n = 4
)

# Plot density function
# Figure 4.1
dn <- plot_multiple_density(tw_dfm, seed, k = k, engine = "rsvd", seed = rseed)
ggsave("plots/density.pdf", dn$density, device = cairo_pdf)

# Plot normality test
# Figure A.2
ad_plot <- ggplot(dn$normality) +
  geom_smooth(aes(x, y = norm), size = 1, color = "black") +
  geom_point(aes(x, y = norm), alpha = 0.2) +
  labs(x = "Number of seedwords", y = "Anderson-Darling normality statistic") +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw()
ggsave("plots/normality_plot.pdf", ad_plot, device = cairo_pdf)

# Plot document polarity
# Figure A.4
docscores_plot <- plot_docscores(pred, n = 1000, seed = rseed, zoom = c(-7.5, 7.5))
ggsave("plots/doc_plot.pdf", docscores_plot, device = cairo_pdf)

# Combine unique tweets with prediction results
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
# Figure 4.3
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


tweets_pred_25 <- tweets_pred[abs(tweets_pred$polarity) >= 0.25, ]
tweets_pred_50 <- tweets_pred[abs(tweets_pred$polarity) >= 0.5, ]
tweets_pred_100 <- tweets_pred[abs(tweets_pred$polarity) >= 1, ]


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


# Aggregate onto district-level
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
grid <- st_make_grid(tweets_by_author, 1000)
grid2 <- aggregate(tweets_by_author["polarity"], grid, FUN = mean, na.rm = TRUE)
grid <- make_grid(grid, 1000)

# Interpolate polarity based on baseline grid
polarity_grid <- gstat::idw(
  polarity ~ 1,
  location = na.omit(grid2),
  newdata = grid,
  idp = 0.5
) %>% st_crop(kreise)

# Map spatial distribution and distribution of counts
# Figure 4.4
tmap_polarity1 <- tm_shape(st_union(kreise)) +
  tm_borders() +
  tm_shape(polarity_grid) +
  tm_raster(col = "var1.pred", palette = "RdBu", style = "quantile", midpoint = NA, labels = c("Negative", "", "Mixed", "", "Positive"), title = "Polarity estimates") +
  tm_layout(title.fontface = "bold", legend.format = "scientific", legend.height = 0.15, frame = FALSE, legend.position = c("left", "bottom"))
tmap_polarity2 <- tm_shape(st_union(kreise)) +
  tm_borders() +
  tm_shape(kreise_polarity) +
  tm_fill(col = "polarity", palette = "RdBu", style = "order", midpoint = 0, title = "Polarity estimates", contrast = 0.5) +
  tm_layout(title.fontface = "bold", legend.format = "scientific", legend.height = 0.15, frame = FALSE, legend.position = c("left", "bottom"))
tmap_counts <- tm_shape(st_union(kreise)) +
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

# Create LaTeX table of top 20 districts in terms of authorship
# Table A.2
district_summary(authors_context, kreise_polarity) %>%
  cat(file = "plots/top_n.tex")



## Context variable preparation ----

# Read context data generated by read_context_bulk.R
context <- read_feather("data/context/context.feather")

# Plot kernel illustration
# Figure 3.5
kplot <- plot_kernels()
ggsave("plots/kplot.pdf", kplot, device = cairo_pdf)

# Prepare context variables and join them with polarity scores
kreise_polarity_context <- kreise_polarity %>%
  left_join(context, by = "ags") %>%
  dplyr::select(where(~!all(is.na(.x)))) %>%
  mutate(across(.fns = randomForest::na.roughfix)) # impute NA by median

# Map distribution of context variables
# Figure A.1
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
  tm_layout(
    frame = FALSE,
    legend.position = c(0, 0.85),
    scale = 0.75,
    legend.title.fontface = "bold",
    legend.title.size = 1.2) +
  tm_facets(ncol = 4)
tmap_save(ind_maps, "plots/ind_maps.png", device = png)

# Center and scale context variables
kreise_polarity_context <- datawizard::standardize(kreise_polarity_context)

# Create LaTeX table of variables and their descriptions
# Table 3.3
kableExtra::kbl(
  var_sel_latex,
  format = "latex",
  row.names = FALSE,
  caption = "Overview and descriptions of the 16 independent variables included in the statistical models",
  label = "indep",
  booktabs = TRUE,
  escape = FALSE,
  align = c("l", "X", "l", "l")
) %>%
  kable_paper() %>%
  str_replace(fixed("{table}"), "{table}\n\\begin{minipage}{\\textwidth}") %>%
  str_replace(fixed("\\end{table}"), "\\end{minipage}\n\\end{table}") %>%
  str_replace(fixed("\\centering"), "\\centering\n\\footnotesize") %>%
  str_replace_all(fixed("tabular"), "tabularx") %>%
  str_replace(fixed("[t]"), "{\\textwidth}") %>%
  cat(file = "plots/variables.tex")

## Multi-level model ----

cli_alert_info("Linking district-level contextual variables to individual tweet authors")
authors_context <- st_join(tweets_by_author, kreise_polarity_context[c("ags", names(var_sel))], st_within)
authors_context_25 <- st_join(tweets_by_author_25, kreise_polarity_context[c("ags", names(var_sel))], st_within)
authors_context_50 <- st_join(tweets_by_author_50, kreise_polarity_context[c("ags", names(var_sel))], st_within)
authors_context_100 <- st_join(tweets_by_author_100, kreise_polarity_context[c("ags", names(var_sel))], st_within)

# Construct formula containing fixed and random intercepts/slopes depending
# on effects

# maximal model formula
formula_lmer_max <- construct_mer_formula(
  polarity                = "dependent",
  industriequote          = c(intercept = "random", slope = "random"),
  kreative_klasse         = c(intercept = "random", slope = "random"),
  akademiker              = c(intercept = "random", slope = "random"),
  erwerbstätige_primsek   = c(intercept = "random", slope = "random"),
  unter_30                = c(intercept = "random", slope = "random"),
  lebenserwartung         = c(intercept = "random", slope = "random"),
  pkwdichte               = c(intercept = "random", slope = "random"),
  scenes                  = c(intercept = "random", slope = "random"),
  stimmenanteile_afd      = c(intercept = "random", slope = "random"),
  neuinanspruchnahme      = c(intercept = "random", slope = "random"),
  städtebauförderung_kurz = c(intercept = "random", slope = "random"),
  sachinvestitionen       = c(intercept = "random", slope = "random"),
  naturschutz             = c(intercept = "random", slope = "random"),
  windkraft_pro_10000     = c(intercept = "random", slope = "random"),
  überschwemmungsgefahr   = c(intercept = "random", slope = "random"),
  erholungsfläche         = c(intercept = "random", slope = "random"),
  grp = "place",
  data = authors_context
)

random_structure <- list(
  industriequote          = c(intercept = "random", slope = "fixed"),
  kreative_klasse         = c(intercept = "fixed",  slope = "fixed"),
  akademiker              = c(intercept = "fixed",  slope = "fixed"),
  erwerbstätige_primsek   = c(intercept = "fixed",  slope = "fixed"),
  unter_30                = c(intercept = "fixed",  slope = "fixed"),
  lebenserwartung         = c(intercept = "fixed",  slope = "fixed"),
  pkwdichte               = c(intercept = "fixed",  slope = "fixed"),
  scenes                  = c(intercept = "fixed",  slope = "fixed"),
  stimmenanteile_afd      = c(intercept = "fixed",  slope = "fixed"),
  neuinanspruchnahme      = c(intercept = "fixed",  slope = "fixed"),
  städtebauförderung_kurz = c(intercept = "random", slope = "fixed"),
  sachinvestitionen       = c(intercept = "fixed",  slope = "random"),
  naturschutz             = c(intercept = "fixed",  slope = "fixed"),
  windkraft_pro_10000     = c(intercept = "fixed",  slope = "random"),
  überschwemmungsgefahr   = c(intercept = "fixed",  slope = "fixed"),
  erholungsfläche         = c(intercept = "fixed",  slope = "fixed")
)

formula_lmer <- inject(construct_mer_formula(
  polarity = "dependent",
  !!!random_structure,
  grp = "ags",
  data = authors_context)
)

random_structure_25 <- list(
  industriequote          = c(intercept = "fixed",  slope = "random"),
  kreative_klasse         = c(intercept = "random", slope = "fixed"),
  akademiker              = c(intercept = "fixed",  slope = "fixed"),
  erwerbstätige_primsek   = c(intercept = "fixed",  slope = "fixed"),
  unter_30                = c(intercept = "fixed",  slope = "fixed"),
  lebenserwartung         = c(intercept = "fixed",  slope = "random"),
  pkwdichte               = c(intercept = "fixed",  slope = "fixed"),
  scenes                  = c(intercept = "fixed",  slope = "fixed"),
  stimmenanteile_afd      = c(intercept = "fixed",  slope = "fixed"),
  neuinanspruchnahme      = c(intercept = "fixed",  slope = "fixed"),
  städtebauförderung_kurz = c(intercept = "fixed",  slope = "random"),
  sachinvestitionen       = c(intercept = "random", slope = "fixed"),
  naturschutz             = c(intercept = "fixed",  slope = "fixed"),
  windkraft_pro_10000     = c(intercept = "fixed",  slope = "random"),
  überschwemmungsgefahr   = c(intercept = "fixed",  slope = "fixed"),
  erholungsfläche         = c(intercept = "fixed",  slope = "fixed")
)

formula_lmer_25 <- inject(construct_mer_formula(
  polarity = "dependent",
  !!!random_structure_25,
  grp = "ags",
  data = authors_context
))

random_structure_50 <- list(
  industriequote          = c(intercept = "fixed",  slope = "fixed"),
  kreative_klasse         = c(intercept = "fixed",  slope = "fixed"),
  akademiker              = c(intercept = "fixed",  slope = "fixed"),
  erwerbstätige_primsek   = c(intercept = "fixed",  slope = "random"),
  unter_30                = c(intercept = "fixed",  slope = "fixed"),
  lebenserwartung         = c(intercept = "fixed",  slope = "random"),
  pkwdichte               = c(intercept = "fixed",  slope = "fixed"),
  scenes                  = c(intercept = "fixed",  slope = "fixed"),
  stimmenanteile_afd      = c(intercept = "fixed",  slope = "fixed"),
  neuinanspruchnahme      = c(intercept = "fixed",  slope = "fixed"),
  städtebauförderung_kurz = c(intercept = "fixed",  slope = "random"),
  sachinvestitionen       = c(intercept = "fixed",  slope = "random"),
  naturschutz             = c(intercept = "fixed",  slope = "fixed"),
  windkraft_pro_10000     = c(intercept = "fixed",  slope = "random"),
  überschwemmungsgefahr   = c(intercept = "fixed",  slope = "fixed"),
  erholungsfläche         = c(intercept = "fixed",  slope = "fixed")
)

formula_lmer_50 <- inject(construct_mer_formula(
  polarity = "dependent",
  !!!random_structure_50,
  grp = "ags",
  data = authors_context
))

random_structure_100 <- list(
  industriequote          = c(intercept = "fixed",  slope = "fixed"),
  kreative_klasse         = c(intercept = "fixed",  slope = "random"),
  akademiker              = c(intercept = "fixed",  slope = "fixed"),
  erwerbstätige_primsek   = c(intercept = "fixed",  slope = "random"),
  unter_30                = c(intercept = "fixed",  slope = "fixed"),
  lebenserwartung         = c(intercept = "fixed",  slope = "fixed"),
  pkwdichte               = c(intercept = "fixed",  slope = "fixed"),
  scenes                  = c(intercept = "fixed",  slope = "random"),
  stimmenanteile_afd      = c(intercept = "fixed",  slope = "fixed"),
  neuinanspruchnahme      = c(intercept = "fixed",  slope = "fixed"),
  städtebauförderung_kurz = c(intercept = "random", slope = "random"),
  sachinvestitionen       = c(intercept = "fixed",  slope = "random"),
  naturschutz             = c(intercept = "fixed",  slope = "fixed"),
  windkraft_pro_10000     = c(intercept = "fixed",  slope = "random"),
  überschwemmungsgefahr   = c(intercept = "fixed",  slope = "fixed"),
  erholungsfläche         = c(intercept = "fixed",  slope = "fixed")
)

formula_lmer_100 <- inject(construct_mer_formula(
  polarity = "dependent",
  !!!random_structure_100,
  grp = "ags",
  data = authors_context
))

# Create LaTeX output for random structure
# Table 4.1
bind_cols(
  bind_rows(random_structure),
  bind_rows(random_structure_25),
  bind_rows(random_structure_50),
  bind_rows(random_structure_100),
  .name_repair = quiet_name_repair
) %>%
  mutate(across(.fns = ~ifelse(.x == "random", "$\\times$", ""))) %>%
  bind_cols(Variable = sel_eng[names(var_sel)], ., .name_repair = quiet_name_repair) %>%
  set_colnames(c("", riffle(rep("I", 4), rep("S", 4)))) %>%
  kbl(
    escape = FALSE,
    format = "latex",
    booktabs = TRUE,
    align = c("l", "c", "c", "c", "c", "c", "c", "c", "c"),
    caption = "Final random structure employed for multilevel modelling. A cross signifies that intercept and/or slope vary across districts.",
    caption.short = "Final random structured employed for multilevel modelling",
    linesep = "",
    label = "random_structure"
  ) %>%
  kable_styling() %>%
  column_spec(c(1, 3, 5, 7, 9), border_right = TRUE) %>%
  #collapse_rows(1, latex_hline = "major") %>%
  kableExtra::footnote("I = Intercept, S = Slope", footnote_as_chunk = TRUE, fixed_small_size = TRUE) %>%
  add_header_above(c(" " = 1, "Model 1" = 2, "Model 2" = 2, "Model 3" = 2, "Model 4" = 2)) %>%
  cat(file = "plots/random_structure.tex")

# Improve performance by selecting a fast model optimizer
lmer_control <- lmerControl(
  calc.derivs = FALSE,
  optCtrl = list(algorithm = "NLOPT_LN_BOBYQA", iprint = 3)
)

# Fit multilevel models (takes around 50-300 iterations each)
iteration <- ""
cli_alert_info("Starting multilevel modelling: Polarity")
cli_progress_message("  {iteration}")
# maximal model:
# mlmodel <- callr::r(
#   call_lmer,
#   args = list(form = formula_lmer_max, data = authors_context, control = lmer_control),
#   callback = lmer_callback,
#   spinner = TRUE
# )
mlmodel <- callr::r(
  call_lmer,
  args = list(form = formula_lmer, data = authors_context, control = lmer_control),
  callback = lmer_callback,
  spinner = TRUE
)
mlmodel_25 <- callr::r(
  call_lmer,
  args = list(form = formula_lmer_25, data = authors_context_25, control = lmer_control),
  callback = lmer_callback,
  spinner = TRUE
)
mlmodel_50 <- callr::r(
  call_lmer,
  args = list(form = formula_lmer_50, data = authors_context_50, control = lmer_control),
  callback = lmer_callback,
  spinner = TRUE
)
mlmodel_100 <- callr::r(
  call_lmer,
  args = list(form = formula_lmer_100, data = authors_context_100, control = lmer_control),
  callback = lmer_callback,
  spinner = TRUE
)

# Heteroscedasticity-robust standard errors
# Takes an extremely long time, thus done manually instead of in a loop
robust.se <- list()
robust.se[[1]] <- vcovCR(mlmodel, type = "CR2")
robust.se[[2]] <- vcovCR(mlmodel_25, type = "CR2")
robust.se[[3]] <- vcovCR(mlmodel_50, type = "CR2")
robust.se[[4]] <- vcovCR(mlmodel_100, type = "CR2")

# Create LaTeX output for multilevel model
# Table 4.2
modelsummary(
  models = c(mlmodel, mlmodel_25, mlmodel_50, mlmodel_100),
  effects = "fixed",
  output = "latex",
  fmt = 3,
  estimate = "{estimate}{stars}",
  statistic = "{std.error}",
  vcov = function(x) as.matrix(clubSandwich::vcovCR(x, type = "CR2")),
  stars = c("$^{*}$" = 0.1, "$^{**}$" = 0.05, "$^{***}$" = 0.01),
  coef_rename = rename_vars,
  gof_map = gm,
  col.names = c("Context variables", "Model 1", "Model 2", "Model 3", "Model 4"),
  title = "Fixed effect sizes, significance levels and heteroskedasticity-robust standard errors of the multilevel regression models",
  caption.short = "Fixed effects of the multilevel regression models",
  label = "multilevel",
  booktabs = TRUE,
  escape = FALSE
) %>%
  kableExtra::footnote("$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", escape = FALSE, footnote_as_chunk = TRUE) %>%
  add_header_above(c(" ", "Model specification" = 4)) %>%
  kable_paper() %>%
  str_replace(fixed("\\centering"), "\\label{tab:multilevel}\\n\\centering\\n\\small") %>%
  cat(file = "plots/multilevel_tab.tex")

# Plot exploratory effect ranges
mlsim <- REsim(mlmodel, seed = rseed)
mlsim_25 <- REsim(mlmodel_25, seed = rseed)
mlsim_50 <- REsim(mlmodel_50, seed = rseed)
mlsim_100 <- REsim(mlmodel_100, seed = rseed)
mlterms <- setdiff(unique(mlsim$term), "(Intercept)")
mlterms_25 <- setdiff(unique(mlsim_25$term), "(Intercept)")
mlterms_50 <- setdiff(unique(mlsim_50$term), "(Intercept)")
mlterms_100 <- setdiff(unique(mlsim_100$term), "(Intercept)")

# Plot random effects of selected terms
# Figure 4.7
caterpillars <- dotplot(
  data = list(mlsim, mlsim_25, mlsim_50, mlsim_100),
  group = "ags",
  terms = list(mlterms, mlterms_25, mlterms_50, mlterms_100),
  ylim = c(-0.18, 0.18)
)
ggsave("plots/effect_ranges.pdf", caterpillars, device = cairo_pdf, height = 10)

# Plot fixed effects
# Figure 4.5
ci_plot <- plot_ci(mlmodel, mlmodel_25, mlmodel_50, mlmodel_100, limits = c(-0.1, 0.2))
ggsave("plots/ci_plot.pdf", ci_plot, device = cairo_pdf, height = 8, width = 10)

# Plot VIF diagnostic
# Figure A.5
vif_plot <- plot_vif(mlmodel, mlmodel_25, mlmodel_50, mlmodel_100)
ggsave("plots/vif_plot.pdf", vif_plot, device = cairo_pdf, height = 8, width = 8)

# Map random effects
mlsimsf <- mlsim %>%
  filter(term %in% mlterms) %>%
  left_join(kreise, by = c("groupID" = "ags")) %>%
  mutate(model = "Model 1") %>%
  st_as_sf() %>%
  mutate(term = factor(term, levels = names(var_sel))) %>%
  mutate(term = recode(term, !!!sel_eng))
mlsimsf_25 <- mlsim_25 %>%
  filter(term %in% mlterms_25) %>%
  left_join(kreise, by = c("groupID" = "ags")) %>%
  mutate(model = "Model 2") %>%
  st_as_sf() %>%
  mutate(term = factor(term, levels = names(var_sel))) %>%
  mutate(term = recode(term, !!!sel_eng))
mlsimsf_50 <- mlsim_50 %>%
  filter(term %in% mlterms_50) %>%
  left_join(kreise, by = c("groupID" = "ags")) %>%
  mutate(model = "Model 3") %>%
  st_as_sf() %>%
  mutate(term = factor(term, levels = names(var_sel))) %>%
  mutate(term = recode(term, !!!sel_eng))
mlsimsf_100 <- mlsim_100 %>%
  filter(term %in% mlterms_100) %>%
  left_join(kreise, by = c("groupID" = "ags")) %>%
  mutate(model = "Model 4") %>%
  st_as_sf() %>%
  mutate(term = factor(term, levels = names(var_sel))) %>%
  mutate(term = recode(term, !!!sel_eng))

# Map random effects
# Figure 4.8
mlmap <- tm_shape(st_union(kreise)) +
  tm_borders() +
  tm_shape(mlsimsf_50) +
  tm_fill(col = "mean", palette = "PRGn", style = "order", midpoint = 0, title = "Coefficient") +
  tm_facets(by = c("term"), ncol = 3, free.scales.fill = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  #tm_scale_bar(text.size = 0.35, width = 0.2, position = c("RIGHT", "BOTTOM")) +
  tm_layout(
    asp = 0.8,
    outer.margins = c(-0.35, 0, -0.35, -0.15),
    panel.label.bg.color = NA,
    panel.label.fontface = "bold",
    legend.position = c(-0.8, 0.25),
    legend.width = 2,
    legend.height = 2,
  )
tmap_save(mlmap, "plots/mlmap.pdf", device = cairo_pdf, height = 6)

# Select reoccurring districts with strong local estimates
reocur_mean <- mlsim_50 %>%
  left_join(kreise, by = c("groupID" = "ags")) %>%
  group_by(term) %>%
  filter(mean > 0.02) %>%
  ungroup() %>%
  group_by(gen) %>%
  summarise(n = n(), ags = groupID) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  distinct() %>%
  filter(n > 1) %>%
  dplyr::select(-ags)
reocur_median <- mlsim_50 %>%
  left_join(kreise, by = c("groupID" = "ags")) %>%
  group_by(term) %>%
  filter(median > 0.02) %>%
  ungroup() %>%
  group_by(gen) %>%
  summarise(n = n(), ags = groupID) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  distinct() %>%
  filter(n > 1) %>%
  dplyr::select(-ags)

# Tab. 5.1
cbind.zoo(reocur_mean, reocur_median) %>%
  na.fill("") %>%
  kbl(
    format = "latex",
    col.names = c("District", "n", "District", "n"),
    booktabs = TRUE,
    caption = "Districts with reoccurences of local estimates higher than 0.02 among randomly varying context variables. $n$ represents the number of reoccurences of over the five random effect variables (Fig. \ref{fig:ml_map})",
    caption.short = "Districts with reoccurences of strong local estimates",
    label = "reoccurences",
    escape = FALSE
  ) %>%
  add_header_above(c("By mean" = 2, "By median" = 2)) %>%
  cat(file = "plots/reoccurences.tex")

# Construct a formula for aggregated regression modelling
formula <- paste("polarity ~", paste(names(var_sel), collapse = " + ")) %>%
  as.formula()
formula_25 <- paste("polarity_25 ~", paste(names(var_sel), collapse = " + ")) %>%
  as.formula()
formula_50 <- paste("polarity_50 ~", paste(names(var_sel), collapse = " + ")) %>%
  as.formula()
formula_100 <- paste("polarity_100 ~", paste(names(var_sel), collapse = " + ")) %>%
  as.formula()

# Fit global aggregated OLS models
global_model <- lm(formula, kreise_polarity_context)
global_model_25 <- lm(formula_25, kreise_polarity_context)
global_model_50 <- lm(formula_50, kreise_polarity_context)
global_model_100 <- lm(formula_100, kreise_polarity_context)

stargazer(global_model, global_model_25, global_model_50, global_model_100, type = "text")

qq <- plot_qq(
  mlmodel, mlmodel_25, mlmodel_50, mlmodel_100,
  global_model, global_model_25, global_model_50, global_model_100
)
qq$data <- qq$data %>%
  mutate(model = recode(model,
                        "Model 1" = "Model 1 (multilevel)",
                        "Model 2" = "Model 2 (multilevel)",
                        "Model 3" = "Model 3 (multilevel)",
                        "Model 4" = "Model 4 (multilevel)",
                        "Model 5" = "Model 1 (OLS)",
                        "Model 6" = "Model 2 (OLS)",
                        "Model 7" = "Model 3 (OLS)",
                        "Model 8" = "Model 4 (OLS)",
  ))
ggsave("plots/qqplot.pdf", qq, device = cairo_pdf, width = 8, height = 8)

# Effect plot for global model
# Figure 4.6
ci_plot_global <- plot_ci(global_model, global_model_25, global_model_50, global_model_100)
ggsave("plots/ci_plot_global.pdf", ci_plot_global, device = cairo_pdf, height = 8, width = 10)

kreise_residuals <- cbind(kreise, residuals(global_model_25))

# Convert to sp object
kreise_polarity_context_sp <- as_Spatial(st_as_sf(kreise_polarity_context))

# Try out all model combinations and select the one with best diagnostics
msel <- model_selection(
  formula_50,
  data = kreise_polarity_context,
  diagnostic = "AIC",
  table_diagnostic = "all"
)

# Create LaTeX table of model combinations
# Table 4.3
msel$table %>% 
  cat(file = "plots/gwr_specs.tex")

approach <- toupper(unname(msel$solution["approach"]))
adaptive <- unname(msel$solution["type"] == "adaptive")
kernel <- tolower(unname(msel$solution["func"]))

# Multi-scale model ----
invisible(captureOutput(
  msgwr_model <- gwr.multiscale(
    formula,
    data = kreise_polarity_context_sp,
    kernel = kernel,
    adaptive = adaptive,
    criterion = "dCVR"
  ) %>%
    repair_msgwr()
))

invisible(captureOutput(
  msgwr_model_25 <- gwr.multiscale(
    formula_25,
    data = kreise_polarity_context_sp,
    kernel = kernel,
    adaptive = adaptive,
    criterion = "dCVR"
  ) %>%
    repair_msgwr()
))

invisible(captureOutput(
  msgwr_model_50 <- gwr.multiscale(
    formula_50,
    data = kreise_polarity_context_sp,
    kernel = kernel,
    adaptive = adaptive,
    criterion = "dCVR"
  ) %>%
    repair_msgwr()
))

invisible(captureOutput(
  msgwr_model_100 <- gwr.multiscale(
    formula_100,
    data = kreise_polarity_context_sp,
    kernel = kernel,
    adaptive = adaptive,
    criterion = "dCVR"
  ) %>%
    repair_msgwr()
))

# Create LaTeX table of multi-scale bandwidth sizes
# Table 4.4
msbw <- data.frame(
  model_1 = msgwr_model$bw,
  model_2 = msgwr_model_25$bw,
  model_3 = msgwr_model_50$bw,
  model_4 = msgwr_model_100$bw
) %>%
  bind_cols(var = row.names(.), .) %>%
  set_rownames(NULL)
  
kbl(
  msbw %>% mutate(var = dplyr::recode(var, !!!c(Intercept = "(Intercept)", sel_eng))),
  format = "latex",
  row.names = FALSE,
  col.names = c("", "Model 1", "Model 2", "Model 3", "Model 4"),
  booktabs = TRUE,
  align = c("l", "c", "c", "c", "c"),
  caption = "Adaptive multi-scale bandwidth sizes estimated by fitting an MSGWR model with an exponential kernel. Low bandwidth sizes indicate spatial non-stationarity.",
  caption.short = "Adaptive multi-scale bandwidth sizes",
  label = "msbw"
) %>%
  add_header_above(c(" " = 1, "Bandwidth sizes" = 4)) %>%
  cat(file = "plots/bandwidths.tex")

msgwr_sf <- msgwr_model_50$SDF %>%
  st_as_sf() %>%
  tidyr::pivot_longer(names(var_sel)) %>%
  mutate(name = factor(str_remove(name, fixed("_L")), levels = names(var_sel))) %>%
  mutate(name = recode(name, !!!sel_eng)) %>%
  dplyr::select(name, value)
msgwr_plot <- tm_shape(st_union(msgwr_sf)) +
  tm_borders() +
  tm_shape(msgwr_sf) +
  tm_fill(col = "value", palette = "PRGn", style = "order", midpoint = 0, title = "Coefficient") +
  tm_facets(by = "name", ncol = 4, free.scales.fill = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_layout(
    asp = 1,
    outer.margins = c(-0.35, 0, -0.35, -0.15),
    panel.label.bg.color = NA,
    panel.label.fontface = "bold"
  )
tmap_save(msgwr_plot, "plots/test.png", device = png)
msgwr_p <- gwr.t.adjust(msgwr_model_50)$SDF %>%
  st_as_sf() %>%
  tidyr::pivot_longer(paste(names(var_sel), "p_bo", sep = "_")) %>%
  mutate(name = factor(str_remove(name, fixed("_p_bo")), levels = names(var_sel))) %>%
  mutate(name = recode(str_remove(name, fixed("_p_bo")), !!!sel_eng)) %>%
  dplyr::select(name, value)
msgwr_p_plot <- tm_shape(st_union(msgwr_p)) +
  tm_borders() +
  tm_shape(msgwr_p) +
  tm_fill(
    col = "value",
    palette = "PRGn",
    style = "fixed",
    breaks = c(0, 0.001, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 1),
    labels = c("p < 0.001", "p < 0.01", "p < 0.05", "p < 0.1", "p < 0.2", "p < 0.3", "p < 0.4", "p > 0.5"),
    midpoint = 0,
    title = "Coefficient"
  ) +
  tm_facets(by = "name", ncol = 4, free.scales.fill = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_layout(
    saturation = 0,
    asp = 1,
    outer.margins = c(-0.35, 0, -0.35, -0.15),
    panel.label.bg.color = NA,
    panel.label.fontface = "bold"
  )
tmap_save(msgwr_p_plot, "plots/test2.png", device = png)

msgwr_sf <- msgwr_sf 


# Basic model ----
bw <- bw.gwr(
  formula,
  data = kreise_polarity_context_sp,
  approach = approach,
  kernel = kernel,
  adaptive = adaptive
)

gwr_model <- gwr.basic(
  formula_50,
  data = kreise_polarity_context_sp,
  bw = bw,
  kernel = kernel,
  adaptive = adaptive,
  cv = TRUE
)

# Mixed model ----
spatial_vars <- msbw %>%
  filter(model_3 < 200) %>%
  pull(var)
formula_mixed <- as.formula(paste("polarity ~", paste(spatial_vars, collapse = " + ")))
bw <- bw.gwr(
  formula_mixed,
  data = kreise_polarity_context_sp,
  approach = approach,
  kernel = kernel,
  adaptive = adaptive
)

gwr_mixed <- gwr.mixed.fixed(
  formula_50,
  data = kreise_polarity_context_sp,
  fixed.vars = setdiff(names(var_sel), spatial_vars),
  bw = bw,
  kernel = kernel,
  adaptive = adaptive
)
mixed_sf <- st_as_sf(gwr_mixed$SDF) %>%
  tidyr::pivot_longer(
    c(
      industriequote_L, kreative_klasse_L, erwerbstätige_primsek_L,
      stimmenanteile_afd_L, windkraft_pro_10000_L
    )
  ) %>%
  mutate(name = factor(str_remove(name, fixed("_L")), levels = names(var_sel))) %>%
  mutate(name = recode(name, !!!sel_eng)) %>%
  dplyr::select(name, value)

t <- gwr.mixed.bootstrap(
  formula,
  kreise_polarity_context_sp,
  fixed.vars = setdiff(names(var_sel), spatial_vars),
  kernel = kernel,
  approach = approach,
  adaptive = adaptive,
  R = 99
)$SDF %>%
  st_as_sf()

# Plot results of SGWR
# Figure 4.9
gwr_maps <- tm_shape(st_union(mixed_sf)) +
  tm_borders() +
  tm_shape(mixed_sf) +
  tm_fill(col = "value", palette = "PRGn", style = "order", midpoint = 0, title = "Coefficient") +
  tm_facets(by = "name", ncol = 3, free.scales.fill = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  #tm_scale_bar(text.size = 0.35, width = 0.2, position = c("RIGHT", "BOTTOM")) +
  tm_layout(
    frame = FALSE,
    asp = 0.8,
    outer.margins = c(-0.35, 0, -0.35, -0.15),
    panel.label.bg.color = NA,
    panel.label.fontface = "bold",
    frame.lwd = NA,
    legend.position = c(-0.8, 0.25),
    legend.width = 2,
    legend.height = 2
  )
tmap_save(gwr_maps, "plots/gwr_maps.pdf", device = cairo_pdf, height = 6)

gwr_maps_pp <- tm_shape(st_union(mixed_sf)) +
  tm_borders() +
  tm_shape(mixed_sf) +
  tm_fill(col = "value", palette = "PRGn", style = "order", midpoint = 0, title = "Coefficient") +
  tm_facets(by = "name", nrow = 1, free.scales.fill = FALSE) +
  tm_compass(position = c("RIGHT", "BOTTOM")) +
  tm_layout(
    outer.margins = c(-0.35, 0, -0.35, -0.22),
    frame = FALSE,
    asp = 0.8,
    panel.label.bg.color = NA,
    panel.label.fontface = "bold",
    frame.lwd = NA,
    legend.width = 2,
    legend.height = 2
  )
tmap_save(gwr_maps_pp, "plots/gwr_maps_pp.png", device = png)

# Plot results of MSGWR
gwr_maps <- plot_gwr(
  msgwr_model,
  col = c(
    "industriequote", "kreative_klasse", "erwerbstätige_primsek",
    "stimmenanteile_afd", "windkraft_pro_10000"
  ),
  style = "mennis4",
  scale = 0.7
)
tmap_save(gwr_maps, "plots/gwr_maps.pdf", device = cairo_pdf)


tweets_pred %>%
  dplyr::select(id, polarity, se) %>%
  st_write("data/tweet_ids.geojson")