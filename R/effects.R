source("R/packages.R")

# Takes a vector of estimates x and a vector of p-values p and appends
# significance stars based on a set of given cutoff values
pvalue_stars <- function(x, p, cutoff = c(0.1, 0.05, 0.01), left_align = FALSE, latex = FALSE) {
  if (left_align) {
    stars <- c("*  ", "** ", "***")
  } else {
    stars <- c("*", "**", "***")
  }
  if (latex) {
    stars <- paste0("$^{", stars, "}$")
  }
  if (!missing(x)) {
    dplyr::case_when(
      p < cutoff[3] ~ paste0(x, stars[3]),
      p < cutoff[2] ~ paste0(x, stars[2]),
      p < cutoff[1] ~ paste0(x, stars[1]),
      TRUE ~ as.character(x)
    )
  } else {
    dplyr::case_when(
      p < cutoff[3] ~ stars[3],
      p < cutoff[2] ~ stars[2],
      p < cutoff[1] ~ stars[1],
      TRUE ~ NA_character_
    )
  }
}


# S3 method for modelsummary to compute between-within p-values and
# cluster-robust standard errors for objects of class lmer
tidy_custom.lmerMod <- function(x, latex = TRUE, left_align = FALSE, ...) {
  vcov <- attr(x, "vcov")
  se <- standard_error(x, vcov = vcov, method = "betwithin")$SE
  ci <- ci_betwithin(x, vcov = vcov, ...)
  pval <- p_value_betwithin(x, vcov = vcov)$p
  broom.mixed::tidy(x, effect = "fixed", cont.int = TRUE) %>%
    mutate(
      std.error = se,
      p.value = pval,
      conf.low = ci[, "CI_low"],
      conf.high = ci[, "CI_high"],
      stars = pvalue_stars("", pval, latex = latex, left_align = left_align)
    )
}


# Creates a latex table containing summary statistics of the top `n` districts
district_summary <- function(data, kreise, n = 20) {
  which <- kreise %>%
    arrange(desc(authors)) %>%
    slice_head(n = n) %>%
    pull(place)
  df <- map_dfr(which, function(w) {
    sel <- st_within(data, kreise[kreise$place %in% w, ], sparse = FALSE)
    fil <- data %>%
      filter(sel) %>%
      dplyr::select(polarity, se) %>%
      st_drop_geometry()
    
    dsty <- density(fil$polarity, na.rm = TRUE)
    
    data.frame(
      District = w,
      N = nrow(fil),
      Mean = round(mean(fil$polarity, na.rm = TRUE), 2),
      SE = round(mean(fil$se, na.rm = TRUE), 2),
      SD = round(sd(fil$polarity, na.rm = TRUE), 2),
      Min = round(min(fil$polarity, na.rm = TRUE), 2),
      Median = round(median(fil$polarity, na.rm = TRUE), 2),
      Max = round(max(fil$polarity, na.rm = TRUE), 2),
      Distribution = ltxsparklines::sparkline(dsty$x, dsty$y)
    )
  })
  
  kbl(
    df,
    format = "latex",
    escape = FALSE,
    booktabs = TRUE,
    col.names = c(
      "District", "N", "Mean", "SE", "SD", "Min", "Median", "Max", "Distribution"
    ),
    caption = "Summary statistics for the 20 most prevalent districts. Standard errors refer to the statistical errors in document scaling.",
    label = "top_districts",
    caption.short = "Summary statistics for the 20 most prevalent districts"
  ) %>%
    kable_paper(full_width = TRUE) %>%
    str_remove_all("Städteregion|Landkreis|Region") %>%
    str_replace(fixed("\\centering"), "\\centering\n\\scriptsize") %>%
    str_replace(fixed("Frankfurt am Main"), "Frankf.a.M.")
}


# Function to plot regression estimates along with their confidence intervals
# and significance stars
plot_ci <- function(..., level = 0.95, cutoff = c(0.1, 0.05, 0.005), limits = NULL, bottom = TRUE, left = TRUE) {
  models <- list(...)
  
  ci <- map_dfr(seq_along(models), function(i) {
    m <- models[[i]]
    if (inherits(m, "lm")) {
      summ <- broom::tidy(m, conf.int = TRUE)
      var_names <- c("(Intercept)" = "(Intercept)", sel_eng)[summ$term]
      
      ci <- tibble(
        variable = factor(var_names, levels = rev(var_names)),
        coef = summ$estimate,
        lower = summ$conf.low,
        upper = summ$conf.high,
        sig = pvalue_stars(p = summ$p.value, left_align = TRUE)
      )
    } else if (inherits(m, "lmerMod")) {
      summ <- tidy_custom.lmerMod(m, level = level, latex = FALSE, left_align = TRUE)
      var_names <- c("(Intercept)" = "(Intercept)", sel_eng)[summ$term]
      ci <- tibble(
        variable = factor(var_names, levels = rev(var_names)),
        coef = summ$estimate,
        lower = summ$conf.low,
        upper = summ$conf.high,
        sig = summ$stars
      )
    }
    
    ci %>% mutate(model = paste("Model", i))
  })

  base_range <- if (is.null(limits)) {
    c(min(ci$lower), max(ci$upper))
  } else {
    limits
  }
  mar <- range(base_range[1], base_range[2]) %>%
    as.list() %>%
    do.call(subtract, .) %>%
    abs() %>%
    {. - (0.97 * .)}

  ggplot(ci, aes(x = variable)) +
    geom_point(aes(y = coef), size = 2) +
    geom_errorbar(aes(y = coef, ymin = lower, ymax = upper), width = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_text(aes(y = upper + mar, label = sig), na.rm = TRUE) +
    theme_bw() +
    labs(y = if (left) "Estimate" else NULL, x = NULL) +
    scale_y_continuous(labels = if (left) \(x) x else NULL) +
    scale_x_discrete(labels = if (bottom) \(x) x else NULL) +
    facet_wrap(~model, ncol = 2) +
    theme(
      panel.grid.major.x = element_line(linetype = "dashed"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = 14, face = "bold")
    ) +
    coord_flip(ylim = limits)
}


# Function to plot "caterpillar" plots or dotplots that illustrate the random
# effects of a mixed effect model
dotplot <- function(data,
                    group,
                    terms,
                    ylim = NULL,
                    level = 0.95,
                    stat = "median",
                    sd = TRUE,
                    plot_sig = FALSE) {
  if (!is.list(data)) data <- list(data)
  if (length(group) != length(data)) group <- as.list(rep(group, length(data)))
  if (!is.list(group)) group <- list(group)
  if (!is.list(terms)) terms <- list(terms)
  
  data <- lapply(seq_along(data), function(i) {
    da <- data[[i]]
    gr <- group[[i]]
    te <- terms[[i]]

    da <- da[da$term %in% te & da$groupFctr %in% gr, ]
    da[["sd"]] <- da[["sd"]] * qnorm(1 - ((1 - level) / 2))
    da[["ymax"]] <- da[[stat]] + da[["sd"]]
    da[["ymin"]] <- da[[stat]] - da[["sd"]]
    da[["sig"]] <- if (plot_sig) da[["ymax"]] < 0 | da[["ymin"]] > 0 else TRUE
    da[["groupID"]] <- factor(da[["groupID"]], levels = unique(da[["groupID"]]), ordered = TRUE)
    da[["term"]] <- factor(da[["term"]], levels = names(var_sel)) %>%
      dplyr::recode(!!!sel_eng)
    da[["stat"]] <- if (stat == "median") {
      da[["median"]]
    } else {
      da[["mean"]]
    }
    da$model <- paste("Model", i)
    da
  })

  data <- bind_rows(data) %>%
    arrange(groupID, model, term) %>%
    mutate(groupID = as.factor(groupID), term = as.factor(term), model = as.factor(model)) %>%
    mutate(groupID = reorder_within(groupID, stat, list(term, model)))

  ggplot(data, aes(x = groupID, y = stat, alpha = sig, group = 1)) +
    geom_hline(yintercept = 0, color = "red", lwd = 0.2, show.legend = FALSE) +
    geom_point(size = 0.2, color = "black", show.legend = FALSE) +
    geom_smooth(aes(y = ymax), color = "black", alpha = 0.3, lwd = 0.2, se = FALSE) +
    geom_smooth(aes(y = ymin), color = "black", alpha = 0.3, lwd = 0.2, se = FALSE) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.text.y = element_text(size = 8, face = "bold")) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_reordered(labels = labeller) +
    scale_alpha_manual(values = c("FALSE" = 1 / (nrow(data) ^ 0.33), "TRUE" = 1)) +
    facet_grid(rows = vars(term), cols = vars(model), switch = "y", scales = "free_x") +
    coord_cartesian(ylim = ylim) +
    labs(x = "Districts", y = "Coefficient")
}


# Auxiliary function for tidytext::reorder_within
labeller <- function(x, sep = "___") {
  x <- x[order(sapply(strsplit(x, sep), "[", 2))]
  reg <- paste0(sep, ".+$")
  gsub(reg, "", x)
}


# Constructs a mixed-model formula
# ... needs to be named combinations of variable names and effect type.
# Options are "fixed" and "random" for independent variables.
# The dependent variable must be passed with "dependent" instead
# A grouping variable has to be provided for multilevel models.
construct_mer_formula <- function(..., data, grp = NULL) {
  check_dots_used()
  dots <- list(...)
  if (is.null(names(dots)) || any(!nzchar(names(dots)))) {
    cli_abort("Dots must be named.")
  }
  form <- ""
  
  for (var in names(dots)) {
    val <- dots[[var]]
    
    if (!"intercept" %in% names(val)) {
      val <- c(val, intercept = "fixed")
    }
    
    if (!"slope" %in% names(val)) {
      val <- c(val, slope = "fixed")
    }
    
    if (!var %in% names(data)) {
      cli_abort("Variable {.var {var}} not in {.var {substitute(data)}}.")
    }
    
    if ("dependent" %in% val) {
      form <- paste0(var, " ~ ", form)
    } else if (val["intercept"] == "fixed") {
      form <- paste0(form, var, " + ")
      if (val["slope"] == "random") {
        form <- paste0(form, "(0 + ", var, "|", grp, ") + ")
      }
    } else if (val["intercept"] == "random") {
      if (val["slope"] == "random") {
        form <- paste0(form, var, " + (1 + ", var, " | ", grp, ") + ")
      } else {
        form <- paste0(form, var, " + (1 | ", grp, ") + ")
      }
    }
  }

  if (str_ends(form, "\\+\\s?")) {
    form <- str_sub(form, 1, nchar(form) - 2)
  }

  as.formula(form)
}


# Plots VIF values along with their confidence intervals
plot_vif <- function(..., colors = c("#3aaf85", "#1b6ca8", "#cd201f"), size_point = 4, size_line = 0.8) {
  dots <- list(...)
  dat <- map_dfr(seq_along(dots), function(i) {
    m <- dots[[i]]
    
    as.data.frame(check_collinearity(m)) %>%
      bind_cols(Component = paste("Model", i)) %>%
      mutate(Term = factor(Term, levels = rev(names(var_sel))) %>% dplyr::recode(!!!sel_eng))
  })
  ci_data <- TRUE
  dat$group <- "low"
  dat$group[dat$VIF >= 5 & dat$VIF < 10] <- "moderate"
  dat$group[dat$VIF >= 10] <- "high"
  dat <- datawizard::data_rename(
    dat,
    pattern = c("Term", "VIF", "SE_factor", "Component"),
    replacement = c("x", "y", "se", "facet")
  )
  
  if (insight::n_unique(dat$facet) <= 1) {
    dat$facet <- NULL
  }
  
  ylim <- ceiling(max(dat$y, na.rm = TRUE))
  xlim <- nrow(dat)
  
  if (ylim < 10) ylim <- 10
  
  dat$group <- factor(dat$group, levels = c("low", "moderate", "high"))
  levels(dat$group) <- c("Low (< 5)", "Moderate (< 10)", "High (≥ 10)")
  names(colors) <- c("Low (< 5)", "Moderate (< 10)", "High (≥ 10)")
  
  p <- ggplot2::ggplot(dat) + 
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      color = .data$group,
      ymin = .data$VIF_CI_low,
      ymax = .data$VIF_CI_high
    ) + 
    ggplot2::annotate(
      geom = "rect", xmin = -Inf, xmax = Inf, 
      ymin = 1, ymax = 5, fill = colors[1], color = NA, 
      alpha = 0.15) +
    ggplot2::annotate(
      geom = "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = 5,
      ymax = 10,
      fill = colors[2],
      color = NA,
      alpha = 0.15
    ) +
    ggplot2::annotate(
      geom = "rect", 
      xmin = -Inf, xmax = Inf, ymin = 10, ymax = Inf, fill = colors[3], 
      color = NA, alpha = 0.15) +
    {
      if (!is.null(ci_data)) {
        list(
          ggplot2::geom_linerange(lwd = size_line),
          ggplot2::geom_segment(
            data = dat[dat$VIF_CI_high >  ylim * 1.15, ],
            mapping = aes(
              x = .data$x,
              xend = .data$x,
              y = .data$y,
              yend = .data$VIF_CI_high
            ),
            lineend = "round", 
            linejoin = "round",
            arrow = ggplot2::arrow(
              ends = "last",
              type = "closed",
              angle = 20,
              length = ggplot2::unit(0.03, "native")
            ),
          show.legend = FALSE)
        )
      }
    } +
    see::geom_point2(size = size_point) +
    ggplot2::labs(x = NULL, y = "VIF") +
    ggplot2::scale_color_manual(
      values = colors,
      aesthetics = c("color", "fill"),
      guide = ggplot2::guide_legend(title = NULL),
      drop = FALSE
    ) +
    theme_bw(base_size = 10) + 
    ggplot2::scale_y_continuous(
      limits = c(1, ylim * 1.15), 
      oob = scales::oob_squish, trans = "log10", expand = c(0, 0),
      breaks = scales::log_breaks(n = 7, base = 10)
    ) + 
    ggplot2::scale_x_discrete() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(-5, -5, -5, -5),
      strip.background = element_blank(),
      strip.text = element_text(size = 14, face = "bold")
    )
  if ("facet" %in% colnames(dat)) {
    p <- p + ggplot2::facet_wrap(~facet, ncol = 2, scales = "fixed")
  }
  p + coord_flip()
}


# Plots residual QQ plots for a given model
plot_qq <- function(...,
                    alpha_level = 0.2,
                    size_point = 2,
                    size_line = 0.8,
                    dot_alpha_level = 0.8,
                    detrend = FALSE,
                    colors = c("black", "#1b6ca8"),
                    theme_style = theme_bw) {
  dots <- list(...)
  
  x <- map_dfr(seq_along(dots), function(i) {
    dat <- dots[[i]]
    resid <- sort(residuals(dat))
    data.frame(res = resid, model = paste("Model", i))
  })

  qq_stuff <- list(
    qqplotr::stat_qq_band(alpha = alpha_level, detrend = detrend),
    ggrastr::rasterise(
      qqplotr::stat_qq_point(
        shape = 16,
        stroke = 0,
        size = size_point,
        colour = colors[2],
        alpha = dot_alpha_level,
        detrend = detrend
      ), dpi = 1000
    ),
    qqplotr::stat_qq_line(linewidth = size_line, colour = colors[1], detrend = detrend)
  )
  
  ggplot2::ggplot(x, aes(sample = res)) +
    qq_stuff + 
    ggplot2::labs(y = "Sample quantiles", x = "Standard normal distribution quantiles") +
    facet_wrap(~model, ncol = 2) +
    theme_style(base_size = 10) +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12, face = "bold"))
}
