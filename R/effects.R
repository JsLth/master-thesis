source("R/packages.R")

pvalue_stars <- function(x, p, cutoff = c(0.1, 0.05, 0.005), left_align = FALSE) {
  if (left_align) {
    stars <- c("*  ", "** ", "***")
  } else {
    stars <- c("*", "**", "***")
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
    str_remove_all("Städteregion|Landkreis")
}


plot_ci <- function(..., level = 0.95, cutoff = c(0.1, 0.05, 0.005), limits = c(-0.2, 0.2), bottom = TRUE, left = TRUE) {
  models <- list(...)
  
  ci <- map_dfr(seq_along(models), function(i) {
    m <- models[[i]]
    if (inherits(m, "lm")) {
      summ <- summary(m)
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
        sig = pvalue_stars(p = summ$coefficients[, "Pr(>|t|)"], left_align = TRUE)
      )
    } else if (inherits(m, "lmerMod")) {
      summ <- broom.mixed::tidy(m, conf.int = TRUE, effects = "fixed")
      p_val <- parameters::p_value_betwithin(m)$p
      coef <- summ$estimate
      var_names <- c("(Intercept)" = "(Intercept)", sel_eng)[summ$term]
      ci <- tibble(
        variable = factor(var_names, levels = rev(var_names)),
        coef = coef,
        lower = summ$conf.low,
        upper = summ$conf.high,
        sig = pvalue_stars(p = p_val, left_align = TRUE)
      )
    }
    
    ci %>% mutate(model = paste("Model", i))
  })

  
  mar <- limits %>%
    as.list() %>%
    do.call(subtract, .) %>%
    abs() %>%
    {. - (0.97 * .)}
  
  ggplot(ci, aes(x = variable)) +
    geom_point(aes(y = coef), size = 2) +
    geom_errorbar(aes(y = coef, ymin = lower, ymax = upper), width = 0.25) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_text(aes(y = upper + mar, label = sig), na.rm = TRUE) +
    theme_bw() +
    labs(y = if (left) "Estimate" else NULL, x = NULL) +
    scale_y_continuous(limits = limits, labels = if (left) \(x) x else NULL) +
    scale_x_discrete(labels = if (bottom) \(x) x else NULL) +
    facet_wrap(~model, ncol = 2) +
    theme(
      panel.grid.major.x = element_line(linetype = "dashed"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = 14, face = "bold")
    ) +
    coord_flip()
}


plot_effectsize <- function(...) {
  models <- list(...)
  data <- map_dfr(seq_along(models), function(i) {
    m <- models[[i]]
    effectsize(m, method = "posthoc", type = "epsilon") %>%
      as_tibble() %>%
      filter(!Parameter %in% "(Intercept)") %>%
      mutate(Parameter = sel_eng[Parameter], i = paste("Model", i)) %>%
      mutate(Parameter = factor(Parameter, levels = rev(unique(Parameter))))
  })
  
  ggplot(data, aes(x = Parameter, y = Std_Coefficient)) +
    geom_point() +
    geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.25) +
    coord_flip() +
    facet_wrap(~i, ncol = 2) +
    scale_y_continuous(breaks = c(-0.06, -0.01, 0.01, 0.06, 0.16)) +
    theme_bw() +
    labs(x = NULL, y = )
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(linetype = "dashed", color = "gray70"),
      panel.grid.minor = element_blank(),
      strip.background = element_blank()
    )
}


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
          strip.text.y = element_text(size = 7)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_reordered(labels = labeller) +
    scale_alpha_manual(values = c("FALSE" = 1 / (nrow(data) ^ 0.33), "TRUE" = 1)) +
    facet_grid(rows = vars(term), cols = vars(model), switch = "y", scales = "free_x") +
    coord_cartesian(ylim = ylim) +
    labs(x = "Districts", y = "Estimate")
}

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
