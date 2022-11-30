source("R/packages.R")

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


plot_ci <- function(model, level = 0.95, cutoff = c(0.1, 0.05, 0.005), limits = c(-0.2, 0.2), bottom = TRUE, left = TRUE) {
  if (inherits(model, "lm")) {
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
  } else if (inherits(model, "lmerMod")) {
    summ <- broom.mixed::tidy(model, conf.int = TRUE, effects = "fixed")
    p_val <- parameters::p_value_betwithin(model)$p
    coef <- summ$estimate
    var_names <- sel_eng[summ$term]
    ci <- tibble(
      variable = factor(var_names, levels = var_names),
      coef = coef,
      lower = summ$conf.low,
      upper = summ$conf.high,
      sig = pvalue_stars(p = p_val)
    )
  }
  
  mar <- coef %>%
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
    labs(y = if (left) "Estimate" else NULL, x = NULL) +
    scale_y_continuous(limits = limits, labels = if (left) \(x) x else NULL) +
    scale_x_discrete(labels = if (bottom) \(x) x else NULL) +
    theme(
      axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}


plotREsim <- function(data, level = 0.95, stat = "median", sd = TRUE,
                      sigmaScale = NULL, oddsRatio = FALSE, labs = FALSE,
                      facet= TRUE){
  
  # error checking
  merTools:::plot_sim_error_chks(type= "RE", level= level, stat= stat, sd= sd, sigmaScale= sigmaScale,
                      oddsRatio= oddsRatio, labs= labs, facet= facet)
  # check for faceting
  facet_logical <- is.logical(facet)
  if (!facet_logical) {
    data <- data[data$groupFctr == facet[[1]] & data$term == facet[[2]], ]
  }
  
  if(!missing(sigmaScale)){
    data[, "sd"] <- data[, "sd"] / sigmaScale
    data[, stat] <- data[, stat] / sigmaScale
  }
  data[, "sd"] <- data[, "sd"] * qnorm(1-((1-level)/2))
  data[, "ymax"] <- data[, stat] + data[, "sd"]
  data[, "ymin"] <- data[, stat] - data[, "sd"]
  data[, "sig"] <- data[, "ymin"] > 0 | data[, "ymax"] < 0
  hlineInt <- 0
  if(oddsRatio == TRUE){
    data[, "ymax"] <- exp(data[, "ymax"])
    data[, stat] <- exp(data[, stat])
    data[, "ymin"] <- exp(data[, "ymin"])
    hlineInt <- 1
  }
  data <- data[order(data[,"groupFctr"], data[,"term"], data[,stat]),]
  rownames(data) <- 1:nrow(data)
  data[,"xvar"] <- factor(paste(data$groupFctr, data$groupID, sep=""),
                          levels=unique(paste(data$groupFctr,data$groupID, sep="")),
                          ordered=TRUE)
  if(labs == TRUE){
    xlabs.tmp <- element_text(face = "bold", angle=90, vjust=.5)
  } else {
    data[,"xvar"] <- as.numeric(data[,"xvar"])
    xlabs.tmp <- element_blank()
  }
  
  p <- ggplot(data, aes_string(x = "xvar", y = stat, ymax = "ymax", ymin = "ymin")) +
    geom_hline(yintercept = hlineInt, color = I("red"), size = I(0.5)) +
    geom_point(color="gray75", alpha=1/(nrow(data)^.33), size=I(0.5)) +
    geom_point(data=subset(data, sig==TRUE), size=I(0.5)) +
    scale_y_continuous(limits = c(-0.06, 0.06)) +
    labs(title = if (facet$term %in% names(sel_eng)) sel_eng[[facet$term]] else "Intercept", x = "", y = "") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = xlabs.tmp,
          axis.ticks.x = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))
  if (sd) {
    p <- p +
      geom_pointrange(alpha = 1/(nrow(data)^.33), size = 0.1) +
      geom_pointrange(data=subset(data, sig==TRUE), alpha = 0.25, size = 0.1)
  }
  # check facet
  if (facet_logical) {
    return(p + facet_grid(term ~ groupFctr, scales = "free_x"))
  } else {
    return(p)
  }
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
      form <- paste(var, "~", form)
    } else if (val["intercept"] == "fixed") {
      form <- paste(form, var, "+ ")
    } else if (val["intercept"] == "random") {
      if (val["slope"] == "random") {
        form <- paste0(form, var, " + (1 + ", var, " | ", grp, ") +")
      } else {
        form <- paste0(form, var, " + (1 | ", grp, ") +")
      }
    }
  }
  
  if (str_ends(form, fixed("+"))) {
    form <- str_sub(form, 1, nchar(form) - 1)
  }

  as.formula(form)
}
