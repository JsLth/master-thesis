source("~/Masterarbeit/R/packages.R")

moran_map <- function(moran,
                      regions,
                      type = c("abel", "geoda"),
                      significance = 0.05,
                      plot_only_significant = TRUE) {
  type <- match.arg(type)
  quad <- attr(moran, "quadr")$mean
  pval <- moran[, 5]
  n <- nrow(moran)
  is_sig <- pval < significance
  labels <- c("High-High", "Low-Low", "Low-High", "High-Low")
  pal <- c("red", "blue", "lightblue", "pink")
  pal0 <- c(pal, "white")
  
  if (type == "abel") {
    colors <- vapply(quad, function(q) {
      if (q == "High-High") pal[1]
      else if (q == "Low-Low") pal[2]
      else if (q == "Low-High") pal[3]
      else if (q == "High-Low") pal[4]
    }, FUN.VALUE = character(1))
    
    is_sig_fct <- as.numeric(as.factor(quad)) * is_sig
    colors1 <- colors
    
    for (i in seq(1, n)) {
      if (!is.na(is_sig_fct[i])) {
        if (!is_sig_fct[i]) colors1[i] <- "white"
      }
    }
    
    pt <- c(st_coordinates(st_centroid(st_union(regions))))
    sig_guide <- data.frame(xmin = pt, xmax = pt, ymin = pt, ymax = pt, fill = c("A","B"))
    
    ggplot(data = regions) +
      geom_sf() +
      geom_sf(data = regions[1:2, ], aes(fill = c("A", "B"))) +
      scale_fill_manual(
        values = c("grey", "white"),
        labels = c("Significant", "Not significant"),
        name = sprintf("Significance (p < %s)", significance)
      ) +
      ggnewscale::new_scale_fill() +
      geom_sf(aes(fill = colors1), show.legend = FALSE) +
      scale_fill_manual(values = set_names(pal0, pal0)) +
      geom_sf(aes(color = colors), fill = NA, lwd = 0.5) +
      scale_color_manual(values = pal, labels = labels, name = "Local Moran's I") +
      xlab("Longitude") +
      ylab("Latitude") +
      ggtitle("Geographical clustering") +
      theme(panel.grid.major = element_line(color = gray(.8), linetype = "dashed", size = 0.5), 
            panel.background = element_rect(fill = "white"))
  } else {
    levels(quad) <- c(levels(quad), "Not significant")
    quad[!is_sig] <- "Not significant"
    labels <- c(labels, "Not significant")
    
    tm_shape(cbind(regions, lisa = quad)) +
      tm_fill("lisa", labels = labels, palette = pal0, type = "cat") +
      tm_borders()
  }

    
}
