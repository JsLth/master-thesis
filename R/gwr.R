source("~/Masterarbeit/R/packages.R")

# Tests models with different calibration technique and kernel functions and
# determines the best specifications based on the model with the lowest diagnostic
# of choice
model_selection <- function(formula,
                            data = list(),
                            diagnostic = c("AIC", "AICc", "RSS.gw", "gw.R2", "BIC"),
                            table_diagnostic = c("all", diagnostic)) {
  stopifnot(inherits(data, "sf"))
  include_all <- FALSE
  table_diagnostic <- match.arg(table_diagnostic)
  if (table_diagnostic == "all") {
    table_diagnostic <- diagnostic
    include_all <- TRUE
  }
  diagnostic <- match.arg(diagnostic)
  modelselect <- list()
  adaptive = c(FALSE, TRUE)
  approaches = c("CV", "AIC")
  kernels = c("gaussian", "exponential", "bisquare", "tricube", "boxcar")
  
  data_sp <- as_Spatial(data)
  dMat <- GWmodel::gw.dist(dp.locat = coords, rp.locat = coords)
  
  cli_progress_bar(
    format = "Testing {pb_extra$tested} {pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
    total = 24,
    extra = list(tested = "")
  )
  
  for (i in seq_along(adaptive)) {
    adaptname <- ifelse(adaptive[i], "adaptive", "fixed")
    modelselect[[adaptname]] <- list()
    
    for (j in seq_along(approaches)) {
      approach <- approaches[j]
      
      for (a in seq_along(kernels)) {
        kernel <- kernels[a]
        cli_progress_update(
          extra = list(tested = sprintf("%s, %s and %s", kernel, approach, adaptname))
        )
        suppressWarnings({
          capture.output(bw <- GWmodel::bw.gwr(
            formula,
            data_sp,
            kernel = kernel,
            approach = approach,
            adaptive = adaptive[i]
          ))
          
          capture.output(
            modelselect[[adaptname]][[approach]][[kernel]] <- gwr.basic(
              formula,
              data = data_sp,
              bw = bw,
              kernel = kernel,
              adaptive = adaptive[i]
            )$GW.diagnostic[c(table_diagnostic)]
          )
        })
      }
    }
  }
  
  if (!include_all) {
    modelselect_df <- do.call(Map, c(f = rbind.data.frame, modelselect)) %>%
      bind_rows() %>%
      set_rownames(NULL) %>%
      bind_cols(approach = c("AIC", "AIC", "CV", "CV"), kernel = rep(names(modelselect), 2), .) %>%
      set_colnames(c("Approach", "Kernel", "Gaussian", "Exponential", "Bi-square", "Tri-cube", "Boxcar"))
    
    kab <- kableExtra::kbl(modelselect_df, booktabs = TRUE, align = "c", linesep = "", format = "latex") %>%
      kableExtra::collapse_rows(1:3, row_group_label_position = "stack", valign = "top") %>%
      kableExtra::add_header_above(c(" " = 2, "Kernel functions" = 5))
    
    opt_fun <- modelselect_df %>%
      dplyr::select(-Approach, -Kernel) %>%
      map_dbl(min) %>%
      which.min() %>%
      names()
    opt_row <- modelselect_df[which.min(modelselect_df[[opt_fun]]), ]
    
    solution <- c(
      func = opt_fun,
      type = opt_row$Kernel,
      approach = opt_row$Approach
    )
  } else {
    exp_specs <- expand.grid(
      Kernel = c("fixed", "adaptive"),
      Function = str_to_title(kernels),
      Approach = str_to_title(approaches)
    ) %>%
      rev()
    modelselect_df <- modelselect %>%
      purrr::transpose() %>%
      map(~do.call(Map, c(f = rbind.data.frame, .x))) %>%
      purrr::transpose() %>%
      c(f = rbind, .) %>%
      do.call(Map, .) %>%
      bind_rows() %>%
      bind_cols(exp_specs, .) %>%
      set_rownames(NULL)
    
    kab <- kableExtra::kbl(modelselect_df, booktabs = TRUE, align = "c", linesep = "", format = "latex") %>%
      kableExtra::collapse_rows(1:3, row_group_label_position = "stack", valign = "top")
    
    opt_row <- modelselect_df %>%
      select(Approach, Function, Kernel, all_of(diagnostic)) %>%
      slice_min(.[[diagnostic]])
    
    solution <- c(
      opt_row$Function,
      opt_row$Kernel,
      opt_row$Approach
    ) %>%
      as.character() %>%
      set_names("func", "type", "approach")
  }
  
  list(
    solution = solution,
    df = modelselect_df,
    table = kab
  )
}


variable_selection <- function(dep, indep, data, ...) {
  coords <- data %>%
    st_geometry() %>%
    st_centroid() %>%
    st_coordinates()
  data <- as_Spatial(data)
  dMat = GWmodel::gw.dist(coords)
  capture.output(
    sel <- GWmodel::gwr.model.selection(dep, indep, data = data, dMat = dMat, ...)
  )
  sel <- GWmodel::gwr.model.sort(sel, length(indep), sel[[2]][, 2])
  list(
    formula = sel[[1]][length(sel[[1]])],
    specs = sel[[2]]
  )
}


mirror_vector <- function(vec, negate = FALSE) {
  mir <- if (negate) rev(vec * -1) else rev(vec)
  c(mir, vec)
}


plot_kernels <- function(d = 1:3000, bw = 1000) {
  global <- rep(1, length(d)) %>%
    mirror_vector()
  gauss <- spgwr::gwr.Gauss(d^2, bw) %>%
    mirror_vector()
  exp <- exp(-d / bw) %>%
    mirror_vector()
  bisquare <- spgwr::gwr.bisquare(d^2, bw) %>%
    mirror_vector()
  tricube <- spgwr::gwr.tricube(d^2, bw) %>%
    mirror_vector()
  boxcar <- ifelse(d > bw, 0, 1) %>%
    mirror_vector()
  d <- d %>%
    mirror_vector(negate = TRUE)

  .data <- data.frame(
    dist = d,
    global = global,
    gauss = gauss,
    exp = exp,
    bisquare = bisquare,
    tricube = tricube,
    boxcar = boxcar
  )
  
  global <- ggplot(.data, aes(x = dist, y = global)) +
    geom_line(size = 1.5) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_continuous(expand = c(0, 0), breaks = c(-2000, -1000, 0, 1000, 2000)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(y = NULL, x = NULL) +
    ggtitle("Global") +
    theme(text = element_text(family = "CMU Serif"))
  gauss <- ggplot(.data, aes(x = dist, y = gauss)) +
    geom_line(size = 1.5) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_continuous(expand = c(0, 0), breaks = c(-2000, -1000, 0, 1000, 2000)) +
    labs(y = NULL, x = NULL) +
    ggtitle("Gaussian") +
    theme(text = element_text(family = "CMU Serif"))
  exp <- ggplot(.data, aes(x = dist, y = exp)) +
    geom_line(size = 1.5) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_continuous(expand = c(0, 0), breaks = c(-2000, -1000, 0, 1000, 2000)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(y = NULL, x = NULL) +
    ggtitle("Exponential") +
    theme(text = element_text(family = "CMU Serif"))
  bisquare <- ggplot(.data, aes(x = dist, y = bisquare)) +
    geom_line(size = 1.5) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_continuous(expand = c(0, 0), breaks = c(-2000, -1000, 0, 1000, 2000)) +
    labs(y = NULL, x = NULL) +
    ggtitle("Bi-square") +
    theme(text = element_text(family = "CMU Serif"))
  tricube <- ggplot(.data, aes(x = dist, y = tricube)) +
    geom_line(size = 1.5) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_continuous(expand = c(0, 0), breaks = c(-2000, -1000, 0, 1000, 2000)) +
    labs(y = NULL, x = NULL) +
    ggtitle("Tri-cube") +
    theme(text = element_text(family = "CMU Serif"))
  boxcar <- ggplot(.data, aes(x = dist, y = boxcar)) +
    geom_line(size = 1.5) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_continuous(expand = c(0, 0), breaks = c(-2000, -1000, 0, 1000, 2000)) +
    labs(y = NULL, x = NULL) +
    ggtitle("Boxcar") +
    theme(text = element_text(family = "CMU Serif"))
  grid.arrange(global, gauss, exp, bisquare, tricube, boxcar,
               bottom = ggpubr::text_grob("Distance", family = "CMU Serif"),
               left = ggpubr::text_grob("Weight", family = "CMU Serif", rot = 90),
               ncol = 2)
}


gwr.mixed.fixed <- function(formula, data, regression.points, fixed.vars,intercept.fixed=FALSE, bw, diagnostic=T,
                      kernel="bisquare", adaptive=FALSE, p=2, theta=0, longlat=F,dMat, dMat.rp)
{
  ##Record the start time
  timings <- list()
  timings[["start"]] <- Sys.time()
  ###################################macth the variables
  this.call <- match.call()
  p4s <- as.character(NA)
  if (diagnostic) 
    hatmatrix <- T
  else 
    hatmatrix <- F
  #####Check the given data frame and regression points
  #####Regression points
  if (missing(regression.points))
  {
    rp.given <- FALSE
    regression.points <- data
    rp.locat<-coordinates(data)
  }
  else
  {
    rp.given <- TRUE
    if (is(regression.points, "Spatial"))
    {
      rp.locat<-coordinates(regression.points)
    }
    else if (is.numeric(regression.points) && dim(regression.points)[2] == 2)
      rp.locat<-regression.points
    else
    {
      warning("Output loactions are not packed in a Spatial object,and it has to be a two-column numeric vector")
      rp.locat<-dp.locat
    }
  }
  ##Data points{
  if (is(data, "Spatial"))
  {
    p4s <- proj4string(data)
    dp.locat<-coordinates(data)
    data <- as(data, "data.frame")
  }
  else
  {
    stop("Given regression data must be Spatial*DataFrame")
  }
  #########Distance matrix is given or not
  dp.n <- nrow(dp.locat)
  rp.n <- nrow(rp.locat)
  if (missing(dMat))
  {
    DM.given<-F
    DM1.given<-F
    dMat <- gw.dist(dp.locat=dp.locat, rp.locat=dp.locat, p=p, theta=theta, longlat=longlat)
    dMat.rp <- gw.dist(dp.locat=dp.locat, rp.locat=rp.locat, p=p, theta=theta, longlat=longlat)
  }
  else
  {
    DM.given<-T
    dim.dMat<-dim(dMat)
    if (dim.dMat[1]!=dp.n||dim.dMat[2]!=dp.n)
      stop("Dimensions of dMat are not correct")
    if (missing(dMat.rp)) {
      dMat.rp <- dMat
    }
    else
    {
      dim.dMat.rp <- dim(dMat.rp)
      if (dim.dMat.rp[1]!=dp.n||dim.dMat.rp[2]!=rp.n)
        stop("Dimensions of dMat.rp are not correct")
    }
    DM1.given<-T 
  }
  ####################
  ######Extract the data frame
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0L)
  
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  y <- model.extract(mf, "response")
  x <- model.matrix(mt, mf)
  idx1 <- match("(Intercept)", colnames(x))
  if(!is.na(idx1))
    colnames(x)[idx1]<-"Intercept" 
  #colnames(x)[1]<-"Intercept"
  if (missing(fixed.vars))
  {
    warning("No independent variables in the formula is specified as fixed terms!")
    if(!intercept.fixed)
      stop("Please use basic GWR function to calibrate this model")
  }
  else
  {
    if(intercept.fixed)
      fixed.vars <- c("Intercept", fixed.vars)
  }
  idx.fixed <- match(fixed.vars, colnames(x))
  x1 <- x[, -idx.fixed]
  x2<- x[, idx.fixed]
  if (!is.null(x1)) x1 <- as.matrix(x1, nrow = dp.n)
  if (!is.null(x2)) x2 <- as.matrix(x2, nrow = dp.n)
  colnames(x1) <- colnames(x)[-idx.fixed]
  colnames(x2) <- colnames(x)[idx.fixed]
  #y <- as.matrix(y, nrow = dp.n)
  model <- gwr.mixed.2.fast(x1, x2, y, adaptive=adaptive, bw=bw,
                            kernel=kernel, dMat=dMat, dMat.rp=dMat.rp)                     
  res <- list()
  res$local <- model$local 
  res$global <- as.matrix(apply(model$global,2,mean,na.rm=T), 1, length(idx.fixed))
  colnames(res$local) <- colnames(x1)
  rownames(res$global) <- colnames(x2)
  mgwr.df <- data.frame(model$local, model$global)
  colnames(mgwr.df) <- c(paste(colnames(x1), "L", sep="_"), paste(colnames(x2), "F", sep="_"))
  rownames(rp.locat)<-rownames(mgwr.df)
  griddedObj <- F
  if (is(regression.points, "Spatial"))
  { 
    if (is(regression.points, "SpatialPolygonsDataFrame"))
    {
      polygons<-polygons(regression.points)
      #SpatialPolygons(regression.points)
      #         #rownames(mgwr.df) <- sapply(slot(polygons, "polygons"),
      #                            #  function(i) slot(i, "ID"))
      SDF <-SpatialPolygonsDataFrame(Sr=polygons, data=mgwr.df, match.ID=F)
    }
    else
    {
      griddedObj <- gridded(regression.points)
      SDF <- SpatialPointsDataFrame(coords=rp.locat, data=mgwr.df, proj4string=CRS(p4s), match.ID=F)
      gridded(SDF) <- griddedObj 
    }
  }
  else
    SDF <- SpatialPointsDataFrame(coords=rp.locat, data=mgwr.df, proj4string=CRS(p4s), match.ID=F)
  # 
  #   if (is(regression.points, "SpatialPolygonsDataFrame"))
  #    {
  #       polygons<-polygons(regression.points)
  #       #SpatialPolygons(regression.points)
  #       rownames(mgwr.df) <- sapply(slot(polygons, "polygons"),
  #                          function(i) slot(i, "ID"))
  #       SDF <-SpatialPolygonsDataFrame(Sr=polygons, data=mgwr.df)
  #    }
  #    else
  #       SDF <- SpatialPointsDataFrame(coords=rp.locat, data=mgwr.df, proj4string=CRS(p4s), match.ID=F)
  
  res$SDF <- SDF
  if (hatmatrix)
  {
    gwr.fitted <- function(x,b) apply(x*b,1,sum)
    edf <- gwr.mixed.trace.fast(x1, x2, y, adaptive=adaptive, bw=bw,
                                kernel=kernel, dMat=dMat)
    model2 <-gwr.mixed.2.fast(x1, x2, y, adaptive=adaptive, bw=bw, 
                              kernel=kernel, dMat=dMat, dMat.rp=dMat)
    #r.ss <- rss(y, cbind(x1,x2), cbind(model2$local, model2$global)) 
    r.ss <- sum((y - gw_fitted(model2$global, x2) - gw_fitted(model2$local,x1))^2)
    n1 <- length(y)
    sigma.aic <- r.ss / n1
    aic <- log(sigma.aic*2*pi) + 1 + 2*(edf + 1)/(n1 - edf - 2)
    aic <- n1*aic
    res$aic <- aic
    res$bic <- n1*log(sigma.aic) + n1*log(2*pi) + edf * log(n1)
    res$df.used <- edf
    res$r.ss <- r.ss
  }
  GW.arguments<-list(formula=formula,rp.given=rp.given,hatmatrix=hatmatrix,bw=bw, 
                     kernel=kernel,adaptive=adaptive, p=p, theta=theta, longlat=longlat,
                     DM.given=DM1.given,diagnostic=diagnostic)
  res$GW.arguments <- GW.arguments
  res$this.call <- this.call
  timings[["stop"]] <- Sys.time()
  res$timings <- timings
  class(res) <- "mgwr"
  res
}


R.utils::reassignInPackage("gwr.mixed", "GWmodel", gwr.mixed.fixed)
