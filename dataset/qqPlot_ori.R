function (x, distribution = "norm", groups, layout, ylim = range(x, 
                                                                 na.rm = TRUE), ylab = deparse(substitute(x)), xlab = paste(distribution, 
                                                                                                                            "quantiles"), glab = deparse(substitute(groups)), main = NULL, 
          las = par("las"), envelope = TRUE, col = carPalette()[1], 
          col.lines = carPalette()[2], lwd = 2, pch = 1, cex = par("cex"), 
          line = c("quartiles", "robust", "none"), id = TRUE, grid = TRUE, 
          ...) 
{
  if (!missing(groups)) {
    if (isTRUE(id)) 
      id <- list(n = 2)
    if (is.null(id$labels)) 
      id$labels <- seq(along = x)
    grps <- levels(as.factor(groups))
    if (missing(layout)) 
      layout <- mfrow(length(grps), max.plots = 12)
    if (prod(layout) < length(grps)) 
      stop("layout cannot accomodate ", length(grps), " plots")
    oldpar <- par(mfrow = layout)
    on.exit(par(oldpar))
    for (group in grps) {
      id.gr <- id
      if (!isFALSE(id)) 
        id.gr$labels <- id$labels[groups == group]
      qqPlot.default(x[groups == group], distribution = distribution, 
                     ylim = ylim, ylab = ylab, xlab = xlab, main = paste(glab, 
                                                                         "=", group), las = las, envelope = envelope, 
                     col = col, col.lines = col.lines, pch = pch, 
                     cex = cex, line = line, id = id.gr, grid = grid, 
                     ...)
    }
    return(invisible(NULL))
  }
  if (!is.list(envelope) && length(envelope == 1) && is.numeric(envelope)) {
    envelope <- list(level = envelope)
  }
  if (!isFALSE(envelope)) {
    envelope <- applyDefaults(envelope, defaults = list(level = 0.95, 
                                                        style = "filled", col = col.lines, alpha = 0.15, 
                                                        border = TRUE))
    style <- match.arg(envelope$style, c("filled", "lines", 
                                         "none"))
    col.envelope <- envelope$col
    conf <- envelope$level
    alpha <- envelope$alpha
    border <- envelope$border
    if (style == "none") 
      envelope <- FALSE
  }
  id <- applyDefaults(id, defaults = list(method = "y", n = 2, 
                                          cex = 1, col = carPalette()[1], location = "lr"), type = "id")
  if (isFALSE(id)) {
    id.n <- 0
    id.method <- "none"
    labels <- id.cex <- id.col <- id.location <- NULL
  }
  else {
    labels <- id$labels
    if (is.null(labels)) 
      labels <- if (!is.null(names(x))) 
        names(x)
    else seq(along = x)
    id.method <- id$method
    id.n <- if ("identify" %in% id.method) 
      Inf
    else id$n
    id.cex <- id$cex
    id.col <- id$col
    id.location <- id$location
  }
  line <- match.arg(line)
  index <- seq(along = x)
  good <- !is.na(x)
  ord <- order(x[good])
  if (length(col) == length(x)) 
    col <- col[good][ord]
  if (length(pch) == length(x)) 
    pch <- pch[good][ord]
  if (length(cex) == length(x)) 
    cex <- cex[good][ord]
  ord.x <- x[good][ord]
  ord.lab <- labels[good][ord]
  q.function <- eval(parse(text = paste("q", distribution, 
                                        sep = "")))
  d.function <- eval(parse(text = paste("d", distribution, 
                                        sep = "")))
  n <- length(ord.x)
  P <- ppoints(n)
  z <- q.function(P, ...)
  plot(z, ord.x, type = "n", xlab = xlab, ylab = ylab, main = main, 
       las = las, ylim = ylim)
  if (grid) {
    grid(lty = 1, equilogs = FALSE)
    box()
  }
  points(z, ord.x, col = col, pch = pch, cex = cex)
  if (line == "quartiles" || line == "none") {
    Q.x <- quantile(ord.x, c(0.25, 0.75))
    Q.z <- q.function(c(0.25, 0.75), ...)
    b <- (Q.x[2] - Q.x[1])/(Q.z[2] - Q.z[1])
    a <- Q.x[1] - b * Q.z[1]
    if (line == "quartiles") 
      abline(a, b, col = col.lines, lwd = lwd)
  }
  if (line == "robust") {
    coef <- coef(MASS::rlm(ord.x ~ z))
    a <- coef[1]
    b <- coef[2]
    abline(a, b, col = col.lines, lwd = lwd)
  }
  if (!isFALSE(envelope)) {
    zz <- qnorm(1 - (1 - conf)/2)
    SE <- (b/d.function(z, ...)) * sqrt(P * (1 - P)/n)
    fit.value <- a + b * z
    upper <- fit.value + zz * SE
    lower <- fit.value - zz * SE
    if (style == "filled") {
      envelope(z, z, lower, upper, col = col.envelope, 
               alpha = alpha, border = border)
    }
    else {
      lines(z, upper, lty = 2, lwd = lwd, col = col.lines)
      lines(z, lower, lty = 2, lwd = lwd, col = col.lines)
    }
  }
  extreme <- showLabels(z, ord.x, labels = ord.lab, method = id.method, 
                        n = id.n, cex = id.cex, col = id.col, location = id.location)
  if (is.numeric(extreme)) {
    nms <- names(extreme)
    extreme <- index[good][ord][extreme]
    if (!all(as.character(extreme) == nms)) 
      names(extreme) <- nms
  }
  if (length(extreme) > 0) 
    extreme
  else invisible(NULL)
}