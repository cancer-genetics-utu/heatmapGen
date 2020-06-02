# as.dendrogram.dendrogram is not yet present in R <= 2.5.0, not sure about 2.5.1
as.dendrogram.dendrogram <- function(x, ...) x


# modified stats:::plot.dendrogram (based on R version 2.4.0)
# !HS! Could not base this on e.g. cutplot.dendrogram (gplots?) because of bugs for horiz=TRUE
# plots a "forest" - a list of trees, with spaces of 1 in between
# simplified some features - center (simplified) and edge.root (automatic, modified)
# spacer is the gap between subgroups (trees)
# !HS! labeling the subclusters?
# Example:
# plf(stats:::cut.dendrogram(as.dendrogram(hclust(dist(USArrests))), h=150)$lower)
plf <- function (forest, type = c("rectangle", "triangle"), center = FALSE,
    nodePar = NULL, edgePar = list(), leaflab = c("perpendicular",
        "textlike", "none"), dLeaf = NULL, xlab = "", ylab = "",
    xaxt = "n", yaxt = "s", horiz = FALSE, frame.plot = FALSE,
    xlim, spacer=1, ...)
{
    type <- match.arg(type)
    leaflab <- match.arg(leaflab)
    if (inherits(forest, "dendrogram") || inherits(forest, "hclust")) {
        forest <- list(forest)
    }
	forest <- lapply(forest, as.dendrogram)
    hgt <- max(sapply(forest, function(x) attr(x, "height")))
    mem.x <- sapply(forest, function(x) stats:::.memberDend(x))
    yTop <- hgt * 1.0625
    if (missing(xlim) || is.null(xlim)) {
        xlim <- c(1 - 0.5, sum(mem.x) + (length(mem.x) - 1)*spacer + 0.5)
    }
    ylim <- c(0, yTop)
    if (horiz) {
        xl <- xlim
        xlim <- rev(ylim)
        ylim <- xl
        tmp <- xaxt
        xaxt <- yaxt
        yaxt <- tmp
    }
    plot(0, xlim = xlim, ylim = ylim, type = "n", xlab = xlab,
        ylab = ylab, xaxt = xaxt, yaxt = yaxt, frame.plot = frame.plot,
        ...)
    if (is.null(dLeaf))
        dLeaf <- 0.75 * (if (horiz)
            strwidth("w")
        else strheight("x"))
    for (i in 1:length(forest)) {
        x <- forest[[i]]
        # !HS! center => adjustment needed??
        x1 <- cumsum(c(0, mem.x))[i] + (i-1)*spacer + 1
        x2 <- cumsum(mem.x)[i] + (i-1)*spacer
        if (is.leaf(x)) {
            x0 <- stats:::plotNodeLimit(x1, x2, x, center)$x
            if (horiz) segments(0, x0, 0.0625*hgt, x0) else segments(x0, 0, x0, 0.0625 * hgt)
        }
        if (!is.null(et <- attr(x, "edgetext"))) {
            my <- mean(hgt, yTop) - hgt
            if (horiz) text(my, x0, et) else text(x0, my, et)
        }
        stats:::plotNode(x1, x2, x, type = type, center = center, leaflab = leaflab,
            dLeaf = dLeaf, nodePar = nodePar, edgePar = edgePar,
            horiz = horiz)
    }
}
