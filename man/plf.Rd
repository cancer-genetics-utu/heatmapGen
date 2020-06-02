\name{plf}
\alias{plf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plotting of forests of multiple dendrograms, with spaces between dendrograms.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plf(forest, type = c("rectangle", "triangle"), center = FALSE, nodePar = NULL, edgePar = list(), leaflab = c("perpendicular", "textlike", "none"), dLeaf = NULL, xlab = "", ylab = "", xaxt = "n", yaxt = "s", horiz = FALSE, frame.plot = FALSE, xlim, spacer = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{forest}{
%%     ~~Describe \code{forest} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{center}{
%%     ~~Describe \code{center} here~~
}
  \item{nodePar}{
%%     ~~Describe \code{nodePar} here~~
}
  \item{edgePar}{
%%     ~~Describe \code{edgePar} here~~
}
  \item{leaflab}{
%%     ~~Describe \code{leaflab} here~~
}
  \item{dLeaf}{
%%     ~~Describe \code{dLeaf} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{xaxt}{
%%     ~~Describe \code{xaxt} here~~
}
  \item{yaxt}{
%%     ~~Describe \code{yaxt} here~~
}
  \item{horiz}{
%%     ~~Describe \code{horiz} here~~
}
  \item{frame.plot}{
%%     ~~Describe \code{frame.plot} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{spacer}{
%%     ~~Describe \code{spacer} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (forest, type = c("rectangle", "triangle"), center = FALSE, 
    nodePar = NULL, edgePar = list(), leaflab = c("perpendicular", 
        "textlike", "none"), dLeaf = NULL, xlab = "", ylab = "", 
    xaxt = "n", yaxt = "s", horiz = FALSE, frame.plot = FALSE, 
    xlim, spacer = 1, ...) 
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
        xlim <- c(1 - 0.5, sum(mem.x) + (length(mem.x) - 1) * 
            spacer + 0.5)
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
        x1 <- cumsum(c(0, mem.x))[i] + (i - 1) * spacer + 1
        x2 <- cumsum(mem.x)[i] + (i - 1) * spacer
        if (is.leaf(x)) {
            x0 <- stats:::plotNodeLimit(x1, x2, x, center)$x
            if (horiz) 
                segments(0, x0, 0.0625 * hgt, x0)
            else segments(x0, 0, x0, 0.0625 * hgt)
        }
        if (!is.null(et <- attr(x, "edgetext"))) {
            my <- mean(hgt, yTop) - hgt
            if (horiz) 
                text(my, x0, et)
            else text(x0, my, et)
        }
        stats:::plotNode(x1, x2, x, type = type, center = center, 
            leaflab = leaflab, dLeaf = dLeaf, nodePar = nodePar, 
            edgePar = edgePar, horiz = horiz)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
