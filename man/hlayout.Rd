\name{hlayout}
\alias{hlayout}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Layout framework for more flexibility when drawing heatmaps
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hlayout(main = NULL, topextra = NULL, top = list(), bottom = list(), left = list(), right = list(), topleft = NULL, topright = NULL, bottomleft = NULL, bottomright = NULL, bottomextra = NULL, ..., zoomx = NULL, zoomy = NULL, execute = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{topextra}{
%%     ~~Describe \code{topextra} here~~
}
  \item{top}{
%%     ~~Describe \code{top} here~~
}
  \item{bottom}{
%%     ~~Describe \code{bottom} here~~
}
  \item{left}{
%%     ~~Describe \code{left} here~~
}
  \item{right}{
%%     ~~Describe \code{right} here~~
}
  \item{topleft}{
%%     ~~Describe \code{topleft} here~~
}
  \item{topright}{
%%     ~~Describe \code{topright} here~~
}
  \item{bottomleft}{
%%     ~~Describe \code{bottomleft} here~~
}
  \item{bottomright}{
%%     ~~Describe \code{bottomright} here~~
}
  \item{bottomextra}{
%%     ~~Describe \code{bottomextra} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{zoomx}{
%%     ~~Describe \code{zoomx} here~~
}
  \item{zoomy}{
%%     ~~Describe \code{zoomy} here~~
}
  \item{execute}{
%%     ~~Describe \code{execute} here~~
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
function (main = NULL, topextra = NULL, top = list(), bottom = list(), 
    left = list(), right = list(), topleft = NULL, topright = NULL, 
    bottomleft = NULL, bottomright = NULL, bottomextra = NULL, 
    ..., zoomx = NULL, zoomy = NULL, execute = TRUE) 
{
    if (!is.null(zoomx)) 
        zoomx <- sort(zoomx)
    if (!is.null(zoomy)) 
        zoomy <- sort(zoomy)
    top <- lapply(top, function(x) {
        if (is.null(x)) 
            emptypart()
        else x
    })
    bottom <- lapply(bottom, function(x) {
        if (is.null(x)) 
            emptypart()
        else x
    })
    left <- lapply(left, function(x) {
        if (is.null(x)) 
            emptypart()
        else x
    })
    right <- lapply(right, function(x) {
        if (is.null(x)) 
            emptypart()
        else x
    })
    lmat <- matrix(1, 1, 1)
    if (length(top) > 0) 
        lmat <- rbind(matrix(length(top):1 + max(lmat), ncol = 1), 
            lmat)
    if (length(bottom) > 0) 
        lmat <- rbind(lmat, matrix(1:length(bottom) + max(lmat), 
            ncol = 1))
    heights <- c(lapply(rev(top), function(x) x$height), 1, lapply(bottom, 
        function(x) x$height))
    template <- c(rep(0, length(top)), 1, rep(0, length(bottom)))
    if (length(left) > 0) {
        tl <- if (!is.null(topleft)) 
            max(lmat) + length(left) + 1
        else 0
        bl <- if (!is.null(bottomleft)) 
            max(lmat) + length(left) + 1 + sign(tl)
        else 0
        tpl2 <- c(rep(tl, length(top)), 0, rep(bl, length(bottom)))
        lmat <- cbind(outer(template, length(left):1 + max(lmat), 
            "*") + outer(tpl2, rep(1, length(left)), "*"), lmat)
    }
    if (length(right) > 0) {
        tr <- if (!is.null(topright)) 
            max(lmat) + length(right) + 1
        else 0
        br <- if (!is.null(bottomright)) 
            max(lmat) + length(right) + 1 + sign(tr)
        else 0
        tpl2 <- c(rep(tr, length(top)), 0, rep(br, length(bottom)))
        lmat <- cbind(lmat, outer(template, 1:length(right) + 
            max(lmat), "*") + outer(tpl2, rep(1, length(right)), 
            "*"))
    }
    widths <- c(lapply(rev(left), function(x) x$width), 1, lapply(right, 
        function(x) x$width))
    if (!is.null(topextra)) {
        heights <- c(topextra$height, heights)
        lmat <- rbind(matrix(max(lmat) + 1, nrow = 1, ncol = ncol(lmat)), 
            lmat)
    }
    if (!is.null(bottomextra)) {
        heights <- c(heights, bottomextra$height)
        lmat <- rbind(lmat, matrix(max(lmat) + 1, nrow = 1, ncol = ncol(lmat)))
    }
    if (is.null(main)) {
        main <- list(FUN = function() {
            plot(0, type = "n", xaxt = "none", yaxt = "none", 
                bty = "n")
        })
    }
    lmat[which(lmat == 1)] <- max(lmat) + 1
    lmat <- ifelse(lmat > 0, lmat - 1, lmat)
    partlist <- c(top, bottom, left, list(topleft, bottomleft), 
        right, list(topright, bottomright, topextra, bottomextra, 
            main))
    partlist <- partlist[!sapply(partlist, is.null)]
    plotlist <- lapply(partlist, function(x) {
        g <- list(x$FUN, ...)
        if ("zoomx" \%in\% names(as.list(args(x$FUN)))) 
            g$zoomx <- zoomx
        if ("zoomy" \%in\% names(as.list(args(x$FUN)))) 
            g$zoomy <- zoomy
        mode(g) <- "call"
        g
    })
    if (execute) {
        par(mai = c(0.025, 0.025, 0.025, 0.025))
        heights <- c(heights, recursive = TRUE)
        widths <- c(widths, recursive = TRUE)
        lo <- layout(lmat, heights = heights, widths = widths)
        lapply(plotlist, eval)
    }
    else {
        invisible(list(lmat = lmat, heights = heights, widths = widths, 
            plotlist = plotlist))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
