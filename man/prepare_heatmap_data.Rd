\name{prepare_heatmap_data}
\alias{prepare_heatmap_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Prepare heatmap data for plotting
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
prepare_heatmap_data(x, labRow = NA, labCol = NA, Rowv = NULL, Colv = NULL, reorder = c(TRUE, TRUE), distfun = dist, hclustfun = hclust, rowMembers = NULL, colMembers = NULL, spacer = 1, scale = "none", trim = NULL, zlim = NULL, col = NULL, filter = c(TRUE, TRUE))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{labRow}{
%%     ~~Describe \code{labRow} here~~
}
  \item{labCol}{
%%     ~~Describe \code{labCol} here~~
}
  \item{Rowv}{
%%     ~~Describe \code{Rowv} here~~
}
  \item{Colv}{
%%     ~~Describe \code{Colv} here~~
}
  \item{reorder}{
%%     ~~Describe \code{reorder} here~~
}
  \item{distfun}{
%%     ~~Describe \code{distfun} here~~
}
  \item{hclustfun}{
%%     ~~Describe \code{hclustfun} here~~
}
  \item{rowMembers}{
%%     ~~Describe \code{rowMembers} here~~
}
  \item{colMembers}{
%%     ~~Describe \code{colMembers} here~~
}
  \item{spacer}{
%%     ~~Describe \code{spacer} here~~
}
  \item{scale}{
%%     ~~Describe \code{scale} here~~
}
  \item{trim}{
%%     ~~Describe \code{trim} here~~
}
  \item{zlim}{
%%     ~~Describe \code{zlim} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{filter}{
%%     ~~Describe \code{filter} here~~
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
function (x, labRow = NA, labCol = NA, Rowv = NULL, Colv = NULL, 
    reorder = c(TRUE, TRUE), distfun = dist, hclustfun = hclust, 
    rowMembers = NULL, colMembers = NULL, spacer = 1, scale = "none", 
    trim = NULL, zlim = NULL, col = NULL, filter = c(TRUE, TRUE)) 
{
    if (length(di <- dim(x)) != 2 || !is.numeric(x)) 
        stop("`x' must be a numeric matrix")
    nr <- di[1]
    nc <- di[2]
    if ((nr <= 1 || nc <= 1) && (reorder[1] || reorder[2])) 
        stop("`x' must have at least 2 rows and 2 columns")
    col <- getPalette(col)
    res <- hm_reorder(x, labRow = labRow, labCol = labCol, Rowv = Rowv, 
        Colv = Colv, reorder = reorder, distfun = distfun, hclustfun = hclustfun, 
        rowMembers = rowMembers, colMembers = colMembers, spacer = spacer, 
        filter = filter)
    res$x <- x <- hm_normalize(res$x, scale = scale, trim = trim)
    rng <- range(x, na.rm = TRUE)
    extreme <- if (is.null(trim)) {
        max(abs(rng), na.rm = TRUE)
    }
    else {
        trim
    }
    if (is.null(zlim)) {
        zlim <- c(-1, 1) * extreme
    }
    else if (is.na(zlim)) {
        zlim <- rng + c(-1, 1) * 0.01 * diff(rng)
    }
    breaks <- seq(zlim[1], zlim[2], length = length(col) + 1)
    res <- c(list(zlim = zlim, breaks = breaks, col = col), res)
    res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
