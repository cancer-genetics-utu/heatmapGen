\name{hm_reorder}
\alias{hm_reorder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Data reordering
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hm_reorder(x, labRow = NA, labCol = NA, Rowv = NULL, Colv = NULL, reorder = c(TRUE, TRUE), distfun = dist, hclustfun = hclust, rowMembers = NULL, colMembers = NULL, spacer = 1, na.rm = TRUE, filter = c(TRUE, TRUE))
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
  \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
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
    rowMembers = NULL, colMembers = NULL, spacer = 1, na.rm = TRUE, 
    filter = c(TRUE, TRUE)) 
{
    dofilter <- function(x, filter) {
        if (filter == TRUE) {
            !apply(is.na(x), 1, all)
        }
        else if (filter > 0) {
            rowSums(is.na(x)) < ncol(x) * filter
        }
        else {
            NULL
        }
    }
    filter <- rep(filter, length.out = 2)
    rowOk <- dofilter(x, filter[1])
    colOk <- dofilter(t(x), filter[2])
    roword <- dendro_order(x, rowv = Rowv, rowMembers = rowMembers, 
        distfun = distfun, hclustfun = hclustfun, reorder = reorder[1], 
        na.rm = na.rm, spacer = spacer, filter = rowOk)
    ddr <- roword$dd
    rowInd <- roword$ind
    colord <- dendro_order(t(x), rowv = Colv, rowMembers = colMembers, 
        distfun = distfun, hclustfun = hclustfun, reorder = reorder[2], 
        na.rm = na.rm, spacer = spacer, filter = colOk)
    ddc <- colord$dd
    colInd <- colord$ind
    x <- x[rowInd, colInd]
    collab <- if (is.null(labCol)) 
        NULL
    else if (length(labCol) == 1 && is.na(labCol)[1]) 
        colnames(x)
    else labCol[colInd]
    rowlab <- if (is.null(labRow)) 
        NULL
    else if (length(labRow) == 1 && is.na(labRow)[1]) 
        rownames(x)
    else labRow[rowInd]
    list(ddr = ddr, ddc = ddc, rowInd = rowInd, colInd = colInd, 
        x = x, collab = collab, rowlab = rowlab, reorder = reorder, 
        rowOk = rowOk, colOk = colOk)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
