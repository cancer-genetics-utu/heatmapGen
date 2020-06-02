\name{dendro_order}
\alias{dendro_order}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Internal helper function for clustering within subgroups
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dendro_order(x, rowv = NULL, rowMembers = NULL, distfun = dist, hclustfun = hclust, reorder = TRUE, na.rm = TRUE, spacer = 1, filter = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{rowv}{
%%     ~~Describe \code{rowv} here~~
}
  \item{rowMembers}{
%%     ~~Describe \code{rowMembers} here~~
}
  \item{distfun}{
%%     ~~Describe \code{distfun} here~~
}
  \item{hclustfun}{
%%     ~~Describe \code{hclustfun} here~~
}
  \item{reorder}{
%%     ~~Describe \code{reorder} here~~
}
  \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
}
  \item{spacer}{
%%     ~~Describe \code{spacer} here~~
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
function (x, rowv = NULL, rowMembers = NULL, distfun = dist, 
    hclustfun = hclust, reorder = TRUE, na.rm = TRUE, spacer = 1, 
    filter = NULL) 
{
    backmap <- NULL
    if (!is.null(filter) && !identical(as.numeric(filter), 0)) {
        backmap <- data.frame(orig = 1:nrow(x), mapped = NA)
        backmap$mapped[filter] <- 1:sum(filter)
        x <- x[filter, ]
        if (!is.null(rowMembers)) 
            rowMembers <- rowMembers[filter]
        if (inherits(rowv, "dendrogram")) {
            if (any(!filter)) 
                stop("filtering is not compatible with an explicitly given dendrogram - use filter=FALSE")
        }
        else if (!is.null(rowv)) {
            rowv <- rowv[filter]
        }
    }
    if (reorder && !inherits(rowv, "dendrogram")) {
        if (is.null(rowMembers)) {
            idxs <- list(1:nrow(x))
            forest <- list(hclustfun(distfun(x)))
        }
        else {
            idxs <- unclass(by(1:nrow(x), rowMembers, function(i) i))
            forest <- lapply(idxs, function(sgi) {
                if (length(sgi) > 1) {
                  hclustfun(distfun(x[sgi, ]))
                }
                else {
                  den <- 1
                  attr(den, "label") <- ""
                  attr(den, "members") <- 1
                  attr(den, "height") <- 0
                  attr(den, "leaf") <- TRUE
                  class(den) <- "dendrogram"
                  den
                }
            })
        }
        dd <- lapply(forest, as.dendrogram)
        if (is.null(rowv)) 
            rowv <- rowMeans(x, na.rm = na.rm)
        dd <- lapply(dd, function(d) reorder(d, rowv))
    }
    else if (reorder && inherits(rowv, "dendrogram")) {
        dd <- list(reorder(rowv, rowMeans(x, na.rm = na.rm)))
        idxs <- list(1:nrow(x))
    }
    else {
        dd <- rowv
    }
    ind <- if (reorder) {
        ord.parts <- lapply(1:length(idxs), function(i) idxs[[i]][order.dendrogram(dd[[i]])])
        ord <- c(lapply(ord.parts, function(p) c(p, rep(NA, spacer))), 
            recursive = TRUE)
        ord[-length(ord)]
    }
    else {
        ord <- 1:nrow(x)
        if (!is.null(rowMembers)) {
            rm2 <- rowMembers[c(2:length(rowMembers), length(rowMembers))]
            chg <- rowMembers != rm2
            chg[length(chg)] <- FALSE
            for (i in rev(which(chg))) {
                ord <- append(ord, rep(NA, spacer), i)
            }
        }
        ord
    }
    if (!is.null(filter)) {
        ind1 <- ind
        ind <- backmap$orig[match(ind1, backmap$mapped)]
        ind[is.na(ind1)] <- NA
    }
    ddo <- if (reorder || inherits(rowv, "dendrogram")) 
        dd
    else NULL
    list(dd = ddo, ind = ind)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
