\name{clustersub}
\alias{clustersub}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Clustering within subgroups of data and related data reorganization.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
clustersub(x, members, spacer = 1, dmethod = "euclidean", clmethod = "complete")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{members}{
%%     ~~Describe \code{members} here~~
}
  \item{spacer}{
%%     ~~Describe \code{spacer} here~~
}
  \item{dmethod}{
%%     ~~Describe \code{dmethod} here~~
}
  \item{clmethod}{
%%     ~~Describe \code{clmethod} here~~
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
function (x, members, spacer = 1, dmethod = "euclidean", clmethod = "complete") 
{
    idxs <- unclass(by(1:nrow(x), members, function(i) i))
    forest <- lapply(idxs, function(sgi) hclust(dist(x[sgi, ], 
        method = dmethod), method = clmethod))
    ord.parts <- lapply(1:length(idxs), function(i) idxs[[i]][forest[[i]]$order])
    ord <- c(lapply(ord.parts, function(p) c(p, rep(NA, spacer))), 
        recursive = TRUE)
    ord <- ord[-length(ord)]
    list(forest = forest, ord = ord, spacer = spacer)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
