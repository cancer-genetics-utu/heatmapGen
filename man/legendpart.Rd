\name{legendpart}
\alias{legendpart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Legend part
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
legendpart(labels, col, height = lcm(4), width = lcm(3), main = NULL, where = c("topleft", "bottomleft"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{labels}{
%%     ~~Describe \code{labels} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{height}{
%%     ~~Describe \code{height} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{where}{
%%     ~~Describe \code{where} here~~
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
function (labels, col, height = lcm(4), width = lcm(3), main = NULL, 
    where = c("topleft", "bottomleft")) 
{
    legendfct <- function(zoomx = NULL, zoomy = NULL) {
        plot.new()
        if (!is.null(main)) {
            title(main, cex = 0.75)
        }
        if (!is.list(labels)) 
            labels <- list(labels)
        if (!is.null(col) && !is.list(col)) 
            col <- list(col)
        lapply(seq(along = labels), function(i) legend(where[i], 
            legend = labels[[i]], fill = col[[i]], bty = "n"))
    }
    list(FUN = legendfct, height = height, width = width)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
