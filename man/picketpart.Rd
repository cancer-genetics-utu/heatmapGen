\name{picketpart}
\alias{picketpart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Picket plot part
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
picketpart(addvar, height = 0.4, width = 0.4, horiz = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{addvar}{
%%     ~~Describe \code{addvar} here~~
}
  \item{height}{
%%     ~~Describe \code{height} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
}
  \item{horiz}{
%%     ~~Describe \code{horiz} here~~
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
function (addvar, height = 0.4, width = 0.4, horiz = TRUE, ...) 
{
    picketfct <- function(zoomx = NULL, zoomy = NULL) {
        xlim <- if (horiz) 
            zoomx
        else NULL
        ylim <- if (horiz) 
            NULL
        else zoomy
        picketplot.n_int(addvar, vertical = !horiz, xlim = xlim, 
            ylim = ylim, ...)
    }
    list(FUN = picketfct, height = height, width = width)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
