\name{draw_heatmap}
\alias{draw_heatmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Wrapper around 'hlayout' to actually plot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
draw_heatmap(dl, set.oma = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dl}{
%%     ~~Describe \code{dl} here~~
}
  \item{set.oma}{
%%     ~~Describe \code{set.oma} here~~
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
function (dl, set.oma = TRUE, ...) 
{
    if (set.oma) {
        par(oma = c(1.5, 0.5, 0.5, 0.5))
    }
    pars <- c(dl, list(...))
    res <- do.call("hlayout", pars)
    invisible(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
