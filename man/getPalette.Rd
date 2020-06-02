\name{getPalette}
\alias{getPalette}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Color utility
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getPalette(col, n = 30)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
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
function (col, n = 30) 
{
    if (is.null(col)) {
        col <- colorRampPalette(c("green", "black", "red"))(n)
    }
    else if (length(col) == 1) {
        col <- switch(col, RdBkGn = colorRampPalette(c("green", 
            "black", "red"))(n), BuYl = colorRampPalette(c("blue", 
            "yellow"))(n), BuWtRd = colorRampPalette(c("blue", 
            "white", "red"))(n), {
            require(RColorBrewer)
            brewer.pal(n, col)
        })
    }
    col
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
