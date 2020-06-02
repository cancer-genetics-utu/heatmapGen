\name{dendropart}
\alias{dendropart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Dendrogram part
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dendropart(dendro, height = 0.2, width = 0.2, horiz = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dendro}{
%%     ~~Describe \code{dendro} here~~
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
function (dendro, height = 0.2, width = 0.2, horiz = FALSE) 
{
    dendrofct <- function(zoomx = NULL, zoomy = NULL) {
        xlim <- if (horiz) 
            zoomy
        else zoomx
        if (!is.null(dendro)) {
            plf(dendro, horiz = horiz, axes = FALSE, xaxs = ifelse(horiz, 
                "r", "i"), yaxs = ifelse(horiz, "i", "r"), leaflab = "none", 
                xlim = xlim)
        }
        else {
            plot(0, xlim = 0:1, ylim = 0:1, type = "n", xaxt = "none", 
                yaxt = "none", bty = "n")
        }
    }
    list(FUN = dendrofct, height = height, width = width)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
