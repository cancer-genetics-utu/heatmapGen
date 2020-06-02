\name{hm_normalize}
\alias{hm_normalize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Data normalisation and trimming
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hm_normalize(x, scale, trim = NULL, na.rm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{scale}{
%%     ~~Describe \code{scale} here~~
}
  \item{trim}{
%%     ~~Describe \code{trim} here~~
}
  \item{na.rm}{
%%     ~~Describe \code{na.rm} here~~
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
function (x, scale, trim = NULL, na.rm = TRUE) 
{
    if (scale == "row") {
        x <- sweep(x, 1, rowMeans(x, na.rm = na.rm))
        sd <- apply(x, 1, sd, na.rm = na.rm)
        x <- sweep(x, 1, sd, "/")
    }
    else if (scale == "column") {
        x <- sweep(x, 2, colMeans(x, na.rm = na.rm))
        sd <- apply(x, 2, sd, na.rm = na.rm)
        x <- sweep(x, 2, sd, "/")
    }
    if (!is.null(trim)) {
        lo <- -trim
        hi <- trim
        x[x < lo] = lo
        x[x > hi] = hi
    }
    x
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
