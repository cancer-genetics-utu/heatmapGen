\name{labelplot}
\alias{labelplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Helper function to label a plot area
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
labelplot(main, cex = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{cex}{
%%     ~~Describe \code{cex} here~~
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
function (main, cex = 2) 
{
    text(x = sum(par("usr")[1:2])/2, y = sum(par("usr")[3:4])/2, 
        main, cex = cex, adj = c(0.5, 0.5))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
