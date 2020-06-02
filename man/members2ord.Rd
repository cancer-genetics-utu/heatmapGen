\name{members2ord}
\alias{members2ord}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Helper function to get row order if no clustering of subgroups
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
members2ord(members, spacer = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{members}{
%%     ~~Describe \code{members} here~~
}
  \item{spacer}{
%%     ~~Describe \code{spacer} here~~
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
function (members, spacer = 1) 
{
    idxs <- unclass(by(1:length(members), members, function(i) i))
    ord <- c(lapply(idxs, function(p) c(p, rep(NA, spacer))), 
        recursive = TRUE)
    ord <- ord[-length(ord)]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
