\name{labelfct}
\alias{labelfct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Add labels around a component
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
labelfct(vertical = TRUE, r.cex = 1, c.cex = 1, label = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vertical}{
%%     ~~Describe \code{vertical} here~~
}
  \item{r.cex}{
%%     ~~Describe \code{r.cex} here~~
}
  \item{c.cex}{
%%     ~~Describe \code{c.cex} here~~
}
  \item{label}{
%%     ~~Describe \code{label} here~~
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
function (vertical = TRUE, r.cex = 1, c.cex = 1, label = NULL) 
{
    if (!is.null(label)) {
        cex.a <- c(c.cex, r.cex, c.cex, r.cex)
        if (length(label) == 1) {
            tmp <- label[1]
            label <- rep("", 4)
            label[ifelse(vertical, 1, 4)] <- tmp
        }
        else if (length(label) == 2) {
            cex.a <- rep(label[2], 4)
            tmp <- label[1]
            label <- rep("", 4)
            label[ifelse(vertical, 1, 4)] <- tmp
        }
        else if (length(label) >= 5) {
            cex.a <- rep(label[5:length(label)], length.out = 4)
            label <- label[1:4]
        }
        centers <- c(mean(par("usr")[1:2]), mean(par("usr")[3:4]), 
            mean(par("usr")[1:2]), mean(par("usr")[3:4]))
        for (i in 1:4) {
            if (!is.na(label[i])) {
                axis(i, centers[i], las = 2, tick = FALSE, line = FALSE, 
                  labels = label[i], cex.axis = cex.a[i])
            }
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
