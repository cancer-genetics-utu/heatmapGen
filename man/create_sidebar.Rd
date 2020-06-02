\name{create_sidebar}
\alias{create_sidebar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Create sidebar
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
create_sidebar(x, prep, vertical = TRUE, width = lcm(1), label = "", na.color = "gray80", cleannames = TRUE, cex = 1, factorpalettefn = "Pastel1")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{prep}{
%%     ~~Describe \code{prep} here~~
}
  \item{vertical}{
%%     ~~Describe \code{vertical} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
}
  \item{label}{
%%     ~~Describe \code{label} here~~
}
  \item{na.color}{
%%     ~~Describe \code{na.color} here~~
}
  \item{cleannames}{
%%     ~~Describe \code{cleannames} here~~
}
  \item{cex}{
%%     ~~Describe \code{cex} here~~
}
  \item{factorpalettefn}{
%%     ~~Describe \code{factorpalettefn} here~~
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
function (x, prep, vertical = TRUE, width = lcm(1), label = "", 
    na.color = "gray80", cleannames = TRUE, cex = 1, factorpalettefn = "Pastel1") 
{
    if (cleannames) {
        label <- gsub("_", " ", label)
    }
    if (is.character(factorpalettefn)) {
        palettename <- factorpalettefn
        require(RColorBrewer)
        factorpalettefn <- function(n) brewer.pal(n, palettename)
    }
    if (is.factor(x)) {
        part <- factorpart(x[if (vertical) 
            prep$rowInd
        else prep$colInd], label = label, width = width, vertical = vertical, 
            na.color = na.color, cex = cex, palettefn = factorpalettefn)
    }
    else {
        mat <- if (vertical) {
            matrix(x[prep$rowInd], nc = 1)
        }
        else {
            matrix(x[prep$colInd], nr = 1)
        }
        part <- imagepart(mat, col = prep$col, breaks = prep$breaks, 
            label = label, height = width, width = width, na.color = na.color, 
            r.cex = cex, c.cex = cex)
    }
    tmpenv <- environment(part$FUN)
    environment(part$FUN) <- new.env()
    lapply(ls(tmpenv), function(n) assign(n, get(n, tmpenv), 
        envir = environment(part$FUN), inherits = FALSE))
    environment(part$FUN) <- tmpenv
    part
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
