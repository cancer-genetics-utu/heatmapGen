\name{colorkeypart}
\alias{colorkeypart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Create colorkey
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
colorkeypart(x, breaks, symkey = FALSE, density.info = "histogram", denscol = "cyan", valuelabel = "Value", height = 0.2, width = 0.2, col, labels = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{breaks}{
%%     ~~Describe \code{breaks} here~~
}
  \item{symkey}{
%%     ~~Describe \code{symkey} here~~
}
  \item{density.info}{
%%     ~~Describe \code{density.info} here~~
}
  \item{denscol}{
%%     ~~Describe \code{denscol} here~~
}
  \item{valuelabel}{
%%     ~~Describe \code{valuelabel} here~~
}
  \item{height}{
%%     ~~Describe \code{height} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{labels}{
%%     ~~Describe \code{labels} here~~
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
function (x, breaks, symkey = FALSE, density.info = "histogram", 
    denscol = "cyan", valuelabel = "Value", height = 0.2, width = 0.2, 
    col, labels = TRUE, ...) 
{
    colorkeyfct <- function() {
        opar <- par(no.readonly = TRUE)
        scale01 <- function(x, low = min(x), high = max(x)) {
            x <- (x - low)/(high - low)
            x
        }
        par(cex = 0.75, mai = if (labels) 
            c(0.5, 0.5, 0.2, 0.2)
        else c(0.3, 0.3, 0.2, 0.2))
        if (symkey) {
            max.raw <- max(abs(x), na.rm = TRUE)
            min.raw <- -max.raw
        }
        else {
            min.raw <- min(x, na.rm = TRUE)
            max.raw <- max(x, na.rm = TRUE)
        }
        z <- seq(min.raw, max.raw, length = length(col))
        image(z = matrix(z, ncol = 1), col = col, breaks = breaks, 
            xaxt = "n", yaxt = "n")
        par(usr = c(0, 1, 0, 1))
        lv <- pretty(breaks)
        xv <- scale01(as.numeric(lv), min.raw, max.raw)
        axis(1, at = xv, labels = lv)
        if (labels) {
            mtext(side = 1, valuelabel, line = 2)
        }
        if (density.info == "histogram") {
            h <- hist(x, plot = FALSE, breaks = breaks)
            hx <- scale01(breaks, min.raw, max.raw)
            hy <- c(h$counts, h$counts[length(h$counts)])
            lines(hx, hy/max(hy) * 0.95, lwd = 1, type = "s", 
                col = denscol)
            axis(2, at = pretty(hy)/max(hy) * 0.95, pretty(hy))
            if (labels) {
                title("Color Key")
                par(cex = 0.5)
                mtext(side = 2, "Count", line = 2)
            }
        }
        else if (labels) {
            title("Color Key")
        }
        par(opar[c("mai", "cex")])
    }
    list(FUN = colorkeyfct, height = height, width = width)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
