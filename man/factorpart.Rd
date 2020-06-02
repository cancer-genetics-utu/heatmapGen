\name{factorpart}
\alias{factorpart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Utility
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
factorpart(fct, col = NULL, label = NULL, cex = 1, vertical = TRUE, width = lcm(1), na.color = "gray80", palettefn = rainbow, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fct}{
%%     ~~Describe \code{fct} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{label}{
%%     ~~Describe \code{label} here~~
}
  \item{cex}{
%%     ~~Describe \code{cex} here~~
}
  \item{vertical}{
%%     ~~Describe \code{vertical} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
}
  \item{na.color}{
%%     ~~Describe \code{na.color} here~~
}
  \item{palettefn}{
%%     ~~Describe \code{palettefn} here~~
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
function (fct, col = NULL, label = NULL, cex = 1, vertical = TRUE, 
    width = lcm(1), na.color = "gray80", palettefn = rainbow, 
    ...) 
{
    fct <- as.factor(fct)
    if (is.null(col)) {
        col <- palettefn(length(levels(fct)))
    }
    labels <- levels(fct)
    factorfct <- function(zoomx = NULL, zoomy = NULL) {
        img <- if (vertical) 
            matrix(as.numeric(fct), nr = 1)
        else matrix(as.numeric(fct), nc = 1)
        xlim <- if (!is.null(zoomx)) 
            zoomx
        else c(0.5, nrow(img) + 0.5)
        ylim <- if (!is.null(zoomy)) 
            zoomy
        else c(0.5, ncol(img) + 0.5)
        image(1:nrow(img), 1:ncol(img), img, xaxt = "n", yaxt = "n", 
            bty = "n", xlim = xlim, ylim = ylim, col = col, ...)
        if (!is.null(na.color) && any(is.na(img))) {
            image(1:nrow(img), 1:ncol(img), ifelse(is.na(img), 
                1, NA), axes = FALSE, xlab = "", ylab = "", col = na.color, 
                add = TRUE)
        }
        box()
        labelfct(vertical = vertical, r.cex = cex, c.cex = cex, 
            label = label)
    }
    list(FUN = factorfct, height = width, width = width, fct = fct, 
        col = col)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
