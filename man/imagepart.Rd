\name{imagepart}
\alias{imagepart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Imagepart
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
imagepart(img, height = 0.1, width = 0.1, ColLab = NULL, RowLab = NULL, r.cex = 1, c.cex = 1, label = NULL, na.color = "gray80", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{img}{
%%     ~~Describe \code{img} here~~
}
  \item{height}{
%%     ~~Describe \code{height} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
}
  \item{ColLab}{
%%     ~~Describe \code{ColLab} here~~
}
  \item{RowLab}{
%%     ~~Describe \code{RowLab} here~~
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
  \item{na.color}{
%%     ~~Describe \code{na.color} here~~
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
function (img, height = 0.1, width = 0.1, ColLab = NULL, RowLab = NULL, 
    r.cex = 1, c.cex = 1, label = NULL, na.color = "gray80", 
    ...) 
{
    imagefct <- function(zoomx = NULL, zoomy = NULL) {
        img <- t(img)
        xlim <- if (!is.null(zoomx)) 
            zoomx
        else c(0.5, nrow(img) + 0.5)
        ylim <- if (!is.null(zoomy)) 
            zoomy
        else c(0.5, ncol(img) + 0.5)
        image(1:nrow(img), 1:ncol(img), img, xaxt = "n", yaxt = "n", 
            bty = "n", xlim = xlim, ylim = ylim, ...)
        if (!is.null(na.color) && any(is.na(img))) {
            image(1:nrow(img), 1:ncol(img), ifelse(is.na(img), 
                1, NA), axes = FALSE, xlab = "", ylab = "", col = na.color, 
                add = TRUE)
        }
        box()
        if (!is.null(ColLab)) {
            axis(1, 1:nrow(img), las = 2, line = -0.5, tick = 0, 
                labels = ColLab, cex.axis = c.cex)
        }
        if (!is.null(RowLab)) {
            axis(4, 1:ncol(img), las = 2, tick = FALSE, labels = RowLab, 
                cex.axis = r.cex)
        }
        labelfct(vertical = nrow(img) == 1, r.cex = r.cex, c.cex = c.cex, 
            label = label)
    }
    list(FUN = imagefct, height = height, width = width)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
