\name{prepare_heatmap}
\alias{prepare_heatmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Prepare heatmap
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
prepare_heatmap(prep, title = "Heatmap", ..., titleheight = 0.12, dendroheight = 0.25, dendrowidth = 0.25, labelheight = 0.1, labelwidth = 0.1, r.cex = 1, c.cex = 1, colorkeylabels = FALSE, na.color = "gray80")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prep}{
%%     ~~Describe \code{prep} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{titleheight}{
%%     ~~Describe \code{titleheight} here~~
}
  \item{dendroheight}{
%%     ~~Describe \code{dendroheight} here~~
}
  \item{dendrowidth}{
%%     ~~Describe \code{dendrowidth} here~~
}
  \item{labelheight}{
%%     ~~Describe \code{labelheight} here~~
}
  \item{labelwidth}{
%%     ~~Describe \code{labelwidth} here~~
}
  \item{r.cex}{
%%     ~~Describe \code{r.cex} here~~
}
  \item{c.cex}{
%%     ~~Describe \code{c.cex} here~~
}
  \item{colorkeylabels}{
%%     ~~Describe \code{colorkeylabels} here~~
}
  \item{na.color}{
%%     ~~Describe \code{na.color} here~~
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
function (prep, title = "Heatmap", ..., titleheight = 0.12, dendroheight = 0.25, 
    dendrowidth = 0.25, labelheight = 0.1, labelwidth = 0.1, 
    r.cex = 1, c.cex = 1, colorkeylabels = FALSE, na.color = "gray80") 
{
    main <- imagepart(prep$x, col = prep$col, breaks = prep$breaks, 
        ColLab = prep$collab, RowLab = prep$rowlab, r.cex = r.cex, 
        c.cex = c.cex, na.color = na.color)
    topextra <- titlepart(main = title, titleheight = titleheight)
    top <- list(dendropart(prep$ddc, height = dendroheight))
    bottom <- list(emptypart(height = labelheight))
    left <- list(dendropart(prep$ddr, horiz = TRUE, width = dendrowidth))
    right <- list(emptypart(width = labelwidth))
    topleft <- colorkeypart(prep$x, breaks = prep$breaks, col = prep$col, 
        labels = colorkeylabels)
    list(main = main, topextra = topextra, top = top, bottom = bottom, 
        left = left, right = right, topleft = topleft, topright = NULL, 
        bottomleft = NULL, bottomright = NULL, bottomextra = NULL, 
        ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
