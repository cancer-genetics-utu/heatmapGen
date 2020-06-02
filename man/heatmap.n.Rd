\name{heatmap.n}
\alias{heatmap.n}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Top-level heatmap call 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
heatmap.n(x, main = "Heatmap", ..., na.color = "gray80", sidebars = NULL, picketdata = NULL, r.cex = 1, c.cex = 1, titleheight = lcm(1.2), dendroheight = lcm(3), dendrowidth = lcm(3), labelheight = lcm(1), labelwidth = lcm(1), picketheight = 0.4, sidebarwidth = lcm(1), sidebar.cex = 1, colorkeylabels = FALSE, legendcorner = "bottomleft", plot = TRUE, factorpalettefn = "Pastel1")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{na.color}{
%%     ~~Describe \code{na.color} here~~
}
  \item{sidebars}{
%%     ~~Describe \code{sidebars} here~~
}
  \item{picketdata}{
%%     ~~Describe \code{picketdata} here~~
}
  \item{r.cex}{
%%     ~~Describe \code{r.cex} here~~
}
  \item{c.cex}{
%%     ~~Describe \code{c.cex} here~~
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
  \item{picketheight}{
%%     ~~Describe \code{picketheight} here~~
}
  \item{sidebarwidth}{
%%     ~~Describe \code{sidebarwidth} here~~
}
  \item{sidebar.cex}{
%%     ~~Describe \code{sidebar.cex} here~~
}
  \item{colorkeylabels}{
%%     ~~Describe \code{colorkeylabels} here~~
}
  \item{legendcorner}{
%%     ~~Describe \code{legendcorner} here~~
}
  \item{plot}{
%%     ~~Describe \code{plot} here~~
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
function (x, main = "Heatmap", ..., na.color = "gray80", sidebars = NULL, 
    picketdata = NULL, r.cex = 1, c.cex = 1, titleheight = lcm(1.2), 
    dendroheight = lcm(3), dendrowidth = lcm(3), labelheight = lcm(1), 
    labelwidth = lcm(1), picketheight = 0.4, sidebarwidth = lcm(1), 
    sidebar.cex = 1, colorkeylabels = FALSE, legendcorner = "bottomleft", 
    plot = TRUE, factorpalettefn = "Pastel1") 
{
    prep <- prepare_heatmap_data(x, ...)
    dl <- prepare_heatmap(prep, title = main, titleheight = titleheight, 
        dendroheight = dendroheight, dendrowidth = dendrowidth, 
        labelheight = labelheight, labelwidth = labelwidth, r.cex = r.cex, 
        c.cex = c.cex, colorkeylabels = colorkeylabels, na.color = na.color)
    if (!is.null(picketdata)) {
        pickpart <- picketpart(addvar = picketdata[prep$colInd, 
            ], height = picketheight)
        dl$bottom <- c(dl$bottom, list(pickpart))
    }
    if (!is.null(sidebars)) {
        parts_for_legend <- list()
        lapply(c("bottom", "left", "top", "right"), function(side) {
            if (!is.null(sidebars[[side]])) {
                sbs <- lapply(seq(along = sidebars[[side]]), 
                  function(col) {
                    create_sidebar(x = sidebars[[side]][, col], 
                      prep = prep, vertical = (side \%in\% c("left", 
                        "right")), label = names(sidebars[[side]])[col], 
                      width = sidebarwidth, na.color = na.color, 
                      cex = sidebar.cex, factorpalettefn = factorpalettefn)
                  })
                if (side \%in\% c("right", "bottom")) {
                  dl[[side]] <<- c(dl[[side]][1], sbs, dl[[side]][-1])
                }
                else {
                  dl[[side]] <<- c(sbs, dl[[side]])
                }
                for (i in seq(along = sbs)) {
                  if (!is.null(sbs[[i]]$fct)) 
                    parts_for_legend <<- c(parts_for_legend, 
                      list(sbs[[i]]))
                }
                if (length(parts_for_legend) > 2) 
                  parts_for_legend <<- parts_for_legend[1:2]
            }
        })
        llabels <- lapply(parts_for_legend, function(x) levels(x$fct))
        lcols <- lapply(parts_for_legend, function(x) x$col)
        dl[[legendcorner]] <- legendpart(llabels, col = lcols)
    }
    if (plot) {
        draw_heatmap(dl)
    }
    invisible(dl)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
