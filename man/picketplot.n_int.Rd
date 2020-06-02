\name{picketplot.n_int}
\alias{picketplot.n_int}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Internal function to generate picket plot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
picketplot.n_int(bindata, grp = NULL, grpcol, control = list(), p.col = NULL, vertical = FALSE, xlim = NULL, ylim = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bindata}{
%%     ~~Describe \code{bindata} here~~
}
  \item{grp}{
%%     ~~Describe \code{grp} here~~
}
  \item{grpcol}{
%%     ~~Describe \code{grpcol} here~~
}
  \item{control}{
%%     ~~Describe \code{control} here~~
}
  \item{p.col}{
%%     ~~Describe \code{p.col} here~~
}
  \item{vertical}{
%%     ~~Describe \code{vertical} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
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
function (bindata, grp = NULL, grpcol, control = list(), p.col = NULL, 
    vertical = FALSE, xlim = NULL, ylim = NULL) 
{
    cc = list(boxw = 1, boxh = 4, hbuff = 0.1, vbuff = 0.1, nacol = gray(0.85))
    cc[names(control)] = control
    if (is.null(xlim)) {
        xlim <- if (vertical) 
            c(0.5, 0.5 + ncol(bindata))
        else c(0.5, 0.5 + nrow(bindata))
    }
    if (is.null(ylim)) {
        ylim <- if (vertical) 
            c(0.5, 0.5 + nrow(bindata))
        else c(0.5, 0.5 + ncol(bindata))
    }
    plot(0, type = "n", xlim = xlim, ylim = ylim, ann = FALSE, 
        xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n")
    fill <- NA
    if (missing(p.col) || is.null(p.col)) {
        fill = ifelse(bindata == 1, "black", "transparent")
    }
    else {
        fill <- bindata
        for (i in 1:length(p.col)) {
            fill[bindata == p.col[i]] <- names(p.col)[i]
        }
    }
    fill[is.na(fill)] = cc$nacol
    k = ncol(bindata)
    n = nrow(bindata)
    x0 <- rep(1:n - 0.5 + cc$hbuff/2, k)
    x1 <- x0 + 1 - cc$hbuff
    y0 <- rep(1:k - 0.5 + cc$vbuff/2, rep(n, k))
    y1 <- y0 + 1 - cc$vbuff
    if (vertical) {
        rect(y0, x0, y1, x1, col = fill, border = "transparent")
    }
    else {
        rect(x0, y0, x1, y1, col = fill, border = "transparent")
    }
    box()
    label = colnames(bindata)
    if (!is.null(label)) {
        yy = 1:ncol(bindata)
        axis(ifelse(vertical, 1, 2), at = yy, label = label, 
            las = TRUE, font = 2, col = par("bg"), col.axis = par("fg"), 
            tick = FALSE)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
