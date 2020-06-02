\name{covarplot3_int}
\alias{covarplot3_int}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  INternal covariate plotting utility
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
covarplot3_int(covar, label = NULL, grp = NULL, grpcol, control = list(), vertical = FALSE, xlim = NULL, ylim = NULL, col = "black", pch = 1, line.at = numeric(0), line.col = "gray", labelside = ifelse(vertical, 1, 2))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{covar}{
%%     ~~Describe \code{covar} here~~
}
  \item{label}{
%%     ~~Describe \code{label} here~~
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
  \item{vertical}{
%%     ~~Describe \code{vertical} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{pch}{
%%     ~~Describe \code{pch} here~~
}
  \item{line.at}{
%%     ~~Describe \code{line.at} here~~
}
  \item{line.col}{
%%     ~~Describe \code{line.col} here~~
}
  \item{labelside}{
%%     ~~Describe \code{labelside} here~~
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
function (covar, label = NULL, grp = NULL, grpcol, control = list(), 
    vertical = FALSE, xlim = NULL, ylim = NULL, col = "black", 
    pch = 1, line.at = numeric(0), line.col = "gray", labelside = ifelse(vertical, 
        1, 2)) 
{
    cc = list(span = 1/3, degree = 1, nacol = gray(0.85))
    cc[names(control)] = control
    if (is.vector(covar)) 
        covar <- matrix(covar, nr = 1)
    n <- ncol(covar)
    ncurves <- nrow(covar)
    col <- rep(col, length.out = ncurves)
    if (vertical) {
        if (is.null(ylim)) 
            ylim <- c(0.5, n + 0.5)
        if (is.null(xlim)) 
            xlim <- range(na.omit(c(covar)))
        plot(0, ann = FALSE, yaxs = "i", xlim = xlim, ylim = ylim, 
            yaxt = "n", xaxt = "n", type = "n")
    }
    else {
        if (is.null(xlim)) 
            xlim <- c(0.5, n + 0.5)
        if (is.null(ylim)) 
            ylim <- range(na.omit(c(covar)))
        plot(0, ann = FALSE, xaxs = "i", xlim = xlim, ylim = ylim, 
            xaxt = "n", yaxt = "n", type = "n")
    }
    xx.grp <- c()
    uu = par("usr")
    if (!is.null(grp)) {
        grp = as.integer(factor(grp, levels = unique(grp)))
        tt = table(grp)
        gg = length(tt)
        xx.grp = c(0.5, 0.5 + cumsum(tt/sum(tt)) * n)
        for (i in 1:gg) {
            if (vertical) {
                rect(uu[1], xx.grp[i], uu[2], xx.grp[i + 1], 
                  col = grpcol[i], border = "transparent")
            }
            else {
                rect(xx.grp[i], uu[3], xx.grp[i + 1], uu[4], 
                  col = grpcol[i], border = "transparent")
            }
        }
    }
    nas <- which(is.na(covar))
    if (length(nas) > 0) {
        if (vertical) {
            rect(uu[1], nas - 0.5, uu[2], nas + 0.5, col = cc$nacol, 
                border = "transparent")
        }
        else {
            rect(nas - 0.5, uu[3], nas + 0.5, uu[4], col = cc$nacol, 
                border = "transparent")
        }
    }
    xx = 1:n
    lcols <- rep(line.col, length.out = length(line.at))
    lapply(seq_along(line.at), function(i) {
        if (vertical) {
            abline(v = line.at[i], col = lcols[i])
        }
        else {
            abline(h = line.at[i], col = lcols[i])
        }
    })
    lapply(1:ncurves, function(i) {
        cv <- covar[i, ]
        if (vertical) {
            points(cv, xx, col = col[i], pch = pch)
        }
        else {
            points(xx, cv, col = col[i], pch = pch)
        }
    })
    lapply(1:ncurves, function(i) {
        cv <- covar[i, ]
        if ((cc$degree > 0) & (cc$span > 0)) {
            covar_ok <- !is.na(cv)
            yy = predict(loess(cv[covar_ok] ~ xx[covar_ok], span = cc$span, 
                degree = cc$degree))
            if (vertical) {
                lines(yy, xx[covar_ok], col = col[i])
            }
            else {
                lines(xx[covar_ok], yy, col = col[i])
            }
        }
    })
    axis(ifelse(vertical, 2, 1), 1:n, labels = FALSE)
    axis(ifelse(vertical, 3, 4))
    box()
    if (!is.null(label)) {
        yy = mean(range(na.omit(c(covar))))
        axis(labelside, at = yy, label = label, las = TRUE, tick = FALSE, 
            font = 2)
    }
    xx.grp
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
