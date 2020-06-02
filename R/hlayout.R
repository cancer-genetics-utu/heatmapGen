labelplot <- function(main, cex=2) {
	text(x=sum(par("usr")[1:2])/2, y=sum(par("usr")[3:4])/2, main, cex=cex, adj=c(0.5, 0.5))
}


# title element
titlepart <- function(main, titleheight=0.1) {
	titlefun <- function(cex=2) {
		plot(0, type="n", xaxt="none", yaxt="none", xlim=0:1, ylim=0:1, bty="n")
		labelplot(main=main, cex=cex)
	}
	# return a list including a new function
	list(height=titleheight, FUN=titlefun)
}


# add label(s) around a component - see imagepart comments for use
# if vertical==TRUE, label on the bottom by default; otherwise on the right
labelfct <- function(vertical=TRUE, r.cex=1, c.cex=1, label=NULL) {
	if (!is.null(label)) {
		cex.a <- c(c.cex, r.cex, c.cex, r.cex)
		if (length(label) == 1) {
			tmp <- label[1]
			label <- rep("", 4)
			label[ifelse(vertical, 1, 4)] <- tmp
		} else if (length(label) == 2) {
			cex.a <- rep(label[2], 4)
			tmp <- label[1]
			label <- rep("", 4)
			label[ifelse(vertical, 1, 4)] <- tmp
		} else if (length(label) >= 5) {
			cex.a <- rep(label[5:length(label)], length.out=4)
			label <- label[1:4]
		}
		centers <- c(mean(par("usr")[1:2]), mean(par("usr")[3:4]), mean(par("usr")[1:2]), mean(par("usr")[3:4]))
		for (i in 1:4) {
			if (!is.na(label[i])) {
				axis(i, centers[i], las = 2, tick = FALSE, line=FALSE, labels = label[i], cex.axis = cex.a[i])
			}
		}
	}
}

# !HS! add a separate axispart?

# heatmap/similar image part
# label is a one-element vector (label at default position), a two-element vector (label and cex.override),
#  a four-element vector (bottom, left, top, right) or a five-to-eight element vector (bottom, left, top, right, cex.override(s)) or NULL
# if a long label vector is used, the unused components should be empty strings or NAs
imagepart <- function(img, height=0.1, width=0.1, ColLab=NULL, RowLab=NULL, r.cex=1, c.cex=1, label=NULL, na.color="gray80", ...) {
	imagefct <- function(zoomx=NULL, zoomy=NULL) {
		# transpose img, as the function image() treats it in a strange way
		img <- t(img)
		xlim <- if (!is.null(zoomx)) zoomx else c(0.5, nrow(img)+0.5)
		ylim <- if (!is.null(zoomy)) zoomy else c(0.5, ncol(img)+0.5)
		image(1:nrow(img), 1:ncol(img), img, xaxt="n", yaxt="n", bty="n", xlim=xlim, ylim=ylim, ...)
		if (!is.null(na.color) && any(is.na(img))) {
			image(1:nrow(img), 1:ncol(img), ifelse(is.na(img), 1, NA), axes = FALSE, xlab = "", ylab = "", col = na.color, add = TRUE)
		}
		box()
		if (!is.null(ColLab)) {
			axis(1, 1:nrow(img), las = 2, line = -0.5, tick = 0, labels = ColLab, cex.axis = c.cex)
		}
		if (!is.null(RowLab)) {
			axis(4, 1:ncol(img), las = 2, tick = FALSE, labels = RowLab, cex.axis = r.cex)
		}
		labelfct(vertical = nrow(img) == 1, r.cex=r.cex, c.cex=c.cex, label=label)
	}
	list(FUN=imagefct, height=height, width=width)
}


# dendrogram part
# dendro can be an hclust, a dendrogram or a forest (something that becomes a list of dendrograms after lapply(FUN=as.dendrogram))
dendropart <- function(dendro, height=0.2, width=0.2, horiz=FALSE) {
	dendrofct <- function(zoomx=NULL, zoomy=NULL) {
		xlim <- if(horiz) zoomy else zoomx
		if (!is.null(dendro)) {
			plf(dendro, horiz=horiz, axes = FALSE, xaxs = ifelse(horiz, "r", "i"), yaxs = ifelse(horiz, "i", "r"), leaflab = "none", xlim=xlim)
		} else {
			plot(0, xlim=0:1, ylim=0:1, type="n", xaxt="none", yaxt="none", bty="n")
		}
	}
	list(FUN=dendrofct, height=height, width=width)
}


# dummy/empty plot part - mostly as a tool in layout design
# note that specifying both height and width is OK - only the appropriate one based on the layout position of the part is used
emptypart <- function(main="", height=0.001, width=0.001, verbose=(main != ""), ...) {
	emptyfct <- function(main="", cex=2, ...) {
		omai <- par("mai")
		if (main=="") par(mai=c(0,0,0,0))
		plot(0, xlim=0:1, ylim=0:1, type="n", xaxt="none", yaxt="none", ...)
		par(mai=omai)
	}
	plotfun <- function() {
		emptyfct(bty=ifelse(verbose, "o", "n"), ...)
		if (verbose) labelplot(main)
	}
	list(height=height, width=width, FUN=plotfun)
}


# !HS! support splitting bottomextra into "slices" for additional legends(?)
# !HS! relate sum of widths to sum of bottomextra widths, determine breaks, duplicate columns if needed to achieve unaligned breaks
# heights and widths can be specified in centimeters using lcm(...)
# A NULL in a top/bottom/left/right list leaves a tiny space
hlayout <- function(
	main = NULL,
	topextra = NULL,
	top = list(),
	bottom = list(),
	left = list(),
	right = list(),
	topleft = NULL, topright = NULL,
	bottomleft = NULL, bottomright = NULL,
	bottomextra = NULL,
	...,
	zoomx = NULL,
	zoomy = NULL,
	execute = TRUE
) {
	# avoid "flipping" the zoomed part
	if (!is.null(zoomx)) zoomx <- sort(zoomx)
	if (!is.null(zoomy)) zoomy <- sort(zoomy)

	top <- lapply(top, function(x) { if (is.null(x)) emptypart() else x })
	bottom <- lapply(bottom, function(x) { if (is.null(x)) emptypart() else x })
	left <- lapply(left, function(x) { if (is.null(x)) emptypart() else x })
	right <- lapply(right, function(x) { if (is.null(x)) emptypart() else x })

	# prepare the layout matrix and row heights, column widths
	lmat <- matrix(1, 1, 1)
	if (length(top) > 0)
		lmat <- rbind(matrix(length(top):1 + max(lmat), ncol=1), lmat)
	if (length(bottom) > 0)
		lmat <- rbind(lmat, matrix(1:length(bottom) + max(lmat), ncol=1))
	heights <- c(lapply(rev(top), function(x) x$height), 1, lapply(bottom, function(x) x$height))

	template <- c(rep(0, length(top)), 1, rep(0, length(bottom)))
	if (length(left) > 0) {
		tl <- if (!is.null(topleft)) max(lmat) + length(left) + 1 else 0
		bl <- if (!is.null(bottomleft)) max(lmat) + length(left) + 1 + sign(tl) else 0
		tpl2 <- c(rep(tl, length(top)), 0, rep(bl, length(bottom)))
		lmat <- cbind(outer(template, length(left):1 + max(lmat), "*") + outer(tpl2, rep(1, length(left)), "*"), lmat)
	}
	if (length(right) > 0) {
		tr <- if (!is.null(topright)) max(lmat) + length(right) + 1 else 0
		br <- if (!is.null(bottomright)) max(lmat) + length(right) + 1 + sign(tr) else 0
		tpl2 <- c(rep(tr, length(top)), 0, rep(br, length(bottom)))
		lmat <- cbind(lmat, outer(template, 1:length(right) + max(lmat), "*") + outer(tpl2, rep(1, length(right)), "*"))
	}
	widths <- c(lapply(rev(left), function(x) x$width), 1, lapply(right, function(x) x$width))

	# add device-wide extra parts on the top and the bottom of the layout
	if (!is.null(topextra)) {
		heights <- c(topextra$height, heights)
		lmat <- rbind(matrix(max(lmat)+1, nrow=1, ncol=ncol(lmat)), lmat)
	}
	if (!is.null(bottomextra)) {
		heights <- c(heights, bottomextra$height)
		lmat <- rbind(lmat, matrix(max(lmat)+1, nrow=1, ncol=ncol(lmat)))
	}

	# prepare a list of calls to create the plots
	# main is a necesary part - make an empty part if nothing else given
	if (is.null(main)) {
		main <- list(FUN=function() {
			plot(0, type="n", xaxt="none", yaxt="none", bty="n")
		})
	}

	# move main last so that its coordinate system will stay active at the end
	lmat[which(lmat == 1)] <- max(lmat) + 1
	lmat <- ifelse(lmat > 0, lmat - 1, lmat)

	# make a list of parts, filter out nulls
	partlist <- c(top, bottom, left, list(topleft, bottomleft), right, list(topright, bottomright, topextra, bottomextra, main))
	partlist <- partlist[!sapply(partlist, is.null)]
	# create the evaluation-ready list of plot actions/functions - this is a bit tricky
	plotlist <- lapply(partlist, function(x) {
		# this way, we can make the top-level ... arguments available to the called functions
		g <- list(x$FUN, ...)
		if ("zoomx" %in% names(as.list(args(x$FUN)))) g$zoomx <- zoomx
		if ("zoomy" %in% names(as.list(args(x$FUN)))) g$zoomy <- zoomy
		mode(g) <- "call"
		g
	})

	# actually do the plots
	if (execute) {
		# !HS! setting par here???
		par(mai=c(0.025,0.025,0.025,0.025))
		# this is needed for cm heights and widths
		heights <- c(heights, recursive=TRUE)
		widths <- c(widths, recursive=TRUE)
		lo <- layout(lmat, heights=heights, widths=widths)
		lapply(plotlist, eval)
	} else {
		# support for "manual adjustments" - modifications of layout before plotting
		invisible(list(lmat=lmat, heights=heights, widths=widths, plotlist=plotlist))
	}
}

