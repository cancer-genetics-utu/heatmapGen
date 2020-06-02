require(Heatplus)


colorkeypart <- function(x, breaks, symkey=FALSE, density.info="histogram", denscol="cyan", valuelabel="Value", height=0.2, width=0.2, col, labels=TRUE, ...) {
	colorkeyfct <- function() {
		opar <- par(no.readonly=TRUE)
		# borrowed/adapted from heatmap.2 - mai etc. modified
	    scale01 <- function(x, low = min(x), high = max(x)) {
	        x <- (x - low)/(high - low)
	        x
	    }
        par(cex = 0.75, mai=if (labels) c(0.5, 0.5, 0.2, 0.2) else c(0.3, 0.3, 0.2, 0.2))
        if (symkey) {
            max.raw <- max(abs(x), na.rm = TRUE)
            min.raw <- -max.raw
        }
        else {
            min.raw <- min(x, na.rm = TRUE)
            max.raw <- max(x, na.rm = TRUE)
        }
        z <- seq(min.raw, max.raw, length = length(col))
        image(z = matrix(z, ncol = 1), col = col, breaks = breaks, xaxt = "n", yaxt = "n")
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
            lines(hx, hy/max(hy) * 0.95, lwd = 1, type = "s", col = denscol)
            axis(2, at = pretty(hy)/max(hy) * 0.95, pretty(hy))
            if (labels) {
	            title("Color Key")
	            par(cex = 0.5)
	            mtext(side = 2, "Count", line = 2)
	        }
        }
        else if (labels) { title("Color Key") }
        par(opar[c("mai", "cex")])
	}
	list(FUN=colorkeyfct, height=height, width=width)
}


# internal part of picket plot itself
# copied here because of major changes, to avoid breaking the original
picketplot.n_int <- function (bindata, grp = NULL, grpcol,
	control = list(), p.col = NULL, vertical=FALSE, xlim=NULL, ylim=NULL)
{
	cc = list(boxw = 1, boxh = 4, hbuff = 0.1, vbuff = 0.1, nacol = gray(0.85))
	cc[names(control)] = control

	if (is.null(xlim)) {
		xlim <- if (vertical) c(0.5, 0.5+ncol(bindata)) else c(0.5, 0.5+nrow(bindata))
	}
	if (is.null(ylim)) {
		ylim <- if (vertical) c(0.5, 0.5+nrow(bindata)) else c(0.5, 0.5+ncol(bindata))
	}
	plot(0, type="n", xlim=xlim, ylim=ylim, ann = FALSE, xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n")

	#Color is specified here.
	fill <- NA
	if (missing(p.col) || is.null(p.col)) {
		fill = ifelse(bindata == 1, "black", "transparent")
	} else {
		fill <- bindata
		for (i in 1:length(p.col)) {
			fill[ bindata == p.col[i] ] <- names(p.col)[i]
		}
	}
	fill[is.na(fill)] = cc$nacol

	k = ncol(bindata)
	n = nrow(bindata)

	x0 <- rep(1:n - 0.5 + cc$hbuff/2, k)
	x1 <- x0 + 1 - cc$hbuff
	y0 <- rep(1:k - 0.5 + cc$vbuff/2, rep(n, k))
	y1 <- y0 + 1 - cc$vbuff

	# !HS! finish
#    ww = n * (cc$boxw + 2 * cc$hbuff)
#    hh = k * (cc$boxh + 2 * cc$vbuff)
#    x0 = rep(seq(cc$hbuff, by = cc$boxw + 2 * cc$hbuff, length = n),
#        k)
#    x1 = x0 + cc$boxw
#    y0 = rep(seq(cc$vbuff, by = cc$boxh + 2 * cc$vbuff, length = k),
#        rep(n, k))
#    y1 = y0 + cc$boxh
#	xx.grp <- c()
#    if (!is.null(grp)) {
#        grp = as.integer(factor(grp, levels = unique(grp)))
#        tt = table(grp)
#        gg = length(tt)
#        xx.grp = c(0, cumsum(tt/sum(tt)) * ww)
#        for (i in 1:gg) {
#    		if (vertical) {
#            	rect(0, xx.grp[i], hh, xx.grp[i + 1], col = grpcol[i],
#            		border = "transparent")
#            } else {
#            	rect(xx.grp[i], 0, xx.grp[i + 1], hh, col = grpcol[i],
#            		border = "transparent")
#            }
#        }
#    }
	if (vertical) {
		rect(y0, x0, y1, x1, col = fill, border = "transparent")
	} else {
		rect(x0, y0, x1, y1, col = fill, border = "transparent")
	}

	box()
	label = colnames(bindata)
	if (!is.null(label)) {
		yy =  1:ncol(bindata) # sort(unique((y0 + y1)/2))
		axis(ifelse(vertical, 1, 2), at = yy, label = label, las = TRUE, font = 2,
			col = par("bg"), col.axis = par("fg"), tick = FALSE)
	}

#    xx.grp
}


# picket plot part
picketpart <- function(addvar, height=0.4, width=0.4, horiz=TRUE, ...) {
	picketfct <- function(zoomx=NULL, zoomy=NULL) {
		xlim <- if (horiz) zoomx else NULL
		ylim <- if (horiz) NULL else zoomy
		picketplot.n_int(addvar, vertical=!horiz, xlim=xlim, ylim=ylim, ...)
	}
	list(FUN=picketfct, height=height, width=width)
}


# covariate part, extracted from picketplot3
# low-level function, not meant to be called from top level
# copied from heatmap.3 mechanism in vttutil
# generalized to support multiple curves in a plot, with colors by curve
# supports reference line(s) with line.at and line.color
covarplot3_int <- function(covar, label=NULL, grp = NULL, grpcol,
    control = list(), vertical=FALSE, xlim=NULL, ylim=NULL, col="black", pch=1, line.at=numeric(0), line.col="gray",
    labelside=ifelse(vertical, 1, 2))
{
    cc = list(span = 1/3, degree = 1, nacol = gray(0.85))
    cc[names(control)] = control

	# if covar is a matrix, its rows are to be drawn as separate curves
	# if not a matrix, convert to one for simpler handling
	if (is.vector(covar)) covar <- matrix(covar, nr=1)
	n <- ncol(covar)
	ncurves <- nrow(covar)
	col <- rep(col, length.out=ncurves)

    if (vertical) {
		if (is.null(ylim)) ylim <- c(0.5, n + 0.5)
		if (is.null(xlim)) xlim <- range(na.omit(c(covar)))
    	plot(0, ann = FALSE, yaxs = "i", xlim = xlim, ylim = ylim, yaxt = "n", xaxt = "n", type = "n")
    } else {
		if (is.null(xlim)) xlim <- c(0.5, n + 0.5)
		if (is.null(ylim)) ylim <- range(na.omit(c(covar)))
    	plot(0, ann = FALSE, xaxs = "i", xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n", type = "n")
    }
	xx.grp <- c()
    uu = par("usr")
    if (!is.null(grp)) {
    	# !HS! currently need to exactly replicate picketplot3_int; otherwise messing up group order!
        grp = as.integer(factor(grp, levels = unique(grp)))
        tt = table(grp)
        gg = length(tt)
        xx.grp = c(0.5, 0.5 + cumsum(tt/sum(tt)) * n)
        for (i in 1:gg) {
    		if (vertical) {
            	rect(uu[1], xx.grp[i], uu[2], xx.grp[i + 1],
            	  col = grpcol[i], border = "transparent")
            } else {
            	rect(xx.grp[i], uu[3], xx.grp[i + 1], uu[4],
            	  col = grpcol[i], border = "transparent")
            }
        }
    }
    # gray background for missing values
    nas <- which(is.na(covar))
    if (length(nas) > 0) {
	    if (vertical) {
	        rect(uu[1], nas-0.5, uu[2], nas+0.5, col = cc$nacol, border = "transparent")
	    } else {
	        rect(nas-0.5, uu[3], nas+0.5, uu[4], col = cc$nacol, border = "transparent")
	    }
	}

	# reference sequence
    xx = 1:n

	# reference lines
	lcols <- rep(line.col, length.out=length(line.at))
	lapply(seq_along(line.at), function(i) {
		if (vertical) {
			abline(v=line.at[i], col=lcols[i])
		} else {
			abline(h=line.at[i], col=lcols[i])
		}
	})

	# points
    lapply(1:ncurves, function(i) {
    	cv <- covar[i, ]
	    if (vertical) {
	    	points(cv, xx, col=col[i], pch=pch)
	    } else {
	    	points(xx, cv, col=col[i], pch=pch)
	    }
	})

	# fitted curves
    lapply(1:ncurves, function(i) {
    	cv <- covar[i, ]
	    if ((cc$degree > 0) & (cc$span > 0)) {
	    	# handle missing values for the covariate: ignore them in the fit
	    	# may not be perfect but ok when not too many values missing
	    	# !HS! should this be done by group separately?
	    	covar_ok <- !is.na(cv)
	        yy = predict(loess(cv[covar_ok] ~ xx[covar_ok], span = cc$span, degree = cc$degree))
	    	if (vertical) {
	        	lines(yy, xx[covar_ok], col=col[i])
	        } else {
	        	lines(xx[covar_ok], yy, col=col[i])
	        }
	    }
	})

    axis(ifelse(vertical, 2, 1), 1:n, labels = FALSE)
    axis(ifelse(vertical, 3, 4))
    box()
    if (!is.null(label)) {
    	# handling missing values properly...
        yy = mean(range(na.omit(c(covar))))
        axis(labelside, at = yy, label = label, las = TRUE, tick = FALSE,
            font = 2)
    }

    xx.grp
}


# covar plot part
covarpart <- function(covar, label="", height=0.3, width=0.3, horiz=TRUE, ...) {
	covarfct <- function(zoomx=NULL, zoomy=NULL) {
		xlim <- if (horiz) zoomx else NULL
		ylim <- if (horiz) NULL else zoomy
		covarplot3_int(covar, label=label, vertical=!horiz, xlim=xlim, ylim=ylim, ...)
	}
	list(FUN=covarfct, height=height, width=width)
}


# legend part for a fixed color scale
# labels and col can be single vectors or lists for multiple legends; where is a vector for multiple legends in the same part
# !HS! this could be improved...
legendpart <- function(labels, col, height=lcm(4), width=lcm(3), main=NULL, where=c("topleft", "bottomleft")) {
	legendfct <- function(zoomx=NULL, zoomy=NULL) {
		# ignore zoom
		plot.new()
		if (!is.null(main)) {
			title(main, cex=0.75)
        }
		#box()
		# support single or multiple legends in a single part
		if (!is.list(labels)) labels <- list(labels)
		if (!is.null(col) && !is.list(col)) col <- list(col)
		lapply(seq(along=labels), function(i) legend(where[i], legend=labels[[i]], fill=col[[i]], bty="n"))
	}
	list(FUN=legendfct, height=height, width=width)
}


factorpart <- function(fct, col=NULL, label=NULL, cex=1, vertical=TRUE, width=lcm(1), na.color="gray80", palettefn=rainbow, ...) {
	fct <- as.factor(fct)
	if (is.null(col)) {
		col <- palettefn(length(levels(fct)))
	}
	labels <- levels(fct)
	factorfct <- function(zoomx=NULL, zoomy=NULL) {
		# this is "transposed" for the image function
		img <- if (vertical) matrix(as.numeric(fct), nr=1) else matrix(as.numeric(fct), nc=1)
		xlim <- if (!is.null(zoomx)) zoomx else c(0.5, nrow(img)+0.5)
		ylim <- if (!is.null(zoomy)) zoomy else c(0.5, ncol(img)+0.5)
		image(1:nrow(img), 1:ncol(img), img, xaxt="n", yaxt="n", bty="n", xlim=xlim, ylim=ylim, col=col, ...)
		if (!is.null(na.color) && any(is.na(img))) {
			image(1:nrow(img), 1:ncol(img), ifelse(is.na(img), 1, NA), axes = FALSE, xlab = "", ylab = "", col = na.color, add = TRUE)
		}
		box()
		labelfct(vertical=vertical, r.cex=cex, c.cex=cex, label=label)
	}
	list(FUN=factorfct, height=width, width=width, fct=fct, col=col)
}


getPalette <- function(col, n=30) {
	if (is.null(col)) {
		col <- colorRampPalette(c("green", "black", "red"))(n)
	} else if (length(col) == 1) {
		col <- switch(col,
			RdBkGn=colorRampPalette(c("green", "black", "red"))(n),
			BuYl=colorRampPalette(c("blue", "yellow"))(n),
			BuWtRd=colorRampPalette(c("blue", "white", "red"))(n),
			{ require(RColorBrewer); brewer.pal(n, col) }
		)
	}
	col
}


# internal helper function for reorganizing data: clustering within subgroups, create an order vector
# !HS! currently, filtering is not compatible with giving a dendrogram as rowv
dendro_order <- function(x, rowv = NULL, rowMembers = NULL, distfun = dist, hclustfun = hclust, reorder = TRUE, na.rm = TRUE, spacer = 1, filter=NULL) {
	# filter input and note what was filtered => can adjust ind by doing a reverse mapping below
	backmap <- NULL
	# this is a trick - would like to do identical(filter, FALSE) but calling code may convert filter to a numeric value
	if (!is.null(filter) && !identical(as.numeric(filter), 0)) {
		# construct a mapping from filtered to unfiltered indices to fix ind below
		backmap <- data.frame(orig=1:nrow(x), mapped=NA)
		backmap$mapped[filter] <- 1:sum(filter)
		x <- x[filter, ]
		if (!is.null(rowMembers)) rowMembers <- rowMembers[filter]
		if (inherits(rowv, "dendrogram")) {
			if (any(!filter))
				stop("filtering is not compatible with an explicitly given dendrogram - use filter=FALSE")
		} else if (!is.null(rowv)) {
			rowv <- rowv[filter]
		}
	}
	if (reorder && !inherits(rowv, "dendrogram")) {
		if (is.null(rowMembers)) {
			idxs <- list(1:nrow(x))
			forest <- list(hclustfun(distfun(x)))
		} else {
			# index vector so that can make sure all operations use same order
			idxs <- unclass(by(1:nrow(x), rowMembers, function(i) i))
			forest <- lapply(idxs, function (sgi) {
				if (length(sgi) > 1) {
					hclustfun(distfun(x[sgi, ]))
				} else {
					# dummy dendrogram - leaf only
					den <- 1
					attr(den, "label") <- ""
					attr(den, "members") <- 1
					attr(den, "height") <- 0
					attr(den, "leaf") <- TRUE
					class(den) <- "dendrogram"
					den
				}
			})
		}
		dd <- lapply(forest, as.dendrogram)
		if (is.null(rowv))
			rowv <- rowMeans(x, na.rm = na.rm)
		# !HS! check; here, rowv is a vector to order by
		dd <- lapply(dd, function(d) reorder(d, rowv))
	} else if (reorder && inherits(rowv, "dendrogram")) {
		# !HS! is this compatible with filter? - need to remove parts of dendrogram if to make this compatible
		# these are needed to support giving a dendrogram as Rovw
		dd <- list(reorder(rowv, rowMeans(x, na.rm = na.rm)))
		idxs <- list(1:nrow(x))
	} else {
		# !HS! no reordering, rowv may be a dendrogram; is this compatible with filter? if rowv is not a dendrogram here, it is ignored later
		dd <- rowv
	}

	ind <- if (reorder) {
		# construct order of all rows with optional spacers
		# order.dendrogram returns order within each subgroup, need to map to "global" indices
		ord.parts <- lapply(1:length(idxs), function(i) idxs[[i]][order.dendrogram(dd[[i]])])
		ord <- c(lapply(ord.parts, function(p) c(p, rep(NA, spacer))), recursive=TRUE)
		ord[-length(ord)]
	} else {
		ord <- 1:nrow(x)
		# insert spacers between groups - the groups might be splintered/mixed!
		if (!is.null(rowMembers)) {
			# find locations where the group changes - TRUE at position i means that we need to insert a spacer after position i
			rm2 <- rowMembers[c(2:length(rowMembers), length(rowMembers))]
			chg <- rowMembers != rm2
			# do not insert a spacer at the end
			chg[length(chg)] <- FALSE
			# reverse order so that do not need to adjust other indices
			for (i in rev(which(chg))) { ord <- append(ord, rep(NA, spacer), i) }
		}
		ord
	}

	# map ind back to original indices using the map constructed above
	if (!is.null(filter)) {
		ind1 <- ind
		ind <- backmap$orig[match(ind1, backmap$mapped)]
		ind[is.na(ind1)] <- NA
	}

	ddo <- if (reorder || inherits(rowv, "dendrogram")) dd else NULL
	list(dd = ddo, ind = ind)
}


# reorder data for a heatmap and return appropriate data as a list
hm_reorder <- function(
	x,
	labRow = NA, labCol = NA,
	Rowv = NULL, Colv = NULL, reorder = c(TRUE, TRUE), distfun = dist, hclustfun = hclust,
	rowMembers = NULL, colMembers = NULL, spacer = 1,
	na.rm = TRUE, filter = c(TRUE, TRUE))
{
	# filtering: TRUE (=1.0) = remove rows/columns with only NAs, 0.5 = remove if >= 50% NAs, ...
	dofilter <- function(x, filter) {
		if (filter==TRUE) {
			!apply(is.na(x), 1, all)
		} else if (filter>0) {
			rowSums(is.na(x)) < ncol(x) * filter
		} else {
			NULL
		}
	}
	filter <- rep(filter, length.out=2)
	rowOk <- dofilter(x, filter[1])
	colOk <- dofilter(t(x), filter[2])

	# dendrograms will match the filtered and reordered x returned by hm_reorder, not the original
	# row and column index vectors will contain original indices but only for the rows/columns accepted by the filter, i.e. they will not be "dense"

	roword <- dendro_order(x, rowv=Rowv, rowMembers=rowMembers, distfun=distfun, hclustfun=hclustfun, reorder=reorder[1], na.rm=na.rm, spacer=spacer, filter=rowOk)
	ddr <- roword$dd
#	rowInd <- rev(roword$ind) # !HS! causes problems - dendrograms not in same order, ...
	rowInd <- roword$ind

	colord <- dendro_order(t(x), rowv=Colv, rowMembers=colMembers, distfun=distfun, hclustfun=hclustfun, reorder=reorder[2], na.rm=na.rm, spacer=spacer, filter=colOk)
	ddc <- colord$dd
	colInd <- colord$ind

	# this also implicitly applies the filtering
	x <- x[rowInd, colInd]

	# optionally create and reorder labels
	# also handles NULL rownames(x)/colnames(x) correctly
	# the extra comparison and indexing eliminate warnings due to the is.na being a vector operation
	collab <- if (is.null(labCol)) NULL else if (length(labCol) == 1 && is.na(labCol)[1]) colnames(x) else labCol[colInd]
	rowlab <- if (is.null(labRow)) NULL else if (length(labRow) == 1 && is.na(labRow)[1]) rownames(x) else labRow[rowInd]

	list(ddr=ddr, ddc=ddc, rowInd=rowInd, colInd=colInd, x=x, collab=collab, rowlab=rowlab, reorder=reorder, rowOk=rowOk, colOk=colOk)
}


# data normalization, trimming
# does not reorder the data rows and columns
hm_normalize <- function(x, scale, trim = NULL, na.rm = TRUE) {
    if (scale == "row") {
        x <- sweep(x, 1, rowMeans(x, na.rm = na.rm))
        sd <- apply(x, 1, sd, na.rm = na.rm)
        x <- sweep(x, 1, sd, "/")
    }
    else if (scale == "column") {
        x <- sweep(x, 2, colMeans(x, na.rm = na.rm))
        sd <- apply(x, 2, sd, na.rm = na.rm)
        x <- sweep(x, 2, sd, "/")
    }

    if (!is.null(trim)) {
        lo <- -trim
        hi <- trim
        x[x < lo] = lo
        x[x > hi] = hi
    }

    x
}


# prepare data for a heatmap: scaling, dendrograms, ...
prepare_heatmap_data <- function(
	x,
	labRow = NA, labCol = NA,
	Rowv = NULL, Colv = NULL, reorder = c(TRUE, TRUE), distfun = dist, hclustfun = hclust,
	rowMembers = NULL, colMembers = NULL, spacer = 1,
	scale = "none", trim = NULL,
	zlim=NULL,
	col = NULL,
	filter=c(TRUE, TRUE))
{
	# check data format
	if (length(di <- dim(x)) != 2 || !is.numeric(x))
		stop("`x' must be a numeric matrix")
	nr <- di[1]
	nc <- di[2]
	# !HS! single-row heatmaps cause problems (show as single-column), but will be allowed anyway in special cases
	# single row or column heatmaps are untested, may break something
	if ((nr <= 1 || nc <= 1) && (reorder[1] || reorder[2]))
		stop("`x' must have at least 2 rows and 2 columns")

	# map palette if a short identifier is given
	col <- getPalette(col)

	# reorder data: cluster or use a user-defined order
	res <- hm_reorder(x, labRow=labRow, labCol=labCol, Rowv=Rowv, Colv=Colv, reorder=reorder, distfun=distfun, hclustfun=hclustfun, rowMembers=rowMembers, colMembers=colMembers, spacer=spacer, filter=filter)


	# scale/trim data
	res$x <- x <- hm_normalize(res$x, scale=scale, trim=trim)


	# calculate color breaks
	rng <- range(x, na.rm=TRUE)
	extreme <- if (is.null(trim)) { max(abs(rng), na.rm=TRUE) } else { trim }
	# support both symmetrical (with default NULL) and asymmetrical (NA) automatic zlim
	if (is.null(zlim)) {
		zlim <- c(-1, 1) * extreme
	} else if (is.na(zlim)) {
		zlim <- rng+c(-1,1)*0.01*diff(rng)
	} # else use user-defined zlim
	breaks <- seq(zlim[1], zlim[2], length = length(col)+1)


	# return results
	res <- c(list(zlim=zlim, breaks=breaks, col=col), res)
	res
}


# prepare an editable "call" to the heatmap: list of appropriate parts etc.
# returns the parameters to hlayout as a list
# !HS! what to do about dendrograms etc. if reorder[i] == FALSE ? (not a new problem...) - find a way to omit dendrograms more cleanly
# !HS! axes should be separate (1-D zoomable) components
prepare_heatmap <- function(prep, title="Heatmap", ..., titleheight=0.12, dendroheight=0.25, dendrowidth=0.25, labelheight=0.1, labelwidth=0.1, r.cex=1, c.cex=1, colorkeylabels=FALSE, na.color="gray80") {
	main <- imagepart(prep$x, col=prep$col, breaks=prep$breaks, ColLab=prep$collab, RowLab=prep$rowlab, r.cex=r.cex, c.cex=c.cex, na.color=na.color)
	topextra <- titlepart(main=title, titleheight=titleheight)
	top <- list(dendropart(prep$ddc, height=dendroheight))
	# placeholder for axis in the main part - to make scales and zooming easier
	bottom <- list(emptypart(height=labelheight))
	left <- list(dendropart(prep$ddr, horiz=TRUE, width=dendrowidth))
	# placeholder for axis in the main part - to make scales and zooming easier
	right <- list(emptypart(width=labelwidth))
	topleft <- colorkeypart(prep$x, breaks=prep$breaks, col=prep$col, labels=colorkeylabels)

	list(main=main, topextra=topextra, top=top, bottom=bottom, left=left, right=right, topleft=topleft, topright=NULL, bottomleft=NULL, bottomright=NULL, bottomextra=NULL, ...)
}


# draw a heatmap based on prepared "display list"
draw_heatmap <- function(dl, set.oma=TRUE, ...) {
	# draw the heatmap
	if (set.oma) {
		par(oma=c(1.5, 0.5, 0.5, 0.5))
	}
	pars <- c(dl, list(...))
	res <- do.call("hlayout", pars)
	invisible(res)
}


# zoom to an already drawn heatmap
zoom_heatmap <- function(dl, ...) {
	zz <- locator(2)
	draw_heatmap(dl, ..., zoomx=sort(zz$x), zoomy=sort(zz$y))
}


# helper function for creating a sidebar
# factorpalettefn can be e.g. rainbow (palette function) or "Pastel1" (RColorBrewer palette name) or function(n) rep(c("red", "green", "blue"), length.out=n)
create_sidebar <- function(x, prep, vertical=TRUE, width=lcm(1), label="", na.color="gray80", cleannames=TRUE, cex=1, factorpalettefn="Pastel1") {
	if (cleannames) {
		label <- gsub("_", " ", label)
	}
	if (is.character(factorpalettefn)) {
		# need to copy the name - otherwise the function would resolve factorpalettefn to itself later (function context)
		palettename <- factorpalettefn
		require(RColorBrewer)
		factorpalettefn <- function(n) brewer.pal(n, palettename)
	}
	# check if the column is a factor
	if (is.factor(x)) {
		part <- factorpart(x[if(vertical) prep$rowInd else prep$colInd], label=label, width=width, vertical=vertical, na.color=na.color, cex=cex, palettefn=factorpalettefn)
	} else {
		mat <- if (vertical) {
			matrix(x[prep$rowInd], nc=1)
		} else {
			matrix(x[prep$colInd], nr=1)
		}
		part <- imagepart(mat, col=prep$col, breaks=prep$breaks, label=label, height=width, width=width, na.color=na.color, r.cex=cex, c.cex=cex)
	}
	# !HS! need to copy the environment - clumsy
	tmpenv <- environment(part$FUN)
	environment(part$FUN) <- new.env()
	lapply(ls(tmpenv), function(n) assign(n, get(n, tmpenv), envir=environment(part$FUN), inherits=FALSE))
	environment(part$FUN) <- tmpenv
	part
}



# combined heatmap function
# the result can be used for zooming
# for simple basic cases, picketvar can be given directly
# factor sidebars are supported, but legends are only shown for the first two
heatmap.n <- function(x, main="Heatmap", ..., na.color="gray80", sidebars=NULL, picketdata=NULL, r.cex=1, c.cex=1, titleheight=lcm(1.2), dendroheight=lcm(3), dendrowidth=lcm(3), labelheight=lcm(1), labelwidth=lcm(1), picketheight=0.4, sidebarwidth=lcm(1), sidebar.cex=1, colorkeylabels=FALSE, legendcorner="bottomleft", plot=TRUE, factorpalettefn="Pastel1") {
	prep <- prepare_heatmap_data(x, ...)
	dl <- prepare_heatmap(prep, title=main, titleheight=titleheight, dendroheight=dendroheight, dendrowidth=dendrowidth, labelheight=labelheight, labelwidth=labelwidth, r.cex=r.cex, c.cex=c.cex, colorkeylabels=colorkeylabels, na.color=na.color)
	# picket part only at bottom by default
	if (!is.null(picketdata)) {
		pickpart <- picketpart(addvar = picketdata[prep$colInd, ], height=picketheight)
		dl$bottom <- c(dl$bottom, list(pickpart))
	}
	if (!is.null(sidebars)) {
		# add legends for the first 1-2 factor sidebars
		parts_for_legend <- list()
		lapply(c("bottom", "left", "top", "right"), function(side) {
			if (!is.null(sidebars[[side]])) {
				sbs <- lapply(seq(along=sidebars[[side]]), function(col) {
					create_sidebar(x=sidebars[[side]][, col], prep=prep, vertical=(side %in% c("left", "right")), label=names(sidebars[[side]])[col], width=sidebarwidth, na.color=na.color, cex=sidebar.cex, factorpalettefn=factorpalettefn)
				})
				# for bottom and right sidebars, try to avoid writing over the axis text
				# !HS! this is suboptimal - moving the axis to be a separate component would be better
				if (side %in% c("right", "bottom")) {
					# need to assign to the parent
					dl[[side]] <<- c(dl[[side]][1], sbs, dl[[side]][-1])
				} else {
					# need to assign to the parent
					dl[[side]] <<- c(sbs, dl[[side]])
				}
				# add legends for the first 1-2 factor sidebars, ignoring further ones
				for (i in seq(along=sbs)) {
					# assign to parent
					if (!is.null(sbs[[i]]$fct)) parts_for_legend <<- c(parts_for_legend, list(sbs[[i]]))
				}
				# limit to 2, add part
				# need to assign to parent
				if (length(parts_for_legend) > 2) parts_for_legend <<- parts_for_legend[1:2]
			}
		})
		llabels <- lapply(parts_for_legend, function(x) levels(x$fct))
		lcols <- lapply(parts_for_legend, function(x) x$col)
		dl[[legendcorner]] <- legendpart(llabels, col=lcols)
	}
	if (plot) {
		draw_heatmap(dl)
	}
	invisible(dl)
}

# test:
#dl <- heatmap.n(m, sidebars=list(left=data.frame(min=apply(m, 1, min), max=apply(m, 1, max), fact=factor(letters[c(1,2,1,2,1,2,1,2,1,2,1,2)])), top=data.frame(min=apply(m, 2, min), max=apply(m, 2, max), fact2=factor(c("first", "second", "third")[rep(1:3, length.out=10)]))), picketdata=pickdata, picketheight=lcm(2))

