clustersub <- function(x, members, spacer=1, dmethod="euclidean", clmethod="complete") {
  # index vector so that can make sure all operations use same order
  idxs <- unclass(by(1:nrow(x), members, function(i) i))
  forest <- lapply(idxs, function (sgi) hclust(dist(x[sgi, ], method=dmethod), method=clmethod))

  # construct order of all rows with optional spacers
  ord.parts <- lapply(1:length(idxs), function(i) idxs[[i]][forest[[i]]$order])
  ord <- c(lapply(ord.parts, function(p) c(p, rep(NA, spacer))), recursive=TRUE)
  ord <- ord[-length(ord)]

  # return results
  list(forest=forest, ord=ord, spacer=spacer)
}

# if clustering of subgroups is not desired (e.g. kmeans only), ord can be created by this function
members2ord <- function(members, spacer=1) {
  idxs <- unclass(by(1:length(members), members, function(i) i))
  ord <- c(lapply(idxs, function(p) c(p, rep(NA, spacer))), recursive=TRUE)
  ord <- ord[-length(ord)]
}
