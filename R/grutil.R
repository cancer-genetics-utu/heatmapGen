hmcols <- function() {
	rgb(
		c(rep(0, 128), seq(1, 255, length.out=127)),
		c(seq(255, 1, length.out=127), rep(0, 128)),
		0,
		max = 255
	)
}
