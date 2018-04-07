permutation.test.discrete <-
function (x, y=NULL, scores, alternative="greater", trials=1000) 
{
	# set up x and y properly
	if(length(y)) {
		n <- length(y)
		if(length(x) != n) stop("x and y have different lengths")
	} else {
		if(ncol(x) != 2)
			stop("x does not have 2 columns and y is missing")
		y <- x[, 2]
		x <- x[, 1]
		n <- length(y)
	}
	x <- as.character(x)
	y <- as.character(y)

	# set up alternative
	if(length(alternative) != 1 || !is.character(alternative))
		stop("alternative must be a single character string")
	altnum <- pmatch(alternative, c("greater", "less"), nomatch=NA)
	if(is.na(altnum)) 
		stop("alternative must partially match 'greater' or 'less'")
	alternative <- c("greater", "less")[altnum]

	# set up scores properly
	orig.tab <- table(x, y)
	otd <- dim(orig.tab)
	odnam <- dimnames(orig.tab)
	scnam <- dimnames(scores)
	if(!is.matrix(scores) || length(scnam) != 2 || !is.numeric(scores)) 
		stop("scores must be a numeric matrix with dimnames")
	scd <- dim(scores)
	if(any(scd != otd) && any(rev(scd) != otd)) {
		stop(paste("scores is not the proper size, should be",
			otd[1], "by", otd[2]))	
	}
	if(any(scd != otd)) {
		scores <- t(scores)
		scd <- dim(scores)
		scnam <- dimnames(scores)
		reverse <- TRUE
	} else {
		reverse <- FALSE
	}
	rownum <- match(scnam[[1]], odnam[[1]], nomatch=NA)
	if(any(is.na(rownum))) {
		if(reverse || otd[1] != otd[2])
			stop("bad dimnames for scores")
		scores <- t(scores)
		scd <- dim(scores)
		scnam <- dimnames(scores)
		rownum <- match(scnam[[1]], odnam[[1]], nomatch=NA)
		if(any(is.na(rownum))) stop("bad dimnames for scores")
	}
	colnum <- match(scnam[[2]], odnam[[2]], nomatch=NA)
	if(any(is.na(colnum))) stop("bad dimnames for scores")
	scores <- scores[rownum, colnum]

	# the main event
	ranseed <- .Random.seed
	orig.score <- sum(orig.tab * scores)
	perm.scores <- numeric(trials)
	for(i in 1:trials) {
		perm.scores[i] <- sum(table(x, sample(y)) * scores)
	}

	if(alternative == "greater") {
		extreme <- sum(perm.scores >= orig.score)
	} else {
		extreme <- sum(perm.scores <= orig.score)
	}
	ans <- list(original.score=orig.score, perm.scores=perm.scores,
		stats=c(nobs=n, trials=trials, extreme=extreme),
		alternative=alternative, random.seed=ranseed, call=match.call())
	class(ans) <- "permtstBurSt"
	ans
}
permutation.test.fun <-
function (x, y=NULL, fun=function(x, y) sum(x * y), alternative="greater", 
	trials=1000) 
{
	# set up x and y properly
	if(length(y)) {
		n <- length(y)
		if(length(x) != n) stop("x and y have different lengths")
		if(!is.numeric(y)) stop("y must be numeric")
	} else {
		if(ncol(x) != 2)
			stop("x does not have 2 columns and y is missing")
		x <- as.matrix(x)
		if(!is.numeric(x)) stop("x must be numeric")
		y <- x[, 2]
		x <- x[, 1]
		n <- length(y)
	}

	# set up alternative
	if(length(alternative) != 1 || !is.character(alternative))
		stop("alternative must be a single character string")
	altnum <- pmatch(alternative, c("greater", "less"), nomatch=NA)
	if(is.na(altnum)) 
		stop("alternative must partially match 'greater' or 'less'")
	alternative <- c("greater", "less")[altnum]

	# the main event
	ranseed <- .Random.seed
	orig.score <- fun(x, y)
	if(length(orig.score) != 1) stop("fun must return a single number")

	perm.scores <- numeric(trials)
	for(i in 1:trials) {
		perm.scores[i] <- fun(x, sample(y))
	}

	if(alternative == "greater") {
		extreme <- sum(perm.scores >= orig.score)
	} else {
		extreme <- sum(perm.scores <= orig.score)
	}
	ans <- list(original.score=orig.score, perm.scores=perm.scores,
		stats=c(nobs=n, trials=trials, extreme=extreme),
		alternative=alternative, random.seed=ranseed, call=match.call())
	class(ans) <- "permtstBurSt"
	ans
}
plot.permtstBurSt <-
function (x, col=c("black", "red"), width=10, uniqlim=40, main="", 
	xlab="Scores", col.hist="yellow", ...) 
{
	orig.score <- x$original.score
	ulen <- length(unique(x$perm.scores))
	if(ulen > uniqlim) {
		hist(x$perm.scores, xlim=range(x$perm.scores, orig.score),
			 main=main, xlab=xlab, col=col.hist,  ...)
		box()
		abline(v=orig.score, col=col[2])
	} else {
		ptab <- table(x$perm.scores)
		vals <- as.numeric(names(ptab))
		if(x$alternative == "greater") {
			extreme <- vals >= orig.score
		} else {
			extreme <- vals <= orig.score
		}
		if(all(vals > orig.score) || all(vals < orig.score)) {
			xrng <- range(vals, orig.score)
		} else {
			xrng <- range(vals)
		}
		plot(vals, as.vector(ptab), type="n", xlab=xlab, ylab="Count", 
			xlim=xrng, ...)
		points(vals[!extreme], ptab[!extreme], type="h",
			col=col[1], lwd=width, ...)
		points(vals[extreme], ptab[extreme], type="h",
			col=col[2],  lwd=width, ...)
		if(nchar(main)) title(main=main)
	}
}
print.permtstBurSt <-
function (x, digits=4, ...) 
{
	cat("Call:\n")
	print(x$call)
	cat("\nOriginal value:", x$original.score, "  Number of observations:",
		x$stats["nobs"], "\n")
	cat("Number of random permutations:", x$stats["trials"], 
		"  Alternative:", x$alternative, "  p-value:", 
		round(x$stats["extreme"] / x$stats["trials"], digits), "\n")
	invisible(x)
}
superscore <-
structure(c(1, 0, 0, 1), .Dim = c(2L, 2L), .Dimnames = list(c("Up", 
"Down"), c("National", "American")))
