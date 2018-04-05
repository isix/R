# bloomfilter.R
# kasterma — Jun 9, 2014, 10:40 AM

library(digest)
library(memoise)
library(testthat)

#' indices
#' 
#' Test function to compute the set of indices used for an item in a
#' bloomfilter
#' 
#' @param bf the bloomfilter
#' @param x the element to index
indices.test <- function(bf, x) {
  # no reason to presume this is reasonable, but it is easy to implement
  d1 <- as.integer(paste("0x",substr(digest(x, algo='sha256'), 1, 5), sep=""))
  d2 <- as.integer(paste("0x",substr(digest(x, algo='sha1'), 1, 5), sep=""))
  sapply(X=seq(bf$numhash)-1, function(i) {(d1 + i * d2) %% bf$buckets})
}

expect_equal(indices.test(list(numhash=3, buckets=10), 2), c(0,9,8))

#' new.bloomfilter
#' 
#' @param size the number of bits in the filter
#' @param indices the index computation function to use
new.bloomfilter <- function(size, numhash, indices=indices.test) {
  structure(list(bits=rep(FALSE, size),
                 buckets=size,
                 numhash=numhash,
                 indices=indices), class="bloomfilter")
}

#' add.bloomfilter
#' 
#' add an element to the bloomfiler
#' 
#' @param bf bloomfilter
#' @param x element to add
add.bloomfilter <- function(bf, x) {
  bf$bits[bf$indices(bf,x)] <- TRUE
  bf
}

#' check.bloomfilter
#' 
#' check membership of an element a bloomfilter
#' 
#' @param bf bloomfilter
#' @param x element to check is in the bloomfilter
check.bloomfilter <- function(bf, x) {
  all(bf$bits[bf$indices(bf,x)])
}

#' make.ideal.hash
#' 
#' Create an ideal hash function (as ideal as the pseudo random number 
#' generation used).  It randomly assigns values, and once a value is assigned
#' it is remembered for the remainder of the session.
#' 
#' @param max max value to output, i.e. range of output values is seq(max)
make.ideal.hash <- function(max) {
  memoise(function(x) {sample(seq(max), size=1)})
}

#' frac.filled
#' 
#' Compute the fraction of bits set in the bloomfilter
#' 
#' @param bf bloomfilter
frac.filled <- function(bf) {
  bs <- bf$bits
  sum(bs)/length(bs)
}

###########
# Example #
###########

mybf <- new.bloomfilter(150, numhash = 2, indices=indices.test)
mybf <- add.bloomfilter(mybf, "Isaias")
mybf <- add.bloomfilter(mybf, "Maria Paula")
mybf <- add.bloomfilter(mybf, "Ana Maria")

# Is "Ana" there?
check.bloomfilter(mybf, "Ana")
# Is "Ana Maria" there?
check.bloomfilter(mybf, "Ana Maria")
# Is "Isaias" there?
check.bloomfilter(mybf, "Isaias")
# Is a mispelling "Isaias" there?
check.bloomfilter(mybf, "Izaias")

particle <- function(x, y) substr(x,y,y+1)

bigrams <- function(x, SPACE = TRUE) {
	if (SPACE) {
		x <- paste(" ", x," ", sep="")
	}
	vx <- 1:(nchar(x)-1)
	ans <- as.array(sapply(vx, function(x, y) substr(y,x,x+1), y = x))
	return(ans)
}

bigram2bf <- function(bf, xbigram) {
	for (i in xbigram) bf <- add.bloomfilter(bf, i)
	return(bf)
}

dice.comp <- function(x, y) {
	A <- sum(x$bits)
	B <- sum(y$bits)
	C <- sum(x$bits & y$bits)
	ans <- (2 * C)/(A+B)
	return(ans)
}

bf2string <- function(x) paste(x$bits*1, collapse = "")


mybf <- new.bloomfilter(150, numhash = 2, indices=indices.test)
mybf <- bigram2bf(mybf, bigrams("Isaias Valente Prestes"))
mybf$bits

mybf2 <- new.bloomfilter(150, numhash = 2, indices=indices.test)
mybf2 <- bigram2bf(mybf2, bigrams("Izaias Valente Prestes"))
mybf2$bits

dice.comp(mybf, mybf2)

mybf <- new.bloomfilter(150, numhash = 2, indices=indices.test)
mybf <- bigram2bf(mybf, bigrams("Maria Pereira"))
mybf$bits

mybf2 <- new.bloomfilter(150, numhash = 2, indices=indices.test)
mybf2 <- bigram2bf(mybf2, bigrams("Izaias Valente Prestes"))
mybf2$bits

dice.comp(mybf, mybf2)

mybf$bits == mybf2$bits



