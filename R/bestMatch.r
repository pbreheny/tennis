bestMatch <- function(url, surface="Hard", n=3, sets=5) {
  res <- urlTour(url, surface, just.bracket=TRUE)
  x <- Delta + Alpha[, surface]
  D <- matrix(x[res], nrow=2)
  ind <- order(apply(D, 2, sum), decreasing=TRUE)[1:n]
  Names <- res[rep(ind*2, each=2) + rep(c(-1:0), n)]
  nm <- matrix(Names, ncol=2, byrow=TRUE)
  X <- matrix(x[Names], ncol=2, byrow=TRUE)
  X <- cbind(X, apply(X, 1, sum))
  X <- cbind(X, apply(apply(nm, 1, function(x) predictMatch(x[1], x[2], surface=surface, sets=sets)), 2, max))
  rownames(X) <- apply(nm, 1, paste, collapse=' v ')
  X
}
