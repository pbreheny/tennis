.predictOuter <- function(x, y, sets) {
  val <- pbinom(sets/2, prob=ilogit(x-y), size=sets, lower.tail=FALSE)
  val
}
predictTourMatch <- function(x, y, wx, wy, sets) {
  P <- outer(x, y, .predictOuter, sets=sets)
  W <- outer(wx, wy)
  mx <- apply(W*P, 1, sum)
  my <- apply(W*(1-P), 2, sum)
  c(mx, my)
}
predictTour <- function(player, surface, Delta, Alpha, sets=5, warn=TRUE) {
  n <- length(player)
  rounds <- log2(n)

  ## Setup x
  x <- numeric(length(player))
  names(x) <- player
  inModel <- player[player %in% names(Delta)]
  x[inModel] <- Delta[inModel] + Alpha[inModel, surface]
  if (any(player=="Bye")) x[player=="Bye"] <- -100
  miss <- setdiff(player, c(names(Delta), "Bye"))
  if (warn & length(miss)) {
    cat("Missing players:", miss, "\n")
    x[miss] <- -1.5
  }

  val <- vector("list", rounds)
  for (i in 1:rounds) {
    m <- n/(2^i)
    val[[i]] <- vector("list", m)
    for (j in 1:m) {
      A <- matrix(1:n, nrow=2^i)
      ind.x <- A[1:2^(i-1),j]
      ind.y <- A[(2^(i-1)+1):(2^i),j]
      if (i==1) {
        val[[i]][[j]] <- predictTourMatch(x[ind.x], x[ind.y], 1, 1, sets=sets)
      } else {
        val[[i]][[j]] <- predictTourMatch(x[ind.x], x[ind.y], val[[i-1]][[2*j-1]], val[[i-1]][[2*j]], sets=sets)
      }
    }
  }
  val
}

winnerProb <- function(res, n, dec=1) {
  if (missing(n)) {
    n <- length(res)
    x <- sort(res[[n]][[1]], decreasing=TRUE)
  } else {
    x <- sort(unlist(res[[n]]), decreasing=TRUE)
  }
  round(100*x, dec)
}
