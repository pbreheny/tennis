make_prior <- function(id, side, year) {
  f <- paste0("psm/", side, "/", min(year)-1, ".RData")
  if (file.exists(f)) {
    load(f)
  } else {
    Eta <- sdEta <- matrix(NA, ncol=3)
    Alpha <- sdAlpha <- matrix(NA, ncol=3)
    rss <- rep(10, 3)
    tdf <- rep(1, 3)
  }
  ind <- match(id, rownames(Eta))
  new <- is.na(ind)
  muEta <- Eta[ind, ncol(Eta)]
  tauEta <- sdEta[ind, ncol(Eta)]^(-2)
  muAlpha <- Alpha[ind, ]
  tauAlpha <- sdAlpha[ind, ]^(-2)
  muEta[new] <- tauEta[new] <- 0
  muAlpha[new,] <- tauAlpha[new,] <- 0

  list(muEta=muEta, tauEta=tauEta, muAlpha=muAlpha, tauAlpha=tauAlpha, rss=rss, tdf=tdf, new=1*new)
}
