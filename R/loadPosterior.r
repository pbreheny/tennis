loadPosterior <- function(side, year) {
  load(paste0("psm/", side, "/", year-1, ".RData"))
  pastEta <- Eta
  load(paste0("psm/", side, "/", year, ".RData"))
  ind <- match(rownames(Eta), rownames(pastEta))
  Eta <- cbind(pastEta[ind,], Eta)

  list(Delta=Delta, Alpha=Alpha, Eta=Eta)
}
