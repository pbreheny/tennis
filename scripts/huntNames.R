side <- "wta"
year <- 2012
Data <- import(year, side)
prior <- makePrior(Data$PlayerID, side, year)
Data$PlayerID[prior$new==1]

last <- sapply(Data$PlayerID[prior$new==1], function(x) strsplit(x, " ")[[1]][1])
for (i in 1:length(last)) {
  l <- last[i]
  ind <- grep(l, names(prior$muEta))
  if (length(ind)) {
    cat("Not found:", names(last)[i], "\n")
    cat("Possible matches:", Data$PlayerID[ind], "\n")
  }
}
