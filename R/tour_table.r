tourTable <- function(res, name) {
  d <- length(res)
  n <- length(res[[d]][[1]])
  Tab <- matrix(NA, n, d)
  rownames(Tab) <- names(res[[d]][[1]])
  for (i in 1:d) {
    for (j in 1:length(res[[i]])) {
      n.j <- length(res[[i]][[j]])
      ind <- 1:n.j + (j-1)*n.j
      Tab[ind,i] <- res[[i]][[j]]
    }
  }
  rNames <- c("R128", "R64", "R32", "R16", "Quarterfinal", "Semifinal", "Final", "Winner")
  colnames(Tab) <- rNames[(8-d+1):8]
  Tab <- Tab[order(Tab[,"Winner"], decreasing=TRUE),]
  Tab <- Tab[!(substr(rownames(Tab), 1, 3)=="NA "),]
  print(htmlTable(Tab, class="\"sortable ctable\""), name=name)
}
