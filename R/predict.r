predictMatch <- function(player1, player2, surface="Hard", sets=5, verbose=FALSE) {
  eta <- Delta[player1] + Alpha[player1, surface] - Delta[player2] - Alpha[player2, surface]
  p <- pbinom(sets/2, prob=ilogit(eta), size=sets, lower.tail=FALSE)
  val <- c(p, 1-p)
  names(val) <- c(player1, player2)
  if (verbose) {
    nch <- max(nchar(names(val)))
    Names <- format(names(val), width=nch, justify='left')
    Pct <- sprintf('%4.1f', 100*val)
    cat("Overall:", '\n')
    for (i in 1:2) cat(Names[i], ': ', Pct[i], '\n', sep='')
    if (sets==3) {
      cat('\n', format('', width=nch), '   In2  In3\n', sep='')
      for (i in 1:2) {
        d <- dbinom(0:3, size=sets, ilogit(c(1,-1)[i]*eta))
        p <- c(d[4] + d[3]/3, 2/3*d[3])
        Pct <- sprintf('%4.1f', 100*p)
        cat(Names[i], ': ', Pct[1], ' ', Pct[2], '\n', sep='')
      }
    } else {
      cat('\n', format('', width=nch), '   In3  In4  In5\n', sep='')
      for (i in 1:2) {
        d <- dbinom(0:5, size=sets, ilogit(c(1,-1)[i]*eta))
        p <- c(d[6] + 2/5*d[5] + 1/10*d[4], 3/5*d[5] + 3/10*d[4], 6/10*d[4])
        Pct <- sprintf('%4.1f', 100*p)
        cat(Names[i], ': ', Pct[1], ' ', Pct[2], ' ', Pct[3], '\n', sep='')
      }
    }
    return(invisible(val))
  }
  val
}
