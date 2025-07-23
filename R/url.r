urlTour <- function(url, surface="Hard", sets=5, just.bracket=FALSE, roundID, ...) {
  suppressMessages(require(XML))
  p <- NULL
  if (grepl("atpworldtour.com", url)) {
    p <- urlATP(url, roundID)
  } else if (grepl("MDS.pdf", url)) {
    p <- urlPDF(url, roundID, ...)
  }
  p <- fixName(p)
  if (just.bracket) return(p)
  predictTour(p, surface, Delta, Alpha, sets=sets)
}
urlATP <- function(url, roundID) {
  tab <- readHTMLTable(url, header=FALSE, stringsAsFactors=FALSE)
  if (missing(roundID)) {
    tab <- tab[-(1:3)]
  } else {
    raw <- tab$scoresDrawTable[,roundID]
    raw <- raw[grep("H2H", raw)]
    raw <- gsub("\\r", "", raw)
    raw <- gsub("\\n", "", raw)
    raw <- gsub("\\t", "", raw)
    raw <- gsub("H2H", "", raw)
    raw <- gsub("[[:digit:]]+", "", raw)
    raw <- trimws(raw)
    n <- length(raw)/2
    tab <- vector("list", n)
    for (i in 1:n) {
      tab[[i]] <- matrix("NA", 3, 2)
      tab[[i]][c(1,3),1] <- raw[1:2 + (i-1)*2]
    }
  }
  n <- length(tab)
  P <- matrix("", 2, n)
  p <- NULL
  for (j in 1:n) {
    if (all(dim(tab[[j]]) == 2:3)) {
      tmp <- matrix(NA, 3, 2)
      tmp[1,1] <- tab[[j]][1,3]
      tmp[3,1] <- tab[[j]][2,3]
      tab[[j]] <- tmp
    }
    p <- c(p, tab[[j]][c(1,3),1])
  }
  p
}
urlPDF <- function(url, roundID, lines) {
  tf <- tempfile()
  PDF <- paste0(tf, ".pdf")
  TXT <- paste0(tf, ".txt")
  system(paste("wget", url, "-O", PDF))
  system(paste("pdftotext", PDF))
  raw <- readLines(TXT)
  if (!missing(lines)) {
    tmp <- raw[lines]
    tmp <- gsub("BYE", "BYE, B", tmp)
    tmp <- gsub("QUALIFIER", "QUALIFIER, Q", tmp)
    X <- matrix(unlist(strsplit(tmp, ",")), byrow=TRUE, ncol=2)
    #X[,1] <- splitright(X[,1], "\\.")
    X[,1] <- trimws(sapply(X[,1], capwords))
    X[,2] <- gsub("\\..*", "", X[,2])
    X[,2] <- gsub("\\[", "", X[,2])
    X[,2] <- gsub("\\]", "", X[,2])
    X[,2] <- gsub("[[:digit:]]+", "", X[,2])
    X[,2] <- trimws(X[,2])
    p <- apply(X[,2:1], 1, paste, collapse=" ")
    p <- gsub("B Bye", "Bye", p)
    p <- gsub("Q Qualifier", "Qualifier", p)
  } else {
    ind <- grep("Round 1", raw)
    p <- NULL
    for (i in 1:length(ind)) {
      nextBlank <- which(raw[-(1:ind[i])]=="")[1]
      tmp <- raw[(ind[i]+1):(ind[i]+nextBlank-1)]
      X <- matrix(unlist(strsplit(tmp, ",")), byrow=TRUE, ncol=2)
      X[,1] <- splitright(X[,1], "\\.")
      X[,1] <- trimws(sapply(X[,1], capwords))
      X[,2] <- gsub("\\..*", "", X[,2])
      X[,2] <- gsub("\\[", "", X[,2])
      X[,2] <- gsub("\\]", "", X[,2])
      X[,2] <- gsub("[[:digit:]]+", "", X[,2])
      X[,2] <- trimws(X[,2])
      p <- c(p, apply(X[,2:1], 1, paste, collapse=" "))
    }
  }
  p
}
capwords <- function(x) {
  x <- tolower(x)
  for (sep in c(" ", "-")) {
    s <- strsplit(x, sep)[[1]]
    x <- paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = sep)
  }
  x
}
