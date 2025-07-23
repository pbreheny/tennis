fix_name <- function(x) {
  y <- rep("", length(x))
  x <- gsub("\\xa0", " ", x) ## Fix non-breaking space issue

  for (i in 1:length(x)) {
    z <- gsub(" \\(.*", "", x[i])
    if (z %in% .special_names[,1]) {
      y[i] <- .special_names[.special_names[,1] == z, 2]
    } else {
      z <- unlist(strsplit(z, " "))
      y[i] <- glue("{z[1]} {substr(z[2], 1, 1)}.")
    }
  }
  y
}
fix_name <- function(x) {
  y <- x
  for (i in 1:length(x)) {
    if (x[i] %in% .fixNames[,1]) {
      y[i] <- .fixNames[.fixNames[,1]==x[i],2]
    }
  }
  y
}
