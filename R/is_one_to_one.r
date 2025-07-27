#' Check whether x and y form a 1:1 map
#' 
#' @param x,y Any two vectors, although numeric vectors could behave erratically
#' 
#' @examples
#' is_one_to_one(
#'   c("a", "b", "a", "c"),
#'   c(1, 2, 1, 3)
#' )
#' is_one_to_one(
#'   c("a", "b", "a", "c"),
#'   c(1, 2, 3, 4)
#' )

is_one_to_one <- function(x, y) {
  dat <- data.table(x, y)
  all(dat[, uniqueN(y), x]$V1 == 1) &&
    all(dat[, uniqueN(x), y]$V1 == 1)
}
