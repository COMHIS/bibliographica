#' @title Compress Field into Fewer Categories
#' @description Merge the less common terms into a single group
#' @param x Vector or factor.
#' @param n Number of top entries to include
#' @param rest Name to be used for the remaining entries
#' @return Compressed version of the input.
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples p <- compress_field(c("A", "A", "A", "B","B","C", "D"), n = 2, rest = "Other")
#' @keywords utilities
compress_field <- function (x, n, rest = "Other") {

  top <- names(top(x, n = n, na.rm = TRUE))
  x <- as.character(x)
  x[which(!x %in% top[1:n])] <- rest
  x <- factor(x, levels = c(top, rest))
  x
  
}
