#' @title Compress Field into Fewer Categories
#' @description Merge the less common terms into a single group
#' @param x Vector or factor.
#' @param keep Number of top entries to include; or a character vector 
#' @param rest Name to be used for the remaining entries
#' @return Compressed version of the input.
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples
#'   p <- compress_field(c("A", "A", "A", "B","B","C", "D"),
#'          keep = 2, rest = "Other")
#'   p <- compress_field(c("A", "A", "A", "B","B","C", "D"),
#'          keep = c("A", "B), rest = "Other")
#' @keywords utilities
compress_field <- function (x, keep, rest = "Other") {

  if (is.numeric(keep)) {	         
    top <- names(top(x, n = keep, na.rm = TRUE))
  } else {
    top <- keep
  }
  x <- as.character(x)
  x[which(!x %in% top)] <- rest
  x <- factor(x, levels = c(top, rest))
  x
  
}
