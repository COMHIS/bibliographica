#' @title Compress Field into Fewer Categories
#' @description Merge the less common terms into a single group
#' @param x Vector or factor.
#' @param topn Number of top entries to include; or a character vector 
#' @param min.freq Minimum frequency for the included groups.
#' @param rest Name to be used for the remaining entries
#' @return Compressed version of the input.
#' @export
#' @details With the min.freq argument, groups that have less items
#'          that this will be merged.
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @aliases collapse
#' @examples
#'   p <- compress_field(c("A", "A", "A", "B","B","C", "D"),
#'          topn = 2, rest = "Other")
#'   p <- compress_field(c("A", "A", "A", "B","B","C", "D"),
#'          topn = c("A", "B"), rest = "Other")
#' @keywords utilities
compress_field <- function (x, topn = NULL, min.freq = NULL, rest = "Other") {

  # keep was replace with topn

  if (!is.null(topn) && !is.null(min.freq)) {
    stop("Both topn and min.freq given; min.freq will be used in this
          case.")
  }

  if (!is.null(min.freq)) {
    topn <- names(which(table(x) >= min.freq))
  }
  
  if (is.numeric(topn)) {	         
    top <- names(top(x, n = topn, na.rm = TRUE))
  } else {
    top <- topn
  }
  x <- as.character(x)
  x[which(!x %in% top)] <- rest
  x <- factor(x, levels = c(top, rest))
  x
  
}

