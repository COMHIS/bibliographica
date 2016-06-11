#' @title Pick Substring Indicated by Separator
#' @description Split the string by separator and pick the former or latter part.
#' @param x Input string
#' @param sep Separator string
#' @param which Indicate which part to pick
#' @return Polished string
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples x2 <- splitpick("London re Edinburgh", " re ", 1)
#' @keywords utilities
splitpick <- function (x, sep, which) {
  # london re edinburgh -> london
  inds <- grep(sep, x)
  if (length(inds) > 0) {
    spl <- unlist(strsplit(x[inds], sep), use.names = FALSE)
    x[inds] <- spl[[which]]
  }

  x
  
}
