#' @title Remove dimension data
#' @description Remove dimension data
#' @param x A character vector that may contain dimension information
#' @return The character vector with dimension information removed
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # remove_dimension("4to 40cm", sheet_sizes())
#' @keywords internal
remove_dimension <- function (x, terms) {

  x <- sapply(x, function(xi) remove_terms(xi, terms))
  x[x == ""] <- NA
  
  x

}


