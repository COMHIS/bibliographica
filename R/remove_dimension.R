#' Remove dimension data
#'
#' @param x A character vector that may contain dimension information
#' @return The character vector with dimension information removed
#'
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples # remove_dimension("4to 40cm", sheet_sizes())
#' @export
#' @keywords internal
remove_dimension <- function (x) {

  # FIXME: check relationship with remove_dimension_info		 

  # Remove commonly used volume formats
  f <- system.file("extdata/remove_dimension.csv", package = "bibliographica")
  terms <- as.character(read.csv(f)[,1])
  for (term in terms) {
    x <- gsub(term, " ", x)
  }
  if (is.na(x) || x == "") {x <- NA}

  x

}


