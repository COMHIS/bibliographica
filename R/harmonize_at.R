
#' @title harmonize_at
#' @description Handle at statements
#'
#' @param x A vector
#' @return Polished vector
#'
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{x2 <- harmonize_at(x)}
#' @keywords utilities
harmonize_at <- function (x) {

  # Remove at from beginning of line
  x <- gsub("^at ", "", x)
  x <- gsub("^At ", "", x)
  x <- gsub("^at. ", "", x)
  x <- gsub("^At. ", "", x)

  # Remove at from end of line
  x <- gsub("at$", "", x)
  x <- gsub("At$", "", x)

  x <- remove_trailing_periods(x)

  x


}


