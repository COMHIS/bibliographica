#' @title remove_time_info
#' @description Remove time information
#'
#' @param x Vector (time field) 
#' @return Polished vector
#'
#' @export
#' @details Remove months, year terms and numerics
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{x2 <- remove_time_info(x)}
#' @keywords utilities
remove_time_info <- function (x) {

  months2 <- c("Ianuary", 
  	       "Janur.",
  	       "Iune",
  	       "Iuly",
  	       "Aug.",
  	       "N.vemb",
  	       "Novemb",
  	       "Octob",
  	       "Decemb")

  # Remove months
  months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

  # other time information
  terms <- c("Anno.", "An. Do.", "year")

  toremove <- c(months, months2, terms)

  x <- remove_terms(x, toremove)

  x

}

