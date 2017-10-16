#' @title Get Decade
#' @description Round year into decade.
#' @param x a numeric variable 
#' @return Decade. A numeric.
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples decade(1962)
#' @keywords utilities
decade <- function (x) {
  x <- as.numeric(as.character(x))
  floor(x/10) * 10
}

#' @title Get Century
#' @description Round year into century.
#' @param x a numeric variable 
#' @return Century. A numeric.
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples century(1962)
#' @keywords utilities
century <- function (x) {
  x <- as.numeric(as.character(x))
  floor(x/100) * 100
}