
#' Capitalize the first letter
#'
#' @param x a character vector to capitalize
#' @return Capitalized character vector
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples capitalize("print")
#' @keywords utilities
capitalize <- function (x) {
  for (a in letters) {
    x <- gsub(paste0("^", a), toupper(a), x)
  }
  x
}
