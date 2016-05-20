#' @title Name Initials
#' @description Pick name initials
#' @param x a vector of names
#' @param format Name format. By default interpreted as First Second Last, with the corresponding initials FSL.
#' @return A vector of initials
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples name_initials("Thomas Albert")
#' @keywords utilities
name_initials <- function (x, format = NULL) {

  x <- as.character(x)

  if (is.null(format)) {
    initials <- sapply(strsplit(x, " "), function (x) {paste(substr(x, 1, 1), collapse = " ")}, USE.NAMES = FALSE)
  }

  initials[initials == "NA"] <- NA

  initials

}


