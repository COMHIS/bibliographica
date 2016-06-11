#' @title Pick First Name
#' @description Pick first name from full name, assuming the format is known 
#' @param x a vector of full names
#' @param format name format
#' @param keep.single If the name is without comma ('Shakespeare,
#'  William' versus 'William'), interpret the name as first name.
#'  Note that in this case also 'Shakespeare' will be interpreted as first name.
#' @return a vector of first names
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples pick_firstname("Hobbes, Thomas")
#' @keywords utilities
pick_firstname <- function (x, format = "last, first", keep.single = FALSE) {

  xorig <- as.character(x)
  xuniq <- unique(xorig)
  x <- xuniq

  if (format == "last, first") {
    first <- sapply(x, function (x) {y <- unlist(strsplit(x, ", "), use.names = FALSE); if (length(y)>1) y[[2]] else if (keep.single) {y[[1]]} else {NA} }, USE.NAMES = FALSE)
  } else if (format == "first last") {
    first <- sapply(x, function (x) {unlist(strsplit(x, " "), use.names = FALSE)[[1]]}, USE.NAMES = FALSE)
  } else {
    stop("Correct the unknown format in pick_firstname function.")
  }

  # Remove possible life year info
  first <- gsub(" \\([0-9|N|A]+-[0-9|N|A]+\\)", "", first)
  first <- condense_spaces(first)

  first[match(xorig, xuniq)]

}


