#' @title pick_firstname
#' @description Pick first name from full name, assuming the format is known 
#' @param x a vector of full names
#' @param format name format
#' @return a vector of first names
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples pick_firstname("Hobbes, Thomas")
#' @keywords utilities
pick_firstname <- function (x, format = "last, first") {

  x <- as.character(x)

  if (format == "last, first") {
    first <- sapply(x, function (x) {y <- unlist(strsplit(x, ", ")); if (length(y)>1) y[[2]] else NA}) 
  } else if (format == "first last") {
    first <- sapply(x, function (x) {unlist(strsplit(x, " "))[[1]]})
  } else {
    stop("Correct the unknown format in pick_firstname function.")
  }

  # Remove possible life year info
  first <- gsub(" \\([0-9|N|A]+-[0-9|N|A]+\\)", "", first)

  first

}

