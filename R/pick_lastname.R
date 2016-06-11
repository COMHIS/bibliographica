#' @title Pick Last Name
#' @description Pick last name from full name, assuming the format is known 
#' @param x a vector of full names
#' @param format name format
#' @param keep.single If the name is without comma ('Shakespeare,
#'  William' versus 'William'), interpret the name as first name.
#'  Note that in this case also 'Shakespeare' will be interpreted as first name.
#'  In the current implementation keep.single is always TRUE.
#' @return a vector of last names
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples pick_lastname("Hobbes, Thomas")
#' @keywords utilities
pick_lastname <- function (x, format = "last, first", keep.single = TRUE) {

  x <- as.character(x)

  if (format == "last, first") {
    last <- sapply(x, function (x) {y <- unlist(strsplit(x, ", "), use.names = FALSE); if (length(y)>=1) y[[1]] else NA}, USE.NAMES = FALSE) 
  } else if (format == "first last") {
    last <- sapply(x, function (x) {y <- unlist(strsplit(x, " "), use.names = FALSE); y[[length(y)]]}, USE.NAMES = FALSE)
  } else {
    stop("Correct the unknown format in pick_lastname function.")
  }

  # Remove possible life year info
  last <- gsub(" \\([0-9|N|A]+-[0-9|N|A]+\\)", "", last)

  last

}


