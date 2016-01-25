#' @title pick_lastname
#' @description Pick last name from full name, assuming the format is known 
#' @param x a vector of full names
#' @param format name format
#' @return a vector of last names
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples pick_lastname("Hobbes, Thomas")
#' @keywords utilities
pick_lastname <- function (x, format = "last, first") {

  x <- as.character(x)

  if (format == "last, first") {
    last <- sapply(x, function (x) {y <- unlist(strsplit(x, ", "), use.names = FALSE); if (length(y)>1) y[[1]] else NA}) 
  } else if (format == "first last") {
    last <- sapply(x, function (x) {y <- unlist(strsplit(x, " "), use.names = FALSE); y[[length(y)]]})
  } else {
    stop("Correct the unknown format in pick_lastname function.")
  }

  # Remove possible life year info
  last <- gsub(" \\([0-9|N|A]+-[0-9|N|A]+\\)", "", last)

  last

}


