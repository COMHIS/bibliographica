#' @title Capitalize strings
#' @description Capitalize the first letter.
#' @param x a character vector to capitalize
#' @param format Capitalization format: "first.letter" (first letter
#'   capitalized) or "all.words" (all words capitalized)
#' @return Capitalized character vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples capitalize("print")
#' @keywords utilities
capitalize <- function (x, format = "first.letter") {

  # Speed up by considering unique instances only
  xorig <- x
  x <- xuniq <- unique(xorig)

  if (format == "first.letter") {
    for (a in letters) {
      x <- gsub(paste0("^", a), toupper(a), x)
    }
  } else if (format == "all.words") {
    x <- sapply(strsplit(x, " "), function (x) {paste(capitalize(x, format = "first.letter"), collapse = " ")}, USE.NAMES = FALSE)
  }

  x[match(xorig, xuniq)]
  
}
