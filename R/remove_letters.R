#' @title Remove Letters.
#' @description Remove specific single letters from the start and/or end of the input string(s).
#' @param x A vector
#' @param ignore.start Vector of letter not to exclude from the start of x
#' @param ignore.end Vector of letter not to exclude from the end of x
#' @return Polished vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{remove_letters(x)}
#' @keywords utilities
remove_letters <- function (x, ignore.start = c("S", "s", "N", "n", "E", "e", "W", "w"), ignore.end = c()) {
  x <- remove_terms(x, setdiff(c(letters, LETTERS), ignore.start), "begin")
  x <- remove_terms(x, setdiff(c(letters, LETTERS), ignore.end), "end")
  x
}

