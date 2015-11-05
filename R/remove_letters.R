#' @title remove_letters
#' @description Remove specific single letters
#' @param x A vector
#' @return Polished vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{remove_letters(x)}
#' @keywords utilities
remove_letters <- function (x) {
  x <- remove_terms(x, setdiff(c(letters, LETTERS), c("S", "s", "N", "n", "E", "e", "W", "w")), "begin")
  x <- remove_terms(x, c(letters, LETTERS), "end")
  x
}
