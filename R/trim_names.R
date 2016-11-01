#' @title Trim Names
#' @description Trim names.
#' @param s A vector of names
#' @param stopwords stopwords 
#' @param remove.letters Remove individual letters
#' @return Polished vector
#' @export 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica") 
#' @examples s2 <- trim_names("james", "and")
#' @keywords utilities
trim_names <- function (s, stopwords, remove.letters = FALSE) {

  # Remove stopwords (TODO also in tau package, check that)
  x <- suppressWarnings(remove_terms(s, stopwords, c("begin", "middle", "end")))

  if (remove.letters) {
    x <- remove_letters(x)
  }

  s <- remove_trailing_periods(s)
  s <- condense_spaces(s)

  s
}

