#' @title trim_names
#' @description Trim names
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

  s <- as.character(s)	   
  s <- tolower(str_trim(gsub("\\.", " ", s)))

  # Remove stopwords (TODO also in tau package, check that)
  s <- remove_stopwords(s, terms = stopwords, remove.letters = remove.letters) 
  s <- remove_trailing_periods(s)
  s <- condense_spaces(s)

  s
}

