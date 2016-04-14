#' @title Remove stopwords
#' @description Remove stopwords from input vector
#' @param x A vector
#' @param terms Stopwords
#' @param remove.letters Logical. Also remove single letters (TRUE by default)
#' @return A vector polished
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # x2 <- remove_stopwords(c("a", "well", "james"), "well", remove.letters = TRUE)
#' @keywords utilities
remove_stopwords <- function (x, terms = NULL, remove.letters = TRUE) {

  if (is.null(terms)) {
    f <- system.file("extdata/stopwords.csv", package = "bibliographica")
    message(paste("No stopwords provided. Reading stopwords from file ", f))
    terms <- as.character(read.csv(f)[,1])
  }
    
  # List all unique terms
  terms <- sort(unique(terms))
  x <- remove_terms(x, terms, c("begin", "middle", "end"))

  if (remove.letters) {
    x <- remove_letters(x)
  }

  x

}


