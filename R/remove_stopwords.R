#' @title Remove Stopwords
#' @description Remove stopwords from input vector.
#' @param x A vector
#' @param terms Stopwords
#' @return A vector polished
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # x2 <- remove_stopwords(c("a", "well", "james"), "well")
#' @keywords utilities
remove_stopwords <- function (x, terms = NULL) {

  if (is.null(terms)) {
    f <- system.file("extdata/stopwords.csv", package = "bibliographica")
    message(paste("No stopwords provided. Reading stopwords from file ", f))
    terms <- as.character(read.csv(f)[,1])
  }
    
  # List all unique terms
  terms <- sort(unique(terms))
  x <- suppressWarnings(remove_terms(x, terms, c("begin", "middle", "end")))

  x

}


