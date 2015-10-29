#' Remove s.l etc. clauses 
#' @param x A character vector
#' @param terms Terms to be removed (a character vector). Optional
#' @return Polished vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples x2 <- remove_sl(c("s.l.", "London"))
#' @keywords utilities
remove_sl <- function (x, terms = NULL) {

  # Get printing terms from a table
  if (is.null(terms)) {
    f <- system.file("extdata/sl.csv", package = "bibliographica") 
    terms <- as.character(read.csv(f)[,1])
  }

  x <- remove_terms(x, terms, include.lowercase = TRUE)

  x

}




