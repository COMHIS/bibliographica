#' @title remove_print_statements
#' @description Remove print statements
#'
#' @param x a vector
#' @return Polished vector
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples x2 <- remove_print_statements("Printed in London")
#' @keywords utilities
remove_print_statements <- function (x) {

  # Get printing terms from a table
  f <- system.file("extdata/printterms.csv", package = "bibliographica")
  terms <- as.character(read.csv(f)[,1])

  # Include also full lowercase versions 
  terms <- c(terms, tolower(terms))

  # Remove endings
  while (length(grep(" at$",terms))>0) {
    terms <- gsub(" at$", "", terms)
  }
  while (length(grep(" in$",terms))>0) {
    terms <- gsub(" in$", "", terms)
  }

  # Include versions with endings
  terms <- c(terms, 
  	     paste(terms, " at", sep = ""),
  	     paste(terms, " in", sep = ""))


  # Include versions with capital first letter
  terms <- c(terms, gsub("^p", "P", terms))
  terms <- c(terms, gsub("^r", "R", terms))
  terms <- c(terms, gsub("^i", "I", terms))
  terms <- c(terms, gsub("^s", "S", terms))

  # List all unique terms
  terms <- sort(unique(terms))

  # Go from longest to shortest term to avoid nested effects
  terms <- terms[rev(order(sapply(terms, nchar)))]

  # Remove printing terms
  x <- remove_terms(x, terms)

  x
}
