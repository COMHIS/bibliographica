#' @title Remove Persons
#' @description Remove persons.
#' @param x A vector
#' @param who names to be removed
#' @return Polished vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x2 <- remove_persons(x)}
#' @keywords utilities
remove_persons <- function (x, who = NULL) {

  # Get printing terms from a table
  # TODO later add names from the complete name list as well ?
  if (is.null(who)) {
    f <- system.file("extdata/persons.csv", package = "bibliographica") 
    terms <- as.character(read.csv(f)[,1])
  }

  x <- remove_terms(x, terms, include.lowercase = TRUE)

  x

}




