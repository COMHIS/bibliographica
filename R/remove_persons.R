#' @title remove_persons
#' @description Remove persons
#'
#' @param x A vector
#' @return Polished vector
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{x2 <- remove_persons(x)}
#' @keywords utilities
remove_persons <- function (x) {

  # Get printing terms from a table
  # TODO later add names from the complete name list as well ?
  f <- system.file("extdata/persons.csv", package = "bibliographica") 
  terms <- as.character(read.csv(f)[,1])

  # Add lowercase versions
  terms <- c(terms, tolower(terms))

  # List all unique terms
  terms <- sort(unique(terms))

  for (person in terms) {
    print(paste("Removing", person))
    x <- gsub(person, " ", x)
  }

  x <- remove_trailing_periods(x)

  x

}




