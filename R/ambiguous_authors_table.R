#' @title Ambiguous Authors Table
#' @description Read table of ambiguous author name synonymes
#' @param file Input file
#' @return Author synonyme data frame with fields 'name' and 'synonyme' 
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @details Each row of the input file corresponds to a unique author with potentially multiple name variants, separated by semicolon. The first column gives the accepted version of the name, the other columns list synonymes that will be mapped to the accepted version.
#' @examples aa <- ambiguous_authors_table()
#' @keywords utilities
ambiguous_authors_table <- function (file = NULL) {

  # Read author synonymes for ambiguous authors
  if (is.null(file)) {
    file <- system.file("extdata/ambiguous-authors.csv", package = "bibliographica")
  }

  aa <- read_mapping(file, mode = "list", sep = ";") 

  aa 

}

