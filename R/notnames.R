#' @title notnames
#' @description List of strings that are not names (manually checked)
#' @param ... Arguments to be passed
#' @return Vector of last names
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x <- notnames()}
#' @keywords utilities
notnames <- function (...) {

  generic <- as.character(read.csv(system.file("extdata/names/notnames/generic.csv", package = "bibliographica"), sep = "\t")[,1])
  organization <- as.character(read.csv(system.file("extdata/organization.csv", package = "bibliographica"), sep = "\t")[,1])

  union(generic, organization)

}


