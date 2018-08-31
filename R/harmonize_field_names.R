#' @title Harmonize field names
#' @description Convert field codes to names 
#' @param x a vector of field codes
#' @return Vector of field names
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples x2 <- harmonize_field_names("041a")
#' @details For MARC field descriptions, see \url{https://www.loc.gov/marc/bibliographic/}.
#' @keywords utilities
harmonize_field_names <- function (x = NULL) {

  # Get printing terms from a table
  f <- system.file("extdata/fieldnames.csv", package = "bibliographica")
  map <- read.csv(f, sep = "|")

  if (!is.null(x)) {
    ret <- map[match(x, map$field), "name"]
  } else {
    ret <- map
  }
  ret
}

