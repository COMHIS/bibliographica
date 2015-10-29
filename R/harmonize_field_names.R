#' @title harmonize_field_names
#' @description Convert field codes to names 
#' @param x a vector of field codes
#' @return Vector of field names
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples x2 <- harmonize_field_names("041a")
#' @keywords utilities
harmonize_field_names <- function (x) {
  
  # Get printing terms from a table
  f <- system.file("extdata/fieldnames.csv", package = "bibliographica")
  map <- read.csv(f, sep = "\t")
  map[match(x, map$field), "name"]
  
}

