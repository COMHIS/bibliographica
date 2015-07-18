#' @title harmonize_dimension
#' @description Harmonize dimension information 
#'
#' @param x A character vector that may contain dimension information
#' @return The character vector with dimension information harmonized
#'
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' 
#' @examples harmonize_dimension("fol.", sheet_sizes())
#' @keywords internal
harmonize_dimension <- function (x, sheetsizes) {

  s <- as.character(x)

  # Harmonize the terms
  f <- system.file("extdata/harmonize_dimensions.csv", package = "bibliographica")
  sn <- as.data.frame(read.csv(f, sep = "\t", stringsAsFactors = FALSE))
  s <- harmonize_names(s, sn, mode = "recursive")$name

  # Add spaces
  s <- gsub("cm", " cm", s)
  s <- gsub("x", " x ", s)

  # Remove extra spaces
  s <- condense_spaces(s)

  s

}

