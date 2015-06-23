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

  # Read the mapping table
  f <- system.file("extdata/harmonization.csv", package = "bibliographica")
  harm <- as.data.frame(read.csv(f, sep = "\t", stringsAsFactors = FALSE))
  
  # Harmonize
  for (i in 1:nrow(harm)) {
    s <- gsub(harm$synonyme[[i]], harm$name[[i]], s)
  }

  # Add some spaces and remove ambiguous terms
  s <- gsub("\\?to", " ", s)
  s <- gsub("cm", " cm", s)
  s <- gsub("x", " x ", s)

  # Remove extra spaces
  s <- condense_spaces(s)

  # With standard gatherings 1/2 = 2
  s <- gsub("1/", "", s)

  gt <- gatherings_table()
  for (i in 1:nrow(gt)) {
    s <- gsub(gt$Alternate[[i]], gt$Standard[[i]], s)
  }
  
  s

}

