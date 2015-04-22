#' @title harmonize_dimension
#' @description Harmonize dimension information 
#'
#' @param x A character vector that may contain dimension information
#' @return The character vector with dimension information harmonized
#'
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("estc")
#' @export
#' 
#' @examples harmonize_dimension("fol.", sheet_sizes())
#' @keywords internal
harmonize_dimension <- function (x, sheetsizes) {

  s <- as.character(x)

  # Mark NAs
  s <- gsub("\\?⁰", " ", s)

  # Harmonize
  s <- gsub(".̊", "⁰", s)
  s <- gsub(" ⁰", "⁰", s)
  s <- gsub("₀", "⁰", s)
  s <- gsub("⁹", "⁰", s)
  s <- gsub(".̥", "⁰", s)
  s <- gsub("'", "⁰", s)
  s <- gsub("⁰", "to", s)

  s <- gsub("cm", " cm", s)
  #s <- gsub("4to", "4⁰", s)
  s <- gsub("8vo", "8to", s)
  s <- gsub("fol.", "2to", s)
  s <- gsub("fol$", "2to", s)
  s <- gsub("fol ", "2to", s)
  s <- gsub("x", " x ", s)
  s <- gsub("  ", " ", s)
  s <- gsub("quarto [fewer than 50 pages]", "4to", s)
  s <- gsub("broadsheet", "broadside", s)

  for (ind in 1:nrow(sheetsizes)) { 
    nam <- sheetsizes[ind, "format"]
    gat <- sheetsizes[ind, "gatherings"]
    #s <- gsub(nam, gsub(gat, "to", "⁰"), s)
    s <- gsub(nam, gsub(gat, "⁰", "to"), s)
  }

  # With standard gatherings 1/2 = 2
  s <- gsub("1/", "", s)

  s

}

