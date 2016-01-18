#' @title polish_volumenumber
#' @description Get volume number from page field if available
#' @param s Page number field. Vector or factor of strings.
#' @param harmonize_volume Harmonize volume info. Logical.
#' @return Volume number
#' @details Refers to single-volume document where the volume has been specified
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples polish_volumenumber("v.4")
#' @keywords utilities
polish_volumenumber <- function (s, harmonize_volume = TRUE) {

  # A summary of page counting rules that this function aims to (approximately) implement
  # https://www.libraries.psu.edu/psul/cataloging/training/bpcr/300.html
  s <- as.character(s)

  # Harmonize volume info
  if (harmonize_volume) {
    s <- harmonize_volume(s)
  }
  
  #' A single document, but check which volume ?
  # (document starting with 'v.*')
  voln <- sapply(s, function (x) {pick_volume(x)})

  voln

}


