#' @title polish_volumecount
#' @description Get volume number from page field if available
#' @param s Page number field. Vector or factor of strings.
#' @return Number of volumes
#' @details Refers to multi-volume document where the number of volumes has been specified
#' @export 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples polish_volumecount("4v.")
#' @keywords utilities
polish_volumecount <- function (s) {

  # A summary of page counting rules that this function aims to (approximately) implement
  # https://www.libraries.psu.edu/psul/cataloging/training/bpcr/300.html
  s <- as.character(s)

  # Harmonize volume info
  s <- harmonize_volume(s)

  # Pick multi-volume information 
  # (document starting with '* v.' or 'v.1-3' etc.)
  voln <- unname(polish_volumenumber(s))
  vols <- sapply(s, function (x) {pick_multivolume(x)})

  # Assume single volume when number not given
  # NOTE: voln (volume number must be NA as well, otherwise we have 
  # one part of a multi-volume document
  vols[is.na(vols) & is.na(voln)] <- 1 

  vols

}





