#' @title Pick volume
#' @description Pick volume
#' @param s Page number field. Vector or factor of strings.
#' @return Volume
#' @details A single document, but check which volume 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica") 
#' @examples \dontrun{pick_volume("v.4")}
#' @keywords utilities
pick_volume <- function (s) {

  # Pick cases v.1 but not v.1-3
  voln <- NA

  if (length(grep("^v\\.[0-9]+$", s))>0) {
    voln <- gsub("^v\\.", "", s)
  } else if (length(grep("^v\\.[0-9]+-[0-9]+", s)) > 0) {
    # ignore v.7-9
    voln <- NA
  } else if (length(grep("^v\\.[0-9]+", s))>0) {
    voln <- gsub("^v\\.", "", s)
    spl <- as.numeric(unlist(strsplit(voln, ""), use.names = FALSE))
    voln <- substr(voln, 1, min(which(is.na(spl)))-1)
  }
  as.numeric(voln)

}


