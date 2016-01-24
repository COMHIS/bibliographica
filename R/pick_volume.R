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
  voln <- rep(NA, length(s))

  inds1 <- grep("^v\\.[0-9]+$", s)
  voln[inds1] <- gsub("^v\\.", "", s[inds1])

  inds2 <- setdiff(grep("^v\\.", s), c(inds1, grep("^v\\.[0-9]+-[0-9]+", s)))
  if (length(inds2) > 0) { 
    v <- gsub("^v\\.", "", s[inds2])
    v <- suppressWarnings(unname(sapply(v, function (x) {spl <- as.numeric(unlist(strsplit(x, ""))); as.numeric(substr(x, 1, min(which(is.na(spl)))-1))})))
    voln[inds2] <- v
  }
  as.numeric(voln)

}

