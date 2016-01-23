#' @title Pick volume
#' @description Pick volume
#' @param s Page number field. Vector or factor of strings.
#' @return Volume
#' @export
#' @details A single document, but check which volume 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica") 
#' @examples pick_volume("v.4")
#' @keywords utilities
pick_volume <- function (s) {

  vol <- NA	    
  if (length(grep("^v\\.", s)) > 0) {
    s <- gsub("^v\\.", "", s)
    i <- 1
    n <- as.numeric(substr(s, 1, 1))
    while (i <= nchar(s) && !is.na(n)) {
      n <- as.numeric(substr(s, 1, i))
      # Pick cases v.1 but not v.1-3
      if (!is.na(n) && !substr(s, i+1, i+1) == "-") {
        vol <- n
      } else if (substr(s, i+1, i+1) == "-") {
        vol <- NA
      } else {
        i <- Inf
      }

      i <- i+1
    }
  }

  vol
}
