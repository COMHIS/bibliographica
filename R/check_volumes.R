#' @title check_volumes
#' @description Pick volume information from page number field
#' @param x Page number field. Vector or factor of strings.
#' @return Volume information
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples check_volumes("v.4, 293")
#' @keywords utilities
check_volumes <- function (x) {

  nvol <- vtext <- NULL
  n2 <- n1 <- NULL

  # Handle some rare special cases manually
  if (is.na(x)) {
    nvol <- NA
    vtext <- NA
  } else if (x == "v.1-3, 5 ;") {
    nvol <- 4
    vtext <- "v.1-3,5"
  } else if (x == "v.1,4-7 ;") {
    nvol <- 5
    vtext <- "v.1,4-7"
  } else if (x == "Vols.6-7,9-12,plates :") {
    nvol <- 6
    vtext <- "v.6-7,9-12"

  } else if (length(grep("^v.[ ]*[0-9]+[ ]*-[ ]*[0-9]+.*$", x)) > 0) {
  
    n1 <- as.numeric(gsub("^v.[ ]*([0-9]+)[ ]*-[ ]*[0-9]+.*$", "\\1", x))
    n2 <- as.numeric(gsub("^v.[ ]*[0-9]+[ ]*-[ ]*([0-9]+).*$", "\\1", x))
    
    # Number of volumes
    nvol <- n2 - n1 + 1
 
    # Volume statement
    vtext <- paste("v.", n1, "-", n2, sep = "")

  }

  if (is.null(vtext)) {vtext <- ""}

  list(n = nvol, text = vtext)
 
}
