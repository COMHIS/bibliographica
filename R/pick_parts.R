#' @title Pick Parts
#' @description Pick parts information.
#' @param x physical_extent field. Vector or factor of strings.
#' @return Volume information
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{pick_parts("2 parts")}
#' @keywords utilities
pick_parts <- function (x) {

  parts <- NA

  if (length(grep("^[0-9]* pts", x))>0) {
    # 73 parts -> 73
    parts <- as.numeric(unlist(strsplit(x, " "))[[1]])
  }
  
  if (length(parts) == 0) {parts <- NA}

  parts
  
}


