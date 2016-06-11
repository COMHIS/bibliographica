#' @title Remove Trailing Periods
#' @description Remove trailing periods.
#' @param x A vector
#' @return A polished vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x2 <- remove_trailing_periods(x)}
#' @keywords utilities
remove_trailing_periods <- function (x){ 

  if (all(is.na(x))) {return(x)}
  x <- gsub("\\.+$", "", x)
  x <- gsub("^\\.+", "", x)
    
  x
}


