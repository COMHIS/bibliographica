#' @title remove_trailing_periods
#' @description Remove trailing periods
#' @param x A vector
#' @return A polished vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x2 <- remove_trailing_periods(x)}
#' @keywords utilities
remove_trailing_periods <- function (x){ 

  if (is.na(x)) {
    return(x)
  }

  xold <- x; xold[[1]] <- "XXXXXX"
  while (!mean(na.omit(xold == x)) == 1) {
    xold <- x
    x <- condense_spaces(x)
    x <- gsub("\\.$", "", x)
    x <- gsub("^\\.", "", x)
  }

  x
}

