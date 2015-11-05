#' @title remove_endings
#' @description Remove specified endings of strings
#' @param x vector
#' @param endings endings to remove
#' @return polished vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x2 <- remove_endings(x, endings)}
#' @keywords utilities
remove_endings <- function (x, endings) {

  for (e in endings) {
    x <- gsub(paste(e, "$", sep = ""), "", x)
  }

  x
}




