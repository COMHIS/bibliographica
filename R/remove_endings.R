#' @title Remove Endings
#' @description Remove specified endings of strings.
#' @param x vector
#' @param endings endings to remove
#' @param random_order Order the endings randomly before removal. TRUE/FALSE
#' @return polished vector
#' @export
#' @importFrom stringr str_c
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x2 <- remove_endings(x, endings)}
#' @keywords utilities
remove_endings <- function (x, endings, random_order=FALSE) {

  if (random_order) {
    e <- str_c(endings, "|", collapse="")
    e <- paste("(" , e, ")*$", sep="")
    x <- gsub(e, "", x)
  }
  for (e in endings) {
    x <- gsub(paste(e, "$", sep = ""), "", x)
  }

  x
}





