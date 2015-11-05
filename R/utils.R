<<<<<<< HEAD
#' @title remove_endings
#' @description Remove specified endings of strings
#'
#' @param x vector
#' @param endings endings to remove
#' @return polished vector
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
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





#' @title remove_numerics
#' @description Remove numeric characters from the input strings
#'
#' @param x A vector
#' @param numbers numeric characters to be removed (all by default)
#' @return A vector with characters removed
#'
#' @details After removing the numerics, beginning, double and ending 
#'          spaces are also removed from the strings.
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{x2 <- remove_numerics(x, numbers = 0:9)}
#' @keywords utilities
remove_numerics <- function (x, numbers = 0:9) {

  for (num in numbers) {
    x <- gsub(num, " ", x)
  }

  x <- condense_spaces(x)

  x
}
=======
>>>>>>> origin/master

