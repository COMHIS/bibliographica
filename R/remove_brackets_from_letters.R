#' @title Remove Brackets from Letters
#' @description Remove brackets surrounding letters.
#' @param x A vector
#' @param myletters Letters to remove
#' @return A polished vector 
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples x2 <- remove_brackets_from_letters("[p]")
#' @keywords utilities
remove_brackets_from_letters <- function (x, myletters = NULL) {

  if (is.null(myletters)) {myletters <- c(letters, LETTERS)}

  # [P] -> P
  for (l in myletters) {
    x <- gsub(paste("\\[", l, "\\]", sep = ""), l, x)
    x <- gsub(paste("\\(", l, "\\)", sep = ""), l, x)
  }   
  x

}


