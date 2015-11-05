#' @title remove_brackets_from_letters
#' @description Remove brackets surrounding letters
#' @param x A vector
#' @return A polished vector 
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples x2 <- remove_brackets_from_letters("[p]")
#' @keywords utilities
remove_brackets_from_letters <- function (x) {

  x <- as.character(x)		

  # [P] -> P
  for (l in c(letters, LETTERS)) {
    x <- gsub(paste("\\[", l, "]", sep = ""), l, x)
    x <- gsub(paste("\\(", l, "\\)", sep = ""), l, x)        
  }

  x

}


