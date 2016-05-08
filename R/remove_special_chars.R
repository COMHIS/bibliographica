#' @title Remove Special Chars
#' @description Remove special characters.
#' @param x Character vector
#' @param chars Characters to be removed
#' @param niter Number of iterations 
#' @return Polished vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples x2 <- remove_special_chars("test;")
#' @keywords utilities
remove_special_chars <- function (x, chars = c(",", ";", ":", "\\(", "\\)", "\\?", "--", "\\&"), niter = 5) {

  for (n in 1:niter) {
  
    x <- str_trim(x)

    for (char in chars) {
      x <- gsub(paste(char, "$", sep = ""), " ", x)
      x <- gsub(paste("^", char, sep = ""), " ", x)
      x <- gsub(char, " ", x)
    }

    for (char in c("\\[", "]")) {
      x <- gsub(paste(char, "$", sep = ""), "", x)
      x <- gsub(paste("^", char, sep = ""), "", x)
      x <- gsub(char, "", x)
    }
  }

  x <- condense_spaces(x)
   
  x[x == ""] <- NA

  x

}

