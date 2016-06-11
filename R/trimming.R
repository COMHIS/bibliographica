#' @title Trimming
#' @description Trim strings.
#' @param x vector
#' @param n iterations
#' @return Polished vector
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x2 <- trimming(x, n = 1)}
#' @keywords utilities
trimming <- function (x, n = 1){ 

  for (k in 1:n) {

  x <- str_trim(x)
  x <- gsub("[\\.|\\,|;|:]*$", "", x)
  x <- gsub("^\\(", "", x)  
  x <- gsub("\\,\\)", "\\)", x)
  x <- gsub("[\\,|\\.]*;", ";", x)
  x <- gsub("[^\\.|\\,]", "", x)
  x <- gsub(" \\. ", " ", x)
  x <- gsub("\\, ", "\\,", x)
  x <- gsub(":", "\\,", x)
  x <- gsub(";and", ";", x)
  x <- gsub(" in ", " ", x)

  }

  x
}

