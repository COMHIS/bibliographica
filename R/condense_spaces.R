#' @title Condense Spaces
#' @description Trim and remove double spaces from the input strings.
#' @param x A vector
#' @importFrom stringr str_trim
#' @return A vector with extra spaces removed
#' @details Beginning, double and ending spaces are also removed from the strings.
#' @export 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica") 
#' @examples x2 <- condense_spaces(" a  b cd ") # "a b cd"
#' @keywords utilities
condense_spaces <- function (x) {

  x <- str_trim(x, "both")
  x <- gsub(" +", " ", x)
  x[x == ""] <- NA
   
  x

}

