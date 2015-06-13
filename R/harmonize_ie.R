#' @title harmonize_ie
#' @description Harmonize ie statement
#'
#' @param x A vector
#' @return A vector polished
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{x2 <- harmonize_ie("i.e.")}
#' @keywords utilities
harmonize_ie <- function (x) {

  x <- as.character(x)
  x <- condense_spaces(x)

  for (ie in c("i.e.", "i.e ", "ie.", "i e", "ie", "p. i.e")) {

    x <- gsub(paste(" ", ie, " ", sep = ""), " i.e ", x)
    x <- gsub(paste(" ", ie, "", sep = ""), " i.e ", x)
    x <- gsub(paste(" ", ie, ",", sep = ""), " i.e ", x)
    x <- gsub(paste(" ", ie, " ,", sep = ""), " i.e ", x)        
    x <- gsub(paste("\\,", ie, "", sep = ""), " i.e ", x)
    x <- gsub(paste("\\, ", ie, "", sep = ""), " i.e ", x) 
    x <- gsub(paste("\\[", ie, "", sep = ""), " i.e ", x)
    x <- gsub(paste("\\[ ", ie, "", sep = ""), " i.e ", x)    
    x <- condense_spaces(x)   
  }

  x <- gsub("\\[ i.e", "\\[i.e", x)
  x <- condense_spaces(x)

  x

}




handle_ie <- function (x) {

  # 183 i.e 297 -> 297	  
  # 183-285 i.e 297 -> 183-297	  
  # 183-285 [i.e 297] -> 183-297	  

  y <- x

  # Handle ie
  if (length(grep("i\\.e", x)) > 0) {

    x2 <- gsub("\\]", " ", str_trim(unlist(strsplit(x, "i.e")))[[2]])
    yspl <- unlist(strsplit(y, "i\\.e"))
    x0 <- NULL

    if (length(grep("-", yspl[[1]]))>0 && length(grep("-", yspl[[2]]))>0) {

      y <- str_trim(yspl[[2]])

    } else if (length(grep("-", yspl[[1]]))>0) {

      y <- suppressWarnings(as.numeric(as.roman(str_trim(x2))))
      spl <- str_trim(unlist(strsplit(x, "-")))

      x0 <- suppressWarnings(as.numeric(as.roman(spl[[1]])))
      x1 <- suppressWarnings(as.numeric(as.roman(unlist(strsplit(spl[[2]], " "))[[1]])))

      if (x2 > x0) {
        y <- paste(x0, x2, sep = "-")
      } else {
        y <- x2
      }
    } else if (length(grep("-", yspl[[2]]))>0) {

      y <- suppressWarnings(as.numeric(as.roman(str_trim(x2))))
      # "4 [i.e. 6]-8 p." -> "6-8"
      spl <- str_trim(unlist(strsplit(x, "-")))

      x0 <- str_trim(unlist(strsplit(spl[[1]], "i.e"))[[2]])
      x1 <- as.numeric(unlist(strsplit(spl[[2]], " "))[[1]])

      if (x2 > x0) {
        y <- paste(x0, x2, sep = "-")
      } else {
        y <- x2
      }
    } else {
      xx <- condense_spaces(x2)
      xx <- unlist(strsplit(xx, " "))
      y <- paste(suppressWarnings(as.numeric(as.roman(xx[[1]]))), xx[-1], sep = " ")
    }
  }

  y
}