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
  x <- gsub("--", "-", x)

  for (ie in c("i\\.e\\.", "i\\.e ", "ie\\.", "i e", "ie", "p\\. i\\.e")) {

    x <- gsub(paste(" ", ie, " ", sep = ""), " i.e ", x)
    x <- gsub(paste(" ", ie, "", sep = ""), " i.e ", x)
    x <- gsub(paste(" ", ie, ",", sep = ""), " i.e ", x)
    x <- gsub(paste(" ", ie, " ,", sep = ""), " i.e ", x)

    x <- gsub(paste("^", ie, " ", sep = ""), " i.e ", x)
    x <- gsub(paste("^", ie, "", sep = ""), " i.e ", x)
    x <- gsub(paste("^", ie, ",", sep = ""), " i.e ", x)
    x <- gsub(paste("^", ie, " ,", sep = ""), " i.e ", x)        

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



#' @title handle_ie
#' @description Handle ie statement
#'
#' @param x A vector
#' @param harmonize Logical. Harmonize ie statements efore interpretation?
#' @return A vector polished
#'
#' @export
#' @importFrom stringr str_sub
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{x2 <- handle_ie("i.e.")}
#' @keywords utilities
handle_ie <- function (x, harmonize = TRUE) {

  # 183 i.e 297 -> 297	  
  # 183-285 i.e 297 -> 183-297	  
  # 183-285 [i.e 297] -> 183-297	  

  y <- x

  if (harmonize) {
    y <- x <- harmonize_ie(y)
  }

  if (is.na(x) || x == "i.e") {return(x)}

  # Handle ie
  if (length(grep("i\\.e.", x)) > 0) {

    x2 <- gsub("\\]", " ", str_trim(unlist(strsplit(x, "i.e")))[[2]])

    # NB! If y ends with "i.e", upper bound doesn't exist
    yspl <- unlist(strsplit(y, "i\\.e"))
    x0 <- NULL

    if (length(yspl) == 2 && length(grep("-", yspl[[1]]))>0 && length(grep("-", yspl[[2]]))>0) {

      y <- str_trim(yspl[[2]])

    } else if (length(grep("-", yspl[[1]]))>0) {

      y <- suppressWarnings(as.numeric(as.roman(str_trim(x2))))
      spl <- str_trim(unlist(strsplit(x, "-")))

      x0 <- suppressWarnings(as.numeric(as.roman(gsub("\\[", "", gsub("\\]", "", spl[[1]])))))
      x1 <- suppressWarnings(as.numeric(as.roman(unlist(strsplit(spl[[2]], " "))[[1]])))

      if (is.na(x0) || x2 > x0) {
        y <- paste(x0, x2, sep = "-")
      } else {
        y <- x2
      }

    } else if (length(yspl) == 2 && length(grep("-", yspl[[2]]))>0) {

      # Why this line? 
      # y is assigned a new value after couple of lines before it's used
      #y <- suppressWarnings(as.numeric(as.roman(str_trim(x2))))
      
      # "4 [i.e. 6]-8 p." -> "6-8"
      spl <- str_trim(unlist(strsplit(x, "-")))

      # Check first, if there'll be two parts or not
      if (str_sub(spl[[1]], -3, -1) != "i.e") {
        
        x0 <- str_trim(unlist(strsplit(spl[[1]], "i.e"))[[2]])
      
        # Unnecessary line, might cause out of bounds error
        #x1 <- as.numeric(unlist(strsplit(spl[[2]], " "))[[1]])

        if (x2 > x0) {
          y <- paste(x0, x2, sep = "-")
        } else {
          y <- x2
        }
      } else {
        y <- x2
      }
      
    } else {
      xx <- gsub("^\\.", "", x2)
      xx <- condense_spaces(xx)
      xx <- unlist(strsplit(xx, " "))
      # Why paste xx[-1], which is empty string?
      y <- paste(suppressWarnings(as.numeric(as.roman(xx[[1]]))), xx[-1], sep = " ")
    }
  }

  y <- str_trim(y)

  y
}