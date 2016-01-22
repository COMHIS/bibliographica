#' @title harmonize_ie
#' @description Harmonize ie statement
#' @param x A vector
#' @return A vector polished
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x2 <- harmonize_ie("i.e.")}
#' @keywords utilities
harmonize_ie <- function (x) {

  x <- as.character(x)
  x <- condense_spaces(x)
  x <- gsub("--", "-", x)

  # FIXME use file and vectorization
  for (ie in c("i\\. e\\.", "i\\.e\\.", "i\\.e ", "ie\\.", "i e", "ie", "p\\. i\\.e")) {

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
    x <- gsub(paste("\\[", ie, "", sep = ""), " [i.e ", x)
    x <- gsub(paste("\\[ ", ie, "", sep = ""), " i.e ", x)    
    x <- condense_spaces(x)
    
  }

  x <- gsub("\\[ i.e", "\\[i.e", x)
  x <- condense_spaces(x)

  x

}



