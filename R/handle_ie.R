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

  # z [x i.e y] -> z [y]
  if (length(grep("\\[[0-9|a-z]* i\\.e [0-9|a-z]*\\]", x))>0) {
    x <- unlist(strsplit(x, "\\["))
    inds <- grep("i\\.e", x)
    x[inds] <- condense_spaces(unlist(strsplit(x[inds], "i\\.e"))[[2]])
    x <- paste(x, collapse = "[")
  }

  # x i.e y -> y
  if (length(grep(" i\\.e ", x))>0) {
    x <- condense_spaces(unlist(strsplit(x, "i\\.e"))[[2]])
  }

  # x [i.e y] -> y
  if (length(grep("\\[i\\.e", x))>0) {
    #x <- gsub("\\]*$", "", unlist(strsplit(x, "i\\.e"))[[2]])
    x <- gsub("\\]*$", "", unlist(strsplit(x, "\\[i\\.e"))[[2]])
    #inds <- grep("i\\.e", x)
    #x[inds] <- handle_ie(x[inds])
    #x <- paste(x, collapse = "[")

  }

  # "mdcxli [1641 i.e 1642]" -> mdcxli [1642]
  if (length(grep("\\[[0-9|a-z]* i\\.e [0-9|a-z]*\\]", x))>0) {

    x <- unlist(strsplit(x, "\\["))
    inds <- grep("i\\.e", x)
    
    x[inds] <- handle_ie(x[inds])

    x <- paste(x, collapse = "[")
    
  }

  str_trim(x)

}
