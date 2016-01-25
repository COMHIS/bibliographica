#' @title handle_ie
#' @description Handle ie statement
#' @param x A vector
#' @param harmonize Logical. Harmonize ie statements efore interpretation?
#' @return A vector polished
#' @importFrom stringr str_sub
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
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
    x <- unlist(strsplit(x, "\\["), use.names = FALSE)
    inds <- grep("i\\.e", x)
    x[inds] <- unlist(strsplit(x[inds], "i\\.e"), use.names = FALSE)[[2]]
    x <- paste(x, collapse = "[")
  } else if (length(grep(" i\\.e ", x))>0) {
    # x i.e y -> y
    x <- unlist(strsplit(x, "i\\.e"), use.names = FALSE)[[2]]
  } else if (length(grep("\\[i\\.e", x))>0) {
    # x [i.e y] -> y
    x <- gsub("\\]*$", "", unlist(strsplit(x, "\\[i\\.e"), use.names = FALSE)[[2]])
  } else if (length(grep("\\[[0-9|a-z]* i\\.e [0-9|a-z]*\\]", x))>0) {
    # "mdcxli [1641 i.e 1642]" -> mdcxli [1642]
    x <- unlist(strsplit(x, "\\["), use.names = FALSE)
    inds <- grep("i\\.e", x)    
    x[inds] <- handle_ie(x[inds])
    x <- paste(x, collapse = "[")
  }
  x <- gsub("\\[ ", "\\[", x)
  x <- condense_spaces(x)

  x
}
