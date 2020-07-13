#' @title Handle ie
#' @description Handle ie statements.
#' @param x Character vector
#' @param harmonize Logical. Harmonize ie statements efore interpretation?
#' @param separator The separator string (i.e by default)
#' @return A vector polished
#' @importFrom stringr str_sub
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x2 <- handle_ie("i.e.")}
#' @export
#' @keywords utilities
handle_ie <- function (x, harmonize = TRUE, separator = "i.e") {

  # 183 i.e 297 -> 297	  
  # 183-285 i.e 297 -> 183-297	  
  # 183-285 [i.e 297] -> 183-297
  # 183-285 i.e 297-299 -> 297-299

  y <- x

  if (harmonize) {
    y <- x <- harmonize_ie(y, separator = separator)
  }
  x <- condense_spaces(x)

  if (length(x) == 1 && (is.na(x) || x == separator)) {return(x)}

  # z [i.e y] -> y
  if (length(grep("[0-9|a-z]*\\.* \\[i\\.e [0-9|a-z]*\\]", x))>0) {

    # This is for "1905 [ie. 15]" -> 1915
    spl <- strsplit(x, " \\[i\\.e ")
    x1 <- sapply(spl, function (x) {x[[1]]})
    x2 <- gsub("\\]", "", sapply(spl, function (x) {x[[2]]}))
    inds <- which(nchar(x1) == 4 & nchar(x2) == 2)
    x[inds] <- sapply(inds, function (ind) {paste(substr(x1[[ind]], 1, 2), x2[[ind]], sep = "")})

    x <- gsub("^[0-9|a-z]*\\.* \\[i\\.e", "", x)

    x <- gsub("\\]$", "", x)
    
  }


  # "[1-3] 4-43 [44-45] 45-51 [i.e 46-52]"
  # keep the first part and just remove "45-51 ie"
  if (length(grep("[0-9]+-[0-9]+ i\\.e [0-9]+-[0-9]+", x)) == 1) {
    spl <- unlist(strsplit(x, " "), use.names = FALSE)    
    rmind <- which(spl == "i.e")
    rmind <- (rmind-1):rmind
    x <- paste(spl[-rmind], collapse = " ")
  }

  # " p 113-111 i.e 128] " -> 113-128
  if (length(grep("-[0-9]+ i\\.e [0-9]+", x)) == 1) {
    spl <- unlist(strsplit(x, "-"), use.names = FALSE)
    spl <- sapply(spl, function (spli) {handle_ie(spli)})
    x <- paste(spl, collapse = "-")
  }

  if (length(grep("-", x)) > 0 && length(grep("i\\.e", x)) > 0) {

    spl <- unlist(strsplit(x, "i\\.e"), use.names = FALSE)

    if (length(grep("-", spl)) == 2) {
      # 1-3 ie 2-4 -> 2-4
      x <- spl[[2]]
    } else {
    
      # [1658]-1659 [i.e. 1660] -> 1658-1660
      spl <- unlist(strsplit(x, "-"), use.names = FALSE)
      u <- sapply(spl, function (s) {handle_ie(s)}, USE.NAMES = FALSE)
      x <- paste(u, collapse = "-")
    }
    
  } else if (length(grep("\\[[0-9|a-z]* *i\\.e [0-9|a-z]*\\]", x))>0) {
  
    # z [x i.e y] -> z [y]  
    x <- unlist(strsplit(x, "\\["), use.names = FALSE)
    inds <- grep("i\\.e", x)
    u <- unlist(strsplit(x[inds], "i\\.e"), use.names = FALSE)
    x[inds] <- u[[min(2, length(u))]]
    x <- paste(x, collapse = "[")
    
  } else if (length(grep(" i\\.e ", x))>0) {
  
    # x i.e y -> y
    x <- unlist(strsplit(x, "i\\.e"), use.names = FALSE)
    x <- x[[min(2, length(x))]]
    
  } else if (length(grep("\\[i\\.e", x))>0) {
  
    # x [i.e y] -> y
    x <- unlist(strsplit(x, "\\[i\\.e"), use.names = FALSE)
    x <- x[[min(2, length(x))]]
    x <- gsub("\\]*$", "", x)
    
  } else if (length(grep("\\[[0-9|a-z]* i\\.e [0-9|a-z]*\\]", x))>0) {
    # "mdcxli [1641 i.e 1642]" -> mdcxli [1642]
    x <- unlist(strsplit(x, "\\["), use.names = FALSE)
    inds <- grep("i\\.e", x)    
    x[inds] <- handle_ie(x[inds])
    x <- paste(x, collapse = "[")
    
  }

  x <- gsub("\\[ ", "[", x)
  x <- gsub("^\\.*", "", x)  
  x <- str_trim(x)

  x
}
