#' @title Pick multivolume
#' @description Pick volume information for multi-volume documents.
#' @param x Page number field. Vector or factor of strings.
#' @return Volume information
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{pick_multivolume("v.1-3, 293")}
#' @keywords utilities
pick_multivolume <- function (x) {

  vols <- NA

  if (length(grep("^[0-9]+ pts in [0-9]+v\\.", x))>0) {
    # 2 pts in 1v. INTO 1v.
    x <- gsub("^[0-9]+ pts in ", "", x)
  }

  if (length(grep("^[0-9]+ pts \\([0-9]+ *v\\.*\\)", x))>0) {
    x <- gsub("^[0-9]+ pts \\(", "", x)
    x <- gsub("\\)", "", x)    
  }

  if (x == "v.") {
    vols <- 1    
  } else if (length(grep("^[0-9]* {0,1}v\\.{0,1}$", x))>0) {  
    # 73 v. -> 73
    vols <- as.numeric(str_trim(gsub("v\\.{0,1}", "", x)))
  } else if (length(grep("^v\\.", x))>0) {
    # v.1-3 -> 3
    vols <- check_volumes(x)$n    
  } else if (length(grep("v\\.", x))>0) {
    # v.1 -> 1
    # FIXME: SPLITMEHERE used as a quick fix as v\\. was unrecognized char and
    # causes error
    vols <- sapply(x, function (xx) {s2 <- gsub("v\\.", "SPLITMEHERE", xx); s2 <- str_trim(unlist(strsplit(s2, "SPLITMEHERE"), use.names = FALSE)); as.numeric(s2[!s2 == ""][[1]])})
  } else {
    if (length(grep(";", x))>0) {
      vols <- length(strsplit(x, ";")[[1]])
    }
  }

  if (length(vols) == 0) {vols <- NA}

  vols
  
}


