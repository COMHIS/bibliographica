#' @title pick_multivolume
#' @description Pick volume information for multi-volume documents
#' @param x Page number field. Vector or factor of strings.
#' @return Volume information
#' @importFrom stringr str_trim
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{pick_multivolume("v.1-3, 293")}
#' @keywords utilities
pick_multivolume <- function (x) {

  # 73 v. -> 73
  if (length(grep("^[0-9]* {0,1}v\\.{0,1}$", x))>0) {
    return(as.numeric(str_trim(gsub("v\\.{0,1}", "", x))))
  }

  # v.1-3 -> 3
  vol <- check_volumes(x)$n

  # v.1 -> 1
  if (is.null(vol)) {
    vol <- NA	   
    inds <- grep("v\\.", s)
    if (length(inds) > 0) {
      # FIXME: SPLITMEHERE used as a quick fix as v\\. was unrecognized char and
      # causes error
      s2 <- gsub("v\\.", "SPLITMEHERE", s)
      vol <- as.numeric(str_trim(unlist(strsplit(s2, "SPLITMEHERE"))[[1]]))
    }
  }

  if (length(vol) == 0) {
    vol <- NA
  }

  vol

}

