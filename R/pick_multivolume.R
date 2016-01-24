#' @title pick_multivolume
#' @description Pick volume information for multi-volume documents
#' @param x Page number field. Vector or factor of strings.
#' @return Volume information
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{pick_multivolume("v.1-3, 293")}
#' @keywords utilities
pick_multivolume <- function (x) {

  vols <- NA

  if (length(grep("^[0-9]* {0,1}v\\.{0,1}$", x))>0) {
    # 73 v. -> 73
    vols <- as.numeric(str_trim(gsub("v\\.{0,1}", "", x)))
  } else if (length(grep("^v\\.", x))>0) {
    # v.1-3 -> 3
    vols <- check_volumes(x)$n
  } else if (length(grep("v\\.", x))>0) {
    # v.1 -> 1
    # FIXME: SPLITMEHERE used as a quick fix as v\\. was unrecognized char and
      # causes error
    vols <- sapply(x, function (xx) {s2 <- gsub("v\\.", "SPLITMEHERE", xx); s2 <- str_trim(unlist(strsplit(s2, "SPLITMEHERE"))); as.numeric(s2[!s2 == ""][[1]])})
  }

  if (length(vols) == 0) {vols <- NA}

  vols
  
}


