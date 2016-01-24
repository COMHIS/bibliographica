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

  vols <- rep(NA, length(x))

  # 73 v. -> 73
  inds1 <- grep("^[0-9]* {0,1}v\\.{0,1}$", x)
  if (length(inds1)>0) {
    vols[inds1] <- as.numeric(str_trim(gsub("v\\.{0,1}", "", x[inds1])))
  }
  
  # v.1-3 -> 3
  inds2 <- setdiff(1:length(x), inds1)
  vols[inds2] <- sapply(x[inds2], function (xx) {check_volumes(xx)$n})

  # v.1 -> 1
  inds3 <- setdiff(grep("v\\.", x), c(inds1, inds2))
  if (length(inds3) > 0) {
      # FIXME: SPLITMEHERE used as a quick fix as v\\. was unrecognized char and
      # causes error
      vols[inds3] <- sapply(x[inds3], function (xx) {s2 <- gsub("v\\.", "SPLITMEHERE", xx); s2 <- str_trim(unlist(strsplit(s2, "SPLITMEHERE"))); s3 <- s2[!s2 == ""]; as.numeric(s3[[1]])})
  }

  vols[sapply(vols, length) == 0] <- NA

  vols
  
}


