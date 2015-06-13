#' @title remove_volume_info
#' @description Remove volume info from the string start
#'
#' @param x Page number field. Vector or factor of strings.
#' @return Page numbers without volume information
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples remove_volume_info("v.4, 293")
#' @keywords utilities
remove_volume_info <- function (x) {

  s <- as.character(x)

  # Harmonize volume info first
  s <- harmonize_volume(s)

  # Remove parts
  s <- remove_parts(s)

  # Remove some rare special cases manually
  s <- gsub("v\\.1-3\\, 5 ;", "", s)
  s <- gsub("v\\.1\\,4-7 ;", "", s)
  s <- gsub("v\\.6-7\\,9-12", "", s)
  s <- gsub("Vols\\.6-7\\,9-12\\,plates :", "plates", s)

  # Pick and remove multi-volume information (document starting with '* v.')
  # TODO: vectorize this

  for (i in 1:length(s)) {
    si <- s[[i]]
    vols <- pick_multivolume(si)  
    if (length(vols) > 0) {
      # Then remove the volume information that was picked
      si <- gsub(paste("^", vols, " v.", sep = ""), paste(vols, "v.", sep = ""), str_trim(si))
      si <- str_trim(gsub(paste("^", vols, "v.", sep = ""), "", si)) 
      si <- str_trim(gsub("^,", "", si))
      s[[i]] <- si
    }
  }

  # Cases 'v.1-3' etc

  inds <- intersect(grep("^v.", s), grep("-", s))
  tmp <- sapply(s[inds], function (si) {gsub(check_volumes(si)$text, "", si)})
  s[inds] <- tmp

  # Pick which volume this might be (if given)
  # Cases 'v.1' etc.
  voln <- pick_volume(s)

  # Then remove the volume information that was picked
  s <- str_trim(gsub(paste("v.", voln, ":", sep = ""), "", s))
  s <- str_trim(gsub(paste("v.", voln, sep = ""), "", s))

  # "v. (183,[2]) -> (183,[2])"
  s <- gsub("^v\\. ", "v.", s)
  s <- gsub("^v.\\(", "(", s)

  s <- gsub("^v\\.[0-9][0-9][0-9]", " ", s)
  s <- gsub("^v\\.[0-9][0-9]", " ", s)
  s <- gsub("^v\\.[0-9]", " ", s)

  s <- gsub("^v\\.", " ", s)
  s <- gsub("^v\\.\\,", " ", s)
  s <- gsub("^v\\.$", "", s)

  s <- gsub("vols\\.", "v.", s)
  s <- gsub("vol\\.", "v.", s)


  # 2 v ; -> 2v.
  s <- gsub("^[0-9] v ", " ", s)
  s <- gsub("^[0-9]v ", " ", s)

  vol.synonymes <- c("atlas", "vols", "vol", "v\\.", "parts", "part", "pts")

  for (vnam in vol.synonymes) {

    s <- gsub(paste("^[0-9][0-9][0-9]", vnam, "\\.", sep = ""), " ", s)
    s <- gsub(paste("^[0-9][0-9][0-9] ", vnam, "\\.", sep = ""), " ", s)
    s <- gsub(paste("^[0-9][0-9][0-9]", vnam, " ", sep = ""), " ", s)
    s <- gsub(paste("^[0-9][0-9][0-9]", vnam, "$", sep = ""), " ", s)

    s <- gsub(paste("^[0-9][0-9]", vnam, "\\.", sep = ""), " ", s)
    s <- gsub(paste("^[0-9][0-9] ", vnam, "\\.", sep = ""), " ", s)
    s <- gsub(paste("^[0-9][0-9] ", vnam, " ", sep = ""), " ", s)
    s <- gsub(paste("^[0-9][0-9] ", vnam, "$", sep = ""), " ", s)

    s <- gsub(paste("^[0-9]", vnam, "\\.", sep = ""), " ", s)
    s <- gsub(paste("^[0-9] ", vnam, "\\.", sep = ""), " ", s)
    s <- gsub(paste("^[0-9] ", vnam, " ", sep = ""), " ", s)
    s <- gsub(paste("^[0-9]", vnam, " ", sep = ""), " ", s)
    s <- gsub(paste("^[0-9]", vnam, "$", sep = ""), " ", s)

    s <- gsub(paste("^[0-9][0-9][0-9]", vnam, "$", sep = ""), " ", s)
    s <- gsub(paste("^[0-9][0-9]", vnam, "$", sep = ""), " ", s)
    s <- gsub(paste("^[0-9]", vnam, "$", sep = ""), " ", s)

  }

  s <- gsub("\\( \\)", " ", s)
  s <- str_trim(s)
  s <- remove_endings(s, c(":", ";"))

  s[s == ""] <- NA

  s

}
