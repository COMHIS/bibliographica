#' @title remove_volume_info
#' @description Remove volume info from the string start
#' @param x Page number field. Vector or factor of strings.
#' @param vols vols
#' @return Page numbers without volume information
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{remove_volume_info("v.4, 293")}
#' @keywords utilities
remove_volume_info <- function (x, vols) {

  x <- gsub("^[0-9]* {0,1}v\\.$", "", x)		   

  # Remove parts
  # "27 pts" -> " "
  s <- gsub("in [0-9]* {0,1}pts", " ", x)  
  s <- gsub("[0-9]* pts in", " ", s)
  s <- gsub("[0-9]* pts", " ", s)
  s <- gsub(" in [0-9]* ", " ", s)  
  s <- condense_spaces(s)  

  # Remove some rare special cases manually
  s <- gsub("v\\.[0-9]-[0-9]\\, [0-9] ;", "", s)
  s <- gsub("v\\.[0-9]\\,[0-9]-[0-9] ;", "", s)
  s <- gsub("v\\.[0-9]-[0-9]\\,[0-9]-[0-9]*", "", s)
  s <- gsub("Vols\\.[0-9]-[0-9]\\,[0-9]-[0-9]*\\,plates :", "plates", s)

  # Cases 'v.1-3' etc
  inds <- intersect(grep("^v.", s), grep("-", s))
  s[inds] <- sapply(s[inds], function (si) {
    gsub(check_volumes(si)$text, "", si)
  })

  # Remove the volume information that was picked
  s <- gsub("^[0-9]{1,4}v\\.", "", s)
  s <- gsub("^v\\.[0-9]{1,4}", "", s)
  s <- gsub("^,", "", s)

  # Remove Cases 'v.1' etc.
  s <- gsub("v\\.[0-9]* {0,1}\\:{0,1}", "", s)

  # "v. (183,[2]) -> (183,[2])"
  s <- gsub("^v\\. ", "v.", s)
  s <- gsub("^v.\\(", "(", s)
  s <- gsub("^v\\.[0-9]{1,3}", " ", s)
  s <- gsub("^v\\.", " ", s)
  s <- gsub("^v\\.\\,", " ", s)

  vol.synonymes <- c("vol", "part")
  for (vnam in vol.synonymes) {
    s <- gsub(paste("^[0-9]{1,3} {0,1}", vnam, "[\\.| ]", sep = ""), " ", s)
    s <- gsub(paste("^[0-9]{1,3} {0,1}", vnam, "$", sep = ""), " ", s)
  }

  # "8p. 21cm. (8vo)"
  s <- gsub("\\([0-9]{1,2}.o\\)" , "", s)
  s <- gsub("[0-9]*cm" , "", s)  
  s <- gsub("\\( \\)", " ", s)
  s <- str_trim(s)
  s <- remove_endings(s, c(":", ";"))

  s[s == "."] <- "" # Faster than gsub
  s[s == ""] <- NA

  s

}
