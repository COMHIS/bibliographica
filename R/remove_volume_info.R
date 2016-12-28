#' @title Remove Volume Info
#' @description Remove volume info from the string start.
#' @param x Page number field. Vector or factor of strings.
#' @return Page numbers without volume information
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{remove_volume_info("v.4, 293")}
#' @keywords utilities
remove_volume_info <- function (x) {

  x <- gsub("^[0-9]+ {0,1}v\\.", "", x)		   

  # Remove parts
  # "27 pts" -> " "
  s <- gsub("in [0-9]* {0,1}pts", " ", x)  
  s <- gsub("[0-9]* pts in", " ", s)
  s <- gsub("[0-9]* pts", " ", s)
  s <- condense_spaces(s)    
  s <- gsub("^\\(", "", s)
  s <- gsub("\\)$", "", s)  
  s <- gsub("^[0-9]+ {0,1}v\\.", "", s)

  # Cases 'v.1-3,7-8' etc
  s <- gsub("^v\\.[0-9]+-[0-9]+,[0-9]+-[0-9]+", "", s)

  # Cases 'v.1-3' etc
  s <- gsub("^v\\. ", "v\\.", s)
  inds <- intersect(grep("^v\\.", s), grep("-", s))

  s[inds] <- sapply(s[inds], function (si) {
    gsub(check_volumes(si)$text, "", si)
  })

  # special cases 
  s <- gsub("v\\.[0-9]-[0-9]\\, [0-9] ;", "", s)
  s <- gsub("v\\.[0-9]\\,[0-9]-[0-9] ;", "", s)
  s <- gsub("v\\.[0-9]-[0-9]\\,[0-9]-[0-9]*", "", s)
  s <- gsub("Vols\\.[0-9]-[0-9]\\,[0-9]-[0-9]*\\,plates :", "plates", s)

  # Remove the volume information 
  s <- gsub("^[0-9]{1,4}v\\.", "", s)

  # Cases 'v.1' etc.
  s <- gsub("v\\.[0-9]* {0,1}\\:{0,1}", "", s)

  # "v. (183,[2]) -> (183,[2])"
  s <- gsub("^v\\.", " ", s)

  # vol.synonymes <- c("vol", "part")
  s <- gsub(paste("^[0-9]{1,3} {0,1}vol[\\.| ]", sep = ""), " ", s)
  s <- gsub(paste("^[0-9]{1,3} {0,1}vol$", sep = ""), " ", s)

  # "8p. 21cm. (8vo)"
  s <- gsub("\\([0-9]{1,2}.o\\)" , "", s)
  s <- gsub("[0-9]*cm" , "", s)
  s <- gsub("^, *" , "", s)
  s <- gsub("^i [0-9]+ " , "", s)  # 6v i 2 (300; 200)

  s[s == ""] <- NA

  s

}
