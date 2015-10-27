#' @title polish_volumenumber
#' @description Get volume number from page field if available
#'
#' @param s Page number field. Vector or factor of strings.
#' @return Volume number
#' @details Refers to single-volume document where the volume has been specified
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples polish_volumenumber("v.4")
#' @keywords utilities
polish_volumenumber <- function (s) {

  # A summary of page counting rules that this function aims to (approximately) implement
  # https://www.libraries.psu.edu/psul/cataloging/training/bpcr/300.html
  s <- as.character(s)

  # Harmonize volume info
  s <- harmonize_volume(s)

  #' A single document, but check which volume ?
  # (document starting with 'v.*')
  voln <- sapply(s, function (x) {pick_volume(x)})

  voln

}


#' @title polish_volumecount
#' @description Get volume number from page field if available
#'
#' @param s Page number field. Vector or factor of strings.
#' @return Number of volumes
#' @details Refers to multi-volume document where the number of volumes has been specified
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples polish_volumecount("4v.")
#' @keywords utilities
polish_volumecount <- function (s) {

  # A summary of page counting rules that this function aims to (approximately) implement
  # https://www.libraries.psu.edu/psul/cataloging/training/bpcr/300.html
  s <- as.character(s)

  # Harmonize volume info
  s <- harmonize_volume(s)

  # Pick multi-volume information 
  # (document starting with '* v.' or 'v.1-3' etc.)
  voln <- unname(polish_volumenumber(s))
  vols <- sapply(s, function (x) {pick_multivolume(x)})

  # Assume single volume when number not given
  # NOTE: voln (volume number must be NA as well, otherwise we have 
  # one part of a multi-volume document
  vols[is.na(vols) & is.na(voln)] <- 1 

  vols

}



#' @title pick_volume
#' @description Pick volume
#'
#' @param s Page number field. Vector or factor of strings.
#' @return Volume
#'
#' @export
#' 
#' @details A single document, but check which volume 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples pick_volume("v.4")
#' @keywords utilities
pick_volume <- function (s) {

  # Remove some rare misleading special cases manually
  s <- gsub("v.1-3, 5 ;", "", s)
  s <- gsub("v.1,4-7 ;", "", s)

  vol <- NA	    
  if (length(grep("^v\\.", s)) > 0) {
    s <- gsub("^v\\.", "", s)
    i <- 1
    n <- as.numeric(substr(s, 1, 1))
    while (i <= nchar(s) && !is.na(n)) {
      n <- as.numeric(substr(s, 1, i))
      # Pick cases v.1 but not v.1-3
      if (!is.na(n) && !substr(s, i+1, i+1) == "-") {
        vol <- n
      } else if (substr(s, i+1, i+1) == "-") {
        vol <- NA
      } else {
        i <- Inf
      }

      i <- i+1
    }
  }

  vol
}



#' @title pick_multivolume
#' @description Pick volume information for multi-volumen documents
#'
#' @param x Page number field. Vector or factor of strings.
#' @return Volume information
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples pick_multivolume("v.1-3, 293")
#' @keywords utilities
pick_multivolume <- function (x) {

  s <- as.character(x)

  # v.1-3 -> 3
  vol <- check_volumes(s)$n

  # v.1 -> 1
  if (is.null(vol)) {
    vol <- NA	   
    inds <- grep("v\\.", s)
    if (length(inds) > 0) {
      # FIXME: SPLITMEHERE used as a quick fix as v\\. was unrecognized char and
      # causes error
      s2 <- gsub("v\\.", "SPLITMEHERE", s)
      #vol <- as.numeric(str_trim(unlist(strsplit(s, "v\\."))[[1]]))
      vol <- as.numeric(str_trim(unlist(strsplit(s2, "SPLITMEHERE"))[[1]]))
    }
  }

  if (length(vol) == 0) {
    vol <- NA
  }

  vol

}

remove_parts <- function (x) {

  s <- as.character(x)

  # Remove parts
  # "27 parts" -> " "

  parts <- c("parts", "part", "pts")
  for (n in parts) {
    s <- gsub(paste("[0-9][0-9][0-9] ", n, " in", sep = ""), " ", s)
    s <- gsub(paste("[0-9][0-9] ", n, " in", sep = ""), " ", s)
    s <- gsub(paste("[0-9] ", n, " in", sep = ""), " ", s)

    s <- gsub(paste("[0-9][0-9][0-9] ", n, sep = ""), " ", s)
    s <- gsub(paste("[0-9][0-9] ", n, sep = ""), " ", s)
    s <- gsub(paste("[0-9] ", n, sep = ""), " ", s)

    s <- gsub(paste("in [0-9][0-9][0-9] ", n, sep = ""), " ", s)
    s <- gsub(paste("in [0-9][0-9] ", n, sep = ""), " ", s)
    s <- gsub(paste("in [0-9] ", n, sep = ""), " ", s)

    s <- gsub(paste("in [0-9][0-9][0-9]", n, sep = ""), " ", s)
    s <- gsub(paste("in [0-9][0-9]", n, sep = ""), " ", s)
    s <- gsub(paste("in [0-9]", n, sep = ""), " ", s)

  }

  s <- gsub(" in [0-9][0-9][0-9] ", " ", s)
  s <- gsub(" in [0-9][0-9] ", " ", s)
  s <- gsub(" in [0-9] ", " ", s)
  
  s <- condense_spaces(s)  

  s
}






