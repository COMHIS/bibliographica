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
  vols <- sapply(s, function (x) {pick_multivolume(x)})

  # Assume single volume when number not given
  vols[is.na(vols)] <- 1 

  vols

}

harmonize_volume <- function (s) {
  s[s == "^v\\. ;"] <- NA
  s <- gsub("^Vol\\.", "v.", s)
  s <- gsub("^vols\\.", "v.", s)
  s <- gsub("^Vols\\.", "v.", s)
  s <- gsub("^Pp\\.", "p.", s)
  s <- gsub("^v\\. ", "v.", s)
  s <- gsub("^v\\.\\(", "(", s)
  s <- gsub("^v\\.,", "", s)
  s <- gsub("v\\.:bill\\. ;", NA, s)
  s
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

# Number of volumes
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

  vol

}

remove_volume_info <- function (s) {

  # Remove some rare special cases manually
  s <- gsub("v.1-3, 5 ;", "", s)
  s <- gsub("v.1,4-7 ;", "", s)
  s <- gsub("v.6-7,9-12", "", s)
  s <- gsub("Vols.6-7,9-12,plates :", "plates", s)
  s <- gsub("^v\\.:", "", s)
  s <- gsub("^v\\.\\,", "", s)
  s <- gsub("^v\\,", "", s)
  s <- gsub("^v\\.", "", s)

  # Pick and remove multi-volume information (document starting with '* v.')
  vols <- pick_multivolume(s)  
  # Then remove the volume information that was picked
  s <- gsub(paste("^", vols, " v.", sep = ""), paste(vols, "v.", sep = ""), str_trim(s))
  s <- str_trim(gsub(paste("^", vols, "v.", sep = ""), "", s)) 
  s <- str_trim(gsub("^,", "", s))

  # Cases 'v.1-3' etc
  inds <- intersect(grep("^v.", s), grep("-", s))
  for (i in inds) {
    s[[i]] <- gsub(check_volumes(s[[i]])$text, "", s[[i]])
  }

  # Pick which volume this might be (if given)
  # Cases 'v.1' etc.
  voln <- pick_volume(s)
  # Then remove the volume information that was picked
  s <- str_trim(gsub(paste("v.", voln, ":", sep = ""), "", s))
  s <- str_trim(gsub(paste("v.", voln, sep = ""), "", s))

  # "v. (183,[2]) -> (183,[2])"
  s <- gsub("^v. ", "v.", s)
  s <- gsub("^v.\\(", "(", s)

  s

}


# v.1-3 -> 3
check_volumes <- function (x) {

  nvol <- vtext <- NULL
  n2 <- n1 <- NULL

  # Handle some rare special cases manually
  if (is.na(x)) {
    nvol <- NA
    vtext <- NA
  } else if (x == "v.1-3, 5 ;") {
    nvol <- 4
    vtext <- "v.1-3,5"
  } else if (x == "v.1,4-7 ;") {
    nvol <- 5
    vtext <- "v.1,4-7"
  } else if (x == "Vols.6-7,9-12,plates :") {
    nvol <- 6
    vtext <- "v.6-7,9-12"
  } else if (length(grep("^v.", x)) > 0 && length(grep("-", x)) > 0) {
    x <- gsub("^v.", "", x)
    x2 <- unlist(strsplit(x, "-"))
    n1 <- as.numeric(x2[[1]])

    i <- 1
    n <- as.numeric(substr(x2[[2]], 1, i))
    while (is.numeric(n) && i <= nchar(x2[[2]])) {
      n2 <- n
      n <- as.numeric(substr(x2[[2]], 1, i))
      i <- i+1
    }

    # Number of volumes
    nvol <- n2 - n1 + 1
 
    # Volume statement
    vtext <- paste("v.", n1, "-", n2, sep = "")

  }

  list(n = nvol, text = vtext)
 
}
