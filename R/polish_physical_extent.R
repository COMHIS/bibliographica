#' @title Polish physical_extent field
#' @description Pick page counts, volume counts and volume numbers
#' @param x Page number field. Vector or factor of strings.
#' @param verbose Print progress info
#' @return Raw and estimated pages per document part
#' @details Document parts are separated by semicolons
#' @export
#' @importFrom stringr str_trim
#' @details A summary of page counting rules that this function aims to (approximately) implement are provided in 
#' \url{https://www.libraries.psu.edu/psul/cataloging/training/bpcr/300.html}
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # polish_pages("4p.")
#' @keywords utilities
polish_physical_extent <- function (x, verbose = FALSE) {

  # Summary of abbreviations
  # http://ac.bslw.com/community/wiki/index.php5/RDA_4.5
  sorig <- as.character(x)
  suniq <- unique(sorig)

  if (verbose) {
    message(paste("Polishing page count field:", length(suniq), "unique cases"))
  }

  #------------------------------------------------------
  
  s <- suniq
  s[grep("^[ |\\.|;|:|!|?]*$", s)] <- NA # ""; "." ; " ... "

  # Harmonize volume info
  inds <- 1:length(s)  
  inds <- setdiff(inds, setdiff(grep("v\\.$", s), grep("^v\\.$", s)))
  if (length(inds)>0) {
    s[inds] <- remove_trailing_periods(s[inds])
  }
  s <- unname(harmonize_volume(s))

  # Polish unique pages separately for each volume
  # Return NA if conversion fails
  ret <- lapply(s, function (s) { a <- try(polish_physext_help(s, verbose = verbose)); if (class(a) == "try-error") { return(NA) } else { return(a) }})

  # Convert to data.frame
  ret <- data.frame(do.call("rbind", ret))

  # Some final polishing
  ret$pagecount[ret$pagecount == 0] <- NA # Set zero page counts to NA

  # Assume single volume when number not given
  # FIXME perhaps this better goes to enrichnment functions?
  # NOTE: voln (volume number must be NA as well, otherwise we have 
  # one part of a multi-volume document
  ret$volcount[is.na(ret$volcount) & is.na(ret$volnumber)] <- 1 

  if (verbose) { message("Project unique entries back to the original list") }
  ret[match(sorig, suniq), ]

}




#' @title Polish physical_extent help field
#' @description Internal
#' @param s Input char
#' @param verbose Print progress info
#' @return Internal
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # TBA
#' @keywords internal
polish_physext_help <- function (s, verbose = verbose) {

  if (verbose) { message(s) } 

  # Estimate pages for each document separately via a for loop
  # Vectorization would be faster but we prefer simplicity and modularity here

  # Convert to string 	    	    
  if (is.na(s)) { return(NA) }
  s <- as.character(s)

  # Pagecount
  spl <- unlist(strsplit(s, ";"))
  x <- try(unname(sapply(spl, function (x) {polish_physext_help2(x)})))
  if (class(x) == "try-error") {
    x <- NA
  } 

  # if (verbose) { message("Sum the volumes") }
  totp <- sum(x, na.rm = TRUE)

  #' A single document, but check which volume number ?
  # (document starting with '* v.' or 'v.1-3' etc.)  
  # (document starting with 'v.*')
  voln <- pick_volume(s)

  # Volume count
  vols <- pick_multivolume(s)

  # Return
  data.frame(pagecount = totp, volnumber = voln, volcount = vols)

}



#' @title Polish physical_extent help field 2
#' @description Internal
#' @param s Input char
#' @return Internal
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # TBA
#' @keywords internal
polish_physext_help2 <- function (s) {

  x <- suppressWarnings(remove_volume_info(s))

  x <- harmonize_pages(x)

  x <- estimate_pages(x)

  x
}




