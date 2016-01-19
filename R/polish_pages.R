#' @title polish_pages
#' @description clean up page numbers
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
polish_pages <- function (x, verbose = FALSE) {

  # Summary of abbreviations
  # http://ac.bslw.com/community/wiki/index.php5/RDA_4.5
  s <- as.character(x)
  suniq <- unique(s)

  s <- suniq
  s[grep("^[ |\\.|;|:|!|?]*$", s)] <- NA # ""; "." ; " ... "
  
  if (verbose) {
    message(paste("Estimating page counts:", length(suniq), "unique cases"))
  }

  # Polish unique pages separately for each volume
  # Return NA if conversion fails
  ret <- lapply(suniq, function (s) {a <- try(polish_pages_help(s, verbose = verbose)); if (class(a) == "try-error") {return(NA)} else {return(a)}})

  if (verbose) { message("Sum the volumes") }
  totp <- sapply(ret, function (x) {sum(x, na.rm = TRUE)})
  totp[totp == 0] <- NA # Set zero page counts to NA

  if (verbose) { message("Project unique cases back to the original list") }
  totp[match(s, suniq)]

}




polish_pages_help <- function (s, verbose = verbose) {

  if (verbose) { message(s) } 

  # Estimate pages for each document separately via a for loop
  # Vectorization would be faster but we prefer simplicity and modularity here

  # Convert to string 	    	    
  if (is.na(s)) { return(NA) }
  s <- as.character(s)
  
  spl <- unlist(strsplit(s, ";"))

  unname(sapply(spl, function (x) {polish_pages_help2(x)}))

}

polish_pages_help2 <- function (s) {

  x <- suppressWarnings(remove_volume_info(s))

  x <- harmonize_pages(x)

  x <- estimate_pages(x)

  x
}




