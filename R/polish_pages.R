#' @title polish_pages
#' @description clean up page numbers
#' @param x Page number field. Vector or factor of strings.
#' @param verbose Print progress info
#' @return Raw and estimated pages per document part
#' @details Document parts are separated by semicolons
#' @export
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

  if (verbose) {
    message(paste("Estimating page counts:", length(suniq), "unique cases"))
  }

  # Polish unique pages separately for each volume
  ret <- lapply(suniq, function (s) {polish_pages_help(s)})

  # Sum the volumes
  totp <- sapply(ret, function (x) {sum(x, na.rm = TRUE)})
  totp[totp == 0] <- NA # Set zero page counts to NA
  
  # Project unique cases back to the original list
  totp[match(s, suniq)]

}




polish_pages_help <- function (s) {

  # Estimate pages for each document separately via a for loop
  # Vectorization would be faster but we prefer simplicity and modularity here

  if (is.na(s) || s == "") {return(NA)}
  spl <- unlist(strsplit(s, ";"))
  
  if (length(spl)>1) {
    xx <- unname(sapply(spl, function (x) {polish_pages_help(x)}))
    return(xx)
  }

  # Catch warnings rather than crashing the loop
  a <- try(pp <- polish_page(s))

  # Save both raw and polished version 
  # We need these later to systematically identify failed cases
  # And to estimate the success fraction
  if ((is.character(a) && a == "try-error") || is.na(pp)) {
    pp <- NA
  }

  pp

}




