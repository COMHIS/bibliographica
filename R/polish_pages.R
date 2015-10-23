#' @title polish_pages
#' @description clean up page numbers
#'
#' @param x Page number field. Vector or factor of strings.
#' @param verbose Print progress info
#' @return Raw and estimated pages per document part
#' @details Document parts are separated by semicolons
#'
#' @export
#' 
#' @details A summary of page counting rules that this function aims to 
#'          (approximately) implement are provided in 
#' \url{https://www.libraries.psu.edu/psul/cataloging/training/bpcr/300.html}
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
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

  # Polish unique pages
  ret <- polish_pages_help(suniq, verbose)

  # Project unique cases back to the original list
  ret2 <- ret[match(s, suniq),]

  # Return
  ret2
}




polish_pages_help <- function (s, verbose) {

  # Estimate pages for each document separately via a for loop
  # Vectorization would be faster but we prefer simplicity and modularity here
  raw <- sp <- list()

  for (i in 1:length(s)) {

    if (verbose) { 
      if (ceiling(i/500) == floor(i/500)) {message(paste(round(100*i/length(s))), "%")}
      # {message(i)}
    }

    # Catch warnings rather than crashing the loop
    a <- try(pp <- polish_page(s[[i]]))

    # Save both raw and polished version 
    # We need these later to systematically identify failed cases
    # And to estimate the success fraction
    if ((is.character(a) && a == "try-error") || is.na(pp$pages)) {
      sp[[i]] <- NA
      raw[[i]] <- s[[i]]
    } else {
      tmp <- unname(unlist(pp$pages))
      tmp[is.infinite(tmp)] <- NA
      sp[[i]] <- tmp
      raw[[i]] <- unname(unlist(pp$raw))
    }
  }

  #print("Total page counts") # Calculate sum of parts
  totp <- as.numeric(sapply(sp, function (x) {sum(as.numeric(na.omit(x)))}))
  totp[totp == 0] <- NA # Set zero page counts to NA
  totp <- as.numeric(as.character(totp))
  
  sp <- as.numeric(as.character(sapply(sp, function (x) {paste(x, sep = " / ")})))
  raw <- as.character(sapply(raw, function (x) {paste(x, sep = " / ")}))

  data.frame(list(estimated.pages = sp, raw.pages = raw, total.pages = totp))

}




