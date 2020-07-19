#' @title Remove Dimension Info
#' @description Remove dimension information from a single document.
#' @param x A character vector that may contain dimension information
#' @return The character vector with dimension information removed
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{remove_dimension_info("4to 40cm")}
#' @keywords internal
remove_dimension_info <- function (x, harmonize = TRUE) {

  s <- x

  if (harmonize) {
    s <- harmonize_dimension(s)
  }
  
  f <- read_sysfile("extdata/remove_dimension_info.csv", "bibliographica")    
  terms <- as.character(read.csv(f)[,1])

  for (term in terms) {
    s <- gsub(term, " ", s)
  }
  s <- remove_endings(s, c(":", ";", "\\."))
  s <- str_trim(s)
  s[s == ""] <- NA

  s

}

