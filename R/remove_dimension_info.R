#' Remove dimension information from a single document
#'
#' @param x A character vector that may contain dimension information
#' @return The character vector with dimension information removed
#'
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples remove_dimension_info("4to 40cm")
#' @export
#' @keywords internal
remove_dimension_info <- function (x) {

  s <- harmonize_dimension(x)

  f <- system.file("extdata/remove_dimension_info.csv", package = "bibliographica")
  terms <- as.character(read.csv(f)[,1])

  for (term in terms) {
    s <- gsub(term, " ", s)
  }
  s <- remove_endings(s, c(":", ";", "\\."))
  s <- str_trim(s)
  s[s == ""] <- NA

  s

}

