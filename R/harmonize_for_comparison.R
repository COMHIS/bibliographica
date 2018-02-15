#' @title Harmonize for Comparison
#' @description Harmonize orthography
#' @param x A vector of publisher names
#' @param languages languages used to detect orthographical variants
#' @return Vector of publishers names
#' @export
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @examples # harmonize_for_comparison(x, languages = "finnish")
#' @keywords utilities
harmonize_for_comparison <- function(x, languages = "english") {

  message(languages)
  
  if ("swedish" %in% languages) {
    f <- system.file("extdata/sv_publisher_comparison.csv", package="bibliographica")
  } else if ("finnish" %in% languages) {
    f <- system.file("extdata/fi_publisher_comparison.csv", package="bibliographica")
  } else {
    return(NULL)    
  }

  synonyms <- read.csv(f, sep = "\t", fileEncoding = "UTF-8")  
  map(x, synonyms, mode="recursive")


}
