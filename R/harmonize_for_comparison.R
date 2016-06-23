#' @title Harmonize for comparison
#' @description Harmonize orthography
#' @param x A vector of publisher names
#' @param language language which is used in detecting orthographical variants
#' @return Vector of publishers names
#' @export
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @examples # harmonize_for_comparison(x, language="finnish")
#' @keywords utilities
harmonize_for_comparison <- function(x, language="english") {
  
  if (language=="swedish") {
    f <- system.file("extdata/sv_publisher_comparison.csv", package="bibliographica")
  } else if (language=="english") {
    
  } else if (language=="finnish") {
    f <- system.file("extdata/fi_publisher_comparison.csv", package="bibliographica")
  } else {
    
  }

  synonyms <- read.csv(f, sep = "\t", fileEncoding = "UTF-8")
  
  x <- map(x, synonyms, mode="recursive")
  x

}
