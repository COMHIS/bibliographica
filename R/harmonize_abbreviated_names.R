#' @title Harmonize abbreviated names
#' @description Writes out abbreviated names.
#' @param x A vector of publisher names
#' @param languages Language which are used in writing out the abbreviated names
#' @return Vector of publishers names
#' @export
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @examples # harmonize_abbreviated_names(c("Phil.", "Joh."), languages=c("finnish"))
#' @keywords utilities
harmonize_abbreviated_names <- function(x, languages="english") {
  
  for (language in languages) {
    if (language=="swedish") {
      f <- system.file("extdata/sv_abbreviated_names.csv", package="bibliographica")
    } else if (language=="english") {
      f <- system.file("extdata/en_abbreviated_names.csv", package="bibliographica")
    } else if (language=="finnish") {
      f <- system.file("extdata/fi_abbreviated_names.csv", package="bibliographica")
    } else if (language=="latin") {
      f <- system.file("extdata/la_abbreviated_names.csv", package="bibliographica")
    } else {
      message(paste0("Unknown language in harmonize_abbreviated_names: ", language))
      f <- NULL      			      
    }

    if (!is.null(f)) {
      synonyms <- read.csv(f, sep = "\t", fileEncoding = "UTF-8")
      inds <- which(!is.na(x))
      x[inds] <- map(x[inds], synonyms, mode = "recursive")
    }
    
  }
  
  x
  
}