#' @title Decapitate keywords
#' @description Removes keywords
#' @param x Vector of personal names
#' @param languages A vector of languages which are used in detecting keywords
#' @return Vector of words
#' @export
#' @importFrom stringr str_replace
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @examples # extract_personal_names(x, languages=c("finnish", "swedish", "latin"))
#' @keywords utilities
decapitate_keywords <- function(x, languages=c("english")) {
  
  # TODO: make a table for languages & related files & purpose
  f <- vector(length=length(languages))
  i <- 1
  if ("finnish" %in% languages) {
    f[i] <- system.file("extdata/fi_lowercase_keywords.csv", package="bibliographica")
    i <- i + 1
  }
  if ("swedish" %in% languages) {
    f[i] <- system.file("extdata/sv_lowercase_keywords.csv", package="bibliographica")
    i <- i + 1
  }
  if ("english" %in% languages) {
    f[i] <- system.file("extdata/en_lowercase_keywords.csv", package="bibliographica")
    i <- i + 1
  }
  if ("latin" %in% languages) {
    f[i] <- system.file("extdata/la_lowercase_keywords.csv", package="bibliographica")
    i <- i + 1
  }
  
  for (fil in f) {
    synonyms <- read.csv(file=fil, sep="\t", fileEncoding="UTF-8")
    for (i in 1:nrow(synonyms)) {
      x <- str_replace(x, as.character(synonyms$synonyme[i]), as.character(synonyms$name[i]))
    }
  }
  x
  
}