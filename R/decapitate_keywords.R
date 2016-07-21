#' @title Decapitate keywords
#' @description Removes keywords.
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

  if ("finnish" %in% languages) {
    f[["finnish"]] <- system.file("extdata/fi_lowercase_keywords.csv", package="bibliographica")
  }
  if ("swedish" %in% languages) {
    f[["swedish"]] <- system.file("extdata/sv_lowercase_keywords.csv", package="bibliographica")
  }
  if ("latin" %in% languages) {
    f[["latin"]] <- system.file("extdata/la_lowercase_keywords.csv", package="bibliographica")
  }
  
  for (fil in f) {
    message(fil)
    synonyms <- read.csv(file = fil, sep="\t", fileEncoding="UTF-8")
    x <- map(x, synonyms, mode = "match") # Should be faster
    #for (i in 1:nrow(synonyms)) {
    #  x <- str_replace(x, as.character(synonyms$synonyme[i]), as.character(synonyms$name[i]))
    #}
  }
  x
  
}