#' @title Get Relation Keyword
#' @description Pick keyword denoting that the person in question is a relative to the named person
#' @param x A character vector (e.g. publisher names)
#' @param full_name A vector of full names (from first name to last) it has been picked from publisher field
#' @param languages A vector of languages which are used in detecting relation keywords
#' @return Vector with relation keywords
#' @export
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @importFrom stringr str_extract
#' @references See citation("bibliographica")
#' @examples # get_relation_keyword(x, full_name, languages=c("finnish", "swedish", "latin"))
#' @keywords utilities
get_relation_keyword <- function(x, full_name, languages=c("english")) {
  
  # Adjustment to version 1.1.0 of stringr, which changed NA-behaviour
  x[which(is.na(x))] <- ""
  if (!is.null(full_name)) {
    full_name[which(is.na(full_name))] <- ""
  }
  
  # TODO: make a table for languages & related files & purpose
  f <- c()

  if ("finnish" %in% languages) {
    f[["finnish"]] <- system.file("extdata/fi_relation_keywords.csv", package="bibliographica")
  }
  if ("swedish" %in% languages) {
    f[["swedish"]] <- system.file("extdata/sv_relation_keywords.csv", package="bibliographica")
  }
  # TODO HR
  # en_relation_keywords.csv does not exist in bibliographica
  # also en_lowercase_keywords.csv was missing
  # add to github if available
  if ("latin" %in% languages) {
    f[["latin"]] <- system.file("extdata/la_relation_keywords.csv", package="bibliographica")
  }

  ret <- character(length = length(x))

  if (length(f) == 0) {
    return(ret)
  }

  
  for (fil in f) {
  
    synonyms <- read.csv(file=fil, sep="\t", fileEncoding="UTF-8")
  
    for (i in 1:nrow(synonyms)) {
      if (is.null(full_name)) {
        pattern <- as.character(synonyms$synonyme[i])
        replacement <- as.character(synonyms$name[i])
      } else {
        pattern <- str_replace(synonyms$synonyme[i], "<name>", full_name)
        replacement <- as.character(synonyms$name[i])
      }
      if (length(x) != 0) {
        inds <- which(is.na(str_extract(x, pattern)))
        if (length(inds) == 0) {
          # intentionally left empty        
        } else {
          inds <- intersect(which(!is.na(str_extract(x, pattern))), which(ret==""))
          ret[inds] <- replacement
        }
      }

    }
  }
  ret
  
}