#' @title Harmonize Publisher
#' @description Harmonizes publishers with slight misspellings.
#' @param x A vector of publisher names
#' @param publication_year Data frame with "from" and "till" years
#' @param languages A vector of languages which are used in detecting relation keywords
#' @return Data frame with orig, mod and comp 
#' @export
#' @importFrom stringdist amatch
#' @importFrom stringr str_replace
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @examples # harmonize_publisher(x, publication_year, languages=c("finnish", "swedish", "latin"))
#' @keywords utilities
harmonize_publisher <- function(x, publication_year, languages=c("english")) {
  
  # Only consider unique terms to speed up		
  xorig <- as.character(x)
  x <- xuniq <- unique(xorig)  
  
  q <- x
  
  # select the first item from the list
  q <- gsub("^([^;]+);.*$", "\\1", q)
  q <- gsub("^([^(]+)[(].*[)]$", "\\1", q)
  q <- gsub("^([^[]+)[[].*[]]$", "\\1", q)
  q <- gsub("^[(].*[)]([^(]+)$", "\\1", q)
  q <- gsub("^[[].*[]]([^[]+)$", "\\1", q)
  
  # remove everything in brackets or parentheses after collecting i komm., distr., exp., för ... -information
  # TBD
  
  # remove naughty characters from the rear
  endings=c(" ", "\\(", "\\)", "\\[", "\\]", "\\.", ";", ":", ",", "'")
  q <- remove_endings(q, endings=endings, random_order=TRUE)
  
  # Back to original indices, then unique again;
  # reduces number of unique cases further
  xorig <- q[match(xorig, xuniq)]
  q <- xuniq <- unique(xorig)
  
  for (language in languages) {
    if (language=="swedish") {
      f <- system.file("extdata/sv_publisher.csv",package = "bibliographica")
    } else if (language=="english") {
      
    } else if (language=="finnish") {
      f <- system.file("extdata/fi_publisher.csv", package = "bibliographica")  
    }
    synonyms <- read.csv(f, sep = "\t", fileEncoding = "UTF-8")
    q <- map(q, synonyms, mode="recursive")
  }
    
  # remove brackets and parentheses (Destructive phase)
  q <- gsub("^[(](.*)[)]$", "\\1", q)
  q <- gsub("^[[](.*)[]]$", "\\1", q)
  q <- gsub("[[]", "", q)
  q <- gsub("[]]", "", q)
  q <- gsub("[(]", "", q)
  q <- gsub("[)]", "", q)
  
  # add space when needed
  q <- gsub("([[:upper:]])&", "\\1 &", q)
  q <- gsub("&([[:upper:]])", "& \\1", q)
  q <- gsub("([[:lower:]])&", "\\1 &", q)
  q <- gsub("&([[:lower:]])", "& \\1", q)
  
  # harmonize initials
  # CWK Raivoinen -> C.W.K. Raivoinen; C. W. K. Raivoinen -> C.W.K. Raivoinen
  q <- gsub("\\b([[:upper:]])[.]?[ ]?([[:upper:]])[.]?[ ]?([[:upper:]])[.]?[ ]?([[:upper:]][[:lower:]])", "\\1.\\2.\\3. \\4", q)
  q <- gsub("\\b([[:upper:]])[.]?[ ]?([[:upper:]])[.]?[ ]?([[:upper:]][[:lower:]])", "\\1.\\2. \\3", q)
  q <- gsub("\\b([[:upper:]])[.]?[ ]?([[:upper:]][[:lower:]])", "\\1. \\2", q)
  
  # Back to original indices
  q <- q[match(xorig, xuniq)]
  
  q
  
}
