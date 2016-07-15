#' @title Clean Publisher
#' @description Pre-cleans publisher field.
#' @param x Vector of publisher names
#' @param languages A vector of languages which are used in detecting relation keywords
#' @return Vector of publisher names
#' @export
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @examples # clean_publisher(x, languages=c("finnish", "swedish", "latin"))
#' @keywords utilities
clean_publisher <- function(x, languages=c("english")) {

  # Only consider unique terms to speed up		
  xorig <- as.character(x)
  q <- x <- xuniq <- unique(xorig)  

  # Used to the first thing to do in this function
  for (language in languages) {
    if (language=="swedish") {
      f <- system.file("extdata/sv_publisher.csv", package="bibliographica")
    } else if (language=="english") {
      f <- system.file("extdata/en_publisher.csv", package="bibliographica")
    } else if (language=="finnish") {
      f <- system.file("extdata/fi_publisher.csv", package="bibliographica")
    } else if (language=="latin") {
      f <- system.file("extdata/lat_publisher.csv", package="bibliographica")
    } else {
      print (paste0("Unknown language in languages: ", language))
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
  # TODO: might be useful also for polishing some other fields - to make a generic function ?
  q <- gsub("\\b([[:upper:]])[.]?[ ]?([[:upper:]])[.]?[ ]?([[:upper:]])[.]?[ ]?([[:upper:]][[:lower:]])", "\\1.\\2.\\3. \\4", q)
  q <- gsub("\\b([[:upper:]])[.]?[ ]?([[:upper:]])[.]?[ ]?([[:upper:]][[:lower:]])", "\\1.\\2. \\3", q)
  q <- gsub("\\b([[:upper:]])[.]?[ ]?([[:upper:]][[:lower:]])", "\\1. \\2", q)


  # Back to original indices
  qret <- q[match(xorig, xuniq)]

  qret

}