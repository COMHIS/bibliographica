#' @title clean publisher destructively
#' @description Highly data specific cleansing of publisher field into a temporary form
#' @param x A vector of publisher names
#' @param languages A vector of languages which are used in detecting relation keywords
#' @return Vector of publisher names
#' @export
#' @author Hege Roivainen \email{hege.roivainen@@gmail.com}
#' @references See citation("bibliographica")
#' @examples # clean_publisher(x, languages=c("finnish", "swedish", "latin"))
#' @keywords utilities
clean_publisher_destructively <- function(x, languages=c("english")) {
  
  # Only consider unique terms to speed up		
  xorig <- as.character(x)
  x <- xuniq <- unique(xorig)  
  
  # Used to be located after all the language specific stuff and before the generic stuff
  q <- x
  
  for (language in languages) {
    if (language == "swedish") {
      msg <- "extdata/sv_publisher_destructive.csv"
      f <- system.file("extdata/sv_publisher_destructive.csv", package="bibliographica")
    } else if (language == "english") {
      f <- system.file("extdata/en_publisher_destructive.csv", package="bibliographica")
      msg <- "extdata/en_publisher_destructive.csv"
    } else if (language == "finnish") {
      f <- system.file("extdata/fi_publisher_destructive.csv", package="bibliographica")
      msg <- "extdata/fi_publisher_destructive.csv"
    } else if (language == "latin") {
      f <- system.file("extdata/lat_publisher_destructive.csv", package="bibliographica")
      msg <- "extdata/la_publisher_destructive.csv"
    } else {
      print (paste0("Unknown language in languages: ", language))
    }
    if (f == "") {
      message(paste0("File: ", msg, " doesn't exist"))
    } else {
      #synonyms <- read.csv(f, sep = "\t", fileEncoding = "UTF-8")
      synonyms <- read_mapping(f, sep = "\t", encoding = "UTF-8")    
    
      q <- map(q, synonyms, mode="recursive")
      # HR 2016-10-20: Revert operations, if everything is destroyed
      # There'll be a separate handling for S.N.
      inds <- which(str_trim(q)=="")
      q[inds] <- x[inds]
    }
  } 
  q <- str_trim(q)
  
  # Back to original indices
  qret <- q[match(xorig, xuniq)]
  
  qret
  
}