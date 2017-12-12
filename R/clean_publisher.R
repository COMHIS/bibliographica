#' @title Clean Publisher
#' @description Preliminary cleanup for the publisher field.
#' @param x A vector of publisher names
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
  x <- xuniq <- unique(xorig)  

  # Used to be located after all the language specific stuff and before the generic stuff
  q <- x  
  
  # select the first item from the list
  # Just make sure that the first in the list is not [S.N.]
	if (length(grep("[[]?[Ss][.]?[Nn][.]?[]]?", q)) > 0) {
		q <- gsub("^([^;]+);([^;]+)(;?.*)$", "\\2", q)
	} else {
		q <- gsub("^([^;]+);.*$", "\\1", q)
  }
  
  # Remove possible remains of separator characters
  endings=c(" ", "\\.", ";", ":", ",", "'")
  q <- remove_endings(q, endings=endings, random_order=TRUE)
  
  # Select those things that aren't in brackets or parentheses
  q <- gsub("^[(](.*)[)]$", "\\1", q)
  q <- gsub("^[[](.*)[]]$", "\\1", q)
  q <- gsub("^([^(]+)[(].*[)]$", "\\1", q)
  q <- gsub("^([^[]+)[[].*[]]$", "\\1", q)
  q <- gsub("^[(].*[)]([^(]+)$", "\\1", q)
  q <- gsub("^[[].*[]]([^[]+)$", "\\1", q)
  
  # remove everything in brackets or parentheses after collecting i komm., distr., exp., för ... -information
  # TBD
   
  # remove naughty characters from the rear
  endings=c(" ", "\\(", "\\)", "\\[", "\\]", "\\.", ";", ":", ",", "'")
  q <- remove_endings(q, endings=endings, random_order=TRUE)
  
  # replace naughty characters from the middle
  # At least in Finto data there's "$b" separating two clauses, and this causes a problem for str_replace
  # I don't know what the real character should be, so I'll just select one at random
  q <- gsub(" [$]b", ".", q)
  q <- gsub(" [$]", "", q)

  # Back to original indices, then unique again;
  # reduces number of unique cases further
  xorig <- q[match(xorig, xuniq)]
  q <- xuniq <- unique(xorig)

  # Used to the first thing to do in this function
  # TODO we already have lots of these terms in inst/extdata/printstop_*.csv
  # could we just switch to those ?
  for (language in languages) {
    if (language == "swedish") {
      f <- system.file("extdata/sv_publisher.csv", package="bibliographica")
    } else if (language == "english") {
      f <- system.file("extdata/en_publisher.csv", package="bibliographica")
    } else if (language == "finnish") {
      f <- system.file("extdata/fi_publisher.csv", package="bibliographica")
    } else if (language == "latin") {
      f <- system.file("extdata/lat_publisher.csv", package="bibliographica")
    } else {
      print (paste0("Unknown language in languages: ", language))
    }

    synonyms <- read_mapping(f, sep = "\t", encoding = "UTF-8")    

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
  q <- str_trim(q)

  # Back to original indices
  qret <- q[match(xorig, xuniq)]

  qret

}