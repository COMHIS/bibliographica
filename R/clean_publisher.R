clean_publisher <- function(x, languages=c("english")) {

  # Used to be located after all the language specific stuff and before the generic stuff
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
  
  # replace naughty characters from the middle
  # At least in Finto data there's "$b" separating two clauses, and this causes a problem for str_replace
  # I don't know what the real character should be, so I'll just select one at random
  q <- gsub(" [$]b", ".", q)
  q <- gsub(" [$]", "", q)
  
  # Used to the first thing to do in this function
  for (language in languages) {
    if (language=="swedish") {
      f <- "sv_publisher.csv"
    } else if (language=="english") {
      f <- "en_publisher.csv"
    } else if (language=="finnish") {
      f <- "fi_publisher.csv"  
    } else if (language=="latin") {
      f <- "lat_publisher.csv"
    } else {
      print (paste0("Unknown language in languages: ", language))
    }
    
    synonyms <- read.csv(f, sep = "\t", fileEncoding = "UTF-8")
    q <- harmonize_names(q, synonyms, mode="recursive")
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
  
  q
}