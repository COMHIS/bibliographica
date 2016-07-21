clean_publisher <- function (x) {

  x <- as.character(x)

  # remove brackets and parentheses (Destructive phase)
  x <- gsub("^[(](.*)[)]$", "\\1", x)
  x <- gsub("^[[](.*)[]]$", "\\1", x)
  x <- gsub("[[]", "", x)
  x <- gsub("[]]", "", x)
  x <- gsub("[(]", "", x)
  x <- gsub("[)]", "", x)
  
  # add space when needed
  x <- gsub("([[:upper:]])&", "\\1 &", x)
  x <- gsub("&([[:upper:]])", "& \\1", x)
  x <- gsub("([[:lower:]])&", "\\1 &", x)
  x <- gsub("&([[:lower:]])", "& \\1", x)
  
  # harmonize initials
  # CWK Raivoinen -> C.W.K. Raivoinen; C. W. K. Raivoinen -> C.W.K. Raivoinen
  # TODO: might be useful also for polishing some other fields - to make a generic function ?
  x <- gsub("\\b([[:upper:]])[.]?[ ]?([[:upper:]])[.]?[ ]?([[:upper:]])[.]?[ ]?([[:upper:]][[:lower:]])", "\\1.\\2.\\3. \\4", x)
  x <- gsub("\\b([[:upper:]])[.]?[ ]?([[:upper:]])[.]?[ ]?([[:upper:]][[:lower:]])", "\\1.\\2. \\3", x)
  x <- gsub("\\b([[:upper:]])[.]?[ ]?([[:upper:]][[:lower:]])", "\\1. \\2", x)

  x

}