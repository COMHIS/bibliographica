# A single instance of pages within commas
harmonize_pages_by_comma <- function (s) {

  # 30 [32]
  if (length(grep("[0-9]+ \\[[0-9]+\\]", s))>0) {
    s <- paste(unlist(strsplit(s, " "), use.names = FALSE)[-1], collapse = " ")
  }

  # Convert plates to pages
  s <- plates2pages(s)

  # 165-167 leaves -> 165-167
  if (length(grep("-", s))>0 && length(grep("leaves", s))>0) {
    s <- gsub("leaves", "", s)
  }

  # After plate operations handle p/s ("pages" / "sivua")
  s <- condense_spaces(s)

  if (length(grep("plates", s)) == 0 && !is.na(s) && length(s) > 0 && length(grep("^sheets*", s))==0) {
    s <- gsub("^[p|s]", "", s)    
    s <- gsub("^[p|s]\\.\\)", " ", s)
    s <- gsub("[p|s] *$", " ", s)
    s <- gsub("^[p|s] ", "", s)
    s <- gsub("[p|s]\\.]$", " ", s)
    s <- gsub(" [p|s]\\.{0,1} {0,1}\\]$", " ", s)
  }

  # Handle some odd cases
  s <- condense_spaces(s)
  s[s == ""] <- NA

  s

}
