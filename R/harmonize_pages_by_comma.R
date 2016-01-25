# A single instance of pages within commas
harmonize_pages_by_comma <- function (s) {

  # Harmonize i.e.  
  s <- handle_ie(s, harmonize = FALSE)

  # Convert plates to pages
  s <- plates2pages(s)

  # 165-167 leaves -> 165-167
  if (length(grep("-", s))>0 && length(grep("leaves", s))>0) {
    s <- gsub("leaves", "", s)
  }

  # After plate operations handle p/s ("pages" / "sivua")
  if (length(grep("plates", s)) == 0) {
    s <- gsub("[p|s]\\.\\)", " ", s)
    s <- gsub("[p|s] *$", " ", s)
    s <- gsub("[p|s]\\.]$", " ", s)
    s <- gsub(" [p|s]\\.{0,1} {0,1}\\]$", " ", s)
  }

  # p66 -> 1
  if (length(grep("^p", s)) > 0 && length(grep("-", s)) == 0) {
    if (is.numeric(str_trim(gsub("^p", "", s)))) { s <- 1 }
  } else if (length(grep("^p", s)) > 0 && length(grep("-", s)) > 0) {
    s <- str_trim(gsub("^p", "", s))
  }

  # Handle some odd cases
  s <- condense_spaces(s)
  s[s == ""] <- NA

  s

}
