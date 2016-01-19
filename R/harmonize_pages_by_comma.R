
# A single instance of pages within commas
harmonize_pages_by_comma <- function (s) {

  # Harmonize i.e.  
  s <- handle_ie(s)

  # Harmonize '1 sheet'
  if (!is.na(s) && (length(grep("1 sheet", s)) > 0 || s == "sheet")) {
    s <- "1 sheet" 
  }

  # Harmonize '* sheets'
  if (length(grep("^[2-9] sheets", s)) > 0) {
    s <- paste(as.numeric(str_trim(unlist(strsplit(s, "sheet"))[[1]])), "sheets", sep = " ")
  }

  # Harmonize '1 broadside'
  if (length(grep("1 broadside", s)) > 0) {
    s <- "1 broadside" 
  }

  # 165-167 leaves -> 165-167
  if (length(grep("-", s))>0 && length(grep("leaves", s))>0) {
    s <- str_trim(gsub("leaves", "", s))
  }

  # Convert plates to pages
  s <- plates2pages(s)

  # After plate operations handle p
  if (length(grep("plates", s)) == 0) {
    s <- gsub("pages", " ", s)
    s <- gsub("page", " ", s)
    s <- gsub("sivua", " ", s)
    s <- gsub("[p|s]\\.\\)", " ", s)
    s <- gsub("[p|s]$", " ", s)
    s <- gsub("[p|s]\\.]$", " ", s)
    s <- gsub(" [p|s] \\]$", " ", s)
    s <- gsub(" [p|s]\\]$", " ", s)
  }
  # p66 -> 1
  if (length(grep("^p", s)) > 0 && length(grep("-", s)) == 0) {
    tmp <- as.numeric(str_trim(gsub("^p", "", s)))
    if (!is.na(tmp)) { s <- 1 }
  } else if (length(grep("^p", s)) > 0 && length(grep("-", s)) > 0) {
    s <- str_trim(gsub("^p", "", s))
  }

  # Handle some odd cases
  s <- gsub("a-m", " ", s)
  s <- trimming(s,n = 5)
  s <- condense_spaces(s)
  s[s == ""] <- NA

  s

}
