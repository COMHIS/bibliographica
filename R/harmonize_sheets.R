harmonize_sheets <- function (s, harm) {
  
  if (length(grep("[0-9]leaf", s))>0) {
    s <- gsub("leaf", " leaf", s)
  }

  # Harmonize
  s <- as.character(map(s, harm, mode = "recursive"))

  # Harmonize '* sheets'
  sheet.inds <- grep("sheet", s)
  s[sheet.inds] <- sapply(s[sheet.inds], function (si) {harmonize_sheets_help(si)})

  # Move into sheet synonyme table (had some problems hence here for now) ?
  s <- gsub("leaf", "sheet", s)
  s <- gsub("leave", "sheet", s)     
  s <- gsub("^1 sheets$", "1 sheet", s)
  s <- gsub("^sheet$", "1 sheet", s)
  s <- gsub("^sheets$", "2 sheets", s)
  s <- gsub("1 leaf \\(\\[2\\]p\\.\\)", "1 sheet", s)

  s <- condense_spaces(s)

  s 

}



harmonize_sheets_help <- function (s) {

  spl <- unlist(strsplit(s, ","))

  for (i in 1:length(spl)) {

    #if (length(grep("^[0-9] sheet", spl[[i]])) > 0) {
    #  xxx <- unlist(strsplit(spl[[i]], "sheet"), use.names = FALSE)
    #  n <- as.numeric(str_trim(xxx[[1]]))
    #  spl[[i]] <- paste(n, "sheets", sep = " ")
    #}

    if (length(grep("\\[^[0-9]|[a-z]\\] sheets", spl[[i]])) > 0) {
      xxx <- unlist(strsplit(spl[[i]], "sheet"), use.names = FALSE)
      n <- as.numeric(as.roman(str_trim(gsub("\\[", "", gsub("\\]", "", xxx[[1]])))))
      spl[[i]] <- paste(n, "sheets", sep = " ")
    }

  }

  # Combine again
  s <- paste(spl, collapse = ",")

  s
}