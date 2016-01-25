harmonize_sheets <- function (s, harm) {
  
  if (length(grep("[0-9]leaf", s))>0) {
    s <- gsub("leaf", " leaf", s)
  }

  # Harmonize
  s <- as.character(harmonize_names(s, harm, mode = "recursive", check.synonymes = FALSE))

  # Harmonize '* sheets'
  spl <- unlist(strsplit(s, ","), use.names = FALSE)

  sheet.inds <- grep("sheet", spl)

  for (i in sheet.inds) {

    if (length(grep("^[0-9] sheet", s)) > 0) {
      n <- as.numeric(str_trim(unlist(strsplit(spl[[i]], "sheet"), use.names = FALSE)[[1]]))
      spl[[i]] <- paste(n, "sheets", sep = " ")
    }

    if (length(grep("\\[^[0-9]|[a-z]\\] sheets", s)) > 0) {
      n <- as.numeric(as.roman(str_trim(gsub("\\[", "", gsub("\\]", "", unlist(strsplit(spl[[i]], "sheet"), use.names = FALSE)[[1]])))))
      spl[[i]] <- paste(n, "sheets", sep = " ")
    }

    s <- paste(spl, collapse = ",")
    s <- gsub("1 sheets", "1 sheet", s)

  }

  s 

}


