harmonize_sheets <- function (s, harm) {
  
  if (length(grep("[0-9]leaf", s))>0) {
    s <- gsub("leaf", " leaf", s)
  }

  # Harmonize
  s <- as.character(harmonize_names(s, harm, mode = "recursive"))

  # Harmonize '* sheets'
  spl <- strsplit(s, ",")

  sheet.inds <- grep("sheet", spl)

  for (i in sheet.inds) {
  
    if (length(grep("^[0-9] sheet", s[[i]])) > 0) {
      xxx <- unlist(strsplit(spl[[i]], "sheet"), use.names = FALSE)
      n <- as.numeric(str_trim(xxx[[1]]))
      spl[[i]] <- paste(n, "sheets", sep = " ")
    }

    if (length(grep("\\[^[0-9]|[a-z]\\] sheets", s[[i]])) > 0) {
      xxx <- unlist(strsplit(spl[[i]], "sheet"), use.names = FALSE)
      n <- as.numeric(as.roman(str_trim(gsub("\\[", "", gsub("\\]", "", xxx[[1]])))))
      spl[[i]] <- paste(n, "sheets", sep = " ")
    }

    s[[i]] <- paste(spl[[i]], collapse = ",")

  }

  s <- gsub("1 sheets", "1 sheet", s)

  s 

}


