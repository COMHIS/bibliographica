harmonize_sheets <- function (s) {

  s <- gsub("[0-9]leaf", paste0(substr(s, 1, 1), " leaf"), s)

  # Read the mapping table
  f <- system.file("extdata/harmonize_sheets.csv", package = "bibliographica")
  harm <- as.data.frame(read.csv(f, sep = "\t", stringsAsFactors = FALSE))

  # Harmonize
  for (i in 1:nrow(harm)) {
    s <- gsub(harm$synonyme[[i]], harm$name[[i]], s)
  }  

  # Harmonize '* sheets'
  spl <- unlist(strsplit(s, ","))

  sheet.inds <- grep("sheet", spl)
  for (i in sheet.inds) {

    if (length(grep("^[0-9] sheet", s)) > 0) {
      n <- as.numeric(str_trim(unlist(strsplit(spl[[i]], "sheet"))[[1]]))
      spl[[i]] <- paste(n, "sheets", sep = " ")
    }

    if (length(grep("\\[^[0-9]|[a-z]\\] sheets", s)) > 0) {
      n <- as.numeric(as.roman(str_trim(gsub("\\[", "", gsub("\\]", "", unlist(strsplit(spl[[i]], "sheet"))[[1]])))))
      spl[[i]] <- paste(n, "sheets", sep = " ")
    }

    s <- paste(spl, collapse = ",")
    s <- gsub("1 sheets", "1 sheet", s)

  }

  s 

}


