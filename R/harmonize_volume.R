harmonize_volume <- function (x) {

  s <- condense_spaces(x)
  s[s %in% c("^v$", "^v;$", "^v ;$", "^v :$")] <- "v"
  s <- gsub("v\\.:bill\\. ;", NA, s)
  s[s == "^v\\. ;"] <- NA

  # Harmonize synonymous terms
  s <- gsub("atlas", " vol", s)
  s <- gsub("Vol", " vol", s)
  s <- gsub("vol", " vol", s)
  s <- gsub("vols", " vol", s)
  s <- condense_spaces(s)

  # " vol" -> " v." etc
  vol.synonymes <- c("vol")
  for (vnam in vol.synonymes) {
    s <- gsub(paste(" ", vnam, "\\. $", sep = ""), " v. ", s)
    s <- gsub(paste(" ", vnam, "\\.$", sep = ""), " v. ", s)
    s <- gsub(paste(" ", vnam, " $", sep = ""), " v. ", s)
    s <- gsub(paste(" ", vnam, "$", sep = ""), " v. ", s)
    s <- gsub(paste("^", vnam, "\\.", sep = ""), "v. ", s)
    s <- gsub(paste(vnam, "\\.$", sep = ""), " v. ", s)
    s <- gsub(paste(vnam, " $", sep = ""), " v. ", s)

  }
  s <- condense_spaces(s)

  s <- gsub("^v\\. ", "v.", s)
  s <- gsub("^v\\.\\(", "(", s)
  s <- gsub("^v\\.,", "", s)
  s <- gsub(" v $", " v.", s)
  s <- gsub(" v\\.$", " v.", s)
  s <- gsub(" v$", " v.", s)
  s <- gsub(" v\\.", " v. ", s)
  s <- condense_spaces(s)

  hvol <- function (s) {
    if (length(grep("^[0-9][0-9][0-9] v\\.", s)) > 0 || length(grep("^[0-9][0-9] v\\.", s)) > 0 || length(grep("^[0-9] v\\.", s)) > 0) {
      si <- unlist(strsplit(s, "v\\."))
      s <- paste(si[[1]], "v.", si[-1], collapse = " ")
    }

    # 2 v ; -> 2v.
    if (length(grep("^[0-9] v ", s))>0) {
      si <- unlist(strsplit(s, " "))
      s <- paste(si[[1]], " v.", s[-1], collapse = "")
    }
    s
  }
  s <- sapply(s, function (x) {hvol(x)})
  s <- condense_spaces(s)

  # "2 v " -> "2v." and "2v " -> "2v."
  s <- sapply(s, function (si) {gsub("^[0-9] ?v ", paste0(substr(si, 1, 1), "v."), si)})
  s <- sapply(s, function (si) {gsub("^[0-9] ?v$", paste0(substr(si, 1, 1), "v."), si)})
  s <- gsub("v\\. ", "v\\.", s)
  
  s
}

