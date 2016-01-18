harmonize_volume <- function (x, verbose = FALSE) {

  if (verbose) {message("Initial harmonization")}
  s <- condense_spaces(x)
  s[s %in% c("^v$", "^v;$", "^v ;$", "^v :$")] <- "v"
  s <- gsub("v\\.:bill\\. ;", NA, s)
  s[s == "^v\\. ;"] <- NA

  if (verbose) {message("Synonymous terms")}
  s <- gsub("atlas", " vol", s)
  s <- gsub("Vol", " vol", s)
  s <- gsub("vol", " vol", s)
  s <- gsub("vols", " vol", s)
  s <- gsub("osaa", " vol", s)
  s <- gsub("nid\\.", " vol", s)
  s <- gsub("vihkoa", " vol", s)      
  s <- condense_spaces(s)

  if (verbose) {message("Volume terms")}
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

  if (verbose) {message("Volume harmonization")}
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

