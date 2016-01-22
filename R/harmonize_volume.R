harmonize_volume <- function (x, verbose = FALSE) {

  if (verbose) {message("Initial harmonization")}
  s <- condense_spaces(x)
  s[s %in% c("^v :$", "^v ;$", "^v:$", "^v;$")] <- "v"
  s[s %in% c("^v\\. ;", "v\\.:bill\\. ;")] <- NA  

  # FIXME list in separate file
  if (verbose) {message("Synonymous terms")}
  s <- gsub("atlas", " vol", s)
  s <- gsub("vol", " vol", s)
  s <- gsub("vols", " vol", s)
  s <- gsub("osaa", " vol", s)
  s <- gsub("nid\\.", " vol", s)
  s <- gsub("vihkoa", " vol", s)
  s <- gsub("parts", " pts", s)
  s <- gsub("part ", " pts", s)
  s <- gsub("part$", " pts", s)  
  s <- condense_spaces(s)

  if (verbose) {message("Volume terms")}
  s <- gsub("^vol\\.", "v. ", s)
  s <- gsub(" {0,1}vol\\.{0,1} {0,1}$", " v. ", s)
  s <- gsub("^v\\.\\(", "(", s)
  s <- gsub("^v\\.,", "", s)
  s <- gsub(" v {0,1}$", " v.", s)
  s <- condense_spaces(s)

  # "2 v " -> "2v." and "2v " -> "2v."
  s <- sapply(s, function (si) {gsub("^[0-9]* ?v ", paste0(substr(si, 1, 1), "v."), si)})
  s <- sapply(s, function (si) {gsub("^[0-9]* ?v$", paste0(substr(si, 1, 1), "v."), si)})
  s <- gsub(" {0,1}v\\. ", "v\\.", s)

  s

}

