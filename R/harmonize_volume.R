harmonize_volume <- function (x, verbose = FALSE, vol.synonyms = NULL) {

  if (is.null(vol.synonyms)) {
    f <- system.file("extdata/harmonize_volume.csv", package = "bibliographica")
    vol.synonyms <- read_mapping(f, sep = ";", mode = "table")  
  }

  if (verbose) {message("Initial harmonization")}
  s <- condense_spaces(x)
  s[grep("^v {0,1}[:|;]$", s)] <- "v"  
  s[s %in% c("v\\. ;", "v\\.:bill\\. ;")] <- NA  

  # FIXME can we put this in synonymes
  s <- gsub("vols*\\.", "v.", s)

  # FIXME list in separate file
  if (verbose) {message("Synonymous terms")}
  s <- map(s, vol.synonyms, mode = "match")
  s <- condense_spaces(s)

  # FIXME these should be done via synonyme list ?
  if (verbose) {message("Volume terms")}
  s <- gsub("^vol\\.", "v. ", s)
  s <- gsub("vol\\.{0,1} {0,1}$", " v. ", s)
  s <- gsub("vol\\.", " v. ", s)  
  s <- gsub("^v\\.\\(", "(", s)
  s <- gsub(" v {0,1}$", " v.", s)
  s <- condense_spaces(s)

  # "2 v " -> "2v." and "2v " -> "2v."
  s <- sapply(s, function (si) {gsub("^[0-9]* ?v ", paste0(substr(si, 1, 1), "v."), si)}, USE.NAMES = FALSE)

  s <- sapply(s, function (si) {gsub("^[0-9]+ ?v$", paste0(gsub("v$", "", si), "v."), si)}, USE.NAMES = FALSE)

  s <- gsub(" v\\. ", "v\\.", s)

  if (length(grep("^v.[0-9]+,[0-9]+", s)) > 0) {
    spl <- unlist(strsplit(s, ","), use.names = FALSE)
    s <- paste(spl[[1]], "-", spl[-1], sep = "")
  }

  s

}

