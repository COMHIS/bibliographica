harmonize_volume <- function (x, verbose = FALSE, vol.synonyms = NULL) {

  if (is.null(vol.synonyms)) {
    f <- system.file("extdata/harmonize_volume.csv", package = "bibliographica")
    if (f == "") {
      f <- system.file("inst/extdata/harmonize_volume.csv", package = "bibliographica")
    }
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

  s <- gsub("\\(\\?\\) v\\.", "v.", s)
  s <- gsub("^vol\\.", "v. ", s)
  s <- gsub("\ *vol\\.{0,1} {0,1}$", " v. ", s)
  s <- gsub("\ *vol\\.", "v. ", s)
  s <- gsub(" vol\\.* *;*", "v. ", s)
  s <- gsub(" vol;", "v. ", s)      
  s <- gsub("^v\\.\\(", "(", s)
  s <- gsub(" v {0,1}$", "v. ", s)
  s <- condense_spaces(s)

  # "65,2v " -> "67v"
  inds <- grep("^[0-9]+,[0-9] *v", s)
  if (length(inds) > 0) {
    s[inds] <- sapply(s[inds], function (x) {vol_helper4(x)})
  }

  # "2 v " -> "2v"
  inds <- grep("^[0-9]+ v", s)
  if (length(inds) > 0) {
    s[inds] <- sapply(s[inds], function (x) {vol_helper3(x)})
  }

  # "2v " -> "2v."
  inds <- grep("^[0-9]+v[ |,]+", s)
  if (length(inds) > 0) {
    s[inds] <- sapply(s[inds], function (x) {vol_helper(x)})
  }

  inds <- grep("^[0-9]v\\(", s)
  if (length(inds) > 0) {
    s[inds] <- gsub("v\\(", "v.(", s[inds])
  }

  s <- sapply(s, function (si) {gsub("^[0-9]+ *v$", paste0(gsub("v$", "", si), "v."), si)}, USE.NAMES = FALSE)

  s <- gsub(" v\\. ", "v\\.", s)

  inds <- grep("^v.[0-9]+,[0-9]+", s)
  if (length(inds) > 0) {
    s[inds] <- sapply(s[inds], function (x) {vol_helper2(x)})
  }

  s

}


vol_helper <- function (s) {

     s <- gsub("v,", "v ,", s)

     spl <- unlist(strsplit(s, " "), use.names = FALSE)

     spl[[1]] <- gsub("v", "v.", spl[[1]])

     s <- spl
     
     if (length(spl) > 1) {     
       s <- paste(spl[1:2], collapse = "")   
     }

     if (length(spl) > 2) {
       s <- paste(s, paste(spl[3:length(spl)], collapse = " "), collapse = "")
     }

  s
}

vol_helper3 <- function (s) {

  # TODO could be combined with vol_helper to speed up	    

     spl <- unlist(strsplit(s, " "), use.names = FALSE)

     s <- spl
     
     if (length(spl) > 1) {     
       s <- paste(spl[1:2], collapse = "")   
     }

     if (length(spl) > 2) {
       s <- paste(s, paste(spl[3:length(spl)], collapse = " "), collapse = "")
     }

  s
}


vol_helper2 <- function (s) {
    spl <- unlist(strsplit(s, ","), use.names = FALSE)
    s <- paste(spl[[1]], "-", spl[-1], sep = "")
    s
}

vol_helper4 <- function (s) {
    spl <- unlist(strsplit(s, ","), use.names = FALSE)
    n1 <- as.numeric(spl[[1]])
    n2 <- as.numeric(pick_starting_numeric(spl[[2]]))
    spl[[2]] <- gsub("^[0-9]*", "", spl[[2]])
    rest <- spl[[2]]
    if (length(spl)>2) {
      rest <- paste(rest, spl[3:length(spl)], sep = ",")
    }
    s <- paste(n1+n2, "", rest, sep = "")
    s
}