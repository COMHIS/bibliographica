#' @title Polish author
#' @description Polish author.
#' @param s Vector of author names
#' @param stopwords Stopwords
#' @param validate Validate the names based on existing first/last name lists
#' @param verbose verbose 
#' @return Polished vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples s2 <- polish_author("Smith, William")
#' @keywords utilities
polish_author <- function (s, stopwords = NULL, validate = FALSE, verbose = FALSE) {

  if (is.null(stopwords)) {
    message("No stopwords provided for authors. Using ready-made stopword lists")

    f <- system.file("extdata/stopwords.csv", package = "bibliographica")
    stopwords.general <- as.character(read.csv(f, sep = "\t")[,1])
    stopwords.general <- c(stopwords.general, stopwords(kind = "en"))

    f <- system.file("extdata/stopwords_for_names.csv", package = "bibliographica")
    stopwords.names <- as.character(read.csv(f, sep = "\t")[,1])

    f <- system.file("extdata/organizations.csv", package = "bibliographica")
    stopwords.organizations <- as.character(read.csv(f, sep = "\t")[,1])
    
    f <- system.file("extdata/stopwords_titles.csv", package = "bibliographica")
    stopwords.titles <- as.character(read.csv(f, sep = "\t")[,1])
    stopwords <- unique(c(stopwords.general, stopwords.organizations, stopwords.names, stopwords.titles))
  }

  # Accept some names that may be on the stopword lists
  # TODO add here all known names
  f <- system.file("extdata/author_accepted.csv", package = "bibliographica")
  author.accepted <- as.character(read.csv(f, sep = "\t")[,1])
  pseudo <- get_pseudonymes()  
  accept.names <- c(pseudo, author.accepted)

  # Then remove those in stopwords (ie accept these in names)
  # Exclude some names and pseudonyms from assumed stopwords
  stopwords <- setdiff(stopwords, accept.names)

  # -------------------------------------------------------------

  s <- tolower(as.character(s))

  # Only handle unique entries, in the end map back to original indices
  sorig <- s
  suniq <- unique(s)
  s <- suniq

  if (verbose) {
    message(paste("Polishing author field: ", length(suniq), "unique entries"))
  }

  # Remove numerics
  s <- gsub("[0-9]", " ", s) 

  # Remove brackets and ending commas / periods
  # Cannot be merged into one regexp ?
  s <- gsub("\\[", " ", s)
  s <- gsub("\\]", " ", s)
  s <- gsub("\\(", " ", s)
  s <- gsub("\\)", " ", s)
  s <- gsub("\\?", " ", s)
  s <- gsub("-+", "-", s)      
  s <- str_trim(s)
  s <- gsub("[\\.|\\,]$", "", s)

  # Map back to original indices, then make unique again. Helps to further reduce cases.
  s <- s[match(sorig, suniq)]
  sorig <- s
  suniq <- unique(s)
  s <- suniq

  if (verbose) { message("Separating names") }
  # Assume names are of format Last, First
  nametab <- t(sapply(strsplit(s, ","), function (x) {
    name <- c(last = x[[1]], first = NA);
    if (length(x)>1) {name[["first"]] <- paste(x[-1], collapse = " ")};
    return(name)
  }))

  if (verbose) { message("Trim names") }
  # TODO O. K. Humble, Verner -> First: Verner O K Last: Humble
  nametab <- as.data.frame(nametab)

  nametab$last  <- gsub("^-", "", trim_names(nametab$last,  stopwords, remove.letters = FALSE))
  nametab$first <- gsub("^-", "", trim_names(nametab$first, stopwords, remove.letters = FALSE))

  if (verbose) { message("Formatting names") }
  # Some additional formatting
  # eg. "Wellesley, Richard Wellesley" -> "Wellesley, Richard"
  firsts <- c()
  for (i in 1:nrow(nametab)) {
    x <- nametab[i,]
    first <- unlist(strsplit(nametab[i, "first"], " "), use.names = FALSE)
    last <- unlist(strsplit(nametab[i, "last"], " "), use.names = FALSE)
    if (length(first) == 0) {first <- NA}
    if (length(last) == 0) {last <- NA}    
    if (!is.na(first) && !is.na(last)) {
      if (last == first[[length(first)]]) {
        first <- first[-length(first)]
      }
    }
    firsts[[i]] <- paste(first, collapse = " ")
  }
  nametab$first <- firsts

  # Remove single letter last names
  # nametab$first[nchar(nametab$first) == 1] <- NA
  nametab$last[nchar(nametab$last) == 1] <- NA   

  # OK, now we have polished first and last names
  # To speed up, discard names where both first and last are not accepted
  valid <- list()
  #invalid <- list()
  for (db in c("first", "last")) {

    namelist <- nametab[[db]]
    v <- list()
    v$validated <- !is.na(namelist)
    v$invalid <- suniq[is.na(namelist)]
    valid[[db]] <- v$validated
    #invalid[[db]] <- v$invalid

  }
  nametab[(!valid[["first"]] | !valid[["last"]]), ] <- NA
  nametab$last[is.na(nametab$first)] <- NA
  nametab$first[is.na(nametab$last)] <- NA
 
#  # FIXME this could go to enrich / qualitycheck
#  ### VALIDATING THE NAMES
#  valid <- list()
#  #invalid <- list()
#  if (verbose) { message("Validate names with known name lists") }
#  if (validate) {  
#    for (db in c("first", "last")) {
#      if (verbose) { message(db) }
#      namelist <- nametab[[db]]
#      v <- validate_names(namelist, db)
#      valid[[db]] <- v$validated
#      #invalid[[db]] <- v$invalid
#    }
#    if (verbose) { message("Remove names that do not have both valid first and last names") }
#    nametab[(!valid[["first"]] | !valid[["last"]]), ] <- NA
#    nametab$last[is.na(nametab$first)] <- NA
#    nametab$first[is.na(nametab$last)] <- NA
#  }

  if (verbose) { message("Capitalize names")}
  nametab$last  <- capitalize(nametab$last, "all.words")
  nametab$first <- capitalize(nametab$first, "all.words")  

  if (verbose) { message("Collapse accepted names to the form: Last, First") }
  full.name <- apply(nametab, 1, function (x) { paste(x, collapse = ", ") })
  full.name[full.name == "NA, NA"] <- NA
  full.name <- gsub(", NA$", "", full.name) # "Tolonen, NA" -> "Tolonen"
  full.name <- gsub("^NA, ", "", full.name) # "NA, Mikael" -> "Mikael"  
  nametab$full <- full.name

  if (verbose) { message("Map to the original indices") }
  nametab <- nametab[match(sorig, suniq), ]
  nametab$original <- sorig

  list(names = nametab)  

}



