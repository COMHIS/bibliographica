#' @title polish_author
#' @description Polish author
#'
#' @param s Vector of author names
#' @param stopwords Stopwords
#' @param validate Validate the names based on existing first/last name lists
#' @return Polished vector
#'
#' @export
#' @importFrom tm stopwords
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples s2 <- polish_author("Smith, William")
#' @keywords utilities
polish_author <- function (s, stopwords = NULL, validate = FALSE) {

  s <- as.character(s)

  # Only handle unique entries, in the end map back to original indices
  sorig <- s
  suniq <- unique(s)
  s <- suniq

  # Remove brackets and ending commas / periods
  s <- remove_numerics(s)
  s <- gsub("\\[", "", s)
  s <- gsub("\\]", "", s)
  s <- gsub("\\(", "", s)
  s <- gsub("\\)", "", s)
  s <- str_trim(s)
  s <- gsub("\\.$", "", s)
  s <- gsub("\\,$", "", s)

  # Assume names are of format Last, First
  nametab <- t(sapply(strsplit(s, ","), function (x) {
    name <- c(last = x[[1]], first = NA);
    if (length(x)>1) {name[["first"]] <- paste(x[-1], collapse = " ")};
    return(name)
  }))

  if (is.null(stopwords)) {
    message("No stopwords provided for authors. Using ready-made stopword lists")
    f <- system.file("extdata/stopwords.csv", package = "bibliographica")
    stopwords.general <- as.character(read.csv(f, sep = "\t")[,1])
    stopwords.general <- c(stopwords.general, stopwords(kind = "en"))
    f <- system.file("extdata/stopwords_for_names.csv", package = "bibliographica")
    stopwords.names <- as.character(read.csv(f, sep = "\t")[,1])
    f <- system.file("extdata/stopwords_titles.csv", package = "bibliographica")
    stopwords.titles <- as.character(read.csv(f, sep = "\t")[,1])
    stopwords <- unique(c(stopwords.general, stopwords.names, stopwords.titles))
  }

  # Must exclude some names from assumed stopwords
  stopwords <- setdiff(stopwords, c("humble", "about", "most"))

  message("Harmonize names")
  # TODO O. K. Humble, Verner -> First: Verner O K Last: Humble  		    
  nametab <- as.data.frame(nametab)
  
  nametab$last  <- gsub("^-", "", trim_names(nametab$last,  stopwords, remove.letters = FALSE))
  nametab$first <- gsub("^-", "", trim_names(nametab$first, stopwords, remove.letters = FALSE))

  # Some additional formatting
  # eg. "Wellesley, Richard Wellesley" -> "Wellesley, Richard"
  firsts <- c()
  for (i in 1:nrow(nametab)) {
    x <- nametab[i,]
    first <- unlist(strsplit(nametab[i, "first"], " "))
    last <- unlist(strsplit(nametab[i, "last"], " "))
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

  ### VALIDATING THE NAMES
  valid <- list()
  invalid <- list()
  
  message("Validate names with known name lists")
  for (db in c("first", "last")) {

    namelist <- nametab[[db]]

    if (validate) {  
      v <- validate_names(namelist, db)
    } else {
      v <- list()
      v$validated <- !is.na(namelist)
      v$invalid <- is.na(namelist)
    }

    valid[[db]] <- v$validated
    invalid[[db]] <- suniq[v$invalid]

  }

  message("Remove names that do not have both valid first and last names")
  nametab[(!valid[["first"]] | !valid[["last"]]), ] <- NA
  nametab$last[is.na(nametab$first)] <- NA
  nametab$first[is.na(nametab$last)] <- NA

  message("Capitalize names")
  nametab$last <- capitalize(nametab$last, "all.words")
  nametab$first <- capitalize(nametab$first, "all.words")  

  message("Collapse accepted names to the form: Last, First")
  full.name <- apply(nametab, 1, function (x) { paste(x, collapse = ", ") })
  full.name <- gsub("NA, NA", NA, full.name) # Handle NAs
  nametab$full <- full.name

  # Then map back to the original indices
  nametab <- nametab[match(sorig, suniq), ]
  nametab$original <- sorig

  list(names = nametab, invalid = invalid)

}



