#' @title Polish author
#' @description Polish author.
#' @param s Vector of author names
#' @param stopwords Stopwords
#' @param verbose verbose 
#' @return Polished vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples s2 <- polish_author("Smith, William")
#' @keywords utilities
polish_author <- function (s, stopwords = NULL, verbose = FALSE) {

  if (is.null(stopwords)) {
    message("No stopwords provided for authors. Using ready-made stopword lists")

    # TODO Use instead the notnames function here ?
    
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
  accept.names <- unique(c(pseudo, author.accepted))
  # Also add individual terms in these names on the list
  accept.names <- c(accept.names, unique(unlist(strsplit(accept.names, " "))))
  # Remove special chars and make lowercase to harmonize
  accept.names <- unique(condense_spaces(gsub("\\,", " ", gsub("\\.", "", tolower(accept.names)))))

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
  s <- gsub("[\\.|\\,]+$", "", s)

  # Map back to original indices, then make unique again. Helps to further reduce cases.
  sorig <- s[match(sorig, suniq)]
  s <- suniq <- unique(sorig)

  # ----------------------------------------------------------------

  if (verbose) {
    message("Separating names")
  }
  # Assume names are of format Last, First
  # TODO O. K. Humble, Verner -> First: Verner O K Last: Humble
  # pseudonymes are taken as such
  # convert to character type
  first <- last <- as.character(rep(NA, length(s)))
  
  pseudo.inds <- which(s %in% pseudo)
  
  inds <- inds1 <- setdiff(grep(",", s), pseudo.inds)
  if (length(inds) > 0) {
    first[inds] <- pick_firstname(s[inds], format = "last, first")
    last[inds]  <-  pick_lastname(s[inds], format = "last, first")
  }
  
  inds <- inds2 <- setdiff(setdiff(grep(" ", s), inds1), pseudo.inds)
  if (length(inds) > 0) {
    first[inds] <- pick_firstname(s[inds], format = "first last")
    last[inds]  <-  pick_lastname(s[inds], format = "first last")
  }
  # Where the name did not match the assumed formats, use the complete form as
  # the last name
  inds <- inds3 <- setdiff(which(is.na(first) & is.na(last)), pseudo.inds)
  if (length(inds) > 0) {
    last[inds] <- as.character(s[inds])
  }
  # Mark pseudonymes as first names
  inds <- inds4 <- pseudo.inds
  if (length(pseudo.inds) > 0) {
    first[inds] <- as.character(s[inds])
  }  

  # ------------------------------------------------------------

  if (verbose) { message("Formatting names") }
  # Some additional formatting
  # eg. "Wellesley, Richard Wellesley" -> "Wellesley, Richard"
  inds <- which(!is.na(first) | !is.na(last))
  for (i in inds) {

    fi <- first[[i]]
    if (!is.na(fi)) {
      fi <- unlist(strsplit(fi, " "), use.names = FALSE)
    }

    la <- last[[i]]    
    if (!is.na(la)) {    
      la <- unlist(strsplit(la, " "), use.names = FALSE)
    }

    if (length(fi) == 0) {fi <- NA}
    if (length(la) == 0) {la <- NA}    
    if (!is.na(fi) && !is.na(la)) {
      if (la == fi[[length(fi)]]) {
        fi <- fi[-length(fi)]
      }
    }
    first[[i]] <- paste(fi, collapse = " ")
    last[[i]] <- paste(la, collapse = " ")    
  }

  message("Name table")
  nametab <- data.frame(last = unname(last),
                        first = unname(first),
                        stringsAsFactors = FALSE
                        )
  rownames(nametab) <- NULL

  message("Remove single letter last names")
  nametab$last[nchar(as.character(nametab$last)) == 1] <- NA   

  if (verbose) { message("Capitalize names")}
  nametab$last  <- capitalize(nametab$last, "all.words")
  nametab$first <- capitalize(nametab$first, "all.words")
  
  message("Remove periods")
  nametab$first <- condense_spaces(gsub("\\.", " ", nametab$first))
  nametab$last  <- condense_spaces(gsub("\\.", " ", nametab$last))  

  if (verbose) { message("Collapse accepted names to the form: Last, First") }
  full.name <- apply(nametab, 1, function (x) { paste(x, collapse = ", ") })
  full.name <- unname(full.name)
  full.name[full.name == "NA, NA"] <- NA
  full.name <- gsub("\\, NA$", "", full.name) # "Tolonen, NA" -> "Tolonen"
  full.name <- gsub("^NA, ", "", full.name) # "NA, Mikael" -> "Mikael"

  if (verbose) { message("Map to the original indices") }
  full.name[match(sorig, suniq)]   

}



