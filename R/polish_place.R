#' @title Polish place
#' @description Polish place names.
#' @param x A vector of place names
#' @param synonymes Synonyme table for place names
#' @param remove.unknown Logical. Remove places that are not validated (ie. listed in the synonyme table)?
#' @param verbose verbose
#' @param harmonize Harmonize the polished names. Logical.
#' @return Polished vector
#' @importFrom sorvi harmonize_names
#' @importFrom stringr str_trim
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # x2 <- polish_place(c("London", "Paris"))
#' @keywords utilities
polish_place <- function (x, synonymes = NULL, remove.unknown = FALSE, verbose = FALSE, harmonize = TRUE) {

  if (is.null(synonymes)) {
    # Harmonize places with synonyme table
    f <- system.file("extdata/PublicationPlaceSynonymes.csv",
		package = "bibliographica")
    synonymes <- read.csv(f, sep = ";", stringsAsFactors = FALSE)
    synonymes$synonyme <- tolower(synonymes$synonyme)
    synonymes <- synonymes[!duplicated(synonymes),]
    if (verbose) { message(paste("Reading publication place synonyme table", f)) }

    # Harmonize places with synonyme table
    f <- system.file("extdata/replace_special_chars.csv",
		package = "bibliographica")
    spechars <- read.csv(f, sep = ";", stringsAsFactors = FALSE)
    spechars <- spechars[!duplicated(spechars),]
    if (verbose) { message(paste("Reading publication place synonyme table", f)) }
    
  }

  f <- system.file("extdata/stopwords.csv", package = "bibliographica")
  message(paste("Reading stopwords from file ", f))
  stopwords <- as.character(read.csv(f)[,1])

  # Unique
  xorig <- x
  xuniq <- sort(unique(x))
  x <- xuniq

  # Prepare
  if (verbose) { message("Convert to lowercase character") }
  x <- tolower(as.character(x))

  # Lo[n]don -> London
  x <- remove_brackets_from_letters(x)

  # Some trivial trimming to speed up
  # Remove numerics
  x <- gsub("[0-9]", " ", x) 
  x <- gsub("s\\:t ", "st ", x)
  x <- gsub("n\\.w", "new", x)

  x <- remove_special_chars(x, chars = c(",", ";", ":", "\\(", "\\)", "\\?", "--", "\\&", "-", "\\-", " :;", "; ", " ;;","; ", ",", "\\[", "\\]", " sic ", "\\=", "\\.", ":$"), niter = 1)
  x <- gsub("^and ", "", x)
  x <- gsub("^from ", "", x)            
  x <- gsub("['|-]", "", x)
  x <- gsub("parliament ", "", x)
  x <- gsub("^s$", "", x)    
  x <- gsub("^re ", "", x)
  x <- gsub("_", " ", x)  

  # Back to original indices, then unique again; reduces number of unique cases further
  x <- x[match(xorig, xuniq)]
  xorig <- x
  xuniq <- sort(unique(x))
  x <- xuniq

  if (verbose) {message(paste("Polishing ", length(xuniq), " unique place names", sep = ""))}
  x <- remove_persons(x)

  x <- remove_print_statements(x, remove.letters = FALSE)
  x <- condense_spaces(x)
  x <- remove_stopwords(x, terms = stopwords, remove.letters = FALSE)
  x <- harmonize_ie(x)
  s <- synonymes$synonyme

  # Back to original indices, then unique again; reduces number of unique cases further
  x <- x[match(xorig, xuniq)]
  xorig <- x
  xuniq <- sort(unique(x))
  x <- xuniq

  x <- unname(sapply(x, function (x) {polish_place_help(unlist(x, use.names = FALSE), s, stopwords = stopwords, verbose = verbose)}))

  # Back to original indices, then unique again; reduces number of unique cases further
  x <- x[match(xorig, xuniq)]
  xorig <- x
  xuniq <- sort(unique(x))
  x <- xuniq

  # Harmonize
  if (harmonize) {

    if (verbose) { message("Harmonize the synonymous names") }
    # First replace some special characters 
    x <- as.character(harmonize_names(x, spechars,
       		remove.unknown = FALSE,
		include.lowercase = TRUE,	
		check.synonymes = F,
		mode = "recursive"))

    # Then match place names to synonymes		
    x <- as.character(harmonize_names(x, synonymes,
       		remove.unknown = remove.unknown,
		include.lowercase = TRUE,	
		check.synonymes = F,
		mode = "exact.match"))


  }

  # Capitalize all names 
  x <- capitalize(x)

  # Mark NAs
  if (verbose) {message("Replace special cases")}
  x[x == c("", "na")] <- NA

  # Convert back to original indices and return
  x[match(xorig, xuniq)]

}


polish_place_help <- function (x, s, stopwords, verbose = FALSE) {

  # if (verbose) {message(x)}

  # London i.e. The Hague ->  The Hague
  # In the Yorke at London -> London
  for (ss in c(" i.e ", " at ", " At ")) {
     spl <- strsplit(x, ss)
     if (length(spl) > 0 ) {x <- spl[[length(spl)]]}
  }  

  # New York N Y -> New York NY
  if (length(grep(" [a-z] [a-z]$", x))>0) {
    n <- nchar(x)
    x <- paste(substr(x, 1, n-2), substr(x, n, n), sep = "")

  }

  # london re/now edinburgh -> london
  spl <- unlist(strsplit(x, " [re|now] "), use.names = FALSE)
  if (length(spl)>0) {x <- spl[[1]]}

  if (!is.na(x) && !is.na(s) && !any(x == s)) {

      # then take the first term that is
      spl <- unlist(strsplit(x, " "), use.names = FALSE)
      inds <- which(!is.na(match(spl, s)))
      if (length(inds) > 0) {
        x <- spl[[min(inds)]]
      }
  }

    x
}