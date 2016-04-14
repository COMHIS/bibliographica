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

  if (all(is.na(x))) {return(x)}
  
  if (is.null(synonymes)) {
    # Harmonize places with synonyme table
    f <- system.file("extdata/PublicationPlaceSynonymes.csv", package = "bibliographica")
    synonymes <- read_synonymes(f, include.lowercase = T, self.match = T, ignore.empty = FALSE, mode = "table")
    if (verbose) { message(paste("Reading publication place synonyme table", f)) }

    # Harmonize places with synonyme table
    f <- system.file("extdata/replace_special_chars.csv",
		package = "bibliographica")
    spechars <- read_synonymes(f, sep = ";", mode = "table", include.lowercase = TRUE)
    if (verbose) { message(paste("Reading publication place synonyme table", f)) }

    f <- system.file("extdata/harmonize_place.csv",
		package = "bibliographica")
    speccases <- read_synonymes(f, sep = ";", mode = "table", include.lowercase = TRUE)
    if (verbose) { message(paste("Reading publication place synonyme table", f)) }

    synonymes.spec <- rbind(spechars, speccases)

  }

  f <- system.file("extdata/stopwords_for_place.csv", package = "bibliographica")
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
  # TODO should go to synonyme list?
  # Remove numerics
  x <- gsub("[0-9]", " ", x) 
  x <- remove_special_chars(x, chars = c("\\(", "\\)", "\\[", "\\]", "\\{", "\\}", "\\="), niter = 1)
  x <- gsub("[_|:|;|,|\\?|\\&|\\.| ]+", " ", x)
  x <- gsub("-+", " ", x)
  x <- gsub("'", " ", x)  
  x <- condense_spaces(x)
  x <- gsub(" sic ", " ", x)
  x <- gsub("^and ", "", x)
  x <- gsub("^from ", "", x)            
  x <- gsub("^re ", "", x)
  x <- gsub("^[a-z]{1,2}$", " ", x)  
  x <- gsub("^[a-z] +[a-z]$", " ", x)
  x <- gsub("^[a-z]\\. [a-z]$", " ", x)  
  x <- condense_spaces(x)

  # Back to original indices, then unique again; reduces number of unique cases further
  x <- x[match(xorig, xuniq)]
  xorig <- x
  xuniq <- sort(unique(x))
  x <- xuniq

  if (verbose) {message(paste("Polishing ", length(xuniq), " unique place names", sep = ""))}
  x <- remove_persons(x)
  if (verbose) {message("Remove print statements")}
  x <- remove_print_statements(x, remove.letters = FALSE)
  x <- condense_spaces(x)

  if (verbose) {message("Remove stopwords")}
  x <- remove_stopwords(x, terms = stopwords, remove.letters = FALSE)

  if (verbose) {message("Harmonize ie")}
  x <- harmonize_ie(x)
  if (length(x) == 0) {return(rep(NA, length(xorig)))}

  # Back to original indices, then unique again; reduces number of unique cases further
  if (verbose) {message("Match to original")}  
  x <- x[match(xorig, xuniq)]
  xorig <- x
  xuniq <- sort(unique(x))
  x <- xuniq

  if (verbose) {message("Detailed polishing")}
  s <- synonymes$synonyme
  x <- unname(sapply(x, function (x) {polish_place_help(unlist(x, use.names = FALSE), s, stopwords = stopwords, verbose = verbose)}))

  if (length(x) == 0) { return(rep(NA, length(xorig))) }

  # Back to original indices, then unique again; reduces
  # number of unique cases further
  if (verbose) {message("Match to original")}    
  x <- x[match(xorig, xuniq)]
  xorig <- x
  xuniq <- sort(unique(x))
  x <- xuniq

  if (length(x) == 0) {return(rep(NA, length(xorig)))}

  if (verbose) { message("Harmonize the synonymous names") }
  # First replace some special characters 
  x <- as.character(harmonize_names(x, synonymes.spec,
		mode = "recursive"))

  # Once more remove stopwords
  # Warning: the names discarded here wont be visible in
  # summary lists of discarded names !
  # For validation purposes might be good to comment this out
  # for initial runs.
  x <- remove_stopwords(x, terms = tolower(stopwords), remove.letters = FALSE)

  if (harmonize) {

    # Then match place names to synonymes		
    x <- as.character(harmonize_names(x, synonymes,
       		remove.unknown = remove.unknown,
		mode = "exact.match"))

  }

  # Remove too short names with just two letters
  x <- gsub("^[a-z]{1,2}$", " ", x)  
  x <- gsub("^[a-z] [a-z]$", " ", x)
  x <- gsub("^[a-z]\\.[a-z]$", " ", x)  

  if (length(x) == 0) {return(rep(NA, length(xorig)))}
  
  if (verbose) {message("Capitalize all names")}    
  x <- capitalize(x)

  if (verbose) {message("Replace special cases")}
  x[x == c("", "na")] <- NA

  if (verbose) {message("Convert back to original indices and return")}
  x[match(xorig, xuniq)]

}


polish_place_help <- function (x, s, stopwords, verbose = FALSE) {

  # London i.e. The Hague ->  The Hague
  # In the Yorke at London -> London
  # TODO use handle_ie function here or make an improved version
  x <- splitpick(x, " i.e ", 2)
  x <- splitpick(x, " at ", 2)  

  # NOTE: this step may loose info on original country
  # london re/now edinburgh -> london
  x <- splitpick(x, " re ", 1)
  # This may loose country info so do not include
  #x <- splitpick(x, " now ", 1) # Should we pick latter here instead ?  

  # New York N Y -> New York NY
  if (length(grep(" [a-z] [a-z]$", x))>0) {
    n <- nchar(x)
    x <- paste(substr(x, 1, n-2), substr(x, n, n), sep = "")
  }

  # Remove - may loose considerable information ?
  # However looks very useful in practice
  if (!is.na(x) && any(!is.na(s)) && !(x %in% na.omit(s))) {
    spl <- unlist(strsplit(x, " "), use.names = FALSE)
    inds <- which(!is.na(match(spl, s)))
    if (length(inds) > 0) {
      # Keep only those terms that are on synonyme list
      x <- paste(unique(spl[inds]), collapse = " ")
    }
  }

  x
   
}