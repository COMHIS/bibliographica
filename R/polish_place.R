#' @title Polish Place
#' @description Polish place names.
#' @param x A vector of place names
#' @param synonymes Synonyme table for place names
#' @param remove.unknown Logical. Remove places that are not validated (ie. listed in the synonyme table)?
#' @param verbose verbose
#' @param harmonize Harmonize the polished names. Logical.
#' @return Polished vector
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
    synonymes <- suppressWarnings(read_mapping(f, include.lowercase = T, self.match = T, ignore.empty = FALSE, mode = "table", trim = TRUE))

    if (verbose) { message(paste("Reading special char table", f)) }
    # Harmonize places with synonyme table
    f <- system.file("extdata/replace_special_chars.csv",
		package = "bibliographica")
    spechars <- suppressWarnings(read_mapping(f, sep = ";", mode = "table", include.lowercase = TRUE))

    if (verbose) { message(paste("Reading publication place synonyme table", f)) }
    f <- system.file("extdata/harmonize_place.csv", package = "bibliographica")
    synonymes.spec <- suppressWarnings(read_mapping(f, sep = ";", mode = "table", include.lowercase = TRUE))
    if (verbose) { message(paste("Reading publication place synonyme table", f)) }
    
  }

  # Prepare
  if (verbose) { message("Convert to lowercase character and make unique list") }
  x0 <- x
  xorig <- tolower(as.character(x))
  x <- xuniq <- unique(xorig)

  if (verbose) { message("Replace special characters") }
  x <- as.character(map(x, spechars, mode = "recursive", verbose = verbose))

  if (verbose) { message("Remove brackets") }
  # Lo[n]don -> London  
  x <- remove_brackets_from_letters(x)

  # Some trivial trimming to speed up
  # TODO should go to synonyme list?
  # Remove numerics
  x <- gsub("[0-9]", " ", x) 
  x <- remove_special_chars(x, chars = c("\\(", "\\)", "\\[", "\\]", "\\{", "\\}", "\\="), niter = 1)
  x <- gsub("[_|:|;|,|\\?|\\&|\\.|/|>|<|\"| ]+", " ", x)
  x <- gsub("-+", " ", x)
  x <- gsub("'", " ", x)  
  x <- condense_spaces(x)
  x <- gsub(" sic ", " ", x)
  x <- gsub("^and ", "", x)
  x <- gsub("^from ", "", x)            
  x <- gsub("^re ", "", x)
  x <- gsub("^[a-z] +[a-z]$", " ", x)
  x <- gsub("^[a-z]\\. [a-z]$", " ", x)
  x[nchar(gsub(" ", "", x)) <= 2] = NA  
  x <- condense_spaces(x)

  # Back to original indices, then unique again;
  # reduces number of unique cases further
  xorig <- x[match(xorig, xuniq)]
  x <- xuniq <- unique(xorig)

  if (verbose) {message(paste("Polishing ", length(xuniq), " unique place names", sep = ""))}
  x <- remove_persons(x)

  if (verbose) {message("Remove print statements")}
  x <- remove_print_statements(x)
  x <- condense_spaces(x)

  # Back to original indices, then unique again;
  # reduces number of unique cases further
  xorig <- x[match(xorig, xuniq)]
  x <- xuniq <- unique(xorig)

  if (verbose) {message("Remove stopwords")}
  f <- system.file("extdata/stopwords_for_place.csv", package = "bibliographica")
  message(paste("Reading stopwords from file ", f))
  stopwords <- unique(tolower(str_trim(as.character(read.csv(f)[,1]))))
  x <- suppressWarnings(remove_terms(x, stopwords, c("begin", "middle", "end"), recursive = TRUE))

  if (verbose) {message("Harmonize ie")}
  x <- harmonize_ie(x)
  if (length(x) == 0) {return(rep(NA, length(xorig)))}

  # Back to original indices, then unique again;
  # reduces number of unique cases further
  if (verbose) {message("Match to original")}
  # Back to original indices, then unique again;
  # reduces number of unique cases further
  xorig <- x[match(xorig, xuniq)]
  x <- xuniq <- unique(xorig)

  if (verbose) {message("Detailed polishing")}
  #s <- synonymes$synonyme

  x <- suppressWarnings(unname(sapply(x, function (x) {polish_place_help(unlist(x, use.names = FALSE), synonymes$synonyme, verbose = verbose)}, USE.NAMES = FALSE)))
  if (length(x) == 0) { return(rep(NA, length(xorig))) }

  # Back to original indices, then unique again; reduces
  # number of unique cases further
  if (verbose) {message("Match to original")}
  xorig <- x[match(xorig, xuniq)]
  x <- xuniq <- unique(xorig)
  if (length(x) == 0) {return(rep(NA, length(xorig)))}

  if (verbose) { message("Harmonize the synonymous names") }
  x <- suppressWarnings(as.character(map(x, synonymes.spec, mode = "recursive")))

  # Once more remove stopwords
  # Warning: the names discarded here wont be visible in
  # summary lists of discarded names !
  # For validation purposes might be good to comment this out
  # for initial runs.
  x <- suppressWarnings(remove_terms(x, stopwords, c("begin", "middle", "end"), recursive = TRUE))

  # Remove roman numerals
  x <- sapply(strsplit(x, " "), function (xi) {paste(xi[!is.roman(suppressWarnings(as.roman(xi)))], collapse = " ")}, USE.NAMES = FALSE)

  if (harmonize) {

    # If the term is not on synonyme or name list but
    # all subterms are, select the first subterm
    ss <- unique(tolower(c(synonymes$synonyme, synonymes$name)))
    inds <- which(!x %in% ss)
    if (length(inds)>0) {
      spl <- strsplit(x[inds], " ")
      inds2 <- which(sapply(spl, function (xi) {all(xi %in% ss) && length(xi) > 0}, USE.NAMES = FALSE))
      if (length(inds2)>0) {
        x[inds[inds2]] = sapply(spl[inds2], function (xi) {xi[[1]]}, USE.NAMES = FALSE)
      }
    }

    # abo abo abo = abo
    x <- sapply(strsplit(x, " "), function (xi) {paste(unique(xi), collapse = " ")})

    # Then match place names to synonymes		
    x <- as.character(map(x, synonymes,
       		remove.unknown = remove.unknown,
		mode = "exact.match"))

  }

  # Remove too short names 
  x[nchar(gsub(" ", "", x)) <= 2] = NA
  x <- gsub("^[a-z] [a-z]$", " ", x)
  x <- gsub("^[a-z]\\.[a-z]$", " ", x)  

  if (length(x) == 0) {return(rep(NA, length(xorig)))}
  
  if (verbose) {message("Replace special cases")}
  x[x %in% c("", " ", "na")] <- NA

  if (verbose) {message("Capitalize all names")}    
  x <- capitalize(x)

  if (verbose) {message("Convert back to original indices and return")}
  x[match(xorig, xuniq)]

}


polish_place_help <- function (x, s, verbose = FALSE) {

  # London i.e. The Hague ->  The Hague
  # In the Yorke at London -> London
  # TODO use handle_ie function here or make an improved version
  x <- splitpick(x, " i.e ", 2)
  x <- splitpick(x, " at ", 2)  

  # NOTE: this step may loose info on original country
  # london re/now edinburgh -> london
  x <- splitpick(x, " re ", 1)
  
  # This may loose country info so skip for now
  #x <- splitpick(x, " now ", 1) # Should we pick latter here instead ?  

  # New York N Y -> New York NY
  if (length(grep(" [a-z] [a-z]$", x))>0) {
    n <- nchar(x)
    x <- paste(substr(x, 1, n-2), substr(x, n, n), sep = "")
  }

  # Remove - may loose considerable information ?
  # However looks very useful in practice
    # If the term is not on synonyme list but all its subterms are,
    # then select the first term
    # ie "zurich cologne" becomes "zurich"  
  if (!is.na(x) && any(!is.na(s)) && !(x %in% na.omit(s))) {
    spl <- unlist(strsplit(x, " "), use.names = FALSE)
    inds <- which(!is.na(match(spl, c(s, "new"))))

    if (length(inds) > 0) {
      # Keep only those terms that are on synonyme list
      # and exception terms "new"
      x <- paste(unique(spl[inds]), collapse = " ")
    }
  }

  

  return(x)
   
}