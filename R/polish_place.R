#' @title polish_place
#' @description Polish place
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
  }

  f <- system.file("extdata/stopwords.csv", package = "bibliographica")
  message(paste("Reading stopwords from file ", f))
  stopwords <- as.character(read.csv(f)[,1])

  # Prepare
  if (verbose) { message("Convert to lowercase character") }
  x <- tolower(as.character(x))
  s <- synonymes$synonyme

  # Lo[n]don -> London
  x <- remove_brackets_from_letters(x)

  # Some trivial trimming to speed up
  x <- remove_numerics(x)
  x <- gsub("s\\:t ", "st ", x)
  x <- gsub("n\\.w", "new", x)    

  x <- remove_special_chars(x, chars = c(",", ";", ":", "\\(", "\\)", "\\?", "--", "\\&", "-", "\\-", " :;", "; ", " ;;","; ", ",", "\\[", "\\]", " sic ", "\\=", "\\.", ":$"), niter = 2)  
  x <- gsub("^a ", "", x)
  x <- gsub("^at ", "", x)
  x <- gsub("^in ", "", x)    
  x <- gsub("^and ", "", x)
  x <- gsub("^from ", "", x)            
  x <- gsub("^printed at ", "", x)
  x <- gsub("^printed in ", "", x)
  x <- gsub("^imprinted at ", "", x)
  x <- gsub(" i e ", " ie ", x)
  x <- gsub("'", "", x)
  x <- gsub("-", "", x)    

  # Polish
  xuniq <- sort(unique(x))
  match.inds <- match(x, xuniq)
  if (verbose) {message(paste("Polishing ", length(xuniq), " unique place names", sep = ""))}
  x <- unname(sapply(xuniq, function (x) {polish_place_help(x, s, stopwords = stopwords, verbose = verbose)}))

  # print(head(subset(synonymes, synonyme == x)))
  # print(x)
  
  # Harmonize
  if (harmonize) {
    if (verbose) { message("Harmonize the synonymous names") }

    x <- as.character(harmonize_names(x, synonymes,
       		remove.unknown = remove.unknown,
		include.lowercase = TRUE,
		mode = "exact.match")$name)

  }
  
  # Mark NAs
  if (verbose) {message("Replace special cases")}
  x[tolower(x) %in% c("", "NA", NA)] <- NA

  # Convert back to original indices
  x <- x[match.inds]

  # Return
  x
  
}


polish_place_help <- function (x, s, stopwords, verbose = FALSE) {

  if (verbose) {message(x)}
  x <- harmonize_ie(x)
  x <- remove_print_statements(x, remove.letters = FALSE)
  x <- remove_stopwords(x, terms = stopwords, remove.letters = FALSE)
  x <- harmonize_at(x)
  x <- unlist(x)
  x <- remove_persons(x)

  # London i.e. The Hague ->  The Hague
  # In the Yorke at London -> London
  for (ss in c(" i.e ", " at ", " At ")) {
     spl <- strsplit(x, ss)
     if (length(spl) > 0 ) {x <- spl[[length(spl)]]}
  }  

  #if (verbose) {message("Custom polish")}
  x <- gsub("Parliament ", "", x)
  x <- gsub("^s$", "", x)    
  x <- gsub("^re ", "", x)
  x <- gsub("_", " ", x)  


  # New York N Y -> New York NY
  inds <- grep(" [a-z] [a-z]$", x)
  for (ind in inds) {
    n <- nchar(x[[ind]])
    x[[ind]] <- paste(substr(x[[ind]], 1, n-2), substr(x[[ind]], n, n), sep = "")
  }

  # london re edinburgh -> london
  spl <- unlist(strsplit(x, " re "))
  if (length(spl)>0) {x <- spl[[1]]}

  # london now edinburgh -> london
  spl <- unlist(strsplit(x, " now "))
  if (length(spl)>0) {x <- spl[[1]]}

  # glasgow london -> glasgow
  # if x itself is not on the synonyme list

  if (!x %in% s) {

      # then take the first term that is
      spl <- unlist(strsplit(x, " "))

      inds <- which(!is.na(match(spl, s)))

      if (length(inds) > 0) {
        x <- spl[[min(inds)]]
      } else {
        x <- x
      }
  }

    x
}