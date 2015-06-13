#' @title polish_place
#' @description Polish place
#'
#' @param x A vector of place names
#' @param synonymes Synonyme table for place names
#' @param remove.unknown Logical. Remove places that are not validated
#'   	  		 	  (ie. listed in the synonyme table)?
#'
#' @return Polished vector
#'
#' @export
#'
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples # x2 <- polish_place(c("London", "Paris"))
#' @keywords utilities
polish_place <- function (x, synonymes = NULL, remove.unknown = FALSE) {

  if (is.null(synonymes)) {
    # Harmonize places with synonyme table
    f <- system.file("extdata/PublicationPlaceSynonymes.csv",
		package = "bibliographica")
    synonymes <- read.csv(f, sep = ";")
    colnames(synonymes) <- c("name", "synonyme")
    message(paste("Reading publication place synonyme table", f))
  }

  message("Convert to character")	
  x <- as.character(x)	    

  # Speed up by handling unique cases only
  xorig <- x
  xorig.unique <- unique(x)  
  x <- xorig.unique

  message("Pick first name only") # Basil;Paris -> Basil
  x <- sapply(strsplit(x, ";"), function (s) {if (length(s) > 0 ) {s[[1]]} else {s}})

  message("Remove brackets from letters")
  x <- remove_brackets_from_letters(x)

  message("Remove time info")
  x <- remove_time_info(x)

  message("Remove numerics")
  x <- remove_numerics(x)

  message("Remove special characters")
  x <- remove_special_chars(x, chars = c(",", ";", ":", "\\(", "\\)", "\\?", "--", "\\&", "-", "\\-", " :;", "; ", " ;;","; ", ",", "\\[", "\\]", " sic ", "\\=", "\\."), niter = 5)
  
  message("Remove print statements")
  x <- remove_print_statements(x)

  message("Remove prefixes")
  x <- remove_stopwords(x)

  message("Handle ie and at: always select the latter place in these cases")
  # Handle IE before AT
  x <- harmonize_ie(x)
  x <- harmonize_at(x)

  # London i.e. The Hague ->  The Hague
  # In the Yorke at London -> London
  for (ss in c(" i.e. ", " at ", " At ")) {
    x <- sapply(strsplit(x, ss), function (s) {if (length(s) > 0 ) {s[[length(s)]]} else {s}})
  }  
  x <- unlist(x)
  x <- remove_trailing_periods(x)

  message("Replace special cases")
  x[tolower(x) %in% c("", "NA", NA)] <- NA

  message("Remove persons")
  x <- remove_persons(x)

  message("Custom polish")
  x <- gsub("And", "and", x)
  x <- gsub("Americae", "America", x)  
  x <- gsub("Parliament ", "", x)    
  x <- gsub("N.rth", "North", x)    
  x <- gsub("Mass.", "Mass", x)
  x <- gsub("N. H.", "N.H.", x)
  x <- gsub("N. J.", "N.J.", x)
  x <- gsub("n. h.", "N.H.", x)
  x <- gsub("n. j.", "N.J.", x)
  x <- gsub("D. C.", "D.C.", x)
  x <- gsub("S. C.", "S.C.", x)

  message("Harmonize the synonymous names")
  x <- as.character(harmonize_names(x, synonymes,
       		remove.unknown = remove.unknown)$name)

  message("Return to full list")
  # The function was sped up by operating with unique terms
  x <- x[match(xorig, xorig.unique)]

}

