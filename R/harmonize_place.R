#' @title polish_place
#' @description Polish place
#'
#' @param x A vector of place names
#'
#' @return Polished vector
#'
#' @export
#'
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{x2 <- polish_place(x, file)}
#' @keywords utilities
polish_place <- function (x) {

  message("Convert to character")	
  x <- as.character(x)	    

  # Speed up by handling unique cases only
  xorig <- x
  xorig.unique <- unique(x)  
  x <- xorig.unique

  message("Pick first name") # Basil;Paris -> Basil
  x <- sapply(strsplit(x, ";"), function (s) {if (length(s) > 0 ) {s[[1]]} else {s}})

  message("Harmonize terms")
  x <- remove_brackets_from_letters(x)

  message("Remove time info")
  x <- remove_time_info(x)

  message("Remove numerics")
  x <- remove_numerics(x)

  message("Remove special characters")
  x <- remove_special_chars(x)

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

  message("Return to full list")
  x <- x[match(xorig, xorig.unique)]

}

