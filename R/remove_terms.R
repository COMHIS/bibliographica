#' @title remove_terms
#' @description Remove the given terms from the strings
#' @param x A vector
#' @param terms Terms to be removed
#' @param where Locations to be removed ("all" / "begin" / "middle" / "end")
#' @param include.lowercase Include also lowercase versions of the terms
#' @param polish polish the entries after removing the terms (remove trailing spaces and periods)
#' @param recursive Apply the changes recursively along the list ?
#' @return Vector with terms removed
#' @details After removing the numerics, beginning, double and ending 
#'          spaces are also removed from the strings.
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x2 <- remove_terms(x, terms, where = "all")}
#' @keywords utilities
remove_terms <- function (x, terms, where = "all", include.lowercase = FALSE, polish = TRUE, recursive = FALSE) {

  # If removal is recursive, then go through the list as is in the given order
  # otherwise, optionally include lowercase,
  # remove unique terms and sort by length 
  if (!recursive) {

    # Add lowercase versions
    if (include.lowercase) {
      terms <- c(terms, tolower(terms))
    }

    # List all unique terms
    terms <- sort(unique(terms))

    # Go from longest to shortest term to avoid nested effects
    terms <- terms[rev(order(sapply(terms, nchar)))]
    
  }

  # "begin ", 
  # " middle ", 
  # " middle. ", 
  # " last$"

  for (term in terms) {
    
    x <- gsub(paste("^", term, "$", sep = ""), " ", x)

    # Here no spaces around the term needed, elsewhere yes
    if (where %in% c("full")) {
      x <- gsub(term, " ", x)
    }

    if (where %in% c("begin", "all")) {
      x <- gsub(paste("^", term, "[ |\\.|\\,]", sep = ""), " ", x)
    }
    
    if (where %in% c("middle", "all")) {
      x <- gsub(paste(" ", term, "[ |\\.|\\,]", sep = ""), " ", x)    
    }

    if (where %in% c("end", "all")) {
      x <- remove_endings(x, c("\\.", "\\,"))
      x <- condense_spaces(x)
      x <- gsub(paste(" ", term, "$", sep = ""), " ", x)
    }

  }

  if (polish) {
    x <- condense_spaces(x)
    x <- remove_trailing_periods(x)
  }
  
  x 

}
