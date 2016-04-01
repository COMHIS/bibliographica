#' @title Remove terms
#' @description Remove the given terms from the strings.
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
  # otherwise, optionally include lowercase, remove unique terms and sort by length 
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

  # Only consider cases with matches, to speed up analysis
  x[x %in% terms] <- " "
  tmp <- matrix(sapply(terms, function (term) grepl(term, x)), ncol = length(terms))
  for (i in 1:length(terms)) {  
    x[tmp[, i]] <- remove_terms_help(x[tmp[, i]], terms[[i]], where)
  }
  
  if (polish) {
    x <- condense_spaces(x)
    x <- remove_trailing_periods(x)
  }
  
  x 

}



remove_terms_help <- function (x, term, where) {

    # Here no spaces around the term needed, elsewhere yes
    if (where == "all") {

      # begin
      rms <- paste("^", term, "[ |\\.|\\,]", sep = "")
      x <- gsub(rms, " ", x)

      # middle
      x <- gsub(paste(" ", term, "[ |\\.|\\,]", sep = ""), " ", x)

      # all
      rms <- paste(" ", term, "$", sep = "")
      x <- gsub(rms, " ", x)

    } else if (where == "full") {
    
      x <- gsub(term, " ", x)
      
    } else if (where == "begin") {
    
      rms <- paste("^", term, "[ |\\.|\\,]", sep = "")
      x <- gsub(rms, " ", x)
      
    } else if (where == "middle") {
    
      x <- gsub(paste(" ", term, "[ |\\.|\\,]", sep = ""), " ", x)
      
    } else if (where == "end") {
    
      rms <- paste(" ", term, "$", sep = "")
      x <- gsub(rms, " ", x)
      
    }
    
    x
    
}

