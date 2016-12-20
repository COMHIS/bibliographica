#' @title Remove Dimension Data
#' @description Remove dimension data.
#' @param x A character vector that may contain dimension information
#' @return The character vector with dimension information removed
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # remove_dimension("4to 40cm", sheet_sizes())
#' @keywords internal
remove_dimension <- function (x, terms) {

  # Go from longest to shortest term to avoid nested effects
  terms <- terms[rev(order(sapply(unique(terms), nchar, USE.NAMES = FALSE)))]

  x[x %in% terms] <- " "

  for (term in terms) {

    inds <- grep(term, x)

    if (length(inds) > 0) {

      # begin
      rms <- paste("^", term, "[ |\\.|\\,]", sep = "")
      x[inds] <- gsub(rms, " ", x[inds])

      # middle
      x[inds] <- gsub(paste(" ", term, "[ |\\.|\\,]", sep = ""), " ", x[inds])

      # all
      rms <- paste(" ", term, "$", sep = "")
      x[inds] <- gsub(rms, " ", x[inds])

    }
    
  }

  x <- condense_spaces(x)
  inds <- which(!x == "v.") # Exclude ^v.$ as a special case
  if (length(inds) > 0) { 
    x[inds] <- remove_trailing_periods(x[inds])
  }

  x[x == ""] <- NA
  
  x

}


