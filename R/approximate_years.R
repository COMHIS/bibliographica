#' @title Approximate Years
#' @description Approximate year for entries that are approximations.
#' @param x year field (a vector) 
#' @param start_synonyms Synonyme table for start year
#' @param end_synonyms Synonyme table for end year
#' @param verbose verbose
#' @param min.year Minimum year accepted
#' @param max.year Maximum year accepted
#' @param check If true, remove entries (replace by NA) where start > end
#' @return data.frame with the fields 'start' and 'end'
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df <- polish_years(c("1746", "1745-1750"))}
#' @keywords utilities
approximate_years <- function(x) {

  x <- gsub("\\.*$", "", x)
  x <- gsub("^\\[", "", x)
  x <- gsub("\\]$", "", x)
  x <- gsub("\\?$", "", x)  
  x <- gsub(" or later", "", x)

  approx <- rep("year", length(x))
  inds <- grep("^[0-9]{3}-*$", x)
  approx[inds] <- "decade"
  inds <- grep("^[0-9]{2}-*$", x)
  approx[inds] <- "century"   
  
  if (grep("^[0-9]*-*$", x)) {
    x <- gsub("-", "0", x)
  }



  data.frame(from = as.numeric(x), approximation = approx)

}