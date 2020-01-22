#' @title Approximate Years
#' @description Approximate year for entries that are approximations.
#' @param x year field (a vector)
#' @return data.frame with the fields 'start' and 'end'
#' @export
#' @author Leo Lahti and Hege Roivainen email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df <- polish_years(c("1746", "1745-1750"))}
#' @keywords utilities
approximate_years <- function(x) {

  x <- gsub("\\.*$", "", x)
  x <- gsub("^\\[", "", x)
  x <- gsub("\\]$", "", x)
  x <- gsub(" or later", "", x)
  x <- gsub("\\[blank\\]", "-", x)  
  approx <- rep("year", length(x))

  inds <- grep("^[0-9]{3}-*\\?*-*$", x)

  if (length(inds) > 0) {
    approx[inds] <- "decade"
    x[inds] <- gsub("-*\\?*-*$", "0", x[inds])
  }

  inds <- grep("^[0-9]{2}-*\\?*-*$", x)
  if (length(inds) > 0) {  
    approx[inds] <- "century"   
    x[inds] <- gsub("-*\\?*-*$", "00", x[inds])    
  }

  if (length(grep("^[0-9]*-*\\?*-*$", x) > 0)) {
    x <- gsub("-", "0", x)
  }

  # Finally, discard years that have more than 4 characters
  x[nchar(x) > 4] <- NA

  data.frame(from = as.numeric(x), approximation = approx)

}