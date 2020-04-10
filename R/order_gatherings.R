#' @title order_gatherings
#' @description Sort gatherings levels 
#' @param x A vector or factor of gathering data
#' @return A factor with ordered gatherings levels
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples order_gatherings(factor(c("2fo", "1to", "8vo")))
#' @export
#' @keywords utilities
order_gatherings <- function (x) {

  # Remove unrecognized formats
  x <- polish_gatherings(x)

  # Recognize gatherings format (2fo or folio)
  format <- gatherings_format(x)
  
  # Denote the ordering directly in the table		 
  glevels <- tolower(unique(gatherings_table()[[format]]))

  # Capitalize
  x <- str_to_title(x)
  glevels <- str_to_title(glevels)
  
  # Exception
  x <- gsub("Bs", "bs", x)
  glevels <- gsub("Bs", "bs", glevels)  

  x <- as.character(x)
  x[is.na(x)] <- "NA"
  fails <- unlist(setdiff(unique(x), glevels), use.names = FALSE)
  x[x %in% fails] <- "NA"
  inds <- which(glevels %in% x)
  glevels <- glevels[inds]

  # Order the levels
  factor(x, levels = glevels)
}
