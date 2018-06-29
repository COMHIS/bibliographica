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

  # Denote the ordering directly in the table		 
  glevels <- gatherings_table()$Standard

  # Recognize format
  if (any(!x %in% glevels)) {
     lower <- all(tolower(x) == x)
     x <- tolower(x)
     # glevels <- map_gatherings(glevels, from = "short", to = "long")
     glevels <- tolower(unique(gatherings_table()$Name))

     if (!lower) {
       x <- str_to_title(x)
       glevels <- str_to_title(glevels)       
     }

  }

  x <- as.character(x)
  x[is.na(x)] <- "NA"
  fails <- unlist(setdiff(unique(x), glevels), use.names = FALSE)
  x[x %in% fails] <- "NA"
  inds <- which(glevels %in% x)
  glevels <- glevels[inds]

  # Order the levels
  xo <- factor(x, levels = glevels)

  xo
}
