#' @title polish_gatherings
#' @description Polish gatherings
#' @param x A vector or factor of gathering data
#' @return A factor with polished gatherings levels
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples polish_gatherings(factor(c("2fo", "1tolded", "8vo")))
#' @export
#' @keywords utilities
polish_gatherings <- function (x) {

  gtab <- gatherings_table()

  # Denote the polishing directly in the table		 
  glevels <- unique(c(gtab$Standard,
               gtab$Name,
      	       gtab$Alternate,
     	       gtab$Symbol))

  glevels <- unique(c(glevels, tolower(glevels)))
  
  # Remove unrecognized formats
  inds <- which(!tolower(x) %in% glevels)
  if (length(inds) > 0) {
    warning(paste("Removing the following unrecognized gatherings:", paste(sort(unique(x[inds])), collapse = ", ")))
    x[inds] <- NA
  }

  tolower(x)

}
