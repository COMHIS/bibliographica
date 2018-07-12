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

  # Denote the polishing directly in the table		 
  glevels <- unique(c(gatherings_table()$Standard,
               gatherings_table()$Name,
      	       gatherings_table()$Alternate,
     	       gatherings_table()$Symbol))

  glevels <- unique(c(glevels, tolower(glevels)))
  
  # Remove unrecognized formats
  inds <- which(!tolower(x) %in% glevels)
  if (length(inds) > 0) {
    warning(paste("Removing the following unrecognized gatherings:", paste(sort(unique(x[inds])), collapse = ", ")))
    x[inds] <- NA
  }

  x

}
