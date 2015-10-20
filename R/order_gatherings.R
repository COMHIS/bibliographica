#' order_gatherings
#'
#' @description Sort gatherings levels 
#'
#' @param x A vector or factor of gathering data
#'
#' @return A factor with ordered gatherings levels
#'
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples order_gatherings(factor(c("2fo", "1to", "8vo")))
#' @export
#' @keywords utilities
order_gatherings <- function (x) {

  glevels <- c("1to", "bs", "2long", "2fo", "2small", "4long", "4to", "4small", "8vo", "12long", "12mo", "16mo", "18mo", "24long", "24mo", "32mo", "40to", "48mo", "64mo", "84to", "NA")

  x <- as.character(x)
  x[is.na(x)] <- "NA"

  fails <- unlist(setdiff(unique(x), glevels))
  x[x %in% fails] <- "NA"
  #if (length(fails)>0) { stop(paste("Add", paste(fails, collapse = "/"), "in gatherings levels in order_gatherings function")) }

  # Order the levels
  xo <- factor(x, levels = glevels)

  xo
}
