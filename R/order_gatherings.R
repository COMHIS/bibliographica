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

  glevels <- c("1to", "bs", "2long", "2fo", "2small", "4long", "4to", "4small", "8vo", "12long", "12mo", "16mo", "18mo", "24long", "24mo", "32mo", "48mo", "64mo", "NA")

  x <- as.character(x)
  x[is.na(x)] <- "NA"

  if (!all(unique(x) %in% glevels)) { stop(paste("Add", paste(unlist(setdiff(unique(x), glevels)), collapse = "/"), "in gatherings levels in order_gatherings function")) }

  # Order the levels
  xo <- factor(x, levels = glevels)

  xo
}
