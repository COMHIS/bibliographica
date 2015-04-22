#' order_gatherings
#'
#' @description Sort gatherings levels 
#'
#' @param x A vector or factor of gathering data
#'
#' @return A factor with ordered gatherings levels
#'
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("estc")
#' 
#' @examples # order_gatherings(factor(c("2to", "1to", "8to")))
#' @export
#' @keywords utilities
order_gatherings <- function (x) {

  glevels <- c("1to", "bs", "2long", "2to", "2small", "4long", "4to", "4small", "8to", "12long", "12to", "16to", "18to", "24long", "24to", "32to", "48to", "64to", NA)		 

  if (!all(unique(x) %in% glevels)) {stop(paste("Add", paste(unlist(setdiff(unique(x), glevels)), collapse = "/"), "in gatherings levels in order_gatherings function"))}

  # Order the levels
  xo <- factor(x, levels = glevels)

  xo
}
