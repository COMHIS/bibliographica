#' @title get_country
#' @description Map geographic places to country names
#' @param x A vector of region names (cities or municipalities etc.)
#' @param map data.frame with region to country mappings (fields 'region' and 'country')
#' @return data.frame
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples x2 <- get_country("Berlin")
#' @keywords utilities
get_country <- function (x, map = NULL) {

  # Speed up by handling unique cases only
  xorig <- as.character(x)
  xorig.unique <- unique(x)  
  x <- xorig.unique

  if (is.null(map)) {
    map <- place2country()
  }
  
  # Map each region in x to a country
  country <- map$country[match(x, map$region)]

  # If multiple possible countries listed; use the first one (most likely)
  country <- sapply(strsplit(as.character(country), " | "), function (x) {x[[1]]})

  # The function was sped up by operating with unique terms
  inds <- match(xorig, xorig.unique)
  x <- x[inds]
  country <- country[inds]  

  df <- data.frame(list(region = x, country = country))

  df

}


