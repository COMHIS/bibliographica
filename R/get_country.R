#' @title Get country
#' @description Map geographic places to country names
#' @param x A vector of region names (cities or municipalities etc.)
#' @param map data.frame with region to country mappings (fields 'region' and 'country')
#' @return Country vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples x2 <- get_country("Berlin")
#' @keywords utilities
get_country <- function (x, map = NULL) {

  # TODO we could perhaps add here some more countries from geonames
  # We could standardize country names but problem is that e.g.
  # England, Scotland
  # etc are not mapped (as UK). But is potentially useful later.
  # devtools::install_github("dsself/standardizecountries")
  # library(standard)
  # df.preprocessed$publication_country2 <- country_name(df.preprocessed$publication_country)
  # df.preprocessed$publication_country.code <- country_code(df.preprocessed$publication_country, "country", "iso3c")

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
  country <- str_trim(sapply(strsplit(as.character(country), "\\|"), function (x) {x[[1]]}))

  # The function was sped up by operating with unique terms
  inds <- match(xorig, xorig.unique)

  #x <- x[inds]
  #df <- data.frame(list(region = x, country = country))
  df <- data.frame(country = country[inds])

}


