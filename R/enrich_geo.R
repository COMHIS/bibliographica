#' @title Enrich Geodata
#' @description Enrich geodata.
#' @param x Publication place character vector
#' @return Augmented data.frame
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df2 <- enrich_geo(df)}
#' @keywords utilities
enrich_geo <- function(x) {

  # NOTE: please keep the input and output formats of this function
  # constant or talk to LL first.

  message("Enriching geo fields..")
  df <- data.frame(place = x)
  geonames <- places.geonames <- NULL

  message("Geocoordinates")
  # Use some manually fetched data
  load(system.file("extdata/geonames.RData", package = "bibliographica"))
  load(system.file("extdata/places.geonames.RData", package = "bibliographica"))
  geoc <- bibliographica::get_geocoordinates(df$place,
            geonames, places.geonames)
  geoc$place <- df$place

  message("Add publication country")
  df$country <- get_country(df$place)
  message(".. publication country added")  

  # We could standardize country names but problem is that e.g. England, Scotland
  # etc are not mapped (as UK). But is potentially useful later.
  #devtools::install_github("dsself/standardizecountries")
  # library(standard)
  # df$publication_country2 <- country_name(df$publication_country)
  # df$publication_country.code <- country_code(df$publication_country, "country", "iso3c")

  return (df)
}
