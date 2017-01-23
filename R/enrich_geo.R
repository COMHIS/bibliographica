#' @title Enrich Geodata
#' @description Enrich geodata.
#' @param df Preprocessed data.frame
#' @return Augmented data.frame
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df2 <- enrich_geo(df)}
#' @keywords utilities
enrich_geo <- function(df) {

  message("Enriching geo fields..")
  geonames <- places.geonames <- NULL

  message("Geocoordinates")
  load(system.file("extdata/geonames.RData", package = "bibliographica"))
  load(system.file("extdata/places.geonames.RData", package = "bibliographica"))
  geoc <- bibliographica::get_geocoordinates(df$publication_place,
          geonames, places.geonames)
  geoc$publication_place <- df$publication_place

  # Remove earlier versions of these fields
  if (any(names(geoc) %in% names(df))) {
    df <- df[, -which(names(df) %in% names(geoc))]
  }
  # Merge 
  df <- cbind(df, geoc)  

  # -----------------------------------------------------------------

  message("Add publication country")
  df$country <- get_country(df$publication_place)

  # We could standardize country names but problem is that e.g. England, Scotland
  # etc are not mapped (as UK). But is potentially useful later.
  #devtools::install_github("dsself/standardizecountries")
  # library(standard)
  # df$publication_country2 <- country_name(df$publication_country)
  # df$publication_country.code <- country_code(df$publication_country, "country", "iso3c")

  return (df)
}
