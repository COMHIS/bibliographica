message("Enriching geo fields..")

print("Geocoordinates")
#source("geocoordinates.R")
load(system.file("extdata/geonames.RData", package = "bibliographica"))
load(system.file("extdata/places.geonames.RData", package = "bibliographica"))

geoc <- bibliographica::get_geocoordinates(df.preprocessed$publication_place,
				geonames, places.geonames)
geoc$publication_place <- df.preprocessed$publication_place

# Remove earlier versions of these fields
if (any(names(geoc) %in% names(df.preprocessed))) {
  df.preprocessed <- df.preprocessed[, -which(names(df.preprocessed) %in% names(geoc))]
}
# Merge 
df.preprocessed <- cbind(df.preprocessed, geoc)  

# -----------------------------------------------------------------

print("Add publication country")
df.preprocessed$country <- get_country(df.preprocessed$publication_place)$country
# We could standardize country names but problem is that e.g. England, Scotland
# etc are not mapped (as UK). But is potentially useful later.
#devtools::install_github("dsself/standardizecountries")
# library(standard)
# df$publication_country2 <- country_name(df$publication_country)
# df$publication_country.code <- country_code(df$publication_country, "country", "iso3c")
