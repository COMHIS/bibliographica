#' @title Get Country
#' @description Map geographic places to country names.
#' @param x A vector of region names (cities or municipalities etc.)
#' @param map data.frame with region to country mappings (fields 'region' and 'country')
#' @return Country vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples x2 <- get_country("Berlin")
#' @keywords utilities
get_country <- function (x, map = NULL) {

  # TODO could add here some more countries from geonames
  # We could standardize country names but problem is that e.g.
  # England, Scotland
  # etc are not mapped (as UK). But is potentially useful later.
  # devtools::install_github("dsself/standardizecountries")
  # library(standard)
  # df.preprocessed$publication_country2 <- country_name(df.preprocessed$publication_country)
  # df.preprocessed$publication_country.code <- country_code(df.preprocessed$publication_country, "country", "iso3c")

  # Speed up by handling unique cases only
  xorig <- as.character(x)
  xorig.unique <- unique(xorig)  
  x <- xorig.unique

  if (is.null(map)) {
    f <- system.file("extdata/reg2country.csv", package = "bibliographica")
    message(paste("Reading region-country mappings from file ", f))
    map <- read_mapping(f, mode = "table", sep = ";", sort = TRUE, self.match = FALSE, include.lowercase = FALSE, ignore.empty = FALSE, remove.ambiguous = TRUE, lowercase.only = FALSE, from = "region", to = "country") 
  }
  
  message("Map each region in x to a country")
  # use lowercase
  # country <- map$country[match(tolower(x), tolower(map$region))]
  spl <- split(tolower(map$country), tolower(map$region)) 
  spl <- spl[tolower(x)]
  
  # If mapping is ambiguous, then name the country as ambiguous
  spl <- lapply(spl, unique)
  spl[which(sapply(spl, function (x) {length(unique(x)) > 1}, USE.NAMES = FALSE))] <- "ambiguous"
  spl[which(sapply(spl, function (x) {length(x) == 0}, USE.NAMES = FALSE))] <- NA  
  spl <- unlist(as.vector(spl))
  country <- spl

  # If multiple possible countries listed and separated by |;
  # use the first one (most likely)
  country <- str_trim(sapply(strsplit(as.character(country), "\\|"), function (x) {ifelse(length(x) > 0, x[[1]], NA)}, USE.NAMES = FALSE))

  # Use the final country names
  country <- map$country[match(tolower(country), tolower(map$country))]

  # The function was sped up by operating with unique terms
  country[match(xorig, xorig.unique)]

}


