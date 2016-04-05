#' Region-country mapping table
#' @param ... Arguments to be passed
#' @return data.frame
#' @keywords utilities
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
place2country <- function (...) {

  f <- system.file("extdata/reg2country.csv", package = "bibliographica")
  message(paste("Reading region-country mappings from file ", f))
  map <- read.csv(f, sep = ";", header = T, row.names = NULL)

  # Remove duplicates
  map <- map[!duplicated(map),]

  # Order by region
  map <- map[order(map$region),]

  map
}

