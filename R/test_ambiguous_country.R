#' @title Identify Ambiguous Country Mappings
#' @description Identify ambiguous country mappings in the region-counry mapping table.
#' @param f Input file
#' @return List of ambiguous country mappings
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples aa <- test_ambiguous_country()
#' @keywords utilities
test_ambiguous_country <- function (f = system.file("extdata/reg2country.csv", package = "bibliographica")) {

  map <- read_mapping(f, mode = "table", sep = ";", sort = TRUE, self.match = FALSE, include.lowercase = FALSE, ignore.empty = FALSE, remove.ambiguous = FALSE, lowercase.only = FALSE, from = "region", to = "country")
  spl <- split(map$country, map$region);
  spl <- spl[sapply(spl, length) > 1]
  
  spl

}