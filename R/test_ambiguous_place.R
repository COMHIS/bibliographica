#' @title Identify Ambiguous Place Mappings
#' @description Identify ambiguous place mappings in the region-counry mapping table.
#' @param f Input file
#' @return List of ambiguous place mappings
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples aa <- test_ambiguous_place()
#' @keywords utilities
test_ambiguous_place <- function (f = system.file("extdata/PublicationPlaceSynonymes.csv", package = "bibliographica")) {

  map <- read_mapping(f, mode = "table", sep = ";", sort = TRUE, self.match = FALSE, include.lowercase = FALSE, ignore.empty = FALSE, remove.ambiguous = FALSE, lowercase.only = TRUE)
  spl <- split(map$name, map$synonyme);
  spl <- spl[sapply(spl, length) > 1]
  
  spl

}