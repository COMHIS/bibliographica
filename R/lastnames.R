#' @title lastnames
#' @description Vector of last names
#' @param ... Arguments to be passed
#' @return Vector of last names
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x <- lastnames()}
#' @keywords utilities
lastnames <- function (...) {

  # Last names: http://www2.census.gov/topics/genealogy/1990surnames/dist.all.last United States Census Bureau. Source: http://www.census.gov/topics/population/genealogy/data/1990_census/1990_census_namefiles.html (also first names available here but less names than in the baby database)
  last.census <- as.character(read.csv(system.file("extdata/names/lastnames/last.csv", package = "bibliographica"), sep = "\t")[,1])

  # Custom last names
  last.custom <- as.character(read.csv(system.file("extdata/names/lastnames/custom.csv", package = "bibliographica"), sep = "\t")[,1])

  # Also add custom first names
  # since in many cases the original catalogue has erroneous notation
  # mixing first and last names and we do not want to filter out valid names if they
  # happen to be in wrong field last/first
  # first.custom <- as.character(read.csv(system.file("extdata/names/firstnames/custom.csv", package = "bibliographica"), sep = "\t")[,1])

  # Also accept pseudonymes
  pseudo <- as.character(read.csv(system.file("extdata/names/pseudonymes/last.csv", package = "bibliographica"), header = TRUE)[,1])

  last <- unique(c(last.census, last.custom, pseudo))

  last
}
