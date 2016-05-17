#' @title Harmonize Gender
#' @description Harmonize commonly used gender terms
#' @param x A vector of gender codes
#' @return Harmonized vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples x2 <- harmonize_gender(c("male", "female", "either"))
#' @keywords utilities
harmonize_gender <- function (x) {
  fn <- system.file("extdata/harmonize_gender.csv", package = "bibliographica")
  sn <- read_mapping(fn, sep = ";", mode = "table")
  as.character(suppressWarnings(map(x, synonymes = sn)))
}