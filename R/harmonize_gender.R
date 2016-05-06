#' @title Harmonize gender
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
  sn <- read_synonymes(fn, sep = ";", mode = "table")
  g <- as.character(suppressWarnings(harmonize_names(x, synonymes = sn)))
  g
}