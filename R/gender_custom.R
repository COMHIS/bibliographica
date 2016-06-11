#' @title Custom Name-Gender Mappings
#' @description Custom first name table, including gender info.
#' @param ... Arguments to be passed
#' @return Table with first name and gender info.
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples \dontrun{x <- gender_custom()}
#' @keywords utilities
gender_custom <- function (...) {

  # Read custom table
  first <- read_mapping(system.file("extdata/names/firstnames/custom_gender.csv", package = "bibliographica"), sep = "\t", from = "name", to = "gender", mode = "table")
  dic <- "custom_firstnames"
  first$dictionary <- dic

  # Set NA gender for individual letters
  first <- rbind(first, cbind(name = letters, gender = NA, dictionary = dic))

  first <- unique(first)

  first

}