#' @title Field Table
#' @description Field codes mapping table.
#' @param ... Arguments to be passed
#' @return Field codes table
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples field_table()
#' @keywords utilities
field_table <- function (...) {

  f <- system.file("extdata/fieldnames.csv", package = "bibliographica")
  dd <- read.csv(f, header = TRUE, sep = "|")
  dd <- apply(dd, 2, as.character)
  dd[dd == ""] <- NA
  dd <- as.data.frame(dd, stringsAsFactors = FALSE)

  dd

}
