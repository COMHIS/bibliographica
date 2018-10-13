#' @title Field Name Table
#' @description Field names mapping table.
#' @param ... Arguments to be passed
#' @return Field names table
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples fieldname_table()
#' @keywords utilities
fieldname_table <- function (...) {

  f <- system.file("extdata/fieldname_table.csv", package = "bibliographica")
  dd <- read.csv(f, header = TRUE, sep = ";")
  dd <- apply(dd, 2, as.character)
  dd[dd == ""] <- NA
  dd <- as.data.frame(dd, stringsAsFactors = FALSE)

  dd

}
