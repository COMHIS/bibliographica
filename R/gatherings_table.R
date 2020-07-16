#' @title Gatherings Table
#' @description Document gatherings names mapping table.
#' @param ... Arguments to be passed
#' @return Document gatherings table
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples gatherings_table()
#' @keywords utilities
gatherings_table <- function (...) {

  f <- system.file("extdata/document_size_abbreviations.csv", package = "bibliographica")
  dd <- read.csv(f, header = TRUE, sep = ";")
  dd <- apply(dd, 2, as.character)
  dd[dd == ""] <- NA
  dd <- as.data.frame(dd, stringsAsFactors = FALSE)

  dd

}
