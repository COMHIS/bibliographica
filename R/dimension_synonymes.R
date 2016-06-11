#' Return dimension synonyme table
#' @param ... Arguments to be passed
#' @return Dimension synonyme table
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples dimension_synonymes()
#' @keywords utilities
dimension_synonymes <- function (...) {

  f <- system.file("extdata/harmonize_dimensions.csv", package = "bibliographica")
  dd <- read.csv(f, header = TRUE, sep = "\t", fileEncoding = "latin1")
  dd <- apply(dd, 2, as.character)
  dd[dd == ""] <- NA
  dd <- as.data.frame(dd, stringsAsFactors = FALSE)

  dd
		    
  
}
