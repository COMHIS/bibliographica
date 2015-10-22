#' @title polish_publisher
#' @description Polish publishing house names
#'
#' @param x publisher field (a vector)
#' @param synonyms Synonyme table
#' @return polished publisher field (a vector)
#'
#' @export
#' 
#' @author Niko Ilomaki \email{niko.ilomaki@@helsinki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{v <- polish_publisher(c("Oxford University Press","tryckt hos Cambridge University Press"))}
#' @keywords utilities
polish_publisher <- function(x, synonyms=NULL) {
  
  if (is.null(synonyms)) {
    f <- system.file("extdata/publisher.csv", package = "bibliographica")
    synonyms <- as.data.frame(read.csv(f, sep = "\t", stringsAsFactors = FALSE, fileEncoding = "UTF-8"))
  }
  
  x <- harmonize_names(x, synonyms)

  x

}
