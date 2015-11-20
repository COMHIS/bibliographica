#' @title polish_publisher
#' @description Polish publishing house names
#' @param x publisher field (a vector)
#' @param synonyms Synonyme table
#' @param verbose verbose
#' @return polished publisher field (a vector)
#' @export
#' @author Niko Ilomaki \email{niko.ilomaki@@helsinki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{v <- polish_publisher(c("Oxford University Press","tryckt hos Cambridge University Press"))}
#' @keywords utilities
polish_publisher <- function(x, synonyms = NULL, verbose = TRUE) {
  
  # Remove stopwords
  f <- system.file("extdata/stopwords_for_names.csv", package = "bibliographica")
  terms <- read.csv(f, sep = "\t", stringsAsFactors = FALSE, fileEncoding = "UTF-8", header = TRUE)$Term

  xorig <- x
  xuniq <- unique(x)
  if (verbose) {
    message(paste("Polishing publisher:", length(xuniq), "unique cases"))
  }

  x <- remove_terms(xuniq, terms, where = "begin")
  x <- remove_print_statements(x)
  x <- remove.squarebrackets(x)
  x <- remove_sl(x)  

  if (is.null(synonyms)) {
    f <- system.file("extdata/publisher.csv", package = "bibliographica")
    synonyms <- as.data.frame(read.csv(f, sep = ";", stringsAsFactors = FALSE, fileEncoding = "UTF-8", header = TRUE))
  }

  x <- as.character(harmonize_names(x, synonyms, mode = "exact.match")$name)

  # Project unique cases back to the original list
  x2 <- as.character(x[match(xorig, xuniq)])

  x2

}
