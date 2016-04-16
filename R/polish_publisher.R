#' @title Polish publisher
#' @description Polish publishing house names
#' @param x publisher field (a vector)
#' @param synonyms Synonyme table
#' @param verbose verbose
#' @param mc.cores Number of cores for parallelization
#' @return polished publisher field (a vector)
#' @export
#' @author Niko Ilomaki \email{niko.ilomaki@@helsinki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{v <- polish_publisher(c("Oxford University Press","tryckt hos Cambridge University Press"))}
#' @keywords utilities
polish_publisher <- function(x, synonyms = NULL, verbose = TRUE, mc.cores = 1) {
  
  # Remove stopwords
  f <- system.file("extdata/stopwords_for_names.csv", package = "bibliographica")
  terms <- as.character(read.csv(f, sep = "\t", stringsAsFactors = FALSE, fileEncoding = "UTF-8", header = TRUE)$Term)

  # Initial hamornization
  x <- tolower(x)
  #x <- remove_special_chars(x, chars = c(",", ";", ":", "\\(", "\\)", "\\?", "--", "\\&", "\\.", "-"), niter = 2)
  x <- gsub("[,|;|:|\\?|-|\\&|\\.]+", "", x) 
  x <- str_trim(gsub("\\(+", "", gsub("\\)+", "", x)))
  
  xorig <- x
  xuniq <- unique(x)
  x <- xuniq

  if (verbose) {
    message(paste("Polishing publisher:", length(xuniq), "unique cases"))
  }

  # TODO: Move this to fennica
  x <- gsub("w ja g", "weilin goos", x)
  
  x <- remove_terms(x, terms, where = "begin")

  x <- str_trim(gsub("\\[", "", gsub("\\]", "", x)))

  x <- remove_print_statements(x)

  # Remove numerics
  x <- gsub("[0-9]", " ", x)
  x <- condense_spaces(x)

  # Remove strings that are single letters
  x[x %in% letters] <- NA
 
  if (is.null(synonyms)) {
    f <- system.file("extdata/publisher.csv", package = "bibliographica")
    synonyms <- read_synonymes(f, sep = ";", mode = "table")
  }

  x <- harmonize_names(x, synonyms, mode = "exact.match")

  # Project unique cases back to the original list
  x2 <- as.character(x[match(xorig, xuniq)])

  x2

}
