#' @title Polish Publisher
#' @description Polish publishing house names.
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

  xorig <- tolower(x)
  xuniq <- unique(xorig)

  # Start polishing
  x <- xuniq

  if (verbose) {
    message(paste("Polishing publisher:", length(xuniq), "unique cases"))
  }

  if (verbose) { message("..reading tables") }  
  f <- system.file("extdata/stopwords_for_names.csv", package = "bibliographica")
  terms <- as.character(read.csv(f, sep = "\t", stringsAsFactors = FALSE, fileEncoding = "UTF-8", header = TRUE)$Term)

  f <- system.file("extdata/replace_special_chars.csv",
		package = "bibliographica")
  spechars <- read_mapping(f, sep = ";", mode = "table", include.lowercase = TRUE, fast = TRUE)
   

  # Initial harmonization
  x <- gsub("[,|;|:|\\?|-|\\&|\\.]+", "", x) 
  x <- str_trim(gsub("\\(+", "", gsub("\\)+", "", x)))
  x <- remove_terms(x, terms, where = "begin")
  x <- str_trim(gsub("\\[", "", gsub("\\]", "", x)))
  x <- gsub("[0-9]", " ", x) # Remove numerics
  x <- condense_spaces(x)

  # Back to original indices, then unique again; reduces
  # number of unique cases further
  x <- x[match(xorig, xuniq)]
  xorig <- x
  xuniq <- sort(unique(x))
  x <- xuniq

  if (verbose) {
    message(paste("..", length(xuniq), "unique cases"))
  }

  if (verbose) { message("..converting special characters") }
  x <- as.character(map(x, spechars, mode = "recursive"))

  # Remove print statements
  x <- remove_print_statements(x)

  # Remove strings that are single letters
  x[x %in% letters] <- NA

  # Back to original indices, then unique again; reduces
  # number of unique cases further
  x <- x[match(xorig, xuniq)]
  xorig <- x
  xuniq <- sort(unique(x))
  x <- xuniq

  if (is.null(synonyms)) {
    f <- system.file("extdata/publisher.csv", package = "bibliographica")
    synonyms <- read_mapping(f, sep = ";", mode = "table", lowercase = TRUE, fast = TRUE)    
  }

  x <- map(x, synonyms, mode = "exact.match")  

  # Project unique cases back to the original list
  x2 <- as.character(x[match(xorig, xuniq)])

  x2

}

