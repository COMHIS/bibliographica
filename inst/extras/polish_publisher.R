#' @title Polish Publisher
#' @description Generic function for preliminary polishing of publishing house names.
#' @param x publisher field (a vector)
#' @param verbose verbose
#' @param mc.cores Number of cores for parallelization
#' @return polished publisher field (a vector)
#' @export
#' @author Niko Ilomaki \email{niko.ilomaki@@helsinki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{v <- polish_publisher(c("Oxford University Press","tryckt hos Cambridge University Press"))}
#' @keywords utilities
polish_publisher <- function(x, verbose = TRUE, mc.cores = 1) {

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

  # Back to original indices, then unique again; reduces
  # number of unique cases further
  xorig <- x[match(xorig, xuniq)]
  x <- xuniq <- unique(xorig)

  x <- remove_terms(x, terms, where = "begin")
  x <- str_trim(gsub("\\[", "", gsub("\\]", "", x)))
  x <- gsub("[0-9]", " ", x) # Remove numerics
  x <- condense_spaces(x)

  # Back to original indices, then unique again; reduces
  # number of unique cases further
  xorig <- x[match(xorig, xuniq)]
  x <- xuniq <- unique(xorig)

  if (verbose) { message("..converting special characters") }
  x <- as.character(map(x, spechars, mode = "recursive"))

  # Remove print statements
  x <- remove_print_statements(x)

  # Remove strings that are single letters
  x[x %in% letters] <- NA

  # select the first item from the list
  x <- gsub("^([^;]+);.*$", "\\1", x)
  x <- gsub("^([^(]+)[(].*[)]$", "\\1", x)
  x <- gsub("^([^[]+)[[].*[]]$", "\\1", x)
  x <- gsub("^[(].*[)]([^(]+)$", "\\1", x)
  x <- gsub("^[[].*[]]([^[]+)$", "\\1", x)


  # remove everything in brackets or parentheses after collecting i
  # komm., distr., exp., för ... -information TBD
   
  # remove naughty characters from the rear
  endings=c(" ", "\\(", "\\)", "\\[", "\\]", "\\.", ";", ":", ",", "'")
  x <- remove_endings(x, endings=endings, random_order=TRUE)
  
  # replace naughty characters from the middle
  # At least in Finto data there's "$b" separating two clauses, and this causes a problem for str_replace
  # I don't know what the real character should be, so I'll just select one at random
  x <- gsub(" [$]b", ".", x)
  x <- gsub(" [$]", "", x)

  # Project to original indices and return
  as.character(x[match(xorig, xuniq)])

}

