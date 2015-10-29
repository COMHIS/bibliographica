#' @title remove_stopwords
#' @description Remove stopwords from input vector
#'
#' @param x A vector
#' @param terms Stopwords
#' @param remove.letters Logical. Also remove single letters (TRUE by default)
#' @return A vector polished
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples x2 <- remove_stopwords(c("a", "well", "james"), "well", remove.letters = TRUE)
#' @keywords utilitie
remove_stopwords <- function (x, terms = NULL, remove.letters = TRUE) {

  if (is.null(terms)) {
    f <- system.file("extdata/stopwords.csv", package = "bibliographica")
    message(paste("No stopwords provided. Reading stopwords from file ", f))
    terms <- as.character(read.csv(f)[,1])
  }
  
  # List all unique terms
  terms <- sort(unique(terms))
  x <- remove_terms(x, terms, c("begin", "middle", "end"))

  if (remove.letters) {
    x <- remove_letters(x)
  }

  x <- remove_trailing_periods(x)

  x

}


#' @title trimming
#' @description Trim strings
#'
#' @param x vector
#' @param n iterations
#' @return Polished vector
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{x2 <- trimming(x, n = 1)}
#' @keywords utilities
trimming <- function (x, n = 1){ 

  for (k in 1:n) {

  x <- str_trim(x)

  x <- gsub("\\.$", "", x)
  x <- gsub("\\,$", "", x)  	
  x <- gsub(";$", "", x)
  x <- gsub(":$", "", x)
  x <- gsub("\\,$", "", x)
  x <- gsub("\\)$", "", x)
  x <- gsub("\\.$", "", x)

  x <- gsub("\\,\\)", "\\)", x)
  x <- gsub("\\,;", ";", x)
  x <- gsub("^\\.", "", x)
  x <- gsub("^\\.,", "", x)
  x <- gsub("^\\.", "", x)
  x <- gsub("^\\,", "", x)
  x <- gsub("^\\.", "", x)
  x <- gsub("^\\(", "", x)

  x <- gsub(";  ", ";", x)
  x <- gsub(" \\. ", " ", x)
  x <- gsub("; ", ";", x)
  x <- gsub(";;", ";", x)
  x[x == ""] <- NA
  x <- gsub("\\, ", "\\,", x)
  x <- gsub("  ", " ", x)

  # print("Remove leading spaces")
  x <- sapply(strsplit(x, ";"), function (s) {paste(str_trim(s), collapse = ";")})

  x <- gsub(":", "\\,", x)
  x <- gsub("; in ", "; ", x)
  x <- gsub(";in ", "; ", x)
  x <- gsub(" \\,", "\\,", x)
  x <- gsub(";and", ";", x)
  x <- gsub("\\.;", ";", x)
  x <- gsub("\\(", " ", x)
  x <- gsub("\\)", " ", x)
  x <- gsub("\\,", " ", x)
  x <- gsub(" in ", " ", x)

  }

  x
}

#' @title remove_trailing_periods
#' @description Remove trailing periods
#'
#' @param x A vector
#' @return A polished vector
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{x2 <- remove_trailing_periods(x)}
#' @keywords utilities
remove_trailing_periods <- function (x){ 

  if (is.na(x)) {
    return(x)
  }

  xold <- x; xold[[1]] <- "XXXXXX"
  while (!mean(na.omit(xold == x)) == 1) {
    xold <- x
    x <- condense_spaces(x)
    x <- gsub("\\.$", "", x)
    x <- gsub("^\\.", "", x)
  }

  x
}

