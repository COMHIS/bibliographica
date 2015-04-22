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
  x <- remove_terms(x, terms, "all")

  if (remove.letters) {
    x <- remove_letters(x)
  }

  x <- remove_trailing_periods(x)

  x

}


#' @title remove_letters
#' @description Remove specific single letters
#'
#' @param x A vector
#' @return Polished vector
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{remove_letters(x)}
#' @keywords utilities
remove_letters <- function (x) {
  x <- remove_terms(x, setdiff(c(letters, LETTERS), c("S", "s", "N", "n", "E", "e", "W", "w")), "begin")
  x <- remove_terms(x, c(letters, LETTERS), "end")
  x
}


#' @title remove_special_chars
#' @description Remove special characters
#'
#' @param x Character vector
#' @param chars Characters to be removed
#' @param niter Number of iterations 
#' @return Polished vector
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{remove_special_chars()}
#' @keywords utilities
remove_special_chars <- function (x, chars = c(",", ";", ":", "\\(", "\\)", "\\?", "--", "\\&"), niter = 5) {

  for (n in 1:niter) {
    for (char in chars) {
      x <- str_trim(x)
      x <- gsub(paste(char, "$", sep = ""), " ", x)
      x <- gsub(paste("^", char, sep = ""), " ", x)
      x <- gsub(char, " ", x)
    }

    for (char in c("\\[", "]")) {
      x <- str_trim(x)
      x <- gsub(paste(char, "$", sep = ""), "", x)
      x <- gsub(paste("^", char, sep = ""), "", x)
      x <- gsub(char, "", x)
    }
  }

  x <- condense_spaces(x)
   
  x[x == ""] <- NA

  x

}


#' @title condense_spaces
#' @description Trim spaces
#'
#' @param x A vector
#' @return A polished vector
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{x2 <- condense_spaces(x)}
#' @keywords utilities
condense_spaces <- function (x) {

  x <- str_trim(x)

  while (length(grep("  ", x)) > 0) {
    x <- gsub("  ", " ", x)
  }

  x[x == ""] <- NA
   
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

  xold <- x; xold[[1]] <- "XXXXXX"
  while (!mean(na.omit(xold == x)) == 1) {
    xold <- x
    x <- condense_spaces(x)
    x <- gsub("\\.$", "", x)
    x <- gsub("^\\.", "", x)
  }

  x
}

