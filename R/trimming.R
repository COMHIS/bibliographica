#' @title trimming
#' @description Trim strings
#' @param x vector
#' @param n iterations
#' @return Polished vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
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

