#' @title Polish Title
#' @description Polish the title field.
#' @param x Vector of titles
#' @return Vector of titles polished
#' @export
#' @details Remove ending commas, periods, spaces and parentheses, 
#' 	    starting prepositions etc.
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x2 <- polish_title(x)}
#' @keywords utilities
polish_title <- function (x) {

  x <- as.character(x)

  x <- gsub("\\,$", "", x)
  x <- gsub("\\.$", "", x)

  x <- gsub(":$", "", x)
  x <- gsub(";$", "", x)

  x <- gsub("^a ", "^A ", x)
  x <- gsub("^\\[A\\] ", "^A ", x)
  x <- gsub("^\\[a\\] ", "^A ", x)
  x <- gsub("^\\[d\\] ", "^D ", x)
  x <- gsub("^\\[D\\] ", "^D ", x)
  x <- gsub("^the ", "^The ", x)
  x <- gsub("\\. A  ", ", a ", x)
  x <- gsub(" TEST TEST TEST ", " ", x)

  # Additions for Kungliga
   x <- gsub("(^[^/]+)([/]+)$", "\\1", x) 
	# Remove slashes from the end
	# unless slashes are used as a container
  x <- gsub("\\.$", "", x)
  x <- gsub("[ ]+$", "", x)
  # Additions end here

  x <- gsub("\\]$", "", x)
  x <- gsub("^\\[", "", x)
  x <- gsub("\\)$", "", x)
  x <- gsub("^\\(", "", x)

  x
}