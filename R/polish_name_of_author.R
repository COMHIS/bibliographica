#' @title polish_name_of_author
#' @description Pick and polish name of author
#' @param x author name field (a vector)
#' @return data.frame with fields for family and first name if available
#' @importFrom tau fixEncoding
#' @export
#' @author Niko Ilomaki \email{niko.ilomaki@@helsinki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df <- polish_name_of_author(c("Washington, George","Louis XIV"))}
#' @keywords utilities
polish_name_of_author <- function(x) {

  # Remove brackets and ending commas / periods
  s <- remove_numerics(x)
  s <- gsub("\\[", "", s)
  s <- gsub("\\]", "", s)
  s <- gsub("\\(", "", s)
  s <- gsub("\\)", "", s)
  s <- str_trim(s)
  s <- gsub("\\.$", "", s)
  s <- gsub("\\,$", "", s)

  name <- fixEncoding(x,latin1=TRUE)

	family <- gsub("^(?!(.*\\, .*$)).+$",NA,name,perl=TRUE)
	family <- gsub("^(.*)\\, .*$","\\1",family)
	
  first <- gsub("^(?!(.*\\, .*$)).+$",NA,name,perl=TRUE)
  first <- gsub("^.*\\, (.*)$","\\1",first)
  
  other <- gsub("^(.*)\\, .*$",NA,name)
  
  # Multiple names: treat as other names
  inds <- grep(";", name)
  family[inds] <- NA
  first[inds] <- NA
  other[inds] <- name[inds]

  name <- apply(cbind(family, first, other), 1, function (x) {paste(x[[1]],",",x[[2]]," ",x[[3]], sep = "")})

  data.frame(list(author_name = name, family_name = family, first_name = first, other_name = other))
	
}
