#' @title polish_name_of_author
#' @description Pick and polish name of author
#'
#' @param x author name field (a vector)
#' @return data.frame with fields for family and first name if available
#'
#' @importFrom tau fixEncoding
#' @export
#' 
#' @author Niko Ilomaki \email{niko.ilomaki@@helsinki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{df <- polish_name_of_author(c("Washington, George","Louis XIV"))}
#' @keywords utilities
polish_name_of_author <- function(x) {
	name <- fixEncoding(x,latin1=TRUE)

	family <- gsub("^(?!(.*\\, .*$)).+$",NA,name,perl=TRUE)
	family <- gsub("^(.*)\\, .*$","\\1",family)

	first <- gsub("^(?!(.*\\, .*$)).+$",NA,name,perl=TRUE)
	first <- gsub("^.*\\, (.*)$","\\1",first)

	other <- gsub("^(.*)\\, .*$",NA,name)

	data.frame(list(family_name = family, first_name = first, other_name = other))
	
}
