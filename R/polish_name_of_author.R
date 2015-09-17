#' @title polish_name_of_author
#' @description Pick and polish name of author
#'
#' @param df Main dataframe
#' @return Main dataframe
#'
#' @importFrom tau fixEncoding
#' @export
#' 
#' @author Niko Ilomaki \email{niko.ilomaki@@helsinki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{df <- polish_name_of_author(df)}
#' @keywords utilities
polish_name_of_author <- function(df) {
	name <- fixEncoding(df$author_name,latin1=TRUE)

	family_name <- gsub("^(?!(.*\\, .*$)).+$",NA,name,perl=TRUE)
	family_name <- gsub("^(.*)\\, .*$","\\1",family_name)
	df$family_name <- family_name

	first_name <- gsub("^(?!(.*\\, .*$)).+$",NA,name,perl=TRUE)
	first_name <- gsub("^.*\\, (.*)$","\\1",first_name)
	df$first_name <- first_name

	other_name <- gsub("^(.*)\\, .*$",NA,name)
	df$other_name <- other_name

	df
}
