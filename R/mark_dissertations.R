#' @title mark_dissertations
#' @description Establish two binary variables: dissertation and synodal dissertation
#'
#' @param df Main dataframe
#' @return Main dataframe
#'
#' @export
#' 
#' @author Niko Ilomaki \email{niko.ilomaki@@helsinki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{df <- mark_dissertations(df)}
#' @keywords utilities
mark_dissertations <- function(df) {
	v <- df$note_dissertation
	v[v=="Diss"] <- TRUE
	v[v=="Diss. :;Diss"] <- TRUE
	v[v=="Diss. synod"] <- FALSE
	v[is.na(v)] <- FALSE
  	v <- as.logical(v)
	df$dissertation <- v

	v <- df$note_dissertation
	v[v=="Diss"] <- FALSE
	v[v=="Diss. :;Diss"] <- FALSE
	v[v=="Diss. synod"] <- TRUE
	v[is.na(v)] <- FALSE
  	v <- as.logical(v)
	df$synodal <- v

	df
}
