#' @title mark_dissertations
#' @description Establish two binary variables: dissertation and synodal dissertation
#' @param x dissertation field (a vector)
#' @return data.frame with fields for regular disserations and for synodal dissertations
#' @export
#' @author Niko Ilomaki \email{niko.ilomaki@@helsinki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{df <- mark_dissertations(c("Diss.","Diss. synod"))}
#' @keywords utilities
mark_dissertations <- function(x) {

	dis <- x
	dis[dis=="Diss"] <- TRUE
	dis[dis=="Diss."] <- TRUE
	dis[dis=="Diss. :;Diss"] <- TRUE
	dis[dis=="Diss. synod"] <- FALSE
	dis[is.na(dis)] <- FALSE
  	dis <- as.logical(dis)

	syn <- x
	syn[syn=="Diss"] <- FALSE
	syn[syn=="Diss."] <- FALSE
	syn[syn=="Diss. :;Diss"] <- FALSE
	syn[syn=="Diss. synod."] <- TRUE
	syn[is.na(syn)] <- FALSE
  	syn <- as.logical(syn)

	data.frame(list(dissertation = dis, synodal = syn))
	
}
