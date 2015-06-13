#' @title harmonize_names
#' @description Harmonize names
#'
#' @param x A character vector 
#' @param synonymes synonyme table.
#'        The selected names are in column 1, the synonymes in column 2.
#' @param remove.unknown Logical. Remove terms that do not have synonymes.
#'
#' @return Harmonized vector where synonymes are renamed by the selected names
#'
#' @export
#'
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{x2 <- harmonize_names(x, file)}
#' @keywords utilities
harmonize_names <- function (x, synonymes, remove.unknown = FALSE) {

  # Check which terms are not on the synonyme list and add them there		
  if (!remove.unknown) {
    r <- setdiff(x, synonymes$synonyme)
    synonymes <- rbind(synonymes,
    	      as.data.frame(list(name = r, synonyme = r)))    
  }

  # Polish the synonyme table 		
  synonymes <- check_synonymes(synonymes)
  colnames(synonymes) <- c("name", "synonyme")

  # Map synonymes to selected names: NA if mapping not available
  xh <- as.character(synonymes$name[match(x, synonymes$synonyme)])

  # message("Return data frame")
  data.frame(list(name = xh, original = x))

}


