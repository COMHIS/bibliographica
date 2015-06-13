#' @title check_synonymes
#'
#' @description Check synonyme table. 
#'
#' @param synonymes synonymes data.frame
#' @return Polished synonyme table
#'
#' @export
#'
#' @details Remove duplicated information. Ensure identical matches
#' are included in synonyme list.
#'
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples \dontrun{s <- check_synonymes(synonymes)}
#' @keywords utilities
check_synonymes <- function (synonymes) {

  synonyme <- NULL		

  # Trim
  # spl[ambiguous]
  synonymes$name <- str_trim(synonymes$name)
  synonymes$synonyme <- str_trim(synonymes$synonyme)

  # Ensure each name is synonyme for itself, also in lowercase
  synonymes <- rbind(synonymes, cbind(name = synonymes$name, synonyme = synonymes$name))
  synonymes <- rbind(synonymes, cbind(name = synonymes$name, synonyme = tolower(synonymes$name)))

  # Remove duplicated info
  synonymes <- synonymes[!duplicated(synonymes),]
  spl <- split(as.character(synonymes$name), as.character(synonymes$synonyme))
  
  # Remove ambiguous names (map to many higher-level names)
  ambiguous <- names(which(sapply(spl, length) > 1))
  synonymes.ambiguous <- subset(synonymes, synonyme %in% ambiguous)
  synonymes <- subset(synonymes, !synonyme %in% ambiguous)
  
  # Order alphabetically
  synonymes <- synonymes[order(as.character(synonymes$name)),]

  synonymes
 
}
