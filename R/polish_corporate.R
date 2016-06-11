#' @title Polish corporate
#' @description Polish corporate
#' @param x A vector of corporate entries
#' @return Polished vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # x2 <- polish_corporate("unknown")
#' @keywords utilities
polish_corporate <- function (x) {
  x <- as.character(x)		 
  x <- gsub("S\\.n\\. \\(tuntematon kirjapaino\\)", NA, x)
  x
}