#' @title remove_print_statements
#' @description Remove print statements
#'
#' @param x a vector
#' @return Polished vector
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples x2 <- remove_print_statements("Printed in London")
#' @keywords utilities
remove_print_statements <- function (x) {

  # Harmonize print statements
  x <- harmonize_print_statements(x)$name

  for (w in c("at", "in", "by", "for")) {
    x <- gsub(paste0("printed ", w), "", x)
    x <- gsub(paste0("print ", w), "", x)    
  }
  x <- condense_spaces(x)

  # remove sine loco
  x <- remove_sl(x)

  # handle some odd cases manually
  # FIXME: this is estc-specific, move there
  x[x=="122 s"] <- NA
  x[x=="204 s"] <- NA
  x[x=="2 p"] <- NA
  x <- gsub("2. p.;","",x)
  x <- gsub("^(.*?);.*$","\\1",x) # nb. non-greedy match

  x[x==""] <- NA

  x

}
