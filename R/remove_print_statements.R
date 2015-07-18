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
#' @examples # x2 <- remove_print_statements("Printed in London")
#' @keywords utilities
remove_print_statements <- function (x) {

  # Harmonize print statements			
  x <- harmonize_print_statements(x)
  x <- gsub("print", "", x)
  
  # remove sine loco
  x <- remove_sl(x)

  # some odd cases manually
  x[x=="122 s"] <- NA
  x[x=="204 s"] <- NA
  x[x=="2 p"] <- NA
  x <- gsub("2. p.;","",x)
  x <- gsub("^(.*?);.*$","\\1",x) # nb. non-greedy match
  x[x==""] <- NA

  x

}
