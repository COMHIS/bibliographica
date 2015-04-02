#' @title sheet_sizes
#' @description Read sheet size table
#'
#' @param ... Arguments to be passed
#' @return Sheet size table
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("estc")
#' 
#' @examples sheetsizes <- sheet_sizes()
#' @keywords utilities
sheet_sizes <- function (...) {  
  # Read the mapping table
  # FIXME: move this out of the function
  f <- system.file("extdata/sheetsizes.csv", package = "estc")
  tab <- as.data.frame(read.csv(f))
  tab[,1] <- str_trim(as.character(tab[,1]))
  tab[,2] <- str_trim(as.character(tab[,2])) 

  tab  
}


