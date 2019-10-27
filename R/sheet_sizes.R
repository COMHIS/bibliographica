#' @title Sheet Sizes
#' @description Read sheet size table.
#' @param ... Arguments to be passed
#' @return Sheet size table
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples sheetsizes <- sheet_sizes()
#' @keywords utilities
sheet_sizes <- function (...) {  

  # Read the mapping table
  f <- system.file("extdata/sheetsizes.csv", package = "bibliographica")
  tab <- as.data.frame(read.csv(f, sep = ","))
  tab$format <- str_trim(as.character(tab$format))
  tab$gatherings <- str_trim(as.character(tab$gatherings)) 
  tab$gatherings <- order_gatherings(tab$gatherings)

  tab  
}


