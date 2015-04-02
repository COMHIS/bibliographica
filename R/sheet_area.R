#' @title sheet_area
#' @description Sheet area in cm2
#'
#' @param x Sheet size 
#' @param verbose Verbose
#'
#' @return Sheet area (cm2)
#'
#' @export
#' @details Sheet size is calculated according to the table given as output from call sheet_area()
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples sheet_area("2to")
#' @keywords utilities
sheet_area <- function (x = NULL, verbose = TRUE) {
	
  # Read the mapping table
  f <- system.file("extdata/sheetsizes.csv", package = "bibliographica")
  tab <- as.data.frame(read.csv(f))
  tab[,1] <- str_trim(as.character(tab[,1]))
  tab[,2] <- str_trim(as.character(tab[,2]))
  for (i in names(tab[3:ncol(tab)])) {
    tab[,i] <- as.numeric(str_trim(as.character(tab[,i])))
  }

  if (!is.null(x)) {
    x <- as.character(x)
    ind <- which((tab$sheet %in% x) | (as.character(tab$gatherings) %in% x))
    if (length(ind) > 0) {
      area <- tab[ind, "area"]
      if (verbose) {
        message(paste("The input", x, "corresponds to", tab[ind, "sheet"], "paper with", tab[ind, "width"], "cm width and", tab[ind, "height"], "cm height and an area of", tab[ind, "area"], "cm2"))
      }
    } else {
      warning(paste(x, "not found from the sheet size conversion table"))
      return(NA)
    }
  } else {
    return(tab)
  }

  area

}

