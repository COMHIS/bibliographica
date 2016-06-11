#' @title Sheet Area
#' @description Sheet area in cm2.
#' @param x Sheet size 
#' @param sheet.dimension.table Table to estimate sheet area. 
#' 	  If not given, the table given by sheet_sizes() is used by default.
#' @param verbose Verbose
#' @return Sheet area (cm2)
#' @export
#' @details Sheet size is calculated according to the table given as output 
#'          from call sheet_area()
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples sheet_area("2to")
#' @keywords utilities
sheet_area <- function (x = NULL, sheet.dimension.table = NULL, verbose = FALSE) {
	
  if (is.null(sheet.dimension.table)) {

    if (verbose) {message("sheet.dimension.table not given, using sheet_sizes() mapping table by default")}
    tab <- sheet_sizes()
  } else {
    tab <- sheet.dimension.table
  }

  # Ensure correct formatting
  tab$format <- str_trim(as.character(tab$format))
  tab$gatherings <- str_trim(as.character(tab$gatherings))
  for (i in c("width", "height", "area")) {
    tab[,i] <- as.numeric(str_trim(as.character(tab[,i])))
  }

  if (!is.null(x)) {
    x <- as.character(x)
    ind <- which((tab$format %in% x) | (tab$gatherings %in% x))

    if (length(ind) > 0) {
      area <- tab[ind, "area"]

      if (verbose) {
        message(paste("The input", x, "corresponds to", tab[ind, "sheet"], "paper with", tab[ind, "width"], "cm width and", tab[ind, "height"], "cm height and an area of", tab[ind, "area"], "cm2"))
      }
    } else {
      # warning(paste(x, "not found from the sheet size conversion table"))
      return(NA)
    }
  } else {
    return(tab)
  }

  area

}

