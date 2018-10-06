#' @title Polish Field Names
#' @description Convert the field names in the data in print-friendly form.
#' @param x Character vector
#' @param from Field to map from.
#' @param to Field to map to.
#' @return Mapped terms
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples map_fieldnames("gatherings") 
#' @keywords utilities
map_fieldnames <- function (x, from = "Data", to = "Name") {

  from <- gsub("short", "Data", from)
  from <- gsub("long", "Name", from)

  to <- gsub("short", "Data", to)
  to <- gsub("long", "Name", to)
  
  xorig <- x
  tab <- fieldname_table()
  x <- as.character(x)
  y <- map(x, from = "Data", to = "Name", synonymes = tab)
  
  y

}

