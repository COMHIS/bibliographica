#' @title Map Gathering Abbreviations and Full Names
#' @description Convert gatherings names between different formats.
#' @param x Gatherings vector
#' @param from Input terms to map (see names(gatherings_table()) for options)
#' @param to Output terms to map (see names(gatherings_table()) for options)
#' @return Mapped gatherings.
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples map_gatherings(c("8vo", "16mo"), from = "short", to = "long") 
#' @keywords utilities
map_gatherings <- function (x, from = "Standard", to = "Name") {

  from <- gsub("short", "Standard", from)
  from <- gsub("long", "Name", from)

  to <- gsub("short", "Standard", to)
  to <- gsub("long", "Name", to)
  
  xorig <- x
  tab <- gatherings_table()
  x <- as.character(x)
  y <- map(x, from = "Standard", to = "Name", synonymes = tab)
  # If original input is factor, make sure that output
  # levels are in the same order

  # Capitalize
  y <- str_to_title(y)
  
  #if (is.factor(xorig) && length(unique(x)) == length(unique(y))) {
  if (is.factor(xorig)) {  
    xorig <- droplevels(xorig)
    xlevels <- as.character(levels(xorig))
    ylevels <- map(xlevels, from = "Standard", to = "Name", synonymes = tab)    
    y <- factor(y, levels = str_to_title(unique(ylevels)))
    
  }

  if (any(na.omit(y) == "Na")) {
    y[y == "Na"] <- NA
    y <- droplevels(y)
  }
  
  y

}

