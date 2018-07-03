#' @title Default Colors
#' @description Default colors for different variables.
#' @param x Name of the variable type ("language"; "gatherings")
#' @return Named character vector of default colors
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples \dontrun{col <- default_colors("language")}
#' @keywords utilities
default_colors <- function (x) {

  if (x == "language") {
    #http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
    #https://www.r-graph-gallery.com/42-colors-names/
    col <- c(
      English = "deepskyblue3",
      Latin = "brown",
      Finnish = "darkblue",
      Swedish = "darkgreen",
      German = "black",
      Dutch = "orange",
      "Greek Ancient to 1453 " = "darkolivegreen4",  
      French = "blue", 
      Undetermined = "lightgray",
      "Multiple languages" = "darkgray", 
      Other = "gray")
    } else if (x == "gatherings") {
      col <- map(unique(gatherings_table()$Name),
                   from = "Name",
		   to = "Color",
		   gatherings_table(), keep.names = TRUE)
    } else {
      warning("Unrecognized variable name in default_colors input.")
      return(NULL)
    }

  col

}

