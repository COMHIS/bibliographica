#' @title Default Colors
#' @description Default colors for different variables.
#' @param x Name of the variable type ("language"; "gatherings")
#' @param v Optional. Vector of elements to color.
#' @return Named character vector of default colors
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @export
#' @examples \dontrun{col <- default_colors("language")}
#' @keywords utilities
default_colors <- function (x, v=NULL) {

  if (x == "catalog") {

    col <- c(
      FNB = "darkblue",
      SNB = "gold",
      ESTC = "darkred",
      HPB = "darkgreen",
      STCN = "orange",
      STCV = "tomato",      
      Other = "gray")


  } else if (x == "language") {
    #http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
    #https://www.r-graph-gallery.com/42-colors-names/
    col <- c(
      English = "deepskyblue3",
      Latin = "brown",
      Finnish = "darkblue",
      Swedish = "darkgreen",
      Scots = "lightblue",      
      German = "black",
      Dutch = "orange",
      #"Greek Ancient to 1453 " = "darkolivegreen4",
      "Greek Ancient to 1453 " = "bisque3",        
      "Greek, Ancient (to 1453)" = "bisque3",        
      French = "blue", 
      Undetermined = "lightgray",
      "Multiple languages" = "darkgray", 
      Other = "gray")
      
    } else if (x == "gatherings") {

      col <- map(unique(gatherings_table()$Name),
                   from = "Name",
		   to = "Color",
		   gatherings_table(), keep.names = TRUE)

      # If vector of getherings is provided then directly provide colors
      # for the entries
      if (!is.null(v)) {
        x <- as.character(tolower(v))
        col <- col[tolower(x)]
      }

    } else if (x == "gatherings_short") {

      col <- map(unique(gatherings_table()$Short),
                   from = "Short",
		   to = "Color",
		   gatherings_table(), keep.names = TRUE)

      # If vector of getherings is provided then directly provide colors
      # for the entries
      if (!is.null(v)) {
        x <- as.character(tolower(v))
        col <- col[tolower(x)]
      }

    } else {
      warning("Unrecognized variable name in default_colors input.")
      return(NULL)
    }

  col

}

