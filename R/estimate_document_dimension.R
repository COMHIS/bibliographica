#' @title estimate_document_dimensions
#' @description Estimate missing dimension information 
#'
#' @param gatherings Available gatherings information
#' @param height Available height information
#' @param width Available width information
#' @param dimension.table Document dimension table (from dimension_table())
#' @return Augmented dimension information
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("estc")
#' 
#' @examples estimate_document_dimensions(gatherings = 2, height = 44)
#' @keywords utilities
estimate_document_dimensions <- function (gatherings = NA, height = NA, width = NA, dimension.table = NULL) {

  # Ensure the inputs are of right format		     
  gatherings <- as.character(gatherings)
  if (length(grep("NA", gatherings)) > 0) { gatherings <- NA }

  if (all(is.na(c(gatherings, height, width)))) {
    return(list(gatherings = gatherings, height = height, width = width))
  }

  # Read dimension height/width/gatherings conversions
  if (is.null(dimension.table)) {
    dimension.table <- dimension_table()
  }

  # Height and gatherings given
  if (is.na(width) && !is.na(height) && !is.na(gatherings)) {
    if (gatherings %in% colnames(dimension.table)) {

      s <- dimension.table[dimension.table$height == height, gatherings]
      width <- as.numeric(as.character(s))

      if (length(width) == 0 || is.na(width)) {
        message("height does not correspond to the gatherings. Try if width matches instead")
        width <- height
        height <- median(na.omit(as.numeric(as.character(dimension.table[which(as.character(dimension.table[, gatherings]) == width), "height"]))))
       }

       if (is.na(height) || is.na(width)) {
         warning("Height and width could not be estimated from the dimension table. Using the default gatherings size instead.")
    	 sheet_info <- sheet_area()
    	 ind <- which(as.character(sheet_info$gatherings) == gatherings)
    	 width <- sheet_info[ind, "width"]
    	 height <- sheet_info[ind, "height"]
       }

    } else {
      warning(paste("gatherings", gatherings, "not available in conversion table!"))
    }
  } else if (!is.na(width) && is.na(height) && !is.na(gatherings)) {
    # Else if width and gatherings given
    warning("Only width and gatherings given, height is estimated from table !")
    g <- gatherings
    if (g %in% colnames(dimension.table)) {
      height <- median(na.omit(as.numeric(as.character(dimension.table[which(as.character(dimension.table[, g]) == width), "height"]))))
    } else {
      warning(paste("gatherings", g, "not available in conversion table!"))
    }

  } else if (is.na(width) && !is.na(height) && is.na(gatherings)) {

    # Only height given
    width <- as.numeric(as.character(dimension.table[dimension.table$height == height, "NA"]))

  } else if (is.na(width) && is.na(height) && !is.na(gatherings)) {

    # Only gatherings given
    sheet_info <- sheet_area()
    ind <- which(as.character(sheet_info$gatherings) == gatherings)
    width <- sheet_info[ind, "width"]
    height <- sheet_info[ind, "height"]

  } else if (!is.na(width) && !is.na(height) && is.na(gatherings)) {

    # Only width and height given; estimate gatherings
    ws <- dimension.table[dimension.table$height == height, ]
    ginds <- c()
    for (wi in 1:nrow(ws)) {
      d <- abs(as.numeric(as.character(unlist(ws[wi,]))) - width)
      ginds <- c(ginds, setdiff(which(d == min(na.omit(d))), 1:2))
    }
    gs <- unique(colnames(dimension.table)[ginds])
    # If gatherings is uniquely determined
    if (length(gs) == 1) {
      gatherings <- gs
    }
  }

  if (length(width) == 0) {width <- NA}
  if (length(height) == 0) {height <- NA}
  if (length(gatherings) == 0) {gatherings <- NA}
  #if (is.na(gatherings) || gatherings == "NAto" || gatherings == "NAlong") {gatherings <- NA}

  list(gatherings = gatherings, height = height, width = width)
}


