#' @title Estimate Missing Dimensions
#' @description Estimate missing dimension information.
#' @param gatherings Available gatherings information
#' @param height Available height information
#' @param width Available width information
#' @param obl Indicates height smaller than width 
#' @param dimension.table Document dimension table (from dimension_table())
#' @param sheet.dimension.table Table to estimate sheet area. 
#' 	  If not given, the table given by sheet_sizes() is used by default.
#' @return Augmented dimension information
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # estimate_document_dimensions(gatherings = 2, height = 44)
#' @keywords utilities
estimate_document_dimensions <- function (gatherings = NA, height = NA, width = NA, obl = NULL, dimension.table = NULL, sheet.dimension.table = NULL) {

  if (is.null(gatherings) || length(gatherings) == 0) { gatherings <- NA }
  if (is.null(height) || length(height) == 0) { height <- NA }
  if (is.null(width) || length(width) == 0)  { width <- NA }

  # Ensure the inputs are of right format		     
  gatherings <- as.character(gatherings)
  width <- as.numeric(as.character(width))
  height <- as.numeric(as.character(height))

  if (length(grep("NA", gatherings)) > 0) { gatherings <- NA }
  if (length(grep("NA", width)) > 0)  { width <- NA }
  if (length(grep("NA", height)) > 0) { height <- NA }

  if (all(is.na(c(gatherings, height, width)))) {
    return(list(gatherings = gatherings, height = height, width = width))
  }

  # Height and gatherings given
  if (is.na(width) && !is.na(height) && !is.na(gatherings)) {

    if (any(gatherings == colnames(dimension.table))) {

      s <- dimension.table[dimension.table$height == round(height), gatherings]
      width <- as.numeric(as.character(s))

      if (length(width) == 0 || is.na(width)) {

        message(paste("Height (", height,") does not correspond to the gatherings (", gatherings, ") and width is not provided: trying to match width instead", sep = ""))
        width <- height
        height <- median(na.omit(as.numeric(as.character(dimension.table[which(as.character(dimension.table[, gatherings]) == round(width)), "height"]))))
       }

       if (is.na(height) || is.na(width)) {
         # warning("Height and width could not be estimated from the dimension table. Using the default gatherings size instead.")
    	 ind <- which(as.character(sheet.dimension.table$gatherings) == gatherings)
    	 width <- sheet.dimension.table[ind, "width"]
    	 height <- sheet.dimension.table[ind, "height"]
       }

    } else {
      # warning(paste("gatherings", gatherings, "not available in conversion table!"))
    }
  } else if (!is.na(width) && is.na(height) && !is.na(gatherings)) {
    # Else if width and gatherings given
    # warning("Only width and gatherings given, height is estimated from table !")
    g <- gatherings

    if (any(g == colnames(dimension.table))) {
      height <- median(na.omit(as.numeric(as.character(dimension.table[which(as.character(dimension.table[, g]) == round(width)), "height"]))))
    } else {
      # warning(paste("gatherings", g, "not available in conversion table!"))
    }

  } else if (is.na(width) && !is.na(height) && is.na(gatherings)) {

    # Only height given
    # pick the closest matches from the table
    hh <- abs(as.numeric(as.character(dimension.table$height)) - height)

    ind <- which(hh == min(hh, na.rm = TRUE))
    width <- as.numeric(as.character(dimension.table[ind, "NA"]))

    if (is.na(width)) {
      # warning(paste("No width found for height ", height, " and gatherings ", gatherings, sep = ""))
      return(
        list(gatherings = unname(gatherings),
       	     height = unname(height),
       	     width = unname(width),
       	     obl = unname(obl))
         )
    }

    # if multiple hits, pick the closest
    width <- mean(width, na.rm = TRUE)

    # Estimate gatherings
    gatherings <- estimate_document_dimensions(gatherings = NA, height = round(height), width = round(width), dimension.table = dimension.table, sheet.dimension.table = sheet.dimension.table)$gatherings    

  } else if (is.na(width) && is.na(height) && !is.na(gatherings)) {

    # Only gatherings given
    ind <- which(as.character(sheet.dimension.table$gatherings) == gatherings)
    #if (length(ind) == 0) {
      # warning(paste("gatherings", gatherings, "not available in bibliographica::sheet_area conversion table"))
    #}
      
    width <- sheet.dimension.table[ind, "width"]
    height <- sheet.dimension.table[ind, "height"]

  } else if (!is.na(width) && !is.na(height) && is.na(gatherings)) {

    # width and height given; estimate gatherings
    # The closest matched for height
    hs <- as.numeric(as.character(dimension.table$height))

    hdif <- abs(hs - height)

    inds <- which(hdif == min(hdif, na.rm = TRUE))

    # corresponding widths
    ws <- dimension.table[inds, ]
    ginds <- c()

    for (wi in 1:nrow(ws)) {
      d <- abs(as.numeric(as.character(unlist(ws[wi,], use.names = FALSE))) - width)
      ginds <- c(ginds, setdiff(which(d == min(d, na.rm = TRUE)), 1:2))
    }
    gs <- unique(colnames(dimension.table)[unique(ginds)])

    # If gatherings is uniquely determined
    if (length(gs) == 1) {
      gatherings <- gs
    } else {
      # warning(paste("Ambiguous gatherings - not determined for width / height ", width, height, paste(gs, collapse = "/")))
    }
  } else if (!is.na(width) && is.na(height) && is.na(gatherings)) {
    # Only width given
    height <- as.numeric(dimension.table[which.min(abs(as.numeric(dimension.table[, "NA"]) - as.numeric(width))), "NA"])
    
    # If multiple heights match, then use average
    height <- mean(height, na.rm = TRUE)

    # Estimate gatherings
    gatherings <- estimate_document_dimensions(gatherings = NA, height = height, width = width, dimension.table = dimension.table)$gatherings
  }


  if (length(width) == 0) {width <- NA}
  if (length(height) == 0) {height <- NA}
  if (length(gatherings) == 0) {gatherings <- NA}

  if (is.na(width)) {width <- NA}
  if (is.na(height)) {height <- NA}
  if (is.na(gatherings)) {gatherings <- NA}

  height <- as.numeric(height)
  width <- as.numeric(width)

  # In obl width > height

  if (length(obl) > 0 && !is.na(obl) && any(obl)) {

    hw <- cbind(height = height, width = width)
    inds <- 1

    if (length(obl) > 1) {
      inds <- which(obl)
    }

    for (i in inds) {
      xx <- hw[i, ]
      if (sum(is.na(xx)) == 0) {
        hw[i, ] <- sort(xx)
      }
    }
    height <- hw[, "height"]
    width <- hw[, "width"]     
  }

  list(gatherings = unname(gatherings),
       height = unname(height),
       width = unname(width),
       obl = unname(obl))
}


