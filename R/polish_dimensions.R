#' @title polish_dimensions
#' @description Polish dimension field for many documents at once
#'
#' @param x A vector of dimension notes
#' @param fill Logical. Estimate and fill in the missing information: TRUE/FALSE
#' @param dimtab Dimension mapping table
#' @return Dimension table
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' 
#' @examples polish_dimensions(c("2fo", "14cm"), fill = TRUE)
#' @keywords utilities
polish_dimensions <- function (x, fill = FALSE, dimtab = NULL) {

  sheetsizes <- sheet_sizes()

  s <- as.character(x)
  tab <- t(sapply(as.character(s), function (x) {polish_dimension(x, sheetsizes)}))
  rownames(tab) <- NULL
  tab <- as.data.frame(tab)

  # Convert to desired format
  tab$original <- as.character(tab$original)
  tab$gatherings <- order_gatherings(tab$gatherings)
  tab$width <- as.numeric(as.character(tab$width))
  tab$height <- as.numeric(as.character(tab$height))

  if (fill) {
    tab <- augment_dimension_table(tab, dimtab = dimtab)
  }

  tab$gatherings <- order_gatherings(tab$gatherings)

  tab

}



#' @title polish_dimension
#' @description Polish dimension field of a single document
#'
#' @param s A dimension note (a single string of one document)
#' @param sheetsizes Sheet size conversion table
#' @return Polished dimension information with the original string 
#' 	   and gatherings and cm information collected from it
#'
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("estc")
#' 
#' @examples # polish_dimension("4")
#' @keywords internal
polish_dimension <- function (s, sheetsizes) {

  if (length(s) > 1) {
    stop("This internal function works only for a single-element character vector")
  }

  s <- sorig <- as.character(s) # NOTE: ⁰ is ASCII char 167 if needed
       	     			#  	  and c2b0 in UTF8 
				# Unicode "\U00B0"; "\U00BA"; "\U02DA"
  for (i in 1:3) {
    s <- remove_endings(s, c(" ", "\\.", "\\,", "\\;", "\\:"))
  }

  # Harmonize terms
  s <- harmonize_dimension(s, sheetsizes)

  # ------------------------------------------------------------

  # "small"
  small <- FALSE
  if (length(grep("sm", s)) > 0) {
    s <- gsub("sm ", "", gsub("sm.", "", s))
    small <- TRUE
  }

  # Obl: height < width
  obl <- FALSE
  if (length(grep("obl.", s))) {
    s <- gsub("obl.", "", s)
    obl <- TRUE
  }

  # Handle long
  long <- FALSE
  if (length(grep("long", s)) > 0) {
    long <- TRUE
    s <- gsub("long", "", s)
  }

  # -------------------------------------------

  # Pick all dimension info

  vol <- width <- height <- NA

  # Units not given. Assume the number refers to the gatherings (not cm)
  x <- unique(str_trim(unlist(strsplit(s, " "))))
  if (length(grep("cm", x)) == 0 && length(grep("[0-9]?o", x)) == 0) {
    if (length(x) == 1) {
      vol <- gsub("\\(", "", gsub("\\)", "", x))
    }
  }

  # Pick gatherings measures separately
  x <- str_trim(unlist(strsplit(s, " ")))

  hits <- grep("[0-9]?o", x)

  if (length(hits) > 0) {
    x <- gsub("\\(", "", gsub("\\)", "", x[hits]))
    x <- gsub("to$", "", x)
    vols <- as.numeric(unique(x))
    if (length(vols) == 1) {
      vol <- vols[[1]]
    } else {
      # Ambiguous gatherings info
      vol <- NA
    }
  }

  # Handle NA, long and small
  if (is.na(vol)) {
    gatherings <- vol
  } else if (long) {
    gatherings <- paste(vol, "long", sep = "")
  } else if (small) {
    gatherings <- paste(vol, "small", sep = "")
  } else if (length(grep("oadside", vol)) == 0 & vol %in% gsub("to", "", sheetsizes[,"gatherings"])) {
    # Convert gatherings to standard format
    gt <- gatherings_table()
    gatherings <- gt[match(paste(vol, "to", sep = ""), gt$Alternate), "Standard"]
  } else {
    gatherings <- NA
  }
  gatherings <- gsub("NA", NA, gatherings)

  # Ambiguous CM information
  x <- unique(str_trim(unlist(strsplit(s, " "))))
  if (length(grep("x", x)) > 1) {
    width <- height <- NA
  }

  # Pick CM x CM format separately when it is unambiguous
  if (length(grep("cm", x)) > 0 && length(grep("x", x)) == 1) {
      # Then pick the dimensions
      x <- str_trim(unlist(strsplit(x, "cm")))
      x <- str_trim(unlist(strsplit(x, " ")))
      i <- which(x == "x")
      height <- as.numeric(str_trim(gsub("\\)", "", x[i+1])))
      width <- as.numeric(str_trim(gsub("\\(", "", x[i-1])))
  } 

  # Pick CM format (single value) separately when it is unambiguous
  if (length(grep("cm", x)) > 0 && length(grep("x", x)) == 0) {
      # Then pick the dimensions
      x <- str_trim(gsub("\\(", "", gsub("\\)", "", unlist(strsplit(x, " ")))))
      i <- which(x == "cm")
      height <- as.numeric(str_trim(x[i-1]))
      width <- NA
  } 

  # Obl: height < width
  if (!is.na(width) && !is.na(height)) {
    dims <- sort(c(width, height))
    if (obl) {
      dims <- rev(dims)
    } 
    width <- dims[[1]]
    height <- dims[[2]]
  }

  # convert names to standard form
  gat <- gatherings_table()
  gatherings <- gat[match(gatherings, gat$Alternate), "Standard"]

  # Return
  list(original = sorig, gatherings = gatherings, width = width, height = height)

}


