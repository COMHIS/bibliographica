
#' @title fill_dimensions
#' @description Estimate missing entries in dimension vector where possible
#'
#' @param x dimension string 
#' @param dimension.table Given mappings between document dimensions
#' @return Augmented dimension vector
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("estc")
#' 
#' @examples # dimension.table <- dimension_table(); 
#'           #  fill_dimensions(c(original = NA, gatherings = NA, 
#'	     #			  width = 30, height = 48), 
#'	     #			  dimension.table)
#' @keywords utilities
fill_dimensions <- function (x, dimension.table) {

    # Pick the available dimension information (some may be NAs)
    h <- as.numeric(as.character(x[["height"]]))
    w <- as.numeric(as.character(x[["width"]]))
    g <- as.character(x[["gatherings"]])
    o <- x[["original"]]

    if (!g %in% colnames(dimension.table) && !is.na(g)) {
      warning(paste("gatherings ", g, " not available in dimension.table"))
    }
    
    # Estimate document widths and heights from conversion table when not available
    e <- estimate_document_dimensions(gatherings = g, height = h, width = w, dimension.table)
    w <- e$width
    h <- e$height
    g <- e$gatherings

    c(original = o, gatherings = g, width = w, height = h)

}

#' @title polish_dimensions
#' @description Polish dimension field for many documents at once
#'
#' @param s A vector of dimension notes
#' @return Table
#'
#' @export
#' 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("estc")
#' 
#' @examples # polish_dimensions(c("2", "4"))
#' @keywords utilities
polish_dimensions <- function (s) {

  sheetsizes <- sheet_sizes()

  s <- as.character(s)
  tab <- t(sapply(as.character(s), function (x) {polish_dimension(x, sheetsizes)}))
  rownames(tab) <- NULL
  tab <- as.data.frame(tab)

  # Convert to desired format
  tab$original <- as.character(tab$original)
  glevels <- c("1to", "bs", "2long", "2to", "2small", "4long", "4to", "4small", "8to", "12long", "12to", "16to", "18to", "24long", "24to", "32to", "48to", "64to", NA)
  if (!all(unique(tab$gatherings) %in% glevels)) {stop(paste("Add", paste(unlist(setdiff(unique(tab$gatherings), glevels)), collapse = "/"), "in gatherings levels in polish_dimensions function"))}
  tab$gatherings <- factor(tab$gatherings, levels = glevels)
  tab$width <- as.numeric(as.character(tab$width))
  tab$height <- as.numeric(as.character(tab$height))
    
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
    stop("This internal function works for a single-element character vector")
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
  if (length(grep("cm", x)) == 0 && length(grep("⁰", x)) == 0) {
    if (length(x) == 1) {
      vol <- gsub("\\(", "", gsub("\\)", "", x))
    }
  }

  # Pick gatherings measures separately
  x <- str_trim(unlist(strsplit(s, " ")))
  hits <- grep("⁰", x)
  if (length(hits) > 0) {
    x <- gsub("\\(", "", gsub("\\)", "", x[hits]))
    x <- gsub("⁰$", "", x)
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
    # Convert gatherings to Xto format
    gatherings <- paste(vol, "to", sep = "")
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

  # Return
  list(original = sorig, gatherings = gatherings, width = width, height = height)

}




#' @title harmonize_dimension
#' @description Harmonize dimension information 
#'
#' @param x A character vector that may contain dimension information
#' @return The character vector with dimension information harmonized
#'
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("estc")
#' 
#' @examples # harmonize_dimension("4to")
#' @keywords internal
harmonize_dimension <- function (x, sheetsizes) {

  s <- as.character(x)

  # Mark NAs
  s <- gsub("\\?⁰", " ", s)

  # Harmonize
  s <- gsub("cm", " cm", s)
  s <- gsub(".̊", "⁰", s)
  s <- gsub("4to", "4⁰", s)
  s <- gsub("8vo", "8⁰", s)
  s <- gsub("fol.", "2⁰", s)
  s <- gsub("fol", "2⁰", s)
  s <- gsub(" ⁰", "⁰", s)
  s <- gsub("₀", "⁰", s)
  s <- gsub("⁹", "⁰", s)
  s <- gsub(".̥", "⁰", s)
  s <- gsub("'", "⁰", s)
  s <- gsub("x", " x ", s)
  s <- gsub("  ", " ", s)
  s <- gsub("quarto [fewer than 50 pages]", "4⁰", s)
  s <- gsub("broadsheet", "broadside", s)

  for (ind in 1:nrow(sheetsizes)) { 
    nam <- sheetsizes[ind, "format"]
    gat <- sheetsizes[ind, "gatherings"]
    s <- gsub(nam, gsub(gat, "to", "⁰"), s)
  }

  # With standard gatherings 1/2 = 2
  s <- gsub("1/", "", s)

  s

}



#' @title remove_dimension_info
#' @description Remove dimension information from a single document
#'
#' @param x A character vector that may contain dimension information
#' @return The character vector with dimension information removed
#'
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("estc")
#' 
#' @examples # s <- remove_dimension("4to", sheet_sizes())
#' @export
#' @keywords internal
remove_dimension_info <- function (x, sheetsizes) {

  s <- harmonize_dimension(x, sheetsizes)

  s <- gsub("[0-9]⁰", " ", s)
  s <- gsub("[0-9] x [0-9][0-9]\\.[0-9] cm\\.", "", s)
  s <- gsub("[0-9][0-9]-[0-9][0-9] cm", " ", s)

  s <- gsub("[0-9][0-9][0-9] cm", " ", s)
  s <- gsub("[0-9][0-9] cm", " ", s)
  s <- gsub("[0-9] cm", " ", s)

  s <- remove_endings(s, c(":", ";", "\\."))

  s[s == ""] <- NA

  s

}

