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
#' @examples # polish_dimensions(c("2fo", "14cm"), fill = TRUE)
#' @keywords utilities
polish_dimensions <- function (x, fill = FALSE, dimtab = NULL) {

 # fill = FALSE; dimtab = NULL

  s <- as.character(x)

  tab <- t(sapply(s, function (x) {
    polish_dimension(x)
    }))
  rownames(tab) <- NULL
  tab <- data.frame(tab)

  # Convert to desired format
  tab$original <- as.character(tab$original)
  tab$gatherings <- order_gatherings(tab$gatherings)
  tab$width <- suppressWarnings(as.numeric(as.character(tab$width)))
  tab$height <- suppressWarnings(as.numeric(as.character(tab$height)))

  if (fill) {
    tab <- augment_dimension_table(tab, dimtab = dimtab)
  }

  tab$gatherings <- order_gatherings(tab$gatherings)

  tab

}



#' @title polish_dimension
#' @description Polish dimension field of a single document
#'
#' @param x A dimension note (a single string of one document)
#' @param sheetsizes Sheet size conversion table
#' @return Polished dimension information with the original string 
#' 	   and gatherings and cm information collected from it
#'
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("estc")
#' 
#' @examples # polish_dimension("4")
#' @keywords internal
polish_dimension <- function (x) {

  # Harmonize terms
  sorig <- as.character(x)

  s <- harmonize_dimension(x)

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

  # Pick all dimension info
  vol <- width <- height <- NA

  # No units given. Assume the number refers to the gatherings (not cm)
  x <- unique(str_trim(unlist(strsplit(s, " "))))
  x[x == "NA"] <- NA
  x[x == "NAto"] <- NA  
  if (length(grep("cm", x)) == 0 && length(grep("[0-9]?o", x)) == 0) {
    if (length(x) == 1 && !is.na(x)) {
      x <- paste(as.numeric(gsub("\\(", "", gsub("\\)", "", x))), "to", sep = "")
    }
  } else {
    # Pick gatherings measures separately
    x <- str_trim(unlist(strsplit(s, " ")))
  }

  hits <- unique(c(grep("[0-9]?o", x), grep("bs", x)))
  gatherings <- NA
  if (length(hits) > 0) {
    x <- gsub("\\(", "", gsub("\\)", "", x[hits]))
    x <- unique(x)
    if (!length(x) == 1) {
      # Ambiguous gatherings info
      gatherings <- x
    } else {
      gatherings <- x
      if (long) {
        a <- unlist(strsplit(gatherings, ""))
	ind <- min(which(is.na(as.numeric(a))))-1
	if (is.na(ind)) {ind <- length(a)}
	gatherings <- paste(paste(a[1:ind], collapse = ""), "long", sep = "")
      } else if (small) {
        a <- unlist(strsplit(gatherings, ""))
	ind <- min(which(is.na(as.numeric(a))))-1
	if (is.na(ind)) {ind <- length(a)}
	gatherings <- paste(paste(a[1:ind], collapse = ""), "long", sep = "")
      }
    }
  }
  gatherings <- na.omit(gsub("NAto", "NA", gatherings))
  gatherings <- harmonize_dimension(gatherings)
  if (length(gatherings) == 0) {gatherings <- NA}

  if (length(unique(gatherings)) > 1) {gatherings <- NA}

  # if (length(gatherings) == 1) {gatherings <- as.list(gatherings)}

  # 4to-4to / 4to-2fo

  inds <- c(grep("^[0-9]+.o-[0-9]+.o$", gatherings), 
            grep("^[0-9]+.o-[0-9]+.o-[0-9]+.o$", gatherings))

  if (length(inds) > 0) {
    li <- lapply(gatherings[inds], function (x) {unique(unlist(strsplit(x, "-")))})
    inds2 <- which(sapply(li, length) == 1)
    inds3 <- na.omit(inds[inds2])
    if (length(inds3) > 0) {
      gatherings[inds3] <- unlist(li[inds3])
      gatherings[setdiff(inds, inds3)] <- NA
    } else {
      gatherings[inds] <- NA
    }
  }
  gatherings <- unlist(gatherings)
  inds <- grep("^[0-9]+.o, [0-9]+.o$", gatherings)
  gatherings[inds] <- gsub("\\.;", "-", gatherings[inds])


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
  list(original = sorig, gatherings = gatherings,
       width = width, height = height)

}


