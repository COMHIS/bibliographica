#' @title Polish Dimension
#' @description Polish dimension field of a single document
#' @param x A dimension note (a single string of one document)
#' @param synonyms synonyms
#' @return Polished dimension information with the original string 
#' 	   and gatherings and cm information collected from it
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # polish_dimension("4")
#' @keywords internal
polish_dimension <- function (x, synonyms) {

  # Harmonize terms
  s <- sorig <- x

  # "small"
  small <- FALSE
  if (length(grep("sm", s)) > 0) {
    s <- gsub("sm ", "", gsub("sm.", "", s))
    small <- TRUE
  }

  # Obl: height < width
  obl <- FALSE
  if (length(grep("obl", s))>0) {
    s <- gsub("obl", "", s)
    obl <- TRUE
  }

  # Handle long
  long <- FALSE
  if (length(grep("long", s)) > 0 || length(grep("\\+", s)) > 0) {
    long <- TRUE
    s <- gsub("long", "", s)
    s <- gsub("\\+", "", s)
  }

  # Pick all dimension info
  vol <- width <- height <- NA
  s <- condense_spaces(s)

  # No units given. Assume the number refers to the gatherings (not cm)
  x <- unique(str_trim(unlist(strsplit(s, " "), use.names = FALSE)))
  x[x == "NA"]   <- NA
  x[x == "NAto"] <- NA

  if (length(grep("cm", x)) == 0 && length(grep("[0-9]?o", x)) == 0) {
    if (length(x) == 1 && !is.na(x)) {
      x <- paste(as.numeric(x), "to", sep = "")
    }
  } else {
    # Pick gatherings measures separately
    x <- str_trim(unlist(strsplit(s, " "), use.names = FALSE))
  }

  hits <- unique(c(grep("[0-9]+[a-z]o", x), grep("bs", x)))

  gatherings <- NA
  if (length(hits) > 0) {
    x <- gsub(";;", "", x[hits])

    x <- unique(x)
    if (!length(x) == 1) {
      # Ambiguous gatherings info
      gatherings <- x
    } else {
      gatherings <- x
      if (long) {
        a <- unlist(strsplit(gatherings, ""), use.names = FALSE)
	ind <- min(which(is.na(as.numeric(a))))-1
	if (is.na(ind)) {ind <- length(a)}
	gatherings <- paste(paste(a[1:ind], collapse = ""), "long", sep = "")
      } else if (small) {
        a <- unlist(strsplit(gatherings, ""), use.names = FALSE)
	ind <- min(which(is.na(as.numeric(a))))-1
	if (is.na(ind)) {ind <- length(a)}
	gatherings <- paste(paste(a[1:ind], collapse = ""), "small", sep = "")
      }
    }
  }

  gatherings <- harmonize_dimension(gatherings, synonyms)

  if ( length(gatherings) == 0 ) { gatherings <- NA }
  if ( length(unique(gatherings)) > 1 ) { gatherings <- NA }

  # 4to-4to / 4to-2fo
  inds <- c(grep("^[0-9]+.o-[0-9]+.o$", gatherings), 
            grep("^[0-9]+.o-[0-9]+.o-[0-9]+.o$", gatherings))

  if (length(inds) > 0) {
    li <- lapply(gatherings[inds], function (x) {unique(unlist(strsplit(x, "-"), use.names = FALSE))})
    inds3 <- na.omit(inds[sapply(li, length) == 1])
    if (length(inds3) > 0) {
      gatherings[inds3] <- unlist(li[inds3], use.names = FALSE)
      gatherings[setdiff(inds, inds3)] <- NA
    } else {
      gatherings[inds] <- NA
    }
  }
  gatherings <- unlist(gatherings, use.names = FALSE)
  inds <- grep("^[0-9]+.o, [0-9]+.o$", gatherings)
  gatherings[inds] <- gsub("\\.;", "-", gatherings[inds])

  # Ambiguous CM information
  x <- unique(str_trim(unlist(strsplit(s, " "), use.names = FALSE)))
  if (length(grep("x", x)) > 1) {
    width <- height <- NA
  }

  # Pick CM x CM format separately when it is unambiguous
  if (length(grep("cm", x)) > 0 && length(grep("x", x)) == 1) {
      # Then pick the dimensions
      x <- unlist(strsplit(x, "cm"), use.names = FALSE)
      x <- str_trim(unlist(strsplit(x, " "), use.names = FALSE))
      i <- which(x == "x")
      if (is.numeric(str_trim(x[i+1])) && is.numeric(str_trim(x[i-1]))) {
        height <- as.numeric(str_trim(x[i+1]))
        width <- as.numeric(str_trim(x[i-1]))
      }
  } else if (length(grep("cm", x)) > 0 && length(grep("x", x)) == 0) {
      # Pick CM format (single value) separately when it is unambiguous
      # Then pick the dimensions
      x <- str_trim(unlist(strsplit(x, " "), use.names = FALSE))
      i <- which(x == "cm")
      height <- as.numeric(str_trim(x[i-1]))
      width <- NA
  } 

  # Obl: height < width
  if (!is.na(width) && !is.na(height)) {
    dims <- c(height, width)
    if (obl) {
      # NOTE: sort width and height and reverse their order if and only if
      # obl is stated; otherwise assume that the dimensions are given as height x width
      dims <- sort(dims)
      dims <- rev(dims)
    }
    width <- dims[[1]]
    height <- dims[[2]]
  }

  # If gatherings length > 1 then collapse
  gatherings <- paste(gatherings, collapse = ";")

  # Return
  list(original = sorig, gatherings = gatherings,
       width = width, height = height, obl = obl)

}


