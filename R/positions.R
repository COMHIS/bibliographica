position_romans <- function (x) {

  positions <- rep(FALSE, length(x))
  for (i in 1:length(x)) {
    spl <- unlist(strsplit(x[[i]], "-"))
    if (any(sapply(spl, is.roman))) {
      positions[[i]] <- TRUE
    }
  }

  list(positions = positions)

}


position_arabics <- function (x) {

  x <- as.character(x)	     

  positions <- rep(FALSE, length(x))

  for (i in 1:length(x)) {
    spl <- unlist(strsplit(x[[i]], "-"))
    if (any(sapply(spl, function (x) {!is.na(suppressWarnings(as.numeric(x)))}))) {
      positions[[i]] <- TRUE
    }
  }

  list(positions = positions)

}


position_squarebrackets <- function (x) {

  indsa <- sort(which(position_arabics(x)$positions)) # arabics
  indsb <- sort(unique(c(grep("\\[", x), grep("\\]", x)))) # square brackets
  inds <- sort(setdiff(indsb, indsa)) # square brackets that are not of form 91-[93]

  # Indicate positions in the page count sequence
  positions <- rep(FALSE, length(x))
  positions[inds] <- TRUE

  # Calculation of square bracket pages
  # depends on their position and dashes so
  # only indicate position for now
  list(positions = positions)

}

position_sheets <- function (x) {

  # Pick separately pages estimated from sheets
  inds <- grep("sheet", x)

  # Indicate positions in the page count sequence
  positions <- rep(FALSE, length(x))
  positions[inds] <- TRUE

  x.sheets <- 0
  if (length(inds) > 0) { 
    x.sheets <- x[inds]
    x <- x[setdiff(1:length(x), inds)]
  }
  # Convert sheets to pages
  x <- suppressWarnings(as.numeric(sheets2pages(x.sheets)))

  list(pages = x, positions = positions, total = sum(na.omit(x)))


}


position_plates <- function (x) {

  # Pick separately the pages estimated from plates
  inds <- grep("pages calculated from plates", x)
  
  # Indicate plate positions in the page count sequence
  positions <- rep(FALSE, length(x))
  positions[inds] <- TRUE

  x.plates <- 0
  if (length(inds) > 0) { 
    x.plates <- x[inds]
    x <- x[setdiff(1:length(x), inds)]
  }

  x <- suppressWarnings(as.numeric(str_trim(gsub("pages calculated from plates", "", x.plates))))

  # pages: pages calculated from plates separately for each position
  # positions: Positions for plate pages on the page count sequence
  # total: total pages calculated from plates
  list(pages = x, positions = positions, total = sum(na.omit(x)))

}
