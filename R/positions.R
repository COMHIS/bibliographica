

position_arabics <- function (x) {

  positions <- rep(FALSE, length(x))

  for (i in 1:length(x)) {
    spl <- unlist(strsplit(x[[i]], "-"), use.names = FALSE)
    if (any(sapply(spl, function (x) {!is.na(suppressWarnings(as.numeric(x)))}, USE.NAMES = FALSE))) {
      positions[[i]] <- TRUE
    }
  }

  positions

}


position_squarebrackets <- function (x) {

  indsa <- sort(which(position_arabics(x))) # arabics
  indsb <- sort(unique(c(grep("\\[", x), grep("\\]", x)))) # square brackets
  inds <- sort(setdiff(indsb, indsa)) # square brackets that are not of form 91-[93]

  # Indicate positions in the page count sequence
  positions <- rep(FALSE, length(x))
  positions[inds] <- TRUE

  # Calculation of square bracket pages
  # depends on their position and dashes
  positions

}

position_sheets <- function (x) {

  grepl("sheet", x)

}


position_plates <- function (x) {

  # pages: pages calculated from plates separately for each position
  # positions: Positions for plate pages on the page count sequence
  # total: total pages calculated from plates
  grepl("pages calculated from plates", x)

}
